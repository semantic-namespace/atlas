(ns atlas.ontology.llm-profile-test
  (:require
   [clojure.test :refer [deftest is use-fixtures testing]]
   [atlas.registry :as registry]
   [atlas.registry.lookup :as lookup]
   [atlas.datalog :as datalog]
   [atlas.invariant :as inv]
   [atlas.ontology.llm-profile :as profile]))

;; A domain's classifier, as yorba-clj's authz/class-for would be: a tool's
;; class from its aspects.
(defn classify [tool]
  (let [aspects (lookup/identity-for tool)]
    (cond (nil? aspects) nil
          (contains? aspects :effect/write) :authz/mutate
          (contains? aspects :effect/send) :authz/external-write
          (contains? aspects :effect/read) :authz/read)))

;; What a surface serves, as a domain would say it: served name and registry id.
(defn served [{:surface/keys [scope]}]
  (case scope
    :mcp-scope/system-observer [{:name "list_fees" :dev-id :tool/list-fees}
                                {:name "delete_fee" :dev-id :tool/delete-fee}
                                {:name "orphan_tool" :dev-id nil}]
    :mcp-scope/dev [{:name "list_fees" :dev-id :tool/list-fees}
                    {:name "unsubscribe" :dev-id :tool/unsubscribe}]
    []))

(use-fixtures :each
  (fn [f]
    (registry/reset-all!)
    (datalog/reset-all!)
    (require 'atlas.ontology.execution-function :reload)
    (require 'atlas.ontology.llm-prompt :reload)
    (require 'atlas.ontology.llm-profile :reload)
    (f)
    (registry/reset-all!)
    (datalog/reset-all!)))

(defn- seed! []
  (registry/register! :tool/list-fees :atlas/execution-function #{:domain/fees :effect/read} {})
  (registry/register! :tool/delete-fee :atlas/execution-function #{:domain/fees :effect/write} {})
  (registry/register! :tool/unsubscribe :atlas/execution-function #{:domain/lists :effect/send} {})
  (registry/register! :tool/mystery :atlas/execution-function #{:domain/misc} {})
  (registry/register!
   :classifier/test-authz :atlas/tool-classifier #{:classifier/test}
   {:classifier/classes #{:authz/read :authz/mutate :authz/external-write}
    :classifier/fn 'atlas.ontology.llm-profile-test/classify
    :classifier/served-fn 'atlas.ontology.llm-profile-test/served})
  (registry/register!
   :prompt/review-fees :atlas/llm-prompt #{:domain/fees :intent/read}
   {:llm-prompt/summary "Review fees" :llm-prompt/file "review.md" :llm-prompt/mcp-deps [:tool/list-fees]})
  (registry/register!
   :llm-profile/developer-base :atlas/llm-profile #{:role/developer :intent/base}
   {:llm-profile/summary "Every developer session"
    :llm-profile/surfaces #{{:surface/env :env/dev :surface/scope :mcp-scope/dev}}
    :llm-profile/classifier :classifier/test-authz
    :llm-profile/class-policy {:authz/read :allow :authz/mutate :ask :authz/external-write :ask}
    :llm-profile/commands {:deny ["psql *prod*"]}})
  (registry/register!
   :llm-profile/prod-observe :atlas/llm-profile #{:role/developer :env/production :intent/observe}
   {:llm-profile/summary "Production, read-only"
    :llm-profile/extends :llm-profile/developer-base
    :llm-profile/surfaces #{{:surface/env :env/production :surface/scope :mcp-scope/system-observer}}
    :llm-profile/class-policy {:authz/mutate :deny :authz/external-write :deny}
    :llm-profile/commands {:deny ["kubectl * delete *"]}
    :llm-profile/prompts #{:prompt/review-fees}}))

(defn- violations-of [inv-key]
  (->> (:violations (inv/check-all))
       (filter #(= inv-key (:invariant %)))))

(deftest decisions-come-from-the-class
  (seed!)
  (is (= {:tool/delete-fee :deny :tool/list-fees :allow :tool/mystery :deny :tool/unsubscribe :deny}
         (profile/decisions :llm-profile/prod-observe [:tool/list-fees :tool/delete-fee :tool/unsubscribe :tool/mystery])))
  (is (= :ask (profile/decision :llm-profile/developer-base :tool/delete-fee)))
  (testing "a tool the classifier can't class is denied"
    (is (nil? (profile/class-of :llm-profile/developer-base :tool/mystery)))
    (is (= :deny (profile/decision :llm-profile/developer-base :tool/mystery)))))

(deftest a-profile-extends-its-base
  (seed!)
  (let [p (profile/effective :llm-profile/prod-observe)]
    (is (= {:authz/read :allow :authz/mutate :deny :authz/external-write :deny} (:llm-profile/class-policy p))
        "the policy merges class by class")
    (is (= ["psql *prod*" "kubectl * delete *"] (get-in p [:llm-profile/commands :deny])) "command patterns add up")
    (is (= #{{:surface/env :env/production :surface/scope :mcp-scope/system-observer}} (:llm-profile/surfaces p))
        "surfaces are restated, never widened by inheritance")
    (is (= :classifier/test-authz (:llm-profile/classifier p)) "inherited when not restated")))

(deftest a-profile-is-not-made-of-what-it-references
  (seed!)
  (registry/compile!)
  (testing "its identity stays its own after compile: no prompt, base or classifier aspects"
    (is (= #{:atlas/llm-profile :role/developer :env/production :intent/observe}
           (lookup/identity-for :llm-profile/prod-observe))))
  (testing "the references are found by lookup instead"
    (is (= [:llm-profile/prod-observe] (profile/hosting-profiles :prompt/review-fees)))
    (is (= [:llm-profile/prod-observe] (profile/extending-profiles :llm-profile/developer-base)))))

(deftest the-policy-is-total
  (seed!)
  (is (empty? (violations-of :llm-profile-policy-total)))
  (testing "a class the classifier gains is undecided until each profile decides it"
    (registry/register!
     :classifier/test-authz :atlas/tool-classifier #{:classifier/test}
     {:classifier/classes #{:authz/read :authz/mutate :authz/external-write :authz/admin}
      :classifier/fn 'atlas.ontology.llm-profile-test/classify})
    (is (= #{[:llm-profile/developer-base [:authz/admin]] [:llm-profile/prod-observe [:authz/admin]]}
           (set (map (juxt :profile :undecided) (:details (first (violations-of :llm-profile-policy-total))))))))
  (testing "a classifier whose function doesn't resolve is reported, never skipped"
    (registry/register!
     :classifier/test-authz :atlas/tool-classifier #{:classifier/test}
     {:classifier/classes #{:authz/read :authz/mutate :authz/external-write}
      :classifier/fn 'no.such/classify})
    (is (some #(= :classifier-fn-unresolved (:problem %))
              (:details (first (violations-of :llm-profile-policy-total)))))))

(deftest hosted-prompts-fit-their-profile
  (seed!)
  (is (empty? (violations-of :llm-profile-prompts-fit)))
  (testing "a prompt needing a tool the profile denies fails"
    (registry/register!
     :prompt/review-fees :atlas/llm-prompt #{:domain/fees :intent/read}
     {:llm-prompt/summary "Review fees" :llm-prompt/file "review.md"
      :llm-prompt/mcp-deps [:tool/list-fees :tool/delete-fee]})
    (is (= [{:profile :llm-profile/prod-observe :prompt :prompt/review-fees :tool :tool/delete-fee
             :class :authz/mutate :problem :tool-denied}]
           (:details (first (violations-of :llm-profile-prompts-fit)))))))

(deftest extends-resolves-without-cycles
  (seed!)
  (is (empty? (violations-of :llm-profile-extends-resolves)))
  (registry/register!
   :llm-profile/developer-base :atlas/llm-profile #{:role/developer :intent/base}
   {:llm-profile/summary "Every developer session"
    :llm-profile/extends :llm-profile/prod-observe
    :llm-profile/surfaces #{{:surface/env :env/dev :surface/scope :mcp-scope/dev}}
    :llm-profile/classifier :classifier/test-authz
    :llm-profile/class-policy {:authz/read :allow :authz/mutate :ask :authz/external-write :ask}})
  (is (some #(= :cycle (:problem %)) (:details (first (violations-of :llm-profile-extends-resolves))))))

(deftest export-decides-every-served-tool
  (seed!)
  (let [x (profile/export)]
    (is (= [{:surface/env :env/production :surface/scope :mcp-scope/system-observer
             :tools {"delete_fee" :deny "list_fees" :allow "orphan_tool" :deny}}]
           (get-in x [:llm-profile/prod-observe :surfaces]))
        "a served tool with no registry id is denied")
    (is (= {"list_fees" :allow "unsubscribe" :ask}
           (get-in x [:llm-profile/developer-base :surfaces 0 :tools])))
    (is (= ["psql *prod*" "kubectl * delete *"] (get-in x [:llm-profile/prod-observe :commands :deny])))))

;; ---------------------------------------------------------------------------
;; Rules: remembered answers
;; ---------------------------------------------------------------------------

;; A member's classes, finer than a developer's: a legal request in the
;; member's name is its own class, which no remembered answer may allow.
(defn classify-member [tool]
  (let [aspects (lookup/identity-for tool)]
    (cond (nil? aspects) nil
          (contains? aspects :effect/legal) :member/legal-request
          (contains? aspects :effect/send) :member/send
          (contains? aspects :effect/write) :member/change
          (contains? aspects :effect/read) :member/read)))

(defn- seed-member! []
  (registry/register! :tool/list-fees :atlas/execution-function #{:domain/fees :effect/read} {})
  (registry/register! :tool/dismiss-fee :atlas/execution-function #{:domain/fees :effect/write} {})
  (registry/register! :tool/unsubscribe :atlas/execution-function #{:domain/lists :effect/send} {})
  (registry/register! :tool/deletion-request :atlas/execution-function #{:domain/privacy :effect/send :effect/legal} {})
  (registry/register!
   :classifier/member :atlas/tool-classifier #{:classifier/member}
   {:classifier/classes #{:member/read :member/change :member/send :member/legal-request}
    :classifier/fn 'atlas.ontology.llm-profile-test/classify-member})
  (registry/register!
   :llm-profile/tidy :atlas/llm-profile #{:role/member :intent/tidy}
   {:llm-profile/summary "Help me tidy: reads free, everything else asks"
    :llm-profile/surfaces #{{:surface/env :env/production :surface/scope :mcp-scope/self}}
    :llm-profile/classifier :classifier/member
    :llm-profile/class-policy {:member/read :allow :member/change :ask
                               :member/send :ask :member/legal-request :ask}
    :llm-profile/fixed #{:member/legal-request}})
  (registry/register!
   :llm-profile/look :atlas/llm-profile #{:role/member :intent/look}
   {:llm-profile/summary "Look only"
    :llm-profile/extends :llm-profile/tidy
    :llm-profile/class-policy {:member/change :deny :member/send :deny}}))

(def ^:private newsletters {:recipient/kind :mailing-list})

(def ^:private remember-newsletters
  {:llm-rule/id :rule/u1-newsletters
   :llm-rule/profile :llm-profile/tidy
   :llm-rule/decision :allow
   :llm-rule/tool :tool/unsubscribe
   :llm-rule/facts {:recipient/kind #{:mailing-list}}})

(deftest a-remembered-answer-allows-what-the-profile-asks-about
  (seed-member!)
  (testing "without the rule, the profile asks"
    (is (= {:decision :ask :base :ask :class :member/send}
           (profile/decide :llm-profile/tidy :tool/unsubscribe {:rules [] :facts newsletters}))))
  (testing "with it, the same call is allowed, and says which rule decided"
    (is (= {:decision :allow :base :ask :class :member/send :rule :rule/u1-newsletters}
           (profile/decide :llm-profile/tidy :tool/unsubscribe {:rules [remember-newsletters] :facts newsletters}))))
  (testing "only for the facts it remembers: a person isn't a mailing list"
    (is (= :ask (:decision (profile/decide :llm-profile/tidy :tool/unsubscribe
                                           {:rules [remember-newsletters] :facts {:recipient/kind :person}})))))
  (testing "only for its profile"
    (is (= :deny (:decision (profile/decide :llm-profile/look :tool/unsubscribe
                                            {:rules [remember-newsletters] :facts newsletters}))))))

(deftest a-rule-never-loosens-what-the-profile-denies-or-fixes
  (seed-member!)
  (testing "Look only denies sending: a remembered allow changes nothing"
    (let [rule (assoc remember-newsletters :llm-rule/profile :llm-profile/look)]
      (is (= :deny (:decision (profile/decide :llm-profile/look :tool/unsubscribe {:rules [rule] :facts newsletters}))))
      (is (= [:allow-outside-ask] (map :problem (profile/rule-problems rule))))))
  (testing "a legal request asks every time, whatever was remembered"
    (let [rule (profile/draft-rule :llm-profile/tidy :tool/deletion-request {} {:decision :allow})]
      (is (= :ask (:decision (profile/decide :llm-profile/tidy :tool/deletion-request {:rules [rule]}))))
      (is (= [:class-fixed] (map :problem (profile/rule-problems rule))))))
  (testing "Look only extends tidy, so it can't free the class tidy fixed"
    (is (= #{:member/legal-request} (:llm-profile/fixed (profile/effective :llm-profile/look))))))

(deftest a-person-may-always-tighten
  (seed-member!)
  (let [no-taxes {:llm-rule/id :rule/u1-no-taxes
                  :llm-rule/profile :llm-profile/tidy
                  :llm-rule/decision :deny
                  :llm-rule/class :member/read
                  :llm-rule/facts {:folder/path #{"/Taxes"}}}]
    (testing "a deny applies even where the profile allows"
      (is (= {:decision :deny :base :allow :class :member/read :rule :rule/u1-no-taxes}
             (profile/decide :llm-profile/tidy :tool/list-fees {:rules [no-taxes] :facts {:folder/path "/Taxes"}})))
      (is (nil? (profile/rule-problems no-taxes))))
    (testing "and wins over an allow that applies too"
      (is (= :deny (:decision (profile/decide :llm-profile/tidy :tool/unsubscribe
                                              {:rules [remember-newsletters
                                                       (assoc no-taxes :llm-rule/class :member/send
                                                              :llm-rule/facts {})]
                                               :facts newsletters})))))))

(deftest a-rule-expires
  (seed-member!)
  (let [rule (assoc remember-newsletters :llm-rule/expires #inst "2026-12-01")]
    (is (= :allow (:decision (profile/decide :llm-profile/tidy :tool/unsubscribe
                                             {:rules [rule] :facts newsletters :now #inst "2026-11-12"}))))
    (is (= :ask (:decision (profile/decide :llm-profile/tidy :tool/unsubscribe
                                           {:rules [rule] :facts newsletters :now #inst "2026-12-02"}))))))

(deftest drafting-a-rule-from-an-answer
  (seed-member!)
  (let [facts {:recipient/kind :mailing-list :recipient/address "news@shop.example"}]
    (testing "by tool, keeping only the facts asked for"
      (is (= {:llm-rule/profile :llm-profile/tidy :llm-rule/decision :allow
              :llm-rule/tool :tool/unsubscribe :llm-rule/facts {:recipient/kind #{:mailing-list}}}
             (profile/draft-rule :llm-profile/tidy :tool/unsubscribe facts
                                 {:decision :allow :keep [:recipient/kind]}))))
    (testing "by class: every tool that sends, to a mailing list"
      (is (= :member/send
             (:llm-rule/class (profile/draft-rule :llm-profile/tidy :tool/unsubscribe facts
                                                  {:decision :allow :by :class :keep [:recipient/kind]})))))))

(deftest registered-rules-are-queryable-and-checked
  (seed-member!)
  (registry/register! :rule/u1-newsletters :atlas/llm-rule #{:member/u1 :domain/lists}
                      (dissoc remember-newsletters :llm-rule/id))
  (testing "a person's rules are a registry query"
    (is (= #{:rule/u1-newsletters} (set (lookup/all-with-aspect :member/u1))))
    (is (= [:rule/u1-newsletters] (map :llm-rule/id (profile/rules-for :llm-profile/tidy)))))
  (testing "decide reads them when none are passed"
    (is (= :rule/u1-newsletters (:rule (profile/decide :llm-profile/tidy :tool/unsubscribe {:facts newsletters})))))
  (testing "the invariant holds for a rule that narrows"
    (is (empty? (violations-of :llm-rule-only-narrows))))
  (testing "and catches one that would allow a legal request"
    (registry/register! :rule/u1-delete-all :atlas/llm-rule #{:member/u1}
                        {:llm-rule/profile :llm-profile/tidy :llm-rule/decision :allow
                         :llm-rule/tool :tool/deletion-request})
    (is (= [{:problem :class-fixed :rule :rule/u1-delete-all :class :member/legal-request}]
           (:details (first (violations-of :llm-rule-only-narrows)))))))
