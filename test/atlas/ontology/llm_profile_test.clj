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
    :classifier/fn 'atlas.ontology.llm-profile-test/classify})
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
