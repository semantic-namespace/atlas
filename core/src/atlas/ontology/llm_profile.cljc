(ns atlas.ontology.llm-profile
  "LLM profile ontology: registers :atlas/llm-profile, what an LLM session may
   use and do in a system, and :atlas/tool-classifier, how a domain classes
   its tools so a profile's policy can be complete.

   A profile declares:
   - the surfaces a session may load, nothing else: {:surface/env :surface/scope};
   - a decision for every class of tool its classifier derives: :allow, :ask
     or :deny;
   - what the agent's own tools may run and reach: command patterns, network
     hosts;
   - the prompts meant to run under it;
   - a base profile it extends;
   - the classes no rule may loosen (:llm-profile/fixed).

   It also registers :atlas/llm-rule, a remembered answer: a rule that narrows
   one profile, for a person or a team. A rule may deny anything, and may allow
   only what its profile asks about, outside the fixed classes. `decide` is a
   profile's decision for one call under the rules that apply, and why.

   A profile's references (prompts, base, classifier) are properties, not
   type-refs: a profile isn't made of them, so it inherits none of their
   aspects.

   The decision for a tool is never stored: it is the tool's class, from the
   domain's classifier, looked up in the profile's policy. A tool the
   classifier can't class is denied. So a new tool is decided by its class,
   in every profile, with no profile to edit.

   Compound-id = what the profile is for (role, environment, intent).
   Props       = the policy.

   Usage:
     (require '[atlas.ontology.llm-profile])"
  (:require [atlas.registry :as registry]
            [atlas.registry.lookup :as entity]
            [clojure.set :as set]
            [clojure.spec.alpha :as s]))

;; ---------------------------------------------------------------------------
;; Specs
;; ---------------------------------------------------------------------------

(s/def :llm-profile/summary string?)
(s/def :surface/env qualified-keyword?)
(s/def :surface/scope qualified-keyword?)
(s/def :llm-profile/surfaces
  (s/coll-of (s/keys :req [:surface/env :surface/scope]) :kind set? :min-count 1))
(s/def :llm-profile/classifier qualified-keyword?)
(s/def :llm-profile/class-policy (s/map-of qualified-keyword? #{:allow :ask :deny}))
(s/def :llm-profile.commands/deny (s/coll-of string? :kind vector?))
(s/def :llm-profile.commands/ask (s/coll-of string? :kind vector?))
(s/def :llm-profile/commands (s/keys :opt-un [:llm-profile.commands/deny :llm-profile.commands/ask]))
(s/def :llm-profile/network (s/coll-of string? :kind set?))
(s/def :llm-profile/prompts (s/coll-of qualified-keyword? :kind set?))
(s/def :llm-profile/extends qualified-keyword?)
;; classes whose decision no rule may loosen: a remembered answer never allows them
(s/def :llm-profile/fixed (s/coll-of qualified-keyword? :kind set?))

(s/def :llm-rule/profile qualified-keyword?)
(s/def :llm-rule/decision #{:allow :deny})
(s/def :llm-rule/class qualified-keyword?)
(s/def :llm-rule/tool qualified-keyword?)
;; {fact-key #{values}}: the rule applies to a call whose facts hold one of the values
(s/def :llm-rule/facts (s/map-of qualified-keyword? set?))
(s/def :llm-rule/expires inst?)
(s/def :llm-rule/summary string?)

(s/def :classifier/classes (s/coll-of qualified-keyword? :kind set? :min-count 1))
(s/def :classifier/fn qualified-symbol?)
;; optional: (fn [surface]) -> [{:name served-name :dev-id kw-or-nil}], the
;; tools a surface serves, so `export` can decide every one by its served name
(s/def :classifier/served-fn qualified-symbol?)

;; ---------------------------------------------------------------------------
;; :atlas/llm-profile and :atlas/tool-classifier
;; ---------------------------------------------------------------------------

(registry/register!
 :atlas/llm-profile
 :atlas/ontology
 #{:atlas/llm-profile}
 {:ontology/for  :atlas/llm-profile
  :ontology/keys [:llm-profile/summary
                  :llm-profile/surfaces
                  :llm-profile/classifier
                  :llm-profile/class-policy]})

(registry/register!
 :atlas/llm-rule
 :atlas/ontology
 #{:atlas/llm-rule}
 {:ontology/for  :atlas/llm-rule
  :ontology/keys [:llm-rule/profile
                  :llm-rule/decision]})

(registry/register!
 :atlas/tool-classifier
 :atlas/ontology
 #{:atlas/tool-classifier}
 {:ontology/for  :atlas/tool-classifier
  :ontology/keys [:classifier/classes
                  :classifier/fn]})

;; No type-refs. A type-ref says an entity is made of what it points at, and
;; compile! folds the target's aspects into the entity's identity. A profile
;; hosts prompts, extends a base and uses a classifier; it isn't made of them.
;; As type-refs, a profile carried its prompts' aspects (:atlas/llm-prompt,
;; :tier/mcp) and was served as a prompt, and a production profile inherited
;; its base's :env/dev. The references stay properties, checked by the
;; invariants below, and found by hosting-profiles and extending-profiles.

;; ---------------------------------------------------------------------------
;; Reading a profile
;; ---------------------------------------------------------------------------

(defn profiles
  "Every registered profile's dev-id, not the ontology itself."
  []
  (sort (for [id (entity/all-with-aspect :atlas/llm-profile)
              :when (not (contains? (entity/identity-for id) :atlas/ontology))]
          id)))

(defn hosting-profiles
  "The profiles that host a prompt."
  [prompt-id]
  (filter #(contains? (set (:llm-profile/prompts (entity/props-for %))) prompt-id) (profiles)))

(defn extending-profiles
  "The profiles that extend a profile directly."
  [profile-id]
  (filter #(= profile-id (:llm-profile/extends (entity/props-for %))) (profiles)))

(defn- chain
  "The profile and the ones it extends, base last; stops at a cycle."
  [id]
  (loop [id id seen []]
    (if (or (nil? id) (some #{id} seen))
      seen
      (recur (:llm-profile/extends (entity/props-for id)) (conj seen id)))))

(defn effective
  "A profile's props merged over the profiles it extends: a key it sets
   replaces the base's, except :llm-profile/class-policy, merged class by
   class, :llm-profile/commands, whose patterns add up, and
   :llm-profile/fixed, which adds up too: a profile can't free a class its
   base fixed. A profile's
   :llm-profile/surfaces and :llm-profile/network replace the base's, so a
   profile can only narrow them by restating."
  [id]
  (reduce (fn [acc props]
            (-> (merge acc (dissoc props :llm-profile/class-policy :llm-profile/commands :llm-profile/fixed))
                (update :llm-profile/class-policy merge (:llm-profile/class-policy props))
                (update :llm-profile/fixed (fnil set/union #{}) (:llm-profile/fixed props))
                (update :llm-profile/commands
                        (fn [c] (merge-with (comp vec distinct concat) c (:llm-profile/commands props))))))
          {}
          (map entity/props-for (reverse (chain id)))))

(defn- resolve-sym [sym]
  (when sym
    #?(:clj (try (requiring-resolve sym) (catch Exception _ nil))
       :cljs nil)))

(defn- classifier-fn [classifier-id]
  (resolve-sym (:classifier/fn (entity/props-for classifier-id))))

(defn- served-fn [classifier-id]
  (resolve-sym (:classifier/served-fn (entity/props-for classifier-id))))

(defn class-of
  "The class a profile's classifier gives a tool, or nil when it can't."
  [profile-id tool-dev-id]
  (when-let [f (classifier-fn (:llm-profile/classifier (effective profile-id)))]
    (try (f tool-dev-id) (catch #?(:clj Exception :cljs :default) _ nil))))

(defn decision
  "What a profile decides for a tool: :allow, :ask or :deny. A tool with no
   class, or a class the policy doesn't name, is denied."
  [profile-id tool-dev-id]
  (let [cls (class-of profile-id tool-dev-id)]
    (or (when cls (get (:llm-profile/class-policy (effective profile-id)) cls)) :deny)))

(defn decisions
  "A profile's decision for each of tools: {tool-dev-id decision}."
  [profile-id tools]
  (into (sorted-map) (for [t tools] [t (decision profile-id t)])))

;; ---------------------------------------------------------------------------
;; Rules: remembered answers that narrow a profile
;; ---------------------------------------------------------------------------

(defn rules
  "Every registered rule, as its props with its dev-id under :llm-rule/id.
   Registered for a person or a team, a rule carries their aspect
   (:member/u1), so their rules are a query: entities of type :atlas/llm-rule
   with that aspect."
  []
  (sort-by :llm-rule/id
           (for [id (entity/all-with-aspect :atlas/llm-rule)
                 :when (not (contains? (entity/identity-for id) :atlas/ontology))]
             (assoc (entity/props-for id) :llm-rule/id id))))

(defn rules-for
  "The registered rules that narrow a profile."
  [profile-id]
  (filter #(= profile-id (:llm-rule/profile %)) (rules)))

(defn- now-ms []
  #?(:clj (System/currentTimeMillis) :cljs (.getTime (js/Date.))))

(defn- active? [rule now]
  (let [expires (:llm-rule/expires rule)]
    (or (nil? expires) (< (inst-ms now) (inst-ms expires)))))

(defn- applies?
  "Whether a rule speaks about this call: same profile, its tool or its class,
   every fact it names holding one of its values, and not expired."
  [profile-id tool cls facts now rule]
  (and (= profile-id (:llm-rule/profile rule))
       (or (= tool (:llm-rule/tool rule))
           (and cls (= cls (:llm-rule/class rule))))
       (every? (fn [[k allowed]] (contains? allowed (get facts k))) (:llm-rule/facts rule))
       (active? rule now)))

(defn decide
  "A profile's decision for one call, under the rules that narrow it, and why:

     {:decision :allow|:ask|:deny, :base <the profile's>, :class <the tool's>,
      :rule <the id of the rule that decided, if one did>}

   A rule that applies and denies, denies: a person may always tighten. One
   that allows decides only where the profile asks, and never for a class the
   profile fixes. Otherwise the profile decides.

   ctx: :rules (default: the registered rules for the profile), :facts about
   the call ({fact-key value}, matched against a rule's :llm-rule/facts), and
   :now (an inst; default the current time), against which a rule expires."
  ([profile-id tool] (decide profile-id tool {}))
  ([profile-id tool {:keys [facts now] :as ctx}]
   (let [cls (class-of profile-id tool)
         base (decision profile-id tool)
         now (or now #?(:clj (java.util.Date. (long (now-ms))) :cljs (js/Date. (now-ms))))
         fixed (set (:llm-profile/fixed (effective profile-id)))
         applying (filter #(applies? profile-id tool cls facts now %)
                          (get ctx :rules (rules-for profile-id)))
         deny (first (filter #(= :deny (:llm-rule/decision %)) applying))
         allow (when (and (= :ask base) cls (not (contains? fixed cls)))
                 (first (filter #(= :allow (:llm-rule/decision %)) applying)))
         by (or deny allow)]
     (cond-> {:decision (cond deny :deny allow :allow :else base)
              :base base
              :class cls}
       by (assoc :rule (:llm-rule/id by))))))

(defn rule-problems
  "Why a rule can't be kept, or nil: checked before a remembered answer is
   saved, and over every registered rule by invariant/llm-rule-only-narrows."
  [rule]
  (let [{:llm-rule/keys [profile class tool]} rule
        rule-decision (:llm-rule/decision rule)
        known? (some #{profile} (profiles))
        classes (when known?
                  (set (:classifier/classes (entity/props-for (:llm-profile/classifier (effective profile))))))
        target-class (or class (when (and known? tool) (class-of profile tool)))
        base (when known?
               (if class
                 (get (:llm-profile/class-policy (effective profile)) class :deny)
                 (when tool (decision profile tool))))
        problem (fn [k & kvs] (apply assoc {:problem k :rule (:llm-rule/id rule)} kvs))]
    (seq
     (remove nil?
             [(when-not known? (problem :profile-unknown :profile profile))
              (when-not (or class tool) (problem :targets-nothing))
              (when (and class tool) (problem :targets-both :class class :tool tool))
              (when (and known? class (not (contains? classes class)))
                (problem :class-unknown :class class))
              (when (and known? (= :allow rule-decision) (or class tool) (not= :ask base))
                (problem :allow-outside-ask :base base))
              (when (and known? (= :allow rule-decision) target-class
                         (contains? (set (:llm-profile/fixed (effective profile))) target-class))
                (problem :class-fixed :class target-class))]))))

(defn draft-rule
  "A rule remembering the answer to one call, as narrow as asked: for the
   call's tool, or (`:by :class`) for every tool of its class; and only while
   the facts named in `:keep` are what they were in this call. Check it with
   `rule-problems` before saving.

   opts: :decision (:allow or :deny), :by (:tool, the default, or :class),
   :keep (fact keys), :expires (an inst), :summary (what the person sees)."
  [profile-id tool facts {:keys [decision by keep expires summary] :or {by :tool}}]
  (cond-> {:llm-rule/profile profile-id
           :llm-rule/decision decision
           :llm-rule/facts (into {} (for [k keep :when (contains? facts k)] [k #{(get facts k)}]))}
    (= :tool by) (assoc :llm-rule/tool tool)
    (= :class by) (assoc :llm-rule/class (class-of profile-id tool))
    expires (assoc :llm-rule/expires expires)
    summary (assoc :llm-rule/summary summary)))

;; ---------------------------------------------------------------------------
;; Invariants
;; ---------------------------------------------------------------------------

(registry/register!
 :invariant/llm-profile-extends-resolves
 :atlas/invariant
 #{:meta/llm-profile-extends-check}
 {:invariant/fn
  (fn []
    (let [violations (concat
                      (for [id (profiles)
                            :let [base (:llm-profile/extends (entity/props-for id))]
                            :when (and base (not (contains? (set (profiles)) base)))]
                        {:profile id :extends base :problem :not-a-profile})
                      (for [id (profiles)
                            :let [c (chain id) next (:llm-profile/extends (entity/props-for (last c)))]
                            :when (and next (some #{next} c))]
                        {:profile id :problem :cycle :chain c}))]
      (when (seq violations)
        {:invariant :llm-profile-extends-resolves
         :violation :llm-profile-extends-broken
         :details   (vec violations)
         :severity  :error
         :message   "A profile extends another registered profile, with no cycle"})))})

(registry/register!
 :invariant/llm-profile-policy-total
 :atlas/invariant
 #{:meta/llm-profile-policy-check}
 {:invariant/fn
  (fn []
    (let [violations
          (for [id (profiles)
                :let [p (effective id)
                      cid (:llm-profile/classifier p)
                      c (entity/props-for cid)
                      classes (set (:classifier/classes c))
                      named (set (keys (:llm-profile/class-policy p)))
                      problem (cond
                                (not (contains? (entity/identity-for cid) :atlas/tool-classifier))
                                {:problem :classifier-not-registered :classifier cid}
                                #?(:clj (nil? (classifier-fn cid)) :cljs false)
                                {:problem :classifier-fn-unresolved :classifier cid :fn (:classifier/fn c)}
                                #?(:clj (and (:classifier/served-fn c) (nil? (served-fn cid))) :cljs false)
                                {:problem :classifier-served-fn-unresolved :classifier cid :fn (:classifier/served-fn c)}
                                (not= classes named)
                                {:problem :policy-incomplete
                                 :undecided (vec (sort (set/difference classes named)))
                                 :unknown (vec (sort (set/difference named classes)))})]
                :when problem]
            (assoc problem :profile id))]
      (when (seq violations)
        {:invariant :llm-profile-policy-total
         :violation :llm-profile-policy-not-total
         :details   (vec violations)
         :severity  :error
         :message   "A profile decides every class its classifier derives, and nothing else: a new class is decided by each profile, never defaulted"})))})

#?(:clj
   (registry/register!
    :invariant/llm-profile-prompts-fit
    :atlas/invariant
    #{:meta/llm-profile-prompts-check}
    {:invariant/fn
     (fn []
       (let [violations
             (for [id (profiles)
                   prompt (sort (:llm-profile/prompts (effective id)))
                   :let [props (entity/props-for prompt)]
                   v (if-not (contains? (entity/identity-for prompt) :atlas/llm-prompt)
                       [{:profile id :prompt prompt :problem :not-a-prompt}]
                       (for [tool (:llm-prompt/mcp-deps props)
                             :when (= :deny (decision id tool))]
                         {:profile id :prompt prompt :tool tool :class (class-of id tool) :problem :tool-denied}))]
               v)]
         (when (seq violations)
           {:invariant :llm-profile-prompts-fit
            :violation :llm-profile-denies-prompt-dep
            :details   (vec violations)
            :severity  :error
            :message   "Every tool a hosted prompt depends on is allowed or asked for by the profile: the prompt can run under it"})))}))

(registry/register!
 :invariant/llm-rule-only-narrows
 :atlas/invariant
 #{:meta/llm-rule-check}
 {:invariant/fn
  (fn []
    (let [violations (mapcat rule-problems (rules))]
      (when (seq violations)
        {:invariant :llm-rule-only-narrows
         :violation :llm-rule-loosens
         :details   (vec violations)
         :severity  :error
         :message   "A rule narrows its profile: it may deny anything, and allow only what the profile asks about, never a fixed class"})))})

;; ---------------------------------------------------------------------------
;; Export: each profile, decided for every tool its surfaces serve
;; ---------------------------------------------------------------------------

(defn export
  "Every profile, effective, with each surface's tools decided by served
   name, for an adapter to turn into an agent's rules (atlas.adapter.claude-code):

     {profile-id {:summary :commands :network :prompts
                  :surfaces [{:surface/env :surface/scope :tools {served-name decision}}]}}

   A surface's tools come from the classifier's :classifier/served-fn; a
   served tool with no registry id is denied. Without a served-fn, :tools is
   empty: the system hasn't said what its surfaces serve."
  []
  (into (sorted-map)
        (for [id (profiles)
              :let [p (effective id)
                    served (served-fn (:llm-profile/classifier p))]]
          [id {:summary (:llm-profile/summary p)
               :commands (:llm-profile/commands p)
               :network (:llm-profile/network p)
               :prompts (:llm-profile/prompts p)
               :surfaces (vec (for [s (sort-by (juxt :surface/env :surface/scope) (:llm-profile/surfaces p))]
                                (assoc s :tools (into (sorted-map)
                                                      (for [{:keys [name dev-id]} (when served (served s))]
                                                        [name (if dev-id (decision id dev-id) :deny)])))))}])))
