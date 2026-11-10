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
   - a base profile it extends.

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
            [atlas.ontology.type-ref :as type-ref]
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

(s/def :classifier/classes (s/coll-of qualified-keyword? :kind set? :min-count 1))
(s/def :classifier/fn qualified-symbol?)

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
 :atlas/tool-classifier
 :atlas/ontology
 #{:atlas/tool-classifier}
 {:ontology/for  :atlas/tool-classifier
  :ontology/keys [:classifier/classes
                  :classifier/fn]})

;; Type-refs use :entity/depends, so dependents-of and blast radius reach a
;; profile from the prompts it hosts, the profile it extends, and its classifier.

(registry/register!
 :type-ref/llm-profile-prompts
 :atlas/type-ref
 #{:meta/ref-llm-profile-prompts}
 {:type-ref/source       :atlas/llm-profile
  :type-ref/property     :llm-profile/prompts
  :type-ref/datalog-verb :entity/depends
  :type-ref/cardinality  :db.cardinality/many})

(registry/register!
 :type-ref/llm-profile-extends
 :atlas/type-ref
 #{:meta/ref-llm-profile-extends}
 {:type-ref/source       :atlas/llm-profile
  :type-ref/property     :llm-profile/extends
  :type-ref/datalog-verb :entity/depends
  :type-ref/cardinality  :db.cardinality/one})

(registry/register!
 :type-ref/llm-profile-classifier
 :atlas/type-ref
 #{:meta/ref-llm-profile-classifier}
 {:type-ref/source       :atlas/llm-profile
  :type-ref/property     :llm-profile/classifier
  :type-ref/datalog-verb :entity/depends
  :type-ref/cardinality  :db.cardinality/one})

(registry/register!
 :datalog-extractor/llm-profile
 :atlas/datalog-extractor
 #{:meta/llm-profile-extractor}
 {:datalog-extractor/fn
  (fn [compound-id props]
    (when (contains? compound-id :atlas/llm-profile)
      (type-ref/extract-reference-facts :atlas/llm-profile compound-id props)))})

;; ---------------------------------------------------------------------------
;; Reading a profile
;; ---------------------------------------------------------------------------

(defn profiles
  "Every registered profile's dev-id, not the ontology itself."
  []
  (sort (for [id (entity/all-with-aspect :atlas/llm-profile)
              :when (not (contains? (entity/identity-for id) :atlas/ontology))]
          id)))

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
   class, and :llm-profile/commands, whose patterns add up. A profile's
   :llm-profile/surfaces and :llm-profile/network replace the base's, so a
   profile can only narrow them by restating."
  [id]
  (reduce (fn [acc props]
            (-> (merge acc (dissoc props :llm-profile/class-policy :llm-profile/commands))
                (update :llm-profile/class-policy merge (:llm-profile/class-policy props))
                (update :llm-profile/commands
                        (fn [c] (merge-with (comp vec distinct concat) c (:llm-profile/commands props))))))
          {}
          (map entity/props-for (reverse (chain id)))))

(defn- classifier-fn [classifier-id]
  (when-let [sym (:classifier/fn (entity/props-for classifier-id))]
    #?(:clj (try (requiring-resolve sym) (catch Exception _ nil))
       :cljs nil)))

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
