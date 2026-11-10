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
