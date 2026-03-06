(ns atlas.aggregation
  "Semantic aggregation — derive implicit aspects from the dependency graph.

   Discovers entity references automatically via the type-ref registry,
   then walks those references transitively and propagates selected aspects
   into :atlas/semantic on every entity's props.

   Usage:
     (aggregate-semantics!)              ; discovers type-refs, uses default ns rules
     (aggregate-semantics! custom-rules) ; override which namespaces propagate

     ;; Query the result
     (:atlas/semantic (atlas.registry.lookup/props-for :fn/create-order))
     ;; => [{:aspect :integration/external :source :component/stripe :depth 1 :via :execution-function/deps}
     ;;     {:aspect :domain/persistence   :source :component/db     :depth 1 :via :execution-function/deps}]"
  (:require [atlas.registry :as registry]
            [atlas.registry.lookup :as entity]
            [atlas.ontology.type-ref :as type-ref]
            [clojure.set :as set]))

;; =============================================================================
;; PROPAGATION RULES
;; =============================================================================

(def default-propagate-namespaces
  "Aspect namespaces that propagate through dependency references by default."
  #{"integration" "domain" "protocol" "temporal"})

;; =============================================================================
;; TYPE-REF DISCOVERY
;; =============================================================================

(defn- discover-ref-properties
  "Read all type-refs from the registry. Returns a seq of maps:
   {:source :atlas/execution-function :property :execution-function/deps :cardinality ...}"
  []
  (->> (type-ref/all-type-refs)
       (map (fn [ref-id]
              (let [props (entity/props-for ref-id)]
                {:source      (:type-ref/source props)
                 :property    (:type-ref/property props)
                 :cardinality (:type-ref/cardinality props)})))
       (filter :property)))

(defn- refs-for-entity
  "Given an entity's compound-id, return the type-ref properties that apply.
   Returns seq of {:property :kw :cardinality :kw}."
  [compound-id ref-properties]
  (filter #(contains? compound-id (:source %)) ref-properties))

;; =============================================================================
;; INTERNALS
;; =============================================================================

(defn- propagatable-aspects
  "Filter aspects to only those whose namespace is in the allowed set."
  [aspects ns-set]
  (set (filter #(contains? ns-set (namespace %)) aspects)))

(defn- resolve-refs
  "Get referenced dev-ids from an entity's props for a given type-ref property."
  [props {:keys [property cardinality]}]
  (let [value (get props property)]
    (cond
      (nil? value) #{}
      (= cardinality :db.cardinality/many)
      (cond
        (set? value) value
        (coll? value) (set value)
        :else #{value})
      :else
      (if (qualified-keyword? value) #{value} #{}))))

(defn- compute-derived
  "Compute derived aspects for a single entity, walking refs transitively.
   Returns a vector of {:aspect kw :source dev-id :depth n :via property-kw}.

   exclude-own?: when true (default), filters out aspects the entity already has
   explicitly. Set to false for redundancy detection."
  ([dev-id ref-properties ns-set]
   (compute-derived dev-id ref-properties ns-set true))
  ([dev-id ref-properties ns-set exclude-own?]
   (let [compound-id (entity/identity-for dev-id)
         entity-refs (refs-for-entity compound-id ref-properties)]
     (when (seq entity-refs)
       (let [own-aspects   (registry/aspects compound-id)
             initial-seen  (if exclude-own? own-aspects #{})
             initial-queue (vec (for [ref-prop entity-refs
                                      dep-id   (sort (resolve-refs (entity/props-for dev-id) ref-prop))]
                                  [dep-id 1 (:property ref-prop)]))]
         (loop [queue        initial-queue
                seen         #{dev-id}
                seen-aspects initial-seen
                result       []]
           (if-let [[dep-id depth via-prop] (first queue)]
             (if (or (seen dep-id) (nil? (entity/identity-for dep-id)))
               (recur (subvec queue 1) seen seen-aspects result)
               (let [dep-cid     (entity/identity-for dep-id)
                     dep-aspects (registry/aspects dep-cid)
                     propagated  (set/difference
                                  (propagatable-aspects dep-aspects ns-set)
                                  seen-aspects)
                     new-entries (mapv (fn [asp] {:aspect asp :source dep-id :depth depth :via via-prop})
                                      (sort propagated))
                     ;; Follow the dep's own refs (transitive)
                     dep-refs    (refs-for-entity dep-cid ref-properties)
                     dep-queue   (vec (for [ref-prop dep-refs
                                           next-id  (sort (resolve-refs (entity/props-for dep-id) ref-prop))
                                           :when (not (seen next-id))]
                                       [next-id (inc depth) (:property ref-prop)]))]
                 (recur (into (subvec queue 1) dep-queue)
                        (conj seen dep-id)
                        (into seen-aspects propagated)
                        (into result new-entries))))
             result)))))))

;; =============================================================================
;; PUBLIC API
;; =============================================================================

(defn aggregate-semantics!
  "Walk all entities in the registry and assoc :atlas/semantic with derived
   aspects from the dependency graph.

   Discovers reference properties from registered type-refs automatically.
   Call after all register! calls (including ontology modules).

   ns-set: set of namespace strings to propagate (default: default-propagate-namespaces).

   Returns a summary map."
  ([] (aggregate-semantics! default-propagate-namespaces))
  ([ns-set]
   (let [ref-properties (discover-ref-properties)
         enrichments    (atom 0)]
     (swap! registry/registry
            (fn [r]
              (reduce-kv
               (fn [acc compound-id props]
                 (let [dev-id  (:atlas/dev-id props)
                       derived (when dev-id (compute-derived dev-id ref-properties ns-set))]
                   (if (seq derived)
                     (do (swap! enrichments inc)
                         (assoc acc compound-id (assoc props :atlas/semantic (vec derived))))
                     ;; Clear stale :atlas/semantic if no derived aspects
                     (if (:atlas/semantic props)
                       (assoc acc compound-id (dissoc props :atlas/semantic))
                       acc))))
               r
               r)))
     {:aggregation/enriched   @enrichments
      :aggregation/total      (count @registry/registry)
      :aggregation/type-refs  (mapv :property ref-properties)})))

(defn semantic-of
  "Get the derived semantic aspects for a dev-id. Returns vector or nil."
  [dev-id]
  (:atlas/semantic (entity/props-for dev-id)))

(defn semantic-aspects
  "Get just the derived aspect keywords as a set for a dev-id."
  [dev-id]
  (set (map :aspect (semantic-of dev-id))))

(defn full-aspects
  "Union of explicit aspects and derived semantic aspects for a dev-id."
  [dev-id]
  (let [explicit (registry/aspects (entity/identity-for dev-id))]
    (set/union (or explicit #{}) (semantic-aspects dev-id))))

(defn semantic-sources
  "For a given derived aspect on a dev-id, return which deps contributed it."
  [dev-id aspect]
  (->> (semantic-of dev-id)
       (filter #(= aspect (:aspect %)))
       (mapv #(select-keys % [:source :depth :via]))))

(defn redundant-aspects
  "Find explicit aspects that are already derivable from deps via type-refs.
   Returns nil or a vector of {:dev-id :redundant [...] :sources {...}} maps.
   Does not require aggregate-semantics! — computes derivable aspects on the fly."
  ([] (redundant-aspects default-propagate-namespaces))
  ([ns-set]
   (let [ref-properties (discover-ref-properties)
         violations
         (->> @registry/registry
              (keep
               (fn [[compound-id props]]
                 (let [dev-id (:atlas/dev-id props)]
                   (when dev-id
                     ;; Compute all derivable aspects WITHOUT excluding own
                     (let [all-derivable (compute-derived dev-id ref-properties ns-set false)
                           derivable-set (set (map :aspect all-derivable))
                           explicit      (registry/aspects compound-id)
                           redundant     (set/intersection explicit derivable-set)]
                       (when (seq redundant)
                         {:dev-id    dev-id
                          :redundant (vec (sort redundant))
                          :sources   (into {}
                                           (map (fn [asp]
                                                  [asp (mapv :source
                                                             (filter #(= asp (:aspect %)) all-derivable))])
                                                redundant))}))))))
              vec)]
     (when (seq violations) violations))))

;; =============================================================================
;; INVARIANT (auto-registered)
;; =============================================================================

(registry/register!
 :invariant/redundant-explicit-aspects
 :atlas/invariant
 #{:meta/redundant-aspects-check}
 {:invariant/fn (fn []
                  "Explicit aspects that are already derivable via type-ref dependencies.

                   These aspects can be removed from the compound-id since they are
                   implied by the entity's deps. Keeping them is not wrong but adds
                   noise — the aggregation system derives them automatically.

                   Does not require aggregate-semantics! to have been called."
                  (when-let [violations (redundant-aspects)]
                    {:invariant  :redundant-explicit-aspects
                     :violation  :redundant-aspects
                     :details    violations
                     :severity   :info
                     :message    "Some entities have explicit aspects already derivable from their deps"}))})
