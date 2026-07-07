(ns atlas.registry.graph
  "Graph algorithms and structural invariants on registry data."
  (:require [atlas.registry :as registry]
            [atlas.ontology :as ontology]
            [clojure.set :as set]))

;; =============================================================================
;; TOPOLOGICAL SORTING
;; =============================================================================

(defn topo-sort
  "Topological sort of dev-ids by dependencies."
  [dev-ids]
  (let [deps-map (into {} (map (fn [id] [id (ontology/deps-for id)]) dev-ids))
        sorted (atom [])
        visited (atom #{})]
    (letfn [(visit [id]
              (when-not (@visited id)
                (swap! visited conj id)
                (doseq [dep (get deps-map id #{})]
                  (visit dep))
                (swap! sorted conj id)))]
      (doseq [id dev-ids]
        (visit id)))
    @sorted))

(defn topo-sort-by-data
  "Topological sort based on data flow (context/response)."
  [dev-ids]
  (let [dev-ids-set (set dev-ids)
        deps-map (into {} (map (fn [id] [id (ontology/compute-data-deps id)]) dev-ids))
        sorted (atom [])
        visited (atom #{})]
    (letfn [(visit [id]
              (when-not (@visited id)
                (swap! visited conj id)
                (doseq [dep (get deps-map id #{})]
                  (when (contains? dev-ids-set dep)
                    (visit dep)))
                (swap! sorted conj id)))]
      (doseq [id dev-ids]
        (visit id)))
    @sorted))

;; =============================================================================
;; GRAPH INVARIANTS
;; =============================================================================

(defn- all-dev-ids []
  (map #(:atlas/dev-id (second %)) (registry/current-registry)))

(defn invariant-deps-exist
  "All dependency keys must reference existing dev-ids."
  []
  (let [all-ids (set (all-dev-ids))
        violations (for [[_ v] (registry/current-registry)
                         :let [dev-id (:atlas/dev-id v)
                               deps (ontology/deps-for dev-id)]
                         :when deps
                         :let [missing (set/difference deps all-ids)]
                         :when (seq missing)]
                     {:dev-id dev-id :missing-deps missing})]
    (when (seq violations)
      {:invariant :deps-exist
       :violation :missing-dependencies
       :details violations
       :severity :error
       :message (str "Dependencies reference non-existent entities: "
                     (set (mapcat :missing-deps violations)))})))

(defn invariant-no-circular-deps
  "Dependency graph must be acyclic."
  []
  (let [all-ids (all-dev-ids)
        deps-map (into {} (map (fn [id] [id (ontology/deps-for id)]) all-ids))]
    ;; `visited` is the set of ancestors on the current DFS branch; reaching an
    ;; id already in it means we looped back = a cycle. (Was `(contains? path id)`
    ;; on a vector, which tests indices, so cycles were never detected.)
    (letfn [(has-cycle? [id visited path]
              (cond
                (contains? visited id) {:cycle (conj (vec path) id)}
                :else (some #(has-cycle? % (conj visited id) (conj path id))
                            (get deps-map id #{}))))]
      (when-let [cycle (some #(has-cycle? % #{} []) all-ids)]
        {:invariant :no-circular-deps
         :violation :dependency-cycle
         :cycle (:cycle cycle)
         :severity :error
         :message (str "Dependency cycle detected: " (:cycle cycle))}))))

(defn invariant-no-compound-id-collision
  "No two different dev-ids may share the same compound identity.
   A collision means the second registration silently overwrites the first
   in the registry map — the earlier dev-id is lost without any error.
   Severity: :error — silent data loss."
  []
  (let [registrations @registry/registrations
        by-compound-id (group-by (fn [r] (conj (:aspects r) (:type r)))
                                 registrations)
        collisions (for [[compound-id entries] by-compound-id
                         :let [dev-ids (distinct (map :dev-id entries))]
                         :when (> (count dev-ids) 1)]
                     {:compound-id compound-id :dev-ids dev-ids})]
    (when (seq collisions)
      {:invariant :no-compound-id-collision
       :violation :duplicate-compound-identity
       :details collisions
       :severity :error
       :message (str "Multiple dev-ids share the same compound identity — "
                     "only the last registration survives: "
                     (mapv :dev-ids collisions))})))

(def graph-invariants
  "Graph invariants in check order."
  [invariant-deps-exist
   invariant-no-circular-deps
   invariant-no-compound-id-collision])

(defn- result-level [result]
  (or (:level result) (:severity result)))

(defn- normalize-result
  "Ensure invariant results carry both :level and :severity."
  [result]
  (cond-> result
    (and (:severity result) (not (:level result)))
    (assoc :level (:severity result))
    (and (:level result) (not (:severity result)))
    (assoc :severity (:level result))))

(defn check
  "Run provided graph invariants and return {:valid? :errors :warnings :violations}."
  [invariants]
  (let [results (keep #(%) invariants)
        normalized (map normalize-result results)
        errors (filter #(= :error (result-level %)) normalized)
        warnings (filter #(= :warning (result-level %)) normalized)]
    {:violations normalized
     :errors errors
     :warnings warnings
     :violations-flat normalized
     :errors-flat errors
     :warnings-flat warnings
     :valid? (empty? errors)}))
