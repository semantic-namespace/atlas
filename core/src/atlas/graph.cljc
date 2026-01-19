(ns atlas.graph
  "Pure graph algorithms and graph invariants"
  (:require [atlas.registry :as registry]
            [atlas.query :as query]
            [atlas.registry.lookup :as entity]
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
  (map #(:atlas/dev-id (second %)) @registry/registry))

(defn invariant-deps-exist
  "All dependency keys must reference existing dev-ids."
  []
  (let [all-ids (set (all-dev-ids))
        violations (for [[_ v] @registry/registry
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
    (letfn [(has-cycle? [id visited path]
              (cond
                (contains? path id) {:cycle (conj (vec path) id)}
                (contains? visited id) nil
                :else (some #(has-cycle? % (conj visited id) (conj path id))
                            (get deps-map id #{}))))]
      (when-let [cycle (some #(has-cycle? % #{} []) all-ids)]
        {:invariant :no-circular-deps
         :violation :dependency-cycle
         :cycle (:cycle cycle)
         :severity :error
         :message (str "Dependency cycle detected: " (:cycle cycle))}))))

(defn invariant-all-fns-reachable
  "Every function should be reachable from some endpoint.

   Excludes functions marked with :lifecycle/* aspects (setup, migration, etc.)"
  []
  (let [endpoints (entity/all-with-aspect :atlas/interface-endpoint)
        all-fns (set (entity/all-with-aspect :atlas/execution-function))
        ;; Exclude lifecycle functions (not meant to be endpoint-reachable)
        lifecycle-fns (->> @registry/registry
                           (filter (fn [[id _]]
                                     (some #(= "lifecycle" (namespace %)) id)))
                           (map (fn [[_ v]] (:atlas/dev-id v)))
                           set)
        checkable-fns (set/difference all-fns lifecycle-fns)
        ;; Find reachable via BFS from endpoints
        reachable (atom #{})
        collect-reachable (fn collect [id]
                            (when-not (@reachable id)
                              (swap! reachable conj id)
                              (doseq [dep (ontology/deps-for id)]
                                (collect dep))))]
    (doseq [ep endpoints]
      (collect-reachable ep))
    (let [unreachable (set/difference checkable-fns @reachable)]
      (when (seq unreachable)
        {:invariant :all-fns-reachable
         :violation :unreachable-functions
         :functions unreachable
         :severity :warning
         :message (str "Functions not reachable from any endpoint: " unreachable
                       " (mark with :lifecycle/* if intentional)")}))))

(def graph-invariants
  "Graph invariants in check order."
  [invariant-deps-exist
   invariant-no-circular-deps
   invariant-all-fns-reachable])

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

(defn check-invariants
  "Run graph invariants."
  []
  (check graph-invariants))
