(ns atlas-ui.graph-data)

(defn entity-nodes
  "Extract all entities from registry as node data.

   Enhanced with explicit :entity-type and :aspects for better performance:
   - :entity-type - Direct access to entity type (no filtering needed)
   - :aspects - Pure aspects only (type removed from set)
   - :identity - Full compound identity (kept for backward compatibility)"
  [registry]
  (for [[compound-id props] registry
        :when (and (set? compound-id)
                   (:atlas/dev-id props))]
    (let [entity-type (:atlas/type props)
          ;; Pure aspects = compound-id minus the type
          aspects (if entity-type
                    (disj compound-id entity-type)
                    compound-id)]
      {:atlas-ui.graph.node/id (:atlas/dev-id props)
       :atlas-ui.graph.node/type :entity
       :atlas-ui.graph.node/entity-type entity-type      ; NEW: direct type access
       :atlas-ui.graph.node/aspects aspects              ; NEW: pure aspects only
       :atlas-ui.graph.node/label (str (:atlas/dev-id props))
       :atlas-ui.graph.node/identity compound-id         ; KEPT: backward compat
       :atlas-ui.graph.node/props props})))

(defn aspect-nodes
  "Extract all unique aspects from all compound identities"
  [registry]
  (->> registry
       keys
       (filter set?)
       (mapcat identity)
       (into #{})
       (map (fn [aspect]
              {:atlas-ui.graph.node/id aspect
               :atlas-ui.graph.node/type :aspect
               :atlas-ui.graph.node/label (if (namespace aspect)
                                            (str (namespace aspect) "/" (name aspect))
                                            (str aspect))}))))

(defn membership-edges
  "Create edges from entities to their aspects (membership)"
  [registry]
  (for [[compound-id props] registry
        :when (and (set? compound-id)
                   (:atlas/dev-id props))
        aspect compound-id]
    {:atlas-ui.graph.edge/source (:atlas/dev-id props)
     :atlas-ui.graph.edge/target aspect
     :atlas-ui.graph.edge/type :membership}))

(defn dependency-edges
  "Create edges from entities to their dependencies"
  [registry]
  (letfn [(deps-for-props [props]
            (let [deps (or (:interface-endpoint/deps props)
                           (:execution-function/deps props)
                           (:structure-component/deps props)
                           (:semantic-namespace/deps props))]
              (cond
                (set? deps) deps
                (coll? deps) (set deps)
                :else #{})))]
    (for [[compound-id props] registry
          :when (and (set? compound-id)
                     (:atlas/dev-id props))
          dep (deps-for-props props)]
      {:atlas-ui.graph.edge/source (:atlas/dev-id props)
       :atlas-ui.graph.edge/target dep
       :atlas-ui.graph.edge/type :dependency})))

(defn build-graph-data
  "Build complete graph data structure from registry"
  [registry]
  (let [entities (entity-nodes registry)
        aspects (aspect-nodes registry)
        edges (concat (membership-edges registry)
                      (dependency-edges registry))]
    {:atlas-ui.graph.data/entities entities
     :atlas-ui.graph.data/aspects aspects
     :atlas-ui.graph.data/edges edges
     :atlas-ui.graph.data/all-nodes (concat entities aspects)}))

(defn find-node-by-id
  "Find a node in graph data by its ID"
  [graph-data node-id]
  (->> (:atlas-ui.graph.data/all-nodes graph-data)
       (filter #(= (:atlas-ui.graph.node/id %) node-id))
       first))

(defn entities-with-aspect
  "Find all entities that have a specific aspect"
  [graph-data aspect]
  (->> (:atlas-ui.graph.data/entities graph-data)
       (filter (fn [entity]
                 (let [identity (:atlas-ui.graph.node/identity entity)
                       has-it (contains? identity aspect)]
                   has-it)))))

(defn entity-aspects
  "Get all aspects for an entity"
  [graph-data entity-id]
  (let [entity (find-node-by-id graph-data entity-id)]
    (:atlas-ui.graph.node/identity entity)))

;; =============================================================================
;; Type-Based Filtering (NEW - Phase 1)
;; =============================================================================

(defn entities-by-type
  "Filter entities by exact entity type - O(1) lookup per entity.

   Example:
     (entities-by-type graph-data :atlas/interface-endpoint)"
  [graph-data entity-type]
  (->> (:atlas-ui.graph.data/entities graph-data)
       (filter #(= (:atlas-ui.graph.node/entity-type %) entity-type))))

(defn entities-by-types
  "Filter entities by multiple entity types.

   Example:
     (entities-by-types graph-data #{:atlas/interface-endpoint
                                      :atlas/execution-function})"
  [graph-data entity-types]
  (let [type-set (set entity-types)]
    (->> (:atlas-ui.graph.data/entities graph-data)
         (filter #(contains? type-set (:atlas-ui.graph.node/entity-type %))))))

(defn group-by-type
  "Group all entities by their entity type - useful for stats/legend.

   Returns a map of {entity-type -> [entities]}

   Example:
     (group-by-type graph-data)
     ;; => {:atlas/interface-endpoint [...], :atlas/execution-function [...], ...}"
  [graph-data]
  (->> (:atlas-ui.graph.data/entities graph-data)
       (group-by :atlas-ui.graph.node/entity-type)))

(defn type-counts
  "Get count of entities for each type.

   Returns a map of {entity-type -> count}

   Example:
     (type-counts graph-data)
     ;; => {:atlas/interface-endpoint 15, :atlas/execution-function 42, ...}"
  [graph-data]
  (->> (:atlas-ui.graph.data/entities graph-data)
       (map :atlas-ui.graph.node/entity-type)
       frequencies))
