(ns atlas.datalog.explorer
  "Explorer and UI-oriented Datalog queries.
   Aspect filtering, similarity analysis, and entity inspection."
  (:require [datascript.core :as d]
            [clojure.set]
            [atlas.datalog :as datalog]
            [atlas.datalog.graph :as graph]
            [atlas.registry :as registry]))

(defn query-entity-aspects
  "Get all aspects for a specific entity."
  [db dev-id]
  (set (d/q '[:find [?aspect ...]
              :in $ ?dev-id
              :where
              [?e :atlas/dev-id ?dev-id]
              [?e :entity/aspect ?aspect]]
            db dev-id)))

(defn query-entity-type
  "Get the entity type for a dev-id. Discovers types dynamically from the registry."
  [db dev-id]
  (let [aspects (query-entity-aspects db dev-id)
        type-aspects (set (registry/registered-types))]
    (first (filter type-aspects aspects))))

(defn query-entities-with-all-aspects
  "Find entities that have ALL of the specified aspects (AND semantics).
   Returns set of dev-ids."
  [db aspects]
  (when (seq aspects)
    (let [aspect-vec (vec aspects)
          initial-set (set (datalog/query-entities-with-aspect db (first aspect-vec)))]
      (reduce (fn [acc aspect]
                (clojure.set/intersection acc (set (datalog/query-entities-with-aspect db aspect))))
              initial-set
              (rest aspect-vec)))))

(defn query-entities-with-any-aspect
  "Find entities that have ANY of the specified aspects (OR semantics).
   Returns set of dev-ids."
  [db aspects]
  (when (seq aspects)
    (->> aspects
         (mapcat #(datalog/query-entities-with-aspect db %))
         set)))

(defn query-all-entities
  "Get all entity dev-ids in the database."
  [db]
  (d/q '[:find [?dev-id ...]
         :where
         [?e :atlas/dev-id ?dev-id]]
       db))

(defn query-explorer-filter
  "Filter entities by AND/OR aspect criteria.
   Returns set of dev-ids."
  [db aspects-and aspects-or]
  (let [and-matches (when (seq aspects-and)
                      (query-entities-with-all-aspects db aspects-and))
        or-matches (when (seq aspects-or)
                     (query-entities-with-any-aspect db aspects-or))]
    (cond
      (and and-matches or-matches) (clojure.set/union and-matches or-matches)
      and-matches and-matches
      or-matches or-matches
      :else #{})))

(defn query-structural-gaps
  "Find entity pairs that are semantically similar but not connected."
  ([db] (query-structural-gaps db 0.5 20))
  ([db min-similarity] (query-structural-gaps db min-similarity 20))
  ([db min-similarity max-results]
   (graph/gaps (query-all-entities db)
               #(query-entity-aspects db %)
               #(set (datalog/query-dependencies db %))
               min-similarity
               max-results)))
