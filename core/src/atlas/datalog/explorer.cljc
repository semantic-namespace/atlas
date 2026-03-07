(ns atlas.datalog.explorer
  "Explorer and UI-oriented Datalog queries.
   Aspect filtering, similarity analysis, and entity inspection."
  (:require [datascript.core :as d]
            [clojure.set]
            [atlas.datalog :as datalog]
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
   (let [all-dev-ids (vec (query-all-entities db))
         aspects-map (into {} (map (fn [id] [id (query-entity-aspects db id)]) all-dev-ids))
         deps-map (into {} (map (fn [id] [id (set (datalog/query-dependencies db id))]) all-dev-ids))
         n (count all-dev-ids)
         gaps (for [i (range n)
                    j (range (inc i) n)
                    :let [dev-a (nth all-dev-ids i)
                          dev-b (nth all-dev-ids j)
                          aspects-a (get aspects-map dev-a #{})
                          aspects-b (get aspects-map dev-b #{})
                          shared (clojure.set/intersection aspects-a aspects-b)
                          union (clojure.set/union aspects-a aspects-b)
                          similarity (if (seq union)
                                       (/ (count shared) (count union))
                                       0)
                          deps-a (get deps-map dev-a #{})
                          deps-b (get deps-map dev-b #{})
                          connected? (or (contains? deps-a dev-b)
                                         (contains? deps-b dev-a))]
                    :when (and (> similarity min-similarity)
                               (not connected?))]
                {:a dev-a
                 :b dev-b
                 :similarity (double similarity)
                 :shared-aspects (vec (sort shared))})]
     (vec (take max-results (sort-by (comp - :similarity) gaps))))))
