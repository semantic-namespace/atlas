(ns atlas.datalog.impact
  "Error impact analysis Datalog queries.
   Traces blast radius when components, functions, or protocols fail."
  (:require [datascript.core :as d]
            [atlas.datalog :as datalog]
            [atlas.datalog.protocol :as protocol]))

(defn query-transitive-reverse-dependencies
  "Find all entities that depend on the given entity, transitively.
   Returns a set of all dependents up the dependency tree."
  [db dev-id]
  (loop [to-visit [dev-id]
         visited #{}
         dependents #{}]
    (if (empty? to-visit)
      dependents
      (let [current (first to-visit)
            current-dependents (datalog/query-reverse-dependencies db current)
            new-dependents (remove visited current-dependents)]
        (recur (concat (rest to-visit) new-dependents)
               (conj visited current)
               (into dependents new-dependents))))))

(defn query-affected-endpoints
  "Find all endpoints affected by an error in a given entity.
   Returns endpoints with their dependency paths to the broken entity."
  [db broken-entity-id]
  (let [all-dependents (query-transitive-reverse-dependencies db broken-entity-id)
        get-aspects (fn [dev-id]
                     (d/q '[:find [?aspect ...]
                            :in $ ?id
                            :where
                            [?e :atlas/dev-id ?id]
                            [?e :entity/aspect ?aspect]]
                          db dev-id))
        endpoints (filter #(contains? (set (get-aspects %)) :atlas/interface-endpoint)
                         all-dependents)
        build-path (fn build-path [from to visited]
                    (if (= from to)
                      [to]
                      (let [deps (datalog/query-dependencies db from)]
                        (some (fn [dep]
                               (when-not (contains? visited dep)
                                 (when-let [path (build-path dep to (conj visited from))]
                                   (cons from path))))
                             deps))))]
    (->> endpoints
         (map (fn [ep-id]
                {:endpoint ep-id
                 :path (vec (build-path ep-id broken-entity-id #{}))}))
         vec)))

(defn query-protocol-error-impact
  "Given a protocol with an error, find all affected endpoints and functions.
   Shows the complete blast radius of a protocol failure."
  [db protocol-id]
  (let [implementers (protocol/query-components-implementing-protocol db protocol-id)
        get-aspects (fn [dev-id]
                     (d/q '[:find [?aspect ...]
                            :in $ ?id
                            :where
                            [?e :atlas/dev-id ?id]
                            [?e :entity/aspect ?aspect]]
                          db dev-id))
        broken-functions (->> implementers
                             (mapcat #(datalog/query-reverse-dependencies db %))
                             (filter #(contains? (set (get-aspects %)) :atlas/execution-function))
                             distinct
                             vec)
        all-broken (concat implementers broken-functions)
        affected-endpoints (->> all-broken
                               (mapcat #(query-affected-endpoints db %))
                               (map :endpoint)
                               distinct
                               vec)]
    {:protocol protocol-id
     :broken-components (vec implementers)
     :broken-functions broken-functions
     :affected-endpoints affected-endpoints
     :total-impact (+ (count implementers) (count broken-functions) (count affected-endpoints))}))

(defn query-function-error-impact
  "Given a function with an error, show all affected endpoints with dependency paths."
  [db function-id]
  (let [affected (query-affected-endpoints db function-id)]
    {:function function-id
     :affected-endpoints affected
     :affected-count (count affected)}))

(defn query-component-error-impact
  "Given a component with an error, show all affected functions and endpoints."
  [db component-id]
  (let [protocols (d/q '[:find [?proto ...]
                        :in $ ?comp-id
                        :where
                        [?e :atlas/dev-id ?comp-id]
                        [?e :entity/aspect ?proto]
                        [(namespace ?proto) ?ns]
                        [(= ?ns "protocol")]]
                      db component-id)
        broken-functions (vec (datalog/query-reverse-dependencies db component-id))
        affected-endpoints (->> broken-functions
                               (mapcat #(query-affected-endpoints db %))
                               (map :endpoint)
                               distinct
                               vec)]
    {:component component-id
     :protocols (vec protocols)
     :broken-functions broken-functions
     :affected-endpoints affected-endpoints
     :total-impact (+ 1 (count broken-functions) (count affected-endpoints))}))
