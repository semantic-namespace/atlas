(ns atlas.datalog.protocol
  "Protocol-oriented Datalog queries.
   Queries that analyze protocol usage, implementation, and dependencies."
  (:require [datascript.core :as d]
            [atlas.datalog :as datalog]))

(defn query-protocol-functions
  "Find all functions declared by a protocol."
  [db protocol-id]
  (d/q '[:find [?fn ...]
         :in $ ?protocol-id
         :where
         [?e :atlas/dev-id ?protocol-id]
         [?e :protocol/function ?fn]]
       db protocol-id))

(defn query-components-implementing-protocol
  "Find all components that declare they implement a specific protocol."
  [db protocol-aspect]
  (d/q '[:find [?dev-id ...]
         :in $ ?protocol-aspect
         :where
         [?e :atlas/dev-id ?dev-id]
         [?e :entity/aspect :atlas/structure-component]
         [?e :entity/aspect ?protocol-aspect]]
       db protocol-aspect))

(defn query-undefined-protocols
  "Find protocol aspects referenced by components but not registered as protocols."
  [db]
  (let [all-protocol-aspects (d/q '[:find [?aspect ...]
                                    :where
                                    [?e :entity/aspect ?aspect]
                                    [(namespace ?aspect) ?ns]
                                    [(= ?ns "protocol")]]
                                  db)
        defined-protocols (set (d/q '[:find [?dev-id ...]
                                      :where
                                      [?e :atlas/dev-id ?dev-id]
                                      [?e :entity/aspect :atlas/interface-protocol]]
                                    db))]
    (remove defined-protocols all-protocol-aspects)))

(defn query-protocol-implementers-by-domain
  "Find all protocol implementers grouped by domain.
   Returns map of domain -> {:components [...] :protocols [...]}"
  [db]
  (let [components (datalog/query-entities-with-aspect db :atlas/structure-component)
        get-domain (fn [comp-id]
                     (first (d/q '[:find [?domain ...]
                                   :in $ ?comp-id
                                   :where
                                   [?e :atlas/dev-id ?comp-id]
                                   [?e :entity/aspect ?domain]
                                   [(namespace ?domain) ?ns]
                                   [(= ?ns "domain")]]
                                 db comp-id)))
        get-protocols (fn [comp-id]
                        (d/q '[:find [?proto ...]
                               :in $ ?comp-id
                               :where
                               [?e :atlas/dev-id ?comp-id]
                               [?e :entity/aspect ?proto]
                               [(namespace ?proto) ?ns]
                               [(= ?ns "protocol")]]
                             db comp-id))]
    (->> components
         (map (fn [comp-id]
                (let [domain (get-domain comp-id)
                      protocols (vec (get-protocols comp-id))]
                  (when domain
                    {:component comp-id
                     :domain domain
                     :protocols protocols}))))
         (remove nil?)
         (group-by :domain)
         (map (fn [[domain items]]
                [domain {:components (mapv :component items)
                        :protocols (vec (distinct (mapcat :protocols items)))}]))
         (into {}))))

(defn query-functions-using-protocol
  "Find all functions that depend on components implementing a specific protocol."
  [db protocol-kw]
  (let [implementers (query-components-implementing-protocol db protocol-kw)]
    (d/q '[:find [?fn-id ...]
           :in $ [?comp-id ...]
           :where
           [?e :atlas/dev-id ?fn-id]
           [?e :entity/aspect :atlas/execution-function]
           [?e :entity/depends ?comp-id]]
         db implementers)))

(defn query-protocol-usage-matrix
  "Build a matrix showing which functions use which protocols.
   Returns: [{:function :fn/foo :protocols [:protocol/oauth :protocol/db]}]"
  [db]
  (let [all-fns (datalog/query-entities-with-aspect db :atlas/execution-function)
        get-component-protocols (fn [comp-id]
                                 (d/q '[:find [?proto ...]
                                        :in $ ?comp-id
                                        :where
                                        [?e :atlas/dev-id ?comp-id]
                                        [?e :entity/aspect ?proto]
                                        [(namespace ?proto) ?ns]
                                        [(= ?ns "protocol")]]
                                      db comp-id))
        get-fn-protocols (fn [fn-id]
                          (->> (datalog/query-dependencies db fn-id)
                               (mapcat #(get-component-protocols %))
                               distinct
                               vec))]
    (->> all-fns
         (map (fn [fn-id]
                (let [protocols (get-fn-protocols fn-id)]
                  (when (seq protocols)
                    {:function fn-id
                     :protocols protocols}))))
         (remove nil?)
         vec)))

(defn query-integration-points
  "Find all external integration points with their protocols."
  [db]
  (d/q '[:find ?comp-id ?domain ?proto
         :where
         [?e :atlas/dev-id ?comp-id]
         [?e :entity/aspect :atlas/structure-component]
         [?e :entity/aspect :integration/external]
         [?e :entity/aspect ?domain]
         [(namespace ?domain) ?ns-domain]
         [(= ?ns-domain "domain")]
         [?e :entity/aspect ?proto]
         [(namespace ?proto) ?ns-proto]
         [(= ?ns-proto "protocol")]]
       db))

(defn query-pure-vs-impure-functions
  "Categorize functions by purity based on component dependencies."
  [db]
  (let [all-fns (datalog/query-entities-with-aspect db :atlas/execution-function)]
    {:pure (->> all-fns
                (filter #(empty? (datalog/query-dependencies db %)))
                vec)
     :impure (->> all-fns
                  (remove #(empty? (datalog/query-dependencies db %)))
                  vec)}))

(defn query-protocol-dependency-graph
  "Build a graph showing which protocols depend on which other protocols."
  [db]
  (let [all-protocols (datalog/query-entities-with-aspect db :atlas/interface-protocol)]
    (->> all-protocols
         (map (fn [proto]
                (let [implementers (query-components-implementing-protocol db proto)
                      dep-components (mapcat #(datalog/query-dependencies db %) implementers)
                      dep-protocols (->> dep-components
                                        (mapcat (fn [comp]
                                                 (d/q '[:find [?p ...]
                                                        :in $ ?comp
                                                        :where
                                                        [?e :atlas/dev-id ?comp]
                                                        [?e :entity/aspect ?p]
                                                        [(namespace ?p) ?ns]
                                                        [(= ?ns "protocol")]]
                                                      db comp)))
                                        distinct
                                        vec)]
                  (when (seq dep-protocols)
                    {:protocol proto
                     :depends-on dep-protocols}))))
         (remove nil?)
         vec)))

(defn query-aspect-co-occurrence
  "Find aspects that commonly appear together."
  [db]
  (let [all-entities (d/q '[:find ?e
                            :where [?e :atlas/dev-id _]]
                          db)
        entity-aspects (fn [entity-id]
                        (d/q '[:find [?aspect ...]
                               :in $ ?e
                               :where [?e :entity/aspect ?aspect]]
                             db entity-id))
        aspect-pairs (mapcat (fn [[entity-id]]
                              (let [aspects (entity-aspects entity-id)]
                                (for [a aspects
                                      b aspects
                                      :when (not= a b)]
                                  (if (neg? (compare (str a) (str b)))
                                    [a b]
                                    [b a]))))
                            all-entities)
        freq-map (frequencies aspect-pairs)]
    (->> freq-map
         (map (fn [[[a b] count]] {:aspect-a a :aspect-b b :count count}))
         (sort-by :count >)
         vec)))

(defn query-endpoint-protocol-coverage
  "For each endpoint, show which protocols are used in its dependency tree."
  [db]
  (let [endpoints (datalog/query-entities-with-aspect db :atlas/interface-endpoint)
        get-all-deps (fn get-deps [id visited]
                      (if (contains? visited id)
                        visited
                        (let [deps (datalog/query-dependencies db id)
                              new-visited (conj visited id)]
                          (reduce #(get-deps %2 %1) new-visited deps))))
        get-protocols-in-tree (fn [endpoint-id]
                               (let [all-deps (get-all-deps endpoint-id #{})
                                     protocols (mapcat
                                               (fn [dep-id]
                                                 (d/q '[:find [?proto ...]
                                                        :in $ ?dep
                                                        :where
                                                        [?e :atlas/dev-id ?dep]
                                                        [?e :entity/aspect ?proto]
                                                        [(namespace ?proto) ?ns]
                                                        [(= ?ns "protocol")]]
                                                      db dep-id))
                                               all-deps)]
                                 (distinct protocols)))]
    (->> endpoints
         (map (fn [ep-id]
                {:endpoint ep-id
                 :protocols (vec (get-protocols-in-tree ep-id))}))
         vec)))
