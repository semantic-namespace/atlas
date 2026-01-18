(ns atlas.datalog
  "Datascript/Datalog backend for invariant checking."
  (:require [datascript.core :as d]
            [atlas.registry :as registry]))

;; =============================================================================
;; FACT GENERATION
;; =============================================================================

(defn registry-facts
  "Convert registry into Datascript facts.
   Returns vector of facts: [entity-id attribute value]"
  []
  (let [facts (atom [])]
    (doseq [[compound-id props] @registry/registry
            :let [dev-id (:atlas/dev-id props)]]
      ;; Entity existence
      (when dev-id
        (swap! facts conj [:db/add dev-id :atlas/dev-id dev-id])

        ;; Aspects (each aspect becomes a fact)
        (doseq [aspect compound-id]
          (swap! facts conj [:db/add dev-id :entity/aspect aspect]))

        ;; Dependencies
        (doseq [dep (:execution-function/deps props)]
          (swap! facts conj [:db/add dev-id :entity/depends dep]))

        ;; Context (consumed keys)
        (doseq [ctx (or (:interface-endpoint/context props)
                        (:execution-function/context props))]
          (swap! facts conj [:db/add dev-id :entity/consumes ctx]))

        ;; Response (produced keys)
        (doseq [resp (or (:interface-endpoint/response props)
                         (:execution-function/response props))]
          (swap! facts conj [:db/add dev-id :entity/produces resp]))

        ;; Endpoint context (special case)
        (when (contains? compound-id :atlas/interface-endpoint)
          (doseq [ctx (:interface-endpoint/context props)]
            (swap! facts conj [:db/add dev-id :endpoint-context ctx]))
          (doseq [resp (:interface-endpoint/response props)]
            (swap! facts conj [:db/add dev-id :endpoint-response resp])))

        ;; Dataflow terminal markers
        (when (contains? compound-id :dataflow/external-input)
          (swap! facts conj [:db/add dev-id :dataflow/external-input dev-id]))
        (when (contains? compound-id :dataflow/display-output)
          (swap! facts conj [:db/add dev-id :dataflow/display-output dev-id]))

        ;; Protocol functions (for protocol entities)
        (when (contains? compound-id :atlas/interface-protocol)
          (doseq [protocol-fn (:interface-protocol/functions props)]
            (swap! facts conj [:db/add dev-id :protocol/function protocol-fn])))))
    @facts)

  )

(defn create-db
  "Create Datascript database with schema and facts."
  []
  (let [schema {:atlas/dev-id {:db/unique :db.unique/identity}
                :entity/aspect {:db/cardinality :db.cardinality/many}
                :entity/depends {:db/cardinality :db.cardinality/many}
                :entity/consumes {:db/cardinality :db.cardinality/many}
                :entity/produces {:db/cardinality :db.cardinality/many}
                :endpoint-context {:db/cardinality :db.cardinality/many}
                :endpoint-response {:db/cardinality :db.cardinality/many}
                :dataflow/external-input {:db/cardinality :db.cardinality/many}
                :dataflow/display-output {:db/cardinality :db.cardinality/many}
                :protocol/function {:db/cardinality :db.cardinality/many}}
        conn (d/create-conn schema)
        facts (registry-facts)]
    ;; First pass: create all entities with their :dev/id
    (let [dev-ids (distinct (map #(nth % 1) facts))]
      (d/transact! conn (mapv (fn [dev-id] {:atlas/dev-id dev-id}) dev-ids)))
    ;; Second pass: add all attributes using lookup refs
    (let [tx-data (mapv (fn [[_ dev-id attr value]]
                          [:db/add [:atlas/dev-id dev-id] attr value])
                        facts)]
      (d/transact! conn tx-data))
    @conn))

;; =============================================================================
;; DATALOG QUERIES
;; =============================================================================

(defn query-entities-with-aspect
  "Find all entities with a specific aspect."
  [db aspect]
  (d/q '[:find [?dev-id ...]
         :in $ ?aspect
         :where
         [?e :atlas/dev-id ?dev-id]
         [?e :entity/aspect ?aspect]]
       db aspect))

(defn query-entities-lacking-aspect
  "Find all entities lacking a specific aspect."
  [db aspect]
  (let [all (d/q '[:find [?dev-id ...]
                   :where
                   [?e :atlas/dev-id ?dev-id]]
                 db)
        with-aspect (set (query-entities-with-aspect db aspect))]
    (remove with-aspect all)))

(defn query-dependencies
  "Find all dependencies of an entity."
  [db dev-id]
  (d/q '[:find [?dep ...]
         :in $ ?dev-id
         :where
         [?e :atlas/dev-id ?dev-id]
         [?e :entity/depends ?dep]]
       db dev-id))

(defn query-depends-on
  "Check if entity depends on specific entity."
  [db dev-id dep-id]
  (seq (d/q '[:find ?e
              :in $ ?dev-id ?dep-id
              :where
              [?e :atlas/dev-id ?dev-id]
              [?e :entity/depends ?dep-id]]
            db dev-id dep-id)))

(defn query-produces
  "Find all keys produced by an entity."
  [db dev-id]
  (d/q '[:find [?key ...]
         :in $ ?dev-id
         :where
         [?e :atlas/dev-id ?dev-id]
         [?e :entity/produces ?key]]
       db dev-id))

(defn query-consumes
  "Find all keys consumed by an entity."
  [db dev-id]
  (d/q '[:find [?key ...]
         :in $ ?dev-id
         :where
         [?e :atlas/dev-id ?dev-id]
         [?e :entity/consumes ?key]]
       db dev-id))

(defn query-producers-of
  "Find all entities that produce a specific key."
  [db key]
  (d/q '[:find [?dev-id ...]
         :in $ ?key
         :where
         [?e :atlas/dev-id ?dev-id]
         [?e :entity/produces ?key]]
       db key))

(defn query-consumers-of
  "Find all entities that consume a specific key."
  [db key]
  (d/q '[:find [?dev-id ...]
         :in $ ?key
         :where
         [?e :atlas/dev-id ?dev-id]
         [?e :entity/consumes ?key]]
       db key))

(defn query-endpoint-inputs
  "Find all keys that are endpoint inputs."
  [db]
  (d/q '[:find [?key ...]
         :where
         [?e :endpoint-context ?key]]
       db))

;; =============================================================================
;; INVARIANT-SPECIFIC QUERIES
;; =============================================================================

(defn query-missing-producers
  "Find consumed keys that have no producer AND don't come from endpoints.
   A key is OK if:
   - It has a producer (some entity produces it), OR
   - The consuming entity is an endpoint (gets it from client), OR
   - It's an endpoint input (appears in some endpoint's context), OR
   - It's marked as :dataflow/external-input"
  [db]
  (d/q '[:find ?ctx ?consumer
         :where
         [?e :atlas/dev-id ?consumer]
         [?e :entity/consumes ?ctx]
         (not [?e :entity/aspect :atlas/interface-endpoint])
         (not-join [?ctx]
                   (or [_ :entity/produces ?ctx]
                       [_ :endpoint-context ?ctx]
                       [_ :dataflow/external-input ?ctx]))]
       db))

(defn query-orphan-outputs
  "Find produced keys that have no consumer AND producer is not an endpoint.
   Endpoint outputs are terminal (go to client) and not orphans."
  [db]
  (d/q '[:find ?resp ?producer
         :where
         [?e :atlas/dev-id ?producer]
         [?e :entity/produces ?resp]
         (not-join [?resp]
                   (or [_ :entity/consumes ?resp]
                       [_ :endpoint-response ?resp]
                       [_ :dataflow/display-output ?resp]))
         (not [?e :entity/aspect :atlas/interface-endpoint])]
       db))

(defn query-missing-dependencies
  "Find dependencies that reference non-existent entities."
  [db]
  (d/q '[:find ?dev ?missing
         :where
         [?e :atlas/dev-id ?dev]
         [?e :entity/depends ?missing]
         (not [_ :atlas/dev-id ?missing])]
       db))

(defn query-acyclic-deps
  "Check if dependency graph is acyclic by finding cycles."
  [db]
  ;; This is complex in pure Datalog, so we use a recursive approach
  (let [all-entities (d/q '[:find [?dev-id ...]
                            :where [?e :atlas/dev-id ?dev-id]]
                          db)
        deps-map (into {} (map (fn [id]
                                 [id (set (query-dependencies db id))])
                               all-entities))]
    (letfn [(has-cycle? [id visited path]
              (cond
                (contains? path id) {:cycle (conj path id)}
                (contains? visited id) nil
                :else (some #(has-cycle? % (conj visited id) (conj path id))
                            (get deps-map id #{}))))]
      (some #(has-cycle? % #{} []) all-entities))))

(defn query-unreachable-functions
  "Find functions not reachable from any endpoint."
  [db]
  (let [endpoints (query-entities-with-aspect db :atlas/interface-endpoint)
        all-fns (set (query-entities-with-aspect db :atlas/execution-function))
        reachable (atom #{})
        collect-reachable (fn collect [id]
                            (when-not (@reachable id)
                              (swap! reachable conj id)
                              (doseq [dep (query-dependencies db id)]
                                (collect dep))))]
    (doseq [ep endpoints]
      (collect-reachable ep))
    (remove @reachable all-fns)))

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
  (let [;; Get all protocol aspects (keywords with 'protocol' namespace)
        all-protocol-aspects (d/q '[:find [?aspect ...]
                                    :where
                                    [?e :entity/aspect ?aspect]
                                    [(namespace ?aspect) ?ns]
                                    [(= ?ns "protocol")]]
                                  db)
        ;; Get protocol aspects that have protocol definitions
        defined-protocols (set (d/q '[:find [?dev-id ...]
                                      :where
                                      [?e :atlas/dev-id ?dev-id]
                                      [?e :entity/aspect :atlas/interface-protocol]]
                                    db))]
    ;; Protocol aspects used but not defined
    (remove defined-protocols all-protocol-aspects)))


;; =============================================================================
;; ADVANCED QUERIES
;; =============================================================================
;;
;; These queries provide powerful semantic analysis capabilities beyond basic
;; invariant checking. They enable:
;;
;; - Data flow tracing (producers/consumers)
;; - Dependency chain analysis (transitive dependencies)
;; - Impact analysis (reverse dependencies)
;; - Semantic distribution (aspect frequency)
;; - Complex semantic queries (multi-aspect filtering)
;;
;; For comprehensive examples and documentation, see:
;; - docs/ADVANCED_QUERIES.md
;; - semantic-namespace.app.pet-shop-queries
;;
;; =============================================================================

(defn query-data-flow
  "Trace data flow from producers to consumers for a specific key."
  [db data-key]
  {:key data-key
   :producers (query-producers-of db data-key)
   :consumers (query-consumers-of db data-key)})

(defn query-dependency-chain
  "Find all transitive dependencies of an entity."
  [db dev-id]
  (loop [to-visit [dev-id]
         visited #{}
         deps #{}]
    (if (empty? to-visit)
      deps
      (let [current (first to-visit)
            current-deps (query-dependencies db current)
            new-deps (remove visited current-deps)]
        (recur (concat (rest to-visit) new-deps)
               (conj visited current)
               (into deps new-deps))))))

(defn query-reverse-dependencies
  "Find all entities that depend on the given entity."
  [db dev-id]
  (d/q '[:find [?dependent ...]
         :in $ ?target
         :where
         [?e :atlas/dev-id ?dependent]
         [?e :entity/depends ?target]]
       db dev-id))

(defn query-aspect-frequency
  "Get frequency count of each aspect."
  [db]
  (d/q '[:find ?aspect (count ?e)
         :where
         [?e :entity/aspect ?aspect]]
       db))

(defn query-entities-by-multiple-aspects
  "Find entities that have ALL of the specified aspects."
  [db aspects]
  (let [aspect-list (vec aspects)]
    (d/q {:find ['?dev-id]
          :in ['$ '[[?aspect ...]]]
          :where [['?e :atlas/dev-id '?dev-id]
                  ['?e :entity/aspect '?aspect]]}
         db [aspect-list])))

;; =============================================================================
;; DSL COMPILATION
;; =============================================================================
;;
;; These functions compile the lightweight logic DSL (used in invariant definitions)
;; into executable Datascript queries.
;;
;; The DSL supports:
;; - :arg/var syntax for query variables
;; - :logic/has-aspect predicate for aspect checking
;; - {:not [...]} for negation
;;
;; Example DSL:
;;   [:find [:arg/dev ...]
;;    :where
;;    [:logic/has-aspect :arg/dev :semantic-namespace/function]
;;    {:not [[:logic/has-aspect :arg/dev :tier/foundation]]}]
;;
;; =============================================================================

(defn has-aspect
  "Datascript predicate that checks if `dev-id` carries a given aspect.
   Intended for use in DSL compilation."
  [db dev-id aspect]
  (boolean (some #{dev-id}
                 (query-entities-with-aspect db aspect))))

(defn- arg->sym
  "Turn `:arg/foo` keywords into Datascript vars like `?foo`. Leave other inputs alone."
  [x]
  (if (and (keyword? x) (= "arg" (namespace x)))
    (symbol (str "?" (name x)))
    x))

(defn- compile-find
  "Convert a DSL find spec (e.g. [:arg/dev ...]) into Datascript form."
  [find-spec]
  (cond
    (and (vector? find-spec)
         (= 2 (count find-spec))
         (= '... (second find-spec)))
    [(arg->sym (first find-spec)) '...]

    (keyword? find-spec)
    (arg->sym find-spec)

    :else find-spec))

(defn- compile-clause
  [clause]
  (cond
    (vector? clause)
    (let [[op & args] clause]
      (if (= op :logic/has-aspect)
        ;; Expand :logic/has-aspect into Datascript patterns
        ;; [:logic/has-aspect :arg/dev :some/aspect] becomes:
        ;; [?e :dev/id ?dev] [?e :entity/aspect :some/aspect]
        (let [[dev-arg aspect-arg] args
              dev-var (arg->sym dev-arg)
              entity-var (gensym "?e")]
          [[entity-var :atlas/dev-id dev-var]
           [entity-var :entity/aspect aspect-arg]])
        (mapv arg->sym clause)))

    (and (map? clause) (:not clause))
    (apply list 'not (mapcat compile-clause (:not clause)))

    :else clause))

(defn compile-logic-query
  "Compile the lightweight logic DSL (using :arg/vars and :logic/ predicates) into
  a runnable Datascript query. Example input:

  [:find [:arg/dev ...]
   :where
   [:logic/has-aspect :arg/dev :semantic-namespace/component]
   {:not [[:logic/has-aspect :arg/dev :tier/foundation]]}]

  This returns the equivalent Datascript query that can be executed with
  `(d/q (compile-logic-query dsl) (create-db))`."
  [dsl]
  (let [parts (vec dsl)
        where-idx (.indexOf parts :where)
        find-spec (get parts 1)
        where-clauses (subvec parts (inc where-idx))]
    (into [:find (compile-find find-spec) :in '$ :where]
          (mapcat (fn [clause]
                    (let [result (compile-clause clause)]
                      (if (and (sequential? result)
                               (sequential? (first result))
                               (not (list? result)))
                        result  ; Already a sequence of clauses
                        [result])))  ; Wrap single clause in vector
                  where-clauses))))

(defn run-logic-query
  "Helper that compiles and runs a DSL query against the live registry."
  [dsl]
  (let [query (compile-logic-query dsl)
        db (create-db)]
    (d/q query db)))




;; =============================================================================
;; PROTOCOL-ORIENTED QUERIES
;; =============================================================================

(defn query-protocol-implementers-by-domain
  "Find all protocol implementers grouped by domain.
   Returns map of domain -> {:components [...] :protocols [...]}"
  [db]
  (let [components (query-entities-with-aspect db :atlas/structure-component)
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
  "Find all functions that depend on components implementing a specific protocol.
   Example: (query-functions-using-protocol db :protocol/oauth)
   Returns functions that use OAuth through component dependencies."
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
  (let [all-fns (query-entities-with-aspect db :atlas/execution-function)
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
                          (->> (query-dependencies db fn-id)
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
  "Find all external integration points with their protocols.
   Useful for understanding system boundaries and dependencies on external services."
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
  "Categorize functions by purity based on component dependencies.
   Pure functions have no component deps, impure ones do."
  [db]
  (let [all-fns (query-entities-with-aspect db :atlas/execution-function)]
    {:pure (->> all-fns
                (filter #(empty? (query-dependencies db %)))
                vec)
     :impure (->> all-fns
                  (remove #(empty? (query-dependencies db %)))
                  vec)}))

(defn query-protocol-dependency-graph
  "Build a graph showing which protocols depend on which other protocols.
   A protocol A depends on B if any component implementing A also depends on
   a component implementing B."
  [db]
  (let [all-protocols (query-entities-with-aspect db :atlas/interface-protocol)]
    (->> all-protocols
         (map (fn [proto]
                (let [implementers (query-components-implementing-protocol db proto)
                      ;; Get all components that implementers depend on
                      dep-components (mapcat #(query-dependencies db %) implementers)
                      ;; Find protocols of those dep components
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
  "Find aspects that commonly appear together.
   Returns pairs of aspects and how often they co-occur.
   Useful for identifying architectural patterns."
  [db]
  (let [all-entities (d/q '[:find ?e
                            :where [?e :atlas/dev-id _]]
                          db)
        entity-aspects (fn [entity-id]
                        (d/q '[:find [?aspect ...]
                               :in $ ?e
                               :where [?e :entity/aspect ?aspect]]
                             db entity-id))
        ;; Generate pairs of aspects from each entity
        aspect-pairs (mapcat (fn [[entity-id]]
                              (let [aspects (entity-aspects entity-id)]
                                (for [a aspects
                                      b aspects
                                      :when (not= a b)]
                                  (if (neg? (compare (str a) (str b)))
                                    [a b]
                                    [b a]))))
                            all-entities)
        ;; Count frequencies
        freq-map (frequencies aspect-pairs)]
    (->> freq-map
         (map (fn [[[a b] count]] {:aspect-a a :aspect-b b :count count}))
         (sort-by :count >)
         vec)))

(defn query-endpoint-protocol-coverage
  "For each endpoint, show which protocols are used in its dependency tree.
   Useful for understanding what external services each API endpoint depends on."
  [db]
  (let [endpoints (query-entities-with-aspect db :atlas/interface-endpoint)
        get-all-deps (fn get-deps [id visited]
                      (if (contains? visited id)
                        visited
                        (let [deps (query-dependencies db id)
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

;; =============================================================================
;; ERROR IMPACT ANALYSIS QUERIES
;; =============================================================================

(defn query-reverse-dependencies
  "Find all entities that depend on the given entity (reverse dependencies)."
  [db dev-id]
  (d/q '[:find [?dependent ...]
         :in $ ?target
         :where
         [?e :entity/depends ?target]
         [?e :atlas/dev-id ?dependent]]
       db dev-id))

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
            current-dependents (query-reverse-dependencies db current)
            new-dependents (remove visited current-dependents)]
        (recur (concat (rest to-visit) new-dependents)
               (conj visited current)
               (into dependents new-dependents))))))

(defn query-affected-endpoints
  "Find all endpoints affected by an error in a given entity.
   Returns endpoints with their dependency paths to the broken entity.

   Example: (query-affected-endpoints db :fn/refresh-oauth-token)
   => [{:endpoint :endpoint/query-availability
        :path [:endpoint/query-availability :fn/collect-available-users :fn/refresh-oauth-token]}]"
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
        ;; Build path from endpoint to broken entity
        build-path (fn build-path [from to visited]
                    (if (= from to)
                      [to]
                      (let [deps (query-dependencies db from)]
                        ;; Find which dependency leads to target
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
   Shows the complete blast radius of a protocol failure.

   Example: (query-protocol-error-impact db :protocol/oauth)
   => {:protocol :protocol/oauth
       :broken-components [:component/google-oauth]
       :broken-functions [:fn/refresh-oauth-token]
       :affected-endpoints [{:endpoint :endpoint/query-availability
                             :why \"depends on :fn/refresh-oauth-token\"}]}"
  [db protocol-id]
  (let [implementers (query-components-implementing-protocol db protocol-id)
        get-aspects (fn [dev-id]
                     (d/q '[:find [?aspect ...]
                            :in $ ?id
                            :where
                            [?e :atlas/dev-id ?id]
                            [?e :entity/aspect ?aspect]]
                          db dev-id))
        ;; Find all functions that depend on these components
        broken-functions (->> implementers
                             (mapcat #(query-reverse-dependencies db %))
                             (filter #(contains? (set (get-aspects %)) :atlas/execution-function))
                             distinct
                             vec)
        ;; Find all affected endpoints (transitively)
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
  "Given a function with an error, show all affected endpoints with dependency paths.

   Example: (query-function-error-impact db :fn/refresh-oauth-token)
   => {:function :fn/refresh-oauth-token
       :affected-endpoints [{:endpoint :endpoint/query-availability
                             :path [:endpoint/query-availability -> :fn/collect-available-users -> :fn/refresh-oauth-token]}]
       :affected-count 1}"
  [db function-id]
  (let [affected (query-affected-endpoints db function-id)]
    {:function function-id
     :affected-endpoints affected
     :affected-count (count affected)}))

(defn query-component-error-impact
  "Given a component with an error, show all affected functions and endpoints.

   Example: (query-component-error-impact db :component/google-oauth)
   => {:component :component/google-oauth
       :protocols [:protocol/oauth]
       :broken-functions [:fn/refresh-oauth-token]
       :affected-endpoints [:endpoint/query-availability]}"
  [db component-id]
  (let [;; Get protocols this component implements
        protocols (d/q '[:find [?proto ...]
                        :in $ ?comp-id
                        :where
                        [?e :atlas/dev-id ?comp-id]
                        [?e :entity/aspect ?proto]
                        [(namespace ?proto) ?ns]
                        [(= ?ns "protocol")]]
                      db component-id)
        ;; Find functions that depend on this component
        broken-functions (vec (query-reverse-dependencies db component-id))
        ;; Find all affected endpoints
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


;; - Protocol usage analysis (who uses what protocols)
;; - Integration point mapping (external service dependencies)
;; - Architectural pattern detection (aspect co-occurrence)
;; - Error impact analysis (what breaks when X fails)
