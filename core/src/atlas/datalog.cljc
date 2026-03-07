(ns atlas.datalog
  "Datascript/Datalog backend for invariant checking."
  (:require [datascript.core :as d]
            [atlas.registry :as registry]
            [atlas.registry.lookup :as entity]
            [atlas.query :as q]))

;; =============================================================================
;; FACT GENERATION
;; =============================================================================

(defn core-registry-facts
  "Extract core Atlas facts (always included).
   These are universal facts that apply to all entities."
  []
  (let [facts (atom [])]
    (doseq [[compound-id props] @registry/registry
            :let [dev-id (:atlas/dev-id props)]]
      (when dev-id
        ;; Entity existence
        (swap! facts conj [:db/add dev-id :atlas/dev-id dev-id])

        ;; Aspects (universal - all entities have aspects)
        (doseq [aspect compound-id]
          (swap! facts conj [:db/add dev-id :entity/aspect aspect]))

        ;; Dataflow terminal markers (core markers)
        (when (contains? compound-id :dataflow/external-input)
          (swap! facts conj [:db/add dev-id :dataflow/external-input dev-id]))
        (when (contains? compound-id :dataflow/display-output)
          (swap! facts conj [:db/add dev-id :dataflow/display-output dev-id]))))
    @facts))

(defn custom-registry-facts
  "Extract facts using extractors discovered from the registry.
   Finds all :atlas/datalog-extractor entities and calls their :datalog-extractor/fn."
  []
  (let [extractors (->> (q/find-by-aspect @registry/registry :atlas/datalog-extractor)
                        vals
                        (map :datalog-extractor/fn)
                        (remove nil?))
        facts (atom [])]
    (doseq [[compound-id props] @registry/registry
            extractor extractors]
      (when-let [extracted (seq (extractor compound-id props))]
        (swap! facts concat extracted)))
    @facts))

(defn registry-facts
  "Convert registry into Datascript facts (core + custom).
   Returns vector of facts: [:db/add entity-id attribute value]"
  []
  (concat (core-registry-facts)
          (custom-registry-facts)))

(defn core-schema
  "Core Atlas schema (always included).
   These attributes are fundamental to all Atlas entities."
  []
  {:atlas/dev-id {:db/unique :db.unique/identity}
   :entity/aspect {:db/cardinality :db.cardinality/many}
   ;; Dataflow markers (core markers for terminal nodes)
   :dataflow/external-input {:db/cardinality :db.cardinality/many}
   :dataflow/display-output {:db/cardinality :db.cardinality/many}})

(defn build-schema
  "Build complete schema (core + contributions from ontology extractors)."
  []
  (let [extractor-schemas (->> (q/find-by-aspect @registry/registry :atlas/datalog-extractor)
                               vals
                               (map :datalog-extractor/schema)
                               (remove nil?))]
    (apply merge (core-schema) extractor-schemas)))

(defn create-db
  "Create Datascript database with extensible schema and facts."
  []
  (let [schema (build-schema)
        conn (d/create-conn schema)
        facts (registry-facts)]
    ;; First pass: create all entities with their :atlas/dev-id
    (let [dev-ids (distinct (map #(nth % 1) facts))]
      (d/transact! conn (mapv (fn [dev-id] {:atlas/dev-id dev-id}) dev-ids)))
    ;; Second pass: add all attributes using lookup refs
    (let [tx-data (mapv (fn [[_ dev-id attr value]]
                          [:db/add [:atlas/dev-id dev-id] attr value])
                        facts)]
      (d/transact! conn tx-data))
    @conn))

;; =============================================================================
;; DB CACHING
;; =============================================================================
;;
;; The registry is loaded at app startup and remains stable thereafter.
;; We lazily create and cache the Datascript DB on first complex query,
;; then reuse it for all subsequent queries.
;;
;; For testing or explicit reload scenarios, use `reset-db-cache!`.

;; Cached Datascript database. Created lazily on first access via `get-db`.
(defonce ^:private db-cache (atom nil))

(defn get-db
  "Get the cached Datascript DB, creating it on first call.

   The DB is built from the current registry state and cached indefinitely,
   since the registry is stable after app startup.

   For complex queries, prefer this over `create-db` to avoid rebuilding
   the database on every query."
  []
  (or @db-cache
      (swap! db-cache (fn [existing]
                        (or existing (create-db))))))

(defn reset-db-cache!
  "Reset the cached DB, forcing recreation on next `get-db` call.

   Use this:
   - In tests (to ensure fresh state)
   - After explicit registry modifications (rare)
   - When ontology extensions are registered after initial load"
  []
  (reset! db-cache nil))

(defn db-cached?
  "Check if the DB has been cached (useful for diagnostics)."
  []
  (some? @db-cache))

(defn reset-all!
  "Reset DB cache (useful for testing).
   Extensions now live in the registry, so only the DB cache needs resetting."
  []
  (reset-db-cache!))

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
  "Find functions not reachable from any endpoint.

   Filters out ontology meta-entities (marked with :atlas/ontology) as they
   are not business logic."
  [db]
  (let [endpoints (query-entities-with-aspect db :atlas/interface-endpoint)
        ;; Get all execution-functions but exclude ontology meta-entities
        all-fns (->> (query-entities-with-aspect db :atlas/execution-function)
                     (remove #(entity/has-aspect? % :atlas/ontology))
                     set)
        reachable (atom #{})
        collect-reachable (fn collect [id]
                            (when-not (@reachable id)
                              (swap! reachable conj id)
                              (doseq [dep (query-dependencies db id)]
                                (collect dep))))]
    (doseq [ep endpoints]
      (collect-reachable ep))
    (remove @reachable all-fns)))

;; Delegation stubs — protocol queries moved to atlas.datalog.protocol
(defn query-protocol-functions          [db protocol-id]    ((requiring-resolve 'atlas.datalog.protocol/query-protocol-functions) db protocol-id))
(defn query-components-implementing-protocol [db protocol-aspect] ((requiring-resolve 'atlas.datalog.protocol/query-components-implementing-protocol) db protocol-aspect))
(defn query-undefined-protocols         [db]                ((requiring-resolve 'atlas.datalog.protocol/query-undefined-protocols) db))


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
  "Find all entities that depend on the given entity (reverse dependencies)."
  [db dev-id]
  (d/q '[:find [?dependent ...]
         :in $ ?target
         :where
         [?e :atlas/dev-id ?dependent]
         [?e :entity/depends ?target]]
       db dev-id))

;; =============================================================================
;; CLOSURE QUERIES (with hop tracking)
;; =============================================================================
;;
;; These functions compute transitive closures with:
;; - Support for multiple start nodes
;; - Max-hops limiting
;; - Hop count tracking per entity
;;
;; Used by atlas.ide and atlas.llm-ide for impact analysis.

(defn query-upstream-closure
  "Transitive closure of dependencies (what this entity depends on).

   Args:
     db        - Datascript database
     start-ids - Single dev-id or set of dev-ids to start from
     max-hops  - Maximum number of hops to traverse (nil = unlimited)

   Returns vector of {:entity dev-id :hops n} sorted by hops ascending.
   Does not include start-ids in the result."
  [db start-ids max-hops]
  (let [start-set (if (set? start-ids) start-ids #{start-ids})]
    (loop [frontier start-set
           visited start-set
           result []
           hop 0]
      (if (or (empty? frontier)
              (and max-hops (>= hop max-hops)))
        result
        (let [next-frontier (->> frontier
                                 (mapcat #(query-dependencies db %))
                                 set
                                 (#(clojure.set/difference % visited)))
              hop-results (map (fn [e] {:entity e :hops (inc hop)}) next-frontier)]
          (recur next-frontier
                 (into visited next-frontier)
                 (into result hop-results)
                 (inc hop)))))))

(defn query-downstream-closure
  "Transitive closure of dependents (what depends on this entity).

   Args:
     db        - Datascript database
     start-ids - Single dev-id or set of dev-ids to start from
     max-hops  - Maximum number of hops to traverse (nil = unlimited)

   Returns vector of {:entity dev-id :hops n} sorted by hops ascending.
   Does not include start-ids in the result."
  [db start-ids max-hops]
  (let [start-set (if (set? start-ids) start-ids #{start-ids})]
    (loop [frontier start-set
           visited start-set
           result []
           hop 0]
      (if (or (empty? frontier)
              (and max-hops (>= hop max-hops)))
        result
        (let [next-frontier (->> frontier
                                 (mapcat #(query-reverse-dependencies db %))
                                 set
                                 (#(clojure.set/difference % visited)))
              hop-results (map (fn [e] {:entity e :hops (inc hop)}) next-frontier)]
          (recur next-frontier
                 (into visited next-frontier)
                 (into result hop-results)
                 (inc hop)))))))

(defn query-aspect-frequency
  "Get frequency count of each aspect."
  [db]
  (d/q '[:find ?aspect (count ?e)
         :where
         [?e :entity/aspect ?aspect]]
       db))


;; Delegation stubs — explorer queries moved to atlas.datalog.explorer
(defn query-entity-aspects             [db dev-id]                     ((requiring-resolve 'atlas.datalog.explorer/query-entity-aspects) db dev-id))
(defn query-entity-type                [db dev-id]                     ((requiring-resolve 'atlas.datalog.explorer/query-entity-type) db dev-id))
(defn query-entities-with-all-aspects  [db aspects]                    ((requiring-resolve 'atlas.datalog.explorer/query-entities-with-all-aspects) db aspects))
(defn query-entities-with-any-aspect   [db aspects]                    ((requiring-resolve 'atlas.datalog.explorer/query-entities-with-any-aspect) db aspects))
(defn query-all-entities               [db]                            ((requiring-resolve 'atlas.datalog.explorer/query-all-entities) db))
(defn query-explorer-filter            [db aspects-and aspects-or]     ((requiring-resolve 'atlas.datalog.explorer/query-explorer-filter) db aspects-and aspects-or))
(defn query-structural-gaps
  ([db]                       ((requiring-resolve 'atlas.datalog.explorer/query-structural-gaps) db))
  ([db min-similarity]        ((requiring-resolve 'atlas.datalog.explorer/query-structural-gaps) db min-similarity))
  ([db min-similarity max-r]  ((requiring-resolve 'atlas.datalog.explorer/query-structural-gaps) db min-similarity max-r)))

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
  `(d/q (compile-logic-query dsl) (get-db))`.

  Prefer using `run-logic-query` which handles caching automatically."
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
  "Helper that compiles and runs a DSL query against the cached DB.

   Uses `get-db` for efficient repeated queries (DB is cached after first call).

   Automatically filters out ontology meta-entities (marked with :atlas/ontology)
   from results, as most business logic queries should only operate on application
   entities."
  [dsl]
  (let [query (compile-logic-query dsl)
        db (get-db)
        results (d/q query db)]
    ;; Filter out ontology meta-entities from results
    (if (and (sequential? results) (every? keyword? results))
      ;; Vector of keywords - filter ontologies
      (vec (remove #(entity/has-aspect? % :atlas/ontology) results))
      ;; Other result format - return as is
      results)))




;; Delegation stubs — protocol-oriented queries moved to atlas.datalog.protocol
(defn query-protocol-implementers-by-domain [db]             ((requiring-resolve 'atlas.datalog.protocol/query-protocol-implementers-by-domain) db))
(defn query-functions-using-protocol        [db protocol-kw] ((requiring-resolve 'atlas.datalog.protocol/query-functions-using-protocol) db protocol-kw))
(defn query-protocol-usage-matrix           [db]             ((requiring-resolve 'atlas.datalog.protocol/query-protocol-usage-matrix) db))
(defn query-integration-points              [db]             ((requiring-resolve 'atlas.datalog.protocol/query-integration-points) db))
(defn query-pure-vs-impure-functions        [db]             ((requiring-resolve 'atlas.datalog.protocol/query-pure-vs-impure-functions) db))
(defn query-protocol-dependency-graph       [db]             ((requiring-resolve 'atlas.datalog.protocol/query-protocol-dependency-graph) db))
(defn query-aspect-co-occurrence            [db]             ((requiring-resolve 'atlas.datalog.protocol/query-aspect-co-occurrence) db))
(defn query-endpoint-protocol-coverage      [db]             ((requiring-resolve 'atlas.datalog.protocol/query-endpoint-protocol-coverage) db))

;; Delegation stubs — error impact queries moved to atlas.datalog.impact
(defn query-transitive-reverse-dependencies [db dev-id]      ((requiring-resolve 'atlas.datalog.impact/query-transitive-reverse-dependencies) db dev-id))
(defn query-affected-endpoints              [db entity-id]   ((requiring-resolve 'atlas.datalog.impact/query-affected-endpoints) db entity-id))
(defn query-protocol-error-impact           [db protocol-id] ((requiring-resolve 'atlas.datalog.impact/query-protocol-error-impact) db protocol-id))
(defn query-function-error-impact           [db function-id] ((requiring-resolve 'atlas.datalog.impact/query-function-error-impact) db function-id))
(defn query-component-error-impact          [db component-id] ((requiring-resolve 'atlas.datalog.impact/query-component-error-impact) db component-id))

(comment
  (reset-db-cache!)



  (query-consumes (get-db) :mcp-api.endpoint/bank-fees-search)
  (query-produces (get-db) :mcp-api.endpoint/inbox-provider)
  (query-consumes (get-db) :endpoint/subscription)
  (query-dependencies (get-db) :endpoint/subscription)
  (query-dependencies (get-db) :endpoint/subscription)
  (query-entity-aspects (get-db) :endpoint/subscription)
  (query-produces (get-db) :endpoint/subscription)

  )
