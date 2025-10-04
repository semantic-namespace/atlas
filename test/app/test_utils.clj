(ns app.test-utils
  "Shared testing utilities for semantic namespace applications.

  Provides common runtime helpers for testing registered functions:
  - Dependency resolution
  - Function invocation with dependency injection
  - Context/response key queries
  - Test fixtures for registry management
  - Runtime integration (invariants, executor, ide queries)"
  (:require
   [atlas.registry :as cid]
   [atlas.entity :as rt]
   [atlas.graph :as graph]
   [atlas.invariant :as ax]
   [atlas.executor :as exec]
   [atlas.ide :as ide]))

;; =============================================================================
;; RUNTIME UTILITIES
;; =============================================================================

(defn resolve-deps
  "Resolves component dependencies for a given dev-id.

  Takes a dev-id and a component-impls map, returns a map of resolved
  component implementations keyed by their short names.

  Example:
    (resolve-deps :fn/get-product component-impls)
    ;=> {:db {...}, :cache {...}}"
  [dev-id component-impls]
  (let [dep-ids (rt/deps-for dev-id)]
    (reduce (fn [acc dep-id]
              (if-let [component (get component-impls dep-id)]
                (assoc acc (keyword (name dep-id)) component)
                acc))
            {}
            dep-ids)))

(defn get-context-keys
  "Returns the context keys declared for a given dev-id.

  Example:
    (get-context-keys :fn/add-to-cart)
    ;=> [:session/data :item/product-id :item/quantity]"
  [dev-id]
  (:interface-endpoint/context (rt/props-for dev-id)))

(defn get-response-keys
  "Returns the response keys declared for a given dev-id.

  Example:
    (get-response-keys :fn/add-to-cart)
    ;=> [:cart/items :item/price]"
  [dev-id]
  (:interface-endpoint/response (rt/props-for dev-id)))

(defn make-invoke
  "Creates an invoke function for testing with dependency injection.

  Returns a function that takes a dev-id and context map, resolves
  dependencies, and invokes the implementation.

  Example:
    (def invoke (make-invoke function-impls component-impls))
    (invoke :fn/get-product {:product/id \"prod-123\"})
    ;=> {:product/name \"Widget\" :product/price 29.99M}"
  [function-impls component-impls]
  (fn invoke [dev-id ctx]
    (if-let [impl (get function-impls dev-id)]
      (let [deps (resolve-deps dev-id component-impls)]
        (impl ctx deps))
      (throw (ex-info (str "No implementation for " dev-id) {:dev-id dev-id})))))

;; =============================================================================
;; TEST FIXTURES
;; =============================================================================

(defn make-fixture
  "Creates a test fixture that resets the registry and initializes it.

  Takes an init-fn that will be called to populate the registry.
  Returns a fixture function suitable for use with clojure.test/use-fixtures.

  Example:
    (use-fixtures :each (make-fixture sut/init-registry!))"
  [init-fn]
  (fn [f]
    (reset! cid/registry {})
    (init-fn)
    (f)))

(defn make-fixture-with-reset
  "Creates a test fixture with custom reset function.

  Takes an init-fn and a reset-fn. The reset-fn is called before
  running tests to reset any stateful mocks.

  Example:
    (use-fixtures :each
      (make-fixture-with-reset
        sut/init-registry!
        reset-mock-sessions!))"
  [init-fn reset-fn]
  (fn [f]
    (reset! cid/registry {})
    (init-fn)
    (reset-fn)
    (f)))

;; =============================================================================
;; COMMON TEST HELPERS
;; =============================================================================

(defn all-function-dev-ids
  "Returns all dev-ids that have the :semantic-namespace/function aspect."
  []
  (->> @cid/registry
       (filter (fn [[aspects _]] (contains? aspects :atlas/execution-function)))
       (map (fn [[_ props]] (:atlas/dev-id props)))
       (remove nil?)))

(defn all-component-dev-ids
  "Returns all dev-ids that have the :semantic-namespace/component aspect."
  []
  (->> @cid/registry
       (filter (fn [[aspects _]] (contains? aspects :atlas/structure-component)))
       (map (fn [[_ props]] (:atlas/dev-id props)))
       (remove nil?)))

(defn all-endpoint-dev-ids
  "Returns all dev-ids that have the :semantic-namespace/endpoint aspect."
  []
  (->> @cid/registry
       (filter (fn [[aspects _]] (contains? aspects :atlas/interface-endpoint)))
       (map (fn [[_ props]] (:atlas/dev-id props)))
       (remove nil?)))

;; =============================================================================
;; ASSERTION HELPERS
;; =============================================================================

(defn has-context?
  "Checks if a dev-id has context keys defined.
  Returns true if context is a non-empty vector."
  [dev-id]
  (let [ctx (get-context-keys dev-id)]
    (and (vector? ctx) (seq ctx))))

(defn has-response?
  "Checks if a dev-id has response keys defined.
  Returns true if response is a non-empty vector."
  [dev-id]
  (let [resp (get-response-keys dev-id)]
    (and (vector? resp) (seq resp))))

(defn deps-exist?
  "Checks if all dependencies for a dev-id exist in the registry.
  Returns [true nil] if all exist, [false missing-deps] otherwise."
  [dev-id all-dev-ids]
  (let [deps (rt/deps-for dev-id)
        missing (remove (set all-dev-ids) deps)]
    (if (empty? missing)
      [true nil]
      [false missing])))

;; =============================================================================
;; RUNTIME AXIOM INTEGRATION
;; =============================================================================

(defn check-invariants
  "Run all runtime invariants and return normalized result.

  Returns:
    {:valid?   boolean
     :errors   [invariant-results with :severity :error]
     :warnings [invariant-results with :severity :warning]
     :violations [all invariant results]}"
  []
  (ax/check-all))

(defn invariant-errors
  "Extract just errors from invariant check result."
  [result]
  (:errors result))

(defn invariant-warnings
  "Extract just warnings from invariant check result."
  [result]
  (:warnings result))

(defn invariants-valid?
  "Check if registry passes all error-level invariants."
  []
  (:valid? (ax/check-all)))

(defn find-invariant
  "Find a specific invariant result by invariant keyword.

  Example:
    (find-invariant result :no-orphan-responses)
    ;=> {:invariant :no-orphan-responses :orphans #{...} ...}"
  [result invariant-key]
  (first (filter #(= invariant-key (:invariant %)) (:violations result))))

(defn invariant-orphan-keys
  "Extract orphan keys from no-orphan-responses invariant result.
  Returns nil if invariant not triggered."
  [result]
  (when-let [orphan-invariant (find-invariant result :no-orphan-responses)]
    (:orphans orphan-invariant)))

(defn invariant-missing-context
  "Extract missing context keys from context-satisfiable invariant result.
  Returns nil if invariant not triggered."
  [result]
  (when-let [ctx-invariant (find-invariant result :context-satisfiable)]
    (:missing ctx-invariant)))

;; =============================================================================
;; SEMANTIC EXECUTOR INTEGRATION
;; =============================================================================

(defn make-semantic-invoke
  "Creates an invoke function with semantic executor wrappers.

  Unlike make-invoke, this version:
  - Validates required context keys before execution
  - Merges response into context for pipeline-style execution
  - Applies timeout wrapper for :integration/external functions
  - Supports tracing via *trace* binding

  Example:
    (def invoke (make-semantic-invoke function-impls component-impls))
    (invoke :fn/get-product {:product/id \"prod-123\"})
    ;=> {:product/id \"prod-123\" :product/name \"Widget\" ...}"
  [function-impls component-impls]
  (fn invoke [dev-id ctx]
    (if-let [impl (get function-impls dev-id)]
      (let [deps (resolve-deps dev-id component-impls)
            wrapped-impl (fn [context] (impl context deps))]
        (exec/execute dev-id wrapped-impl ctx))
      (throw (ex-info (str "No implementation for " dev-id) {:dev-id dev-id})))))

(defn make-traced-invoke
  "Like make-semantic-invoke but with tracing enabled.
  Prints input/output keys for each function call."
  [function-impls component-impls]
  (let [base-invoke (make-semantic-invoke function-impls component-impls)]
    (fn [dev-id ctx]
      (binding [exec/*trace* true]
        (base-invoke dev-id ctx)))))

(defn build-endpoint-pipeline
  "Build an execution pipeline for an endpoint.

  Returns a function that takes initial context and executes
  all dependencies in topological order by data flow.

  Example:
    (def pipeline (build-endpoint-pipeline :endpoint/purchase-pet impl-map))
    (pipeline {:auth/user-id \"user-1\" :pet/id \"pet-1\" :payment/token \"tok_123\"})"
  [endpoint-id function-impls component-impls]
  (let [invoke (make-semantic-invoke function-impls component-impls)
        all-fns (conj (rt/deps-for endpoint-id) endpoint-id)
        order (graph/topo-sort-by-data all-fns)]
    (fn [initial-ctx]
      (reduce (fn [ctx dev-id]
                (if (:error ctx)
                  (reduced ctx)
                  (if (get function-impls dev-id)
                    (invoke dev-id ctx)
                    ctx)))
              initial-ctx
              order))))

;; =============================================================================
;; IDE QUERY INTEGRATION
;; =============================================================================

(defn list-entities
  "List all registered entities with their types.
  Wrapper around ide/list-all-entities."
  []
  (ide/list-all-entities))

(defn entity-info
  "Get detailed info about an entity.
  Wrapper around ide/entity-info."
  [dev-id]
  (ide/entity-info dev-id))

(defn find-by-aspect
  "Find all entities with a given aspect.
  Wrapper around ide/entities-with-aspect."
  [aspect]
  (ide/entities-with-aspect aspect))

(defn who-depends-on
  "Find all entities that depend on the given entity.
  Wrapper around ide/dependents-of."
  [dev-id]
  (ide/dependents-of dev-id))

(defn what-produces
  "Find all functions that produce a given data key.
  Wrapper around ide/producers-of."
  [data-key]
  (ide/producers-of data-key))

(defn what-consumes
  "Find all functions that consume a given data key.
  Wrapper around ide/consumers-of."
  [data-key]
  (ide/consumers-of data-key))

(defn trace-data-key
  "Trace how a data key flows through the system.
  Wrapper around ide/trace-data-flow."
  [data-key]
  (ide/trace-data-flow data-key))

(defn impact-of
  "Show what would be affected if an entity changes.
  Wrapper around ide/impact-of-change."
  [entity-id]
  (ide/impact-of-change entity-id))
