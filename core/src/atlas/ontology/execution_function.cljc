(ns atlas.ontology.execution-function
  "Execution-function ontology module.

   This module defines the `:atlas/execution-function` entity type and related
   functionality. Auto-registers on require (like clojure.spec).

   Usage:
     (require '[atlas.ontology.execution-function])

   After requiring, you can:
   - Register execution-functions with :execution-function/context, :response, :deps
   - Use ontology/context-for, ontology/response-for, ontology/deps-for
   - Use templates like template:service-function, template:api-endpoint
   - Invariants like invariant-pure-has-no-deps are auto-discovered"
  (:require [atlas.registry :as registry]
            [atlas.registry.lookup :as entity]
            [atlas.ontology :as ontology]
            [atlas.ontology.type-ref :as type-ref]
            [atlas.query :as query]))

;; =============================================================================
;; ONTOLOGY DEFINITION
;; =============================================================================

(def ontology-definition
  "The ontology definition for :atlas/execution-function"
  {:ontology/for :atlas/execution-function
   :ontology/keys [:execution-function/context
                   :execution-function/response
                   :execution-function/deps]
   :dataflow/context-key :execution-function/context
   :dataflow/response-key :execution-function/response
   :dataflow/deps-key :execution-function/deps})

;; =============================================================================
;; TEMPLATES - Reducing boilerplate
;; =============================================================================

(defn template:service-function
  "Template for service-tier business logic functions.

   Usage:
     (template:service-function :domain/users :operation/fetch
       :external? false :pure? true :pii? false)"
  [domain operation & {:keys [external? pure? pii?]
                       :or {external? false pure? false pii? false}}]
  (cond-> #{:atlas/execution-function
            :semantic/docs
            :tier/service
            :effect/read}
    domain (conj domain)
    operation (conj operation)
    external? (conj :integration/external :temporal/async :temporal/timeout-configured :observability/traced)
    (not external?) (conj :observability/metered)
    pure? (conj :effect/pure)
    pii? (conj :compliance/pii :compliance/audited)))

(defn template:api-endpoint
  "Template for API endpoints that also act as execution functions.

   Usage:
     (template:api-endpoint :domain/users :operation/list
       :auth-required? true :rate-limited? false :pii? false)"
  [domain operation & {:keys [auth-required? rate-limited? pii?]
                       :or {auth-required? true rate-limited? false pii? false}}]
  (cond-> #{:atlas/interface-endpoint
            :atlas/execution-function
            :semantic/docs
            :tier/api
            :effect/read
            :protocol/http
            :observability/logged
            :observability/metered}
    domain (conj domain)
    operation (conj operation)
    auth-required? (conj :authorization/required)
    rate-limited? (conj :capacity/rate-limited)
    pii? (conj :compliance/pii :compliance/audited)))

;; =============================================================================
;; DATALOG INTEGRATION
;; =============================================================================

(def datalog-schema
  "Datascript schema for execution-function properties."
  {:entity/depends {:db/cardinality :db.cardinality/many}
   :entity/consumes {:db/cardinality :db.cardinality/many}
   :entity/produces {:db/cardinality :db.cardinality/many}})

;; =============================================================================
;; INVARIANTS - Execution-function specific rules
;; =============================================================================

(defn invariant-pure-has-no-deps
  "Functions marked :effect/pure should have no component dependencies.

   A pure function computes output solely from its inputs, so it should not
   depend on infrastructure components that have side effects."
  []
  (let [pure-fns (filter #(entity/has-aspect? % :effect/pure)
                          (entity/all-with-aspect :atlas/execution-function))
        violations (for [fn-id pure-fns
                          :let [deps (ontology/deps-for fn-id)
                                component-deps (filter #(entity/has-aspect? % :atlas/structure-component) deps)]
                          :when (seq component-deps)]
                      {:fn fn-id :component-deps (vec component-deps)})]
    (when (seq violations)
      {:invariant :pure-has-no-deps
       :violation :pure-fn-with-deps
       :details violations
       :severity :error
       :message "Pure functions should not depend on components"})))

(defn invariant-external-is-async
  "Functions marked :integration/external should also be :temporal/async."
  []
  (let [external-fns (filter #(entity/has-aspect? % :atlas/execution-function)
                             (entity/all-with-aspect :integration/external))
        violations (remove #(entity/has-aspect? % :temporal/async) external-fns)]
    (when (seq violations)
      {:invariant :external-is-async
       :violation :external-not-async
       :functions violations
       :severity :warning
       :message (str "External integrations should be async: " violations)})))

(defn invariant-internal-fn-outputs-consumed
  "Internal function outputs (non-endpoint) should be consumed by other functions.

   This checks that execution-functions that are NOT endpoints have their
   response keys consumed by some other function's context. Catches dead code
   in service layers where everything should chain."
  []
  (let [;; All execution-functions that are NOT endpoints
        internal-fn-ids (->> (entity/all-with-aspect :atlas/execution-function)
                             (remove #(entity/has-aspect? % :atlas/interface-endpoint)))
        ;; Collect all response keys from internal functions
        produced (->> internal-fn-ids
                      (mapcat ontology/response-for)
                      set)
        ;; Collect all context keys from all functions
        consumed (->> (entity/all-with-aspect :atlas/execution-function)
                      (mapcat ontology/context-for)
                      set)
        orphans (clojure.set/difference produced consumed)]
    (when (seq orphans)
      {:invariant :internal-fn-outputs-consumed
       :violation :orphan-internal-outputs
       :orphans orphans
       :severity :warning
       :message (str "Internal function outputs not consumed: " orphans)})))

(def invariants
  "All invariants specific to execution-function ontology"
  [invariant-pure-has-no-deps
   invariant-external-is-async
   invariant-internal-fn-outputs-consumed])

;; =============================================================================
;; AUTO-REGISTRATION (top-level, like clojure.spec)
;; =============================================================================

;; Ontology
(registry/register!
 :atlas/execution-function
 :atlas/ontology
 #{:atlas/execution-function}
 ontology-definition)

;; Type-ref: execution-function → structure-component (deps)
(registry/register!
 :type-ref/execution-function-deps
 :atlas/type-ref
 #{:meta/ref-execution-function-deps}
 {:type-ref/source :atlas/execution-function
  :type-ref/target :atlas/structure-component
  :type-ref/property :execution-function/deps
  :type-ref/datalog-verb :entity/depends
  :type-ref/cardinality :db.cardinality/many})

;; Datalog extractor
(registry/register!
 :datalog-extractor/execution-function
 :atlas/datalog-extractor
 #{:meta/execution-function-extractor}
 {:datalog-extractor/fn (fn [compound-id props]
                          (when (contains? compound-id :atlas/execution-function)
                            (let [dev-id (:atlas/dev-id props)
                                  ;; Accept both execution-function and interface-endpoint property names
                                  ;; for backward compatibility
                                  context-keys (or (:execution-function/context props)
                                                   (:interface-endpoint/context props))
                                  response-keys (or (:execution-function/response props)
                                                    (:interface-endpoint/response props))]
                              (vec
                               (concat
                                ;; Automatic reference extraction via type-ref
                                (type-ref/extract-reference-facts
                                 :atlas/execution-function
                                 compound-id
                                 props)

                                ;; Manual extraction for non-reference properties
                                (cond-> []
                                  ;; Context (consumed keys)
                                  context-keys
                                  (concat (map (fn [ctx]
                                                 [:db/add dev-id :entity/consumes ctx])
                                               context-keys))

                                  ;; Response (produced keys)
                                  response-keys
                                  (concat (map (fn [resp]
                                                 [:db/add dev-id :entity/produces resp])
                                               response-keys))))))))
  :datalog-extractor/schema datalog-schema})

;; Invariants
(registry/register!
 :invariant/pure-has-no-deps
 :atlas/invariant
 #{:meta/pure-function-check}
 {:invariant/fn invariant-pure-has-no-deps})

(registry/register!
 :invariant/external-is-async
 :atlas/invariant
 #{:meta/external-async-check}
 {:invariant/fn invariant-external-is-async})

(registry/register!
 :invariant/internal-fn-outputs-consumed
 :atlas/invariant
 #{:meta/output-consumption-check}
 {:invariant/fn invariant-internal-fn-outputs-consumed})
