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
            [clojure.spec.alpha :as s]))

(s/def :execution-function/context (s/coll-of qualified-keyword?))
(s/def :execution-function/response (s/coll-of qualified-keyword?))
(s/def :execution-function/deps (s/coll-of :atlas/dev-id))

;; Ontology
(registry/register!
 :atlas/execution-function
 :atlas/ontology
 #{:atlas/execution-function}
 {:ontology/for :atlas/execution-function
   :ontology/keys [:execution-function/context
                   :execution-function/response
                   :execution-function/deps]
   :dataflow/context-key :execution-function/context
   :dataflow/response-key :execution-function/response
   :dataflow/deps-key :execution-function/deps})

;; Type-ref: execution-function → structure-component (deps)
(registry/register!
 :type-ref/execution-function-deps
 :atlas/type-ref
 #{:meta/ref-execution-function-deps}
 {:type-ref/source :atlas/execution-function
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
  :datalog-extractor/schema {:entity/depends {:db/cardinality :db.cardinality/many}
                             :entity/consumes {:db/cardinality :db.cardinality/many}
                             :entity/produces {:db/cardinality :db.cardinality/many}}})

;; Invariants
(registry/register!
 :invariant/pure-has-no-deps
 :atlas/invariant
 #{:meta/pure-function-check}
 {:invariant/fn (fn []
                  "Functions marked :effect/pure should have no component dependencies.

   A pure function computes output solely from its inputs, so it should not
   depend on infrastructure components that have side effects."

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
                       :message "Pure functions should not depend on components"})))})

(registry/register!
 :invariant/external-is-async
 :atlas/invariant
 #{:meta/external-async-check}
 {:invariant/fn (fn []
                  ;; "Functions marked :integration/external should also be :temporal/async."
                  (let [external-fns (filter #(entity/has-aspect? % :atlas/execution-function)
                                             (entity/all-with-aspect :integration/external))
                        violations (remove #(entity/has-aspect? % :temporal/async) external-fns)]
                    (when (seq violations)
                      {:invariant :external-is-async
                       :violation :external-not-async
                       :functions violations
                       :severity :warning
                       :message (str "External integrations should be async: " violations)})))})

(registry/register!
 :invariant/internal-fn-outputs-consumed
 :atlas/invariant
 #{:meta/output-consumption-check}
 {:invariant/fn (fn []
                  "Internal function outputs (non-endpoint) should be consumed by other functions.

   This checks that execution-functions that are NOT endpoints have their
   response keys consumed by some other function's context. Catches dead code
   in service layers where everything should chain."
                  
                  (let [;; All execution-functions that are NOT endpoints
                        internal-fn-ids (->> (entity/all-with-aspect :atlas/execution-function)
                                             (remove #(or (entity/has-aspect? % :atlas/ontology)
                                                          (entity/has-aspect? % :atlas/interface-endpoint))))
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
                       :message (str "Internal function outputs not consumed: " orphans)})))})
