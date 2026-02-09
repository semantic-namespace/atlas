(ns atlas.ontology.interface-endpoint
  "Interface-endpoint ontology module.

   This module defines the `:atlas/interface-endpoint` entity type and related
   functionality. Auto-registers on require.

   Usage:
     (require '[atlas.ontology.interface-endpoint])"
  (:require [atlas.registry :as registry]
            [atlas.registry.lookup :as entity]
            [atlas.ontology :as ontology]
            [atlas.ontology.type-ref :as type-ref]
            [clojure.set :as set]))

;; =============================================================================
;; ONTOLOGY DEFINITION
;; =============================================================================

(def ontology-definition
  "The ontology definition for :atlas/interface-endpoint"
  {:ontology/for :atlas/interface-endpoint
   :ontology/keys [:interface-endpoint/context
                   :interface-endpoint/response
                   :interface-endpoint/deps]
   :dataflow/context-key :interface-endpoint/context
   :dataflow/response-key :interface-endpoint/response
   :dataflow/deps-key :interface-endpoint/deps})

;; =============================================================================
;; DATALOG INTEGRATION
;; =============================================================================

(def datalog-schema
  "Datascript schema for interface-endpoint properties."
  {:endpoint-context {:db/cardinality :db.cardinality/many}
   :endpoint-response {:db/cardinality :db.cardinality/many}})

;; =============================================================================
;; INVARIANTS - Interface-endpoint specific rules
;; =============================================================================

(defn invariant-endpoints-are-api-tier
  "Endpoints should be :tier/api."
  []
  (let [endpoints (->> (entity/all-with-aspect :atlas/interface-endpoint)
                       (remove #(entity/has-aspect? % :atlas/ontology)))
        violations (remove #(entity/has-aspect? % :tier/api) endpoints)]
    (when (seq violations)
      {:invariant :endpoints-are-api-tier
       :violation :wrong-tier
       :endpoints violations
       :severity :error
       :message (str "Endpoints should be :tier/api: " violations)})))

(defn invariant-all-fns-reachable
  "Every execution-function should be reachable from some endpoint.

   This invariant expresses that all business logic (execution-functions)
   must be accessible through the API layer (interface-endpoints).
   Unreachable functions are dead code.

   Note: This makes explicit that interface-endpoint depends on
   execution-function in the ontology module hierarchy.

   Filters out ontology meta-entities (marked with :atlas/ontology) as they
   are not business logic."
  []
  (let [endpoints (entity/all-with-aspect :atlas/interface-endpoint)
        ;; Get all execution-functions but exclude ontology meta-entities
        all-fns (->> (entity/all-with-aspect :atlas/execution-function)
                     (remove #(entity/has-aspect? % :atlas/ontology))
                     set)
        ;; Find reachable via BFS from endpoints
        reachable (atom #{})
        collect-reachable (fn collect [id]
                            (when-not (@reachable id)
                              (swap! reachable conj id)
                              (doseq [dep (ontology/deps-for id)]
                                (collect dep))))]
    (doseq [ep endpoints]
      (collect-reachable ep))
    (let [unreachable (set/difference all-fns @reachable)]
      (when (seq unreachable)
        {:invariant :all-fns-reachable
         :violation :unreachable-functions
         :functions unreachable
         :severity :warning
         :message (str "Functions not reachable from any endpoint: " unreachable)}))))

(def invariants
  "All invariants specific to interface-endpoint ontology"
  [invariant-endpoints-are-api-tier
   invariant-all-fns-reachable])

;; =============================================================================
;; AUTO-REGISTRATION (top-level, like clojure.spec)
;; =============================================================================

;; Ontology
(registry/register!
 :atlas/interface-endpoint
 :atlas/ontology
 #{:atlas/interface-endpoint}
 ontology-definition)

;; Type-ref: interface-endpoint → execution-function (deps)
(registry/register!
 :type-ref/interface-endpoint-deps
 :atlas/type-ref
 #{:meta/ref-interface-endpoint-deps}
 {:type-ref/source :atlas/interface-endpoint
  :type-ref/target :atlas/execution-function
  :type-ref/property :interface-endpoint/deps
  :type-ref/datalog-verb :entity/depends
  :type-ref/cardinality :db.cardinality/many})

;; Datalog extractor
(registry/register!
 :datalog-extractor/interface-endpoint
 :atlas/datalog-extractor
 #{:meta/interface-endpoint-extractor}
 {:datalog-extractor/fn (fn [compound-id props]
                          (when (contains? compound-id :atlas/interface-endpoint)
                            (let [dev-id (:atlas/dev-id props)
                                  ;; Accept both interface-endpoint and execution-function property names
                                  ;; for backward compatibility
                                  context-keys (or (:interface-endpoint/context props)
                                                   (:execution-function/context props))
                                  response-keys (or (:interface-endpoint/response props)
                                                    (:execution-function/response props))]
                              (concat
                               ;; Automatic reference extraction via type-ref
                               (type-ref/extract-reference-facts
                                :atlas/interface-endpoint
                                compound-id
                                props)

                               ;; Manual extraction for non-reference properties
                               (cond-> []
                                 ;; Context (consumed keys) - generic
                                 context-keys
                                 (concat (map (fn [ctx]
                                                [:db/add dev-id :entity/consumes ctx])
                                              context-keys))

                                 ;; Response (produced keys) - generic
                                 response-keys
                                 (concat (map (fn [resp]
                                                [:db/add dev-id :entity/produces resp])
                                              response-keys))

                                 ;; Endpoint-specific context (for invariant checking)
                                 context-keys
                                 (concat (map (fn [ctx]
                                                [:db/add dev-id :endpoint-context ctx])
                                              context-keys))

                                 ;; Endpoint-specific response (for invariant checking)
                                 response-keys
                                 (concat (map (fn [resp]
                                                [:db/add dev-id :endpoint-response resp])
                                              response-keys)))))))
  :datalog-extractor/schema datalog-schema})

;; Invariants
(registry/register!
 :invariant/endpoints-are-api-tier
 :atlas/invariant
 #{:meta/api-tier-check}
 {:invariant/fn invariant-endpoints-are-api-tier})

(registry/register!
 :invariant/all-fns-reachable
 :atlas/invariant
 #{:meta/reachability-check}
 {:invariant/fn invariant-all-fns-reachable})
