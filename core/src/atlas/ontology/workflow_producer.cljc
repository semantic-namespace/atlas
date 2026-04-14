(ns atlas.ontology.workflow-producer
  "Workflow-producer ontology module.

   Defines the `:atlas/workflow-producer` entity type. Auto-registers on require.

   A workflow-producer is an FSM state node that wraps an execution-function
   with FSM-specific metadata: which signals it can emit.

   The wrapped execution-function must return three qualified-keyword values
   in its response: :workflow/signal, :async/seconds-to-delay, and a
   producer-specific result key.

   Usage:
     (require '[atlas.ontology.workflow-producer])

   Example registration:
     (registry/register!
       :wp/fetch-inventory
       :atlas/workflow-producer
       #{:domain/warehouse :workflow/inventory-check}
       {:workflow-producer/execution-function :fn/fetch-inventory
        :workflow-producer/signals #{:signal/in-stock :signal/out-of-stock}})"
  (:require [atlas.registry :as registry]
            [atlas.registry.lookup :as entity]
            [atlas.ontology :as ontology]
            [atlas.ontology.type-ref :as type-ref]
            [clojure.set :as set]
            [clojure.spec.alpha :as s]))

;; =============================================================================
;; SPECS
;; =============================================================================

;; The dev-id of the wrapped execution-function
(s/def :workflow-producer/execution-function :atlas/dev-id)

;; Set of qualified-keyword signals this producer can emit
(s/def :workflow-producer/signals (s/coll-of qualified-keyword? :kind set? :min-count 1))

;; Keys this producer reads from shared storage (separate from ef context
;; which is the function-call-level input)
(s/def :workflow-producer/context (s/coll-of qualified-keyword?))

;; Keys this producer writes to shared storage as side effects
;; (separate from the ef response tuple which is FSM plumbing)
(s/def :workflow-producer/output (s/coll-of qualified-keyword?))

;; The response contract: execution-functions wrapped by workflow-producers
;; must include these keys in their :execution-function/response
(s/def :workflow/signal qualified-keyword?)
(s/def :async/seconds-to-delay qualified-keyword?)

;; Full workflow-producer props spec
(s/def :atlas/workflow-producer-props
  (s/keys :req [:workflow-producer/execution-function
                :workflow-producer/signals
                :workflow-producer/context
                :workflow-producer/output]))

;; =============================================================================
;; ONTOLOGY
;; =============================================================================

(registry/register!
 :atlas/workflow-producer
 :atlas/ontology
 #{:atlas/workflow-producer}
 {:ontology/for :atlas/workflow-producer
  :ontology/keys [:workflow-producer/execution-function
                  :workflow-producer/signals
                  :workflow-producer/context
                  :workflow-producer/output]
  :dataflow/context-key :workflow-producer/context
  :dataflow/context-verb :entity/consumes
  :dataflow/response-key :workflow-producer/output
  :dataflow/response-verb :entity/produces
  :dataflow/deps-key :workflow-producer/execution-function})

;; Type-ref: workflow-producer → execution-function
(registry/register!
 :type-ref/workflow-producer-execution-function
 :atlas/type-ref
 #{:meta/ref-workflow-producer-ef}
 {:type-ref/source :atlas/workflow-producer
  :type-ref/property :workflow-producer/execution-function
  :type-ref/datalog-verb :workflow-producer/wraps
  :type-ref/cardinality :db.cardinality/one})

;; =============================================================================
;; DATALOG EXTRACTOR
;; =============================================================================

(registry/register!
 :datalog-extractor/workflow-producer
 :atlas/datalog-extractor
 #{:meta/workflow-producer-extractor}
 {:datalog-extractor/fn
  (fn [compound-id props]
    (when (contains? compound-id :atlas/workflow-producer)
      (let [dev-id (:atlas/dev-id props)
            signals (:workflow-producer/signals props)
            ef-id (:workflow-producer/execution-function props)
            context-keys (:workflow-producer/context props)
            output-keys (:workflow-producer/output props)]
        (vec
         (concat
          ;; Automatic reference extraction via type-ref
          (type-ref/extract-reference-facts
           :atlas/workflow-producer
           compound-id
           props)

          ;; Signal declarations as datalog facts
          (when signals
            (map (fn [sig] [:db/add dev-id :workflow-producer/emits sig])
                 signals))

          ;; Dependency on wrapped execution-function
          (when ef-id
            [[:db/add dev-id :entity/depends ef-id]])

          ;; Context (consumed keys) — uniform with execution-function
          (when context-keys
            (map (fn [k] [:db/add dev-id :entity/consumes k])
                 context-keys))

          ;; Output (produced keys) — uniform with execution-function
          (when output-keys
            (map (fn [k] [:db/add dev-id :entity/produces k])
                 output-keys)))))))

  :datalog-extractor/schema {:workflow-producer/wraps {:db/cardinality :db.cardinality/one}
                             :workflow-producer/emits {:db/cardinality :db.cardinality/many}
                             :entity/depends {:db/cardinality :db.cardinality/many}
                             :entity/consumes {:db/cardinality :db.cardinality/many}
                             :entity/produces {:db/cardinality :db.cardinality/many}}})

;; =============================================================================
;; INVARIANTS
;; =============================================================================

(registry/register!
 :invariant/workflow-producer-ef-exists
 :atlas/invariant
 #{:meta/workflow-producer-ef-exists}
 {:invariant/fn
  (fn []
    (let [producers (->> (entity/all-with-aspect :atlas/workflow-producer)
                         (remove #(entity/has-aspect? % :atlas/ontology)))
          violations (for [wp-id producers
                           :let [props (entity/props-for wp-id)
                                 ef-id (:workflow-producer/execution-function props)]
                           :when ef-id
                           :when (nil? (entity/identity-for ef-id))]
                       {:workflow-producer wp-id :execution-function ef-id})]
      (when (seq violations)
        {:invariant :workflow-producer-ef-exists
         :violation :execution-function-not-found
         :details (vec violations)
         :severity :error
         :message (str "Workflow-producer references non-existent execution-functions: "
                       (mapv :execution-function violations))})))})

(registry/register!
 :invariant/workflow-producer-ef-has-signal-response
 :atlas/invariant
 #{:meta/workflow-producer-signal-response}
 {:invariant/fn
  (fn []
    "The wrapped execution-function must include :workflow/signal and
     :async/seconds-to-delay in its response keys."
    (let [producers (->> (entity/all-with-aspect :atlas/workflow-producer)
                         (remove #(entity/has-aspect? % :atlas/ontology)))
          required-response #{:workflow/signal :async/seconds-to-delay}
          violations (for [wp-id producers
                           :let [props (entity/props-for wp-id)
                                 ef-id (:workflow-producer/execution-function props)]
                           :when ef-id
                           :when (entity/identity-for ef-id)
                           :let [ef-response (set (ontology/response-for ef-id))
                                 missing (set/difference required-response ef-response)]
                           :when (seq missing)]
                       {:workflow-producer wp-id
                        :execution-function ef-id
                        :missing-response-keys missing})]
      (when (seq violations)
        {:invariant :workflow-producer-ef-has-signal-response
         :violation :missing-workflow-response-keys
         :details (vec violations)
         :severity :error
         :message "Execution-functions wrapped by workflow-producers must return :workflow/signal and :async/seconds-to-delay"})))})

(registry/register!
 :invariant/workflow-producer-has-signals
 :atlas/invariant
 #{:meta/workflow-producer-has-signals}
 {:invariant/fn
  (fn []
    (let [producers (->> (entity/all-with-aspect :atlas/workflow-producer)
                         (remove #(entity/has-aspect? % :atlas/ontology)))
          violations (for [wp-id producers
                           :let [signals (:workflow-producer/signals
                                          (entity/props-for wp-id))]
                           :when (empty? signals)]
                       wp-id)]
      (when (seq violations)
        {:invariant :workflow-producer-has-signals
         :violation :no-signals-declared
         :details (vec violations)
         :severity :error
         :message (str "Workflow-producers must declare possible signals: "
                       (vec violations))})))})

(registry/register!
 :invariant/workflow-producer-props-valid
 :atlas/invariant
 #{:meta/workflow-producer-props-valid}
 {:invariant/fn
  (fn []
    "All workflow-producer entities must conform to :atlas/workflow-producer-props spec."
    (let [producers (->> (entity/all-with-aspect :atlas/workflow-producer)
                         (remove #(entity/has-aspect? % :atlas/ontology)))
          violations (for [wp-id producers
                           :let [props (entity/props-for wp-id)]
                           :when (not (s/valid? :atlas/workflow-producer-props props))]
                       {:workflow-producer wp-id
                        :explain (s/explain-str :atlas/workflow-producer-props props)})]
      (when (seq violations)
        {:invariant :workflow-producer-props-valid
         :violation :invalid-props
         :details (vec violations)
         :severity :error
         :message "Workflow-producer props do not conform to spec"})))})
