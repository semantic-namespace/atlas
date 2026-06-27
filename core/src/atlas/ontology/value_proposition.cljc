(ns atlas.ontology.value-proposition
  "Value-proposition ontology module. Auto-registers on require."
  (:require [atlas.registry :as registry]
            [atlas.registry.lookup :as entity]
            [atlas.ontology.type-ref :as type-ref]))

(registry/register!
 :atlas/value-proposition
 :atlas/ontology
 #{:atlas/value-proposition}
 {:ontology/for :atlas/value-proposition
  :ontology/keys [:value-proposition/business-problem
                  :value-proposition/before-state
                  :value-proposition/after-state
                  :value-proposition/time-saved
                  :value-proposition/solution
                  :value-proposition/metrics-improved
                  :value-proposition/user-segment
                  :value-proposition/business-value-quantified
                  :value-proposition/business-value
                  :value-proposition/competitive-advantage
                  :value-proposition/implements-pattern
                  :value-proposition/trust-factors
                  :value-proposition/risk-mitigation
                  :value-proposition/compliance-benefit
                  :value-proposition/external-boundaries]
  :dataflow/deps-key [:value-proposition/implements-pattern
                      :value-proposition/external-boundaries]})

;; Type-refs
(registry/register!
 :type-ref/value-proposition-pattern
 :atlas/type-ref
 #{:meta/ref-value-proposition-pattern}
 {:type-ref/source      :atlas/value-proposition
  :type-ref/property    :value-proposition/implements-pattern
  :type-ref/datalog-verb :entity/depends
  :type-ref/cardinality :db.cardinality/one})

(registry/register!
 :type-ref/value-proposition-metrics
 :atlas/type-ref
 #{:meta/ref-value-proposition-metrics}
 {:type-ref/source      :atlas/value-proposition
  :type-ref/property    :value-proposition/metrics-improved
  :type-ref/datalog-verb :value-proposition/metrics-improved
  :type-ref/cardinality :db.cardinality/many})

(registry/register!
 :type-ref/value-proposition-segments
 :atlas/type-ref
 #{:meta/ref-value-proposition-segments}
 {:type-ref/source      :atlas/value-proposition
  :type-ref/property    :value-proposition/user-segment
  :type-ref/datalog-verb :value-proposition/user-segment
  :type-ref/cardinality :db.cardinality/many})

(registry/register!
 :type-ref/value-proposition-trust
 :atlas/type-ref
 #{:meta/ref-value-proposition-trust}
 {:type-ref/source      :atlas/value-proposition
  :type-ref/property    :value-proposition/trust-factors
  :type-ref/datalog-verb :value-proposition/trust-factors
  :type-ref/cardinality :db.cardinality/many})

(registry/register!
 :type-ref/value-proposition-external-boundaries
 :atlas/type-ref
 #{:meta/ref-value-proposition-external-boundaries}
 {:type-ref/source      :atlas/value-proposition
  :type-ref/property    :value-proposition/external-boundaries
  :type-ref/datalog-verb :entity/depends
  :type-ref/cardinality :db.cardinality/many})

;; Invariants — existence implies wiring

(defn- all-value-propositions
  "All registered value-proposition entities, excluding the ontology definition itself."
  []
  (->> (entity/all-with-aspect :atlas/value-proposition)
       (filter #(= :atlas/value-proposition (:atlas/type (entity/props-for %))))))

(registry/register!
 :invariant/value-proposition-has-pattern
 :atlas/invariant
 #{:meta/value-proposition-has-pattern}
 {:invariant/fn
  (fn []
    (let [violations (for [vp-id (all-value-propositions)
                           :let [props   (entity/props-for vp-id)
                                 pattern (:value-proposition/implements-pattern props)]
                           :when (or (nil? pattern)
                                     (nil? (entity/props-for pattern)))]
                       {:value-proposition vp-id :missing-pattern pattern})]
      (when (seq violations)
        {:invariant :value-proposition-has-pattern
         :violation :missing-or-invalid-implements-pattern
         :details   violations
         :severity  :error
         :message   (str "Value propositions without a valid implements-pattern: "
                         (mapv :value-proposition violations))})))})

(registry/register!
 :invariant/value-proposition-has-external-boundaries
 :atlas/invariant
 #{:meta/value-proposition-has-external-boundaries}
 {:invariant/fn
  (fn []
    (let [violations (for [vp-id (all-value-propositions)
                           :let [props      (entity/props-for vp-id)
                                 boundaries (:value-proposition/external-boundaries props)]
                           :when (or (nil? boundaries) (empty? boundaries))]
                       {:value-proposition vp-id})]
      (when (seq violations)
        {:invariant :value-proposition-has-external-boundaries
         :violation :missing-external-boundaries
         :details   violations
         :severity  :error
         :message   (str "Value propositions without external boundaries: "
                         (mapv :value-proposition violations))})))})

;; Datalog extractor
(registry/register!
 :datalog-extractor/value-proposition
 :atlas/datalog-extractor
 #{:meta/value-proposition-extractor}
 {:datalog-extractor/fn (fn [compound-id props]
                          (when (contains? compound-id :atlas/value-proposition)
                            (type-ref/extract-reference-facts
                             :atlas/value-proposition
                             compound-id
                             props)))
  :datalog-extractor/schema {:entity/depends {:db/cardinality :db.cardinality/many}
                             :value-proposition/metrics-improved {:db/cardinality :db.cardinality/many}
                             :value-proposition/user-segment {:db/cardinality :db.cardinality/many}
                             :value-proposition/trust-factors {:db/cardinality :db.cardinality/many}}})
