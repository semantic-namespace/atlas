(ns atlas.ontology.value-proposition
  "Value-proposition ontology module. Auto-registers on require."
  (:require [atlas.registry :as registry]
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
                   :value-proposition/compliance-benefit]})

;; Type-refs
(registry/register!
 :type-ref/value-proposition-pattern
 :atlas/type-ref
 #{:meta/ref-value-proposition-pattern}
 {:type-ref/source      :atlas/value-proposition
  :type-ref/property    :value-proposition/implements-pattern
  :type-ref/datalog-verb :value-proposition/implements-pattern
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
  :datalog-extractor/schema {:value-proposition/implements-pattern {:db/cardinality :db.cardinality/one}
   :value-proposition/metrics-improved {:db/cardinality :db.cardinality/many}
   :value-proposition/user-segment {:db/cardinality :db.cardinality/many}
   :value-proposition/trust-factors {:db/cardinality :db.cardinality/many}}})
