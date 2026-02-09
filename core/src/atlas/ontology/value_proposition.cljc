(ns atlas.ontology.value-proposition
  "Value-proposition ontology module. Auto-registers on require."
  (:require [atlas.registry :as registry]))

(def ontology-definition
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

;; =============================================================================
;; DATALOG INTEGRATION
;; =============================================================================

(def datalog-schema
  "Datascript schema for value-proposition properties."
  {:value-proposition/implements-pattern {:db/cardinality :db.cardinality/one}
   :value-proposition/metrics-improved {:db/cardinality :db.cardinality/many}
   :value-proposition/user-segment {:db/cardinality :db.cardinality/many}
   :value-proposition/trust-factors {:db/cardinality :db.cardinality/many}})

;; =============================================================================
;; AUTO-REGISTRATION (top-level, like clojure.spec)
;; =============================================================================

(registry/register!
 :atlas/value-proposition
 :atlas/ontology
 #{:atlas/value-proposition}
 ontology-definition)

;; Datalog extractor
(registry/register!
 :datalog-extractor/value-proposition
 :atlas/datalog-extractor
 #{:meta/value-proposition-extractor}
 {:datalog-extractor/fn (fn [compound-id props]
                          (when (contains? compound-id :atlas/value-proposition)
                            (let [dev-id (:atlas/dev-id props)
                                  pattern (:value-proposition/implements-pattern props)
                                  metrics (:value-proposition/metrics-improved props)
                                  segments (:value-proposition/user-segment props)
                                  trust (:value-proposition/trust-factors props)]
                              (cond-> []
                                ;; Implements pattern (reference)
                                pattern
                                (conj [:db/add dev-id :value-proposition/implements-pattern pattern])

                                ;; Metrics improved (collection)
                                metrics
                                (concat (map (fn [metric]
                                               [:db/add dev-id :value-proposition/metrics-improved metric])
                                             (if (coll? metrics) metrics [metrics])))

                                ;; User segments (collection)
                                segments
                                (concat (map (fn [segment]
                                               [:db/add dev-id :value-proposition/user-segment segment])
                                             (if (coll? segments) segments [segments])))

                                ;; Trust factors (collection)
                                trust
                                (concat (map (fn [factor]
                                               [:db/add dev-id :value-proposition/trust-factors factor])
                                             (if (coll? trust) trust [trust])))))))
  :datalog-extractor/schema datalog-schema})
