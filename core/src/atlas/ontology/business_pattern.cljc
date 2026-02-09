(ns atlas.ontology.business-pattern
  "Business-pattern ontology module. Auto-registers on require."
  (:require [atlas.registry :as registry]))

(def ontology-definition
  {:ontology/for :atlas/business-pattern
   :ontology/keys [:business-pattern/principle
                   :business-pattern/justification
                   :business-pattern/experience-journey
                   :business-pattern/failure-recovery
                   :business-pattern/alternative-rejected
                   :business-pattern/why-rejected
                   :business-pattern/business-value
                   :business-pattern/metrics-improved]})

;; =============================================================================
;; DATALOG INTEGRATION
;; =============================================================================

(def datalog-schema
  "Datascript schema for business-pattern properties."
  {:business-pattern/experience-journey {:db/cardinality :db.cardinality/one}
   :business-pattern/metrics-improved {:db/cardinality :db.cardinality/many}})

;; =============================================================================
;; AUTO-REGISTRATION (top-level, like clojure.spec)
;; =============================================================================

(registry/register!
 :atlas/business-pattern
 :atlas/ontology
 #{:atlas/business-pattern}
 ontology-definition)

;; Datalog extractor
(registry/register!
 :datalog-extractor/business-pattern
 :atlas/datalog-extractor
 #{:meta/business-pattern-extractor}
 {:datalog-extractor/fn (fn [compound-id props]
                          (when (contains? compound-id :atlas/business-pattern)
                            (let [dev-id (:atlas/dev-id props)
                                  journey (:business-pattern/experience-journey props)
                                  metrics (:business-pattern/metrics-improved props)]
                              (cond-> []
                                ;; Experience journey reference (if it's a dev-id)
                                journey
                                (conj [:db/add dev-id :business-pattern/experience-journey journey])

                                ;; Metrics improved (collection)
                                metrics
                                (concat (map (fn [metric]
                                               [:db/add dev-id :business-pattern/metrics-improved metric])
                                             metrics))))))
  :datalog-extractor/schema datalog-schema})
