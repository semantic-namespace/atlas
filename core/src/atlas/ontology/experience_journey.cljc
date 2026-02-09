(ns atlas.ontology.experience-journey
  "Experience-journey ontology module. Auto-registers on require."
  (:require [atlas.registry :as registry]))

(def ontology-definition
  {:ontology/for :atlas/experience-journey
   :ontology/keys [:experience-journey/user-journey
                   :experience-journey/time-to-complete
                   :experience-journey/friction-points
                   :experience-journey/why-designed-this-way
                   :experience-journey/user-sentiment
                   :experience-journey/risk-failure-mode
                   :experience-journey/recovery-time
                   :experience-journey/delivers-value
                   :experience-journey/replaces]})

;; =============================================================================
;; DATALOG INTEGRATION
;; =============================================================================

(def datalog-schema
  "Datascript schema for experience-journey properties."
  {:journey/risk-failure-mode {:db/cardinality :db.cardinality/many}
   :journey/delivers-value {:db/cardinality :db.cardinality/one}
   :journey/replaces {:db/cardinality :db.cardinality/one}
   :journey/friction-points {:db/cardinality :db.cardinality/many}})

;; =============================================================================
;; AUTO-REGISTRATION (top-level, like clojure.spec)
;; =============================================================================

(registry/register!
 :atlas/experience-journey
 :atlas/ontology
 #{:atlas/experience-journey}
 ontology-definition)

;; Datalog extractor
(registry/register!
 :datalog-extractor/experience-journey
 :atlas/datalog-extractor
 #{:meta/experience-journey-extractor}
 {:datalog-extractor/fn (fn [compound-id props]
                          (when (contains? compound-id :atlas/experience-journey)
                            (let [dev-id (:atlas/dev-id props)
                                  risks (:experience-journey/risk-failure-mode props)
                                  delivers-value (:experience-journey/delivers-value props)
                                  replaces (:experience-journey/replaces props)
                                  friction (:experience-journey/friction-points props)]
                              (cond-> []
                                ;; Risk failure modes (collection of references)
                                risks
                                (concat (map (fn [risk]
                                               [:db/add dev-id :journey/risk-failure-mode risk])
                                             (if (coll? risks) risks [risks])))

                                ;; Delivers value (reference to value-proposition)
                                delivers-value
                                (conj [:db/add dev-id :journey/delivers-value delivers-value])

                                ;; Replaces (reference to previous journey/feature)
                                replaces
                                (conj [:db/add dev-id :journey/replaces replaces])

                                ;; Friction points (collection)
                                friction
                                (concat (map (fn [point]
                                               [:db/add dev-id :journey/friction-points point])
                                             (if (coll? friction) friction [friction])))))))
  :datalog-extractor/schema datalog-schema})
