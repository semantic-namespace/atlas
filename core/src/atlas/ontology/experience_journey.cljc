(ns atlas.ontology.experience-journey
  "Experience-journey ontology module. Auto-registers on require."
  (:require [atlas.registry :as registry]
            [atlas.ontology.type-ref :as type-ref]))

(registry/register!
 :atlas/experience-journey
 :atlas/ontology
 #{:atlas/experience-journey}
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

;; Type-refs
(registry/register!
 :type-ref/experience-journey-risks
 :atlas/type-ref
 #{:meta/ref-experience-journey-risks}
 {:type-ref/source      :atlas/experience-journey
  :type-ref/property    :experience-journey/risk-failure-mode
  :type-ref/datalog-verb :journey/risk-failure-mode
  :type-ref/cardinality :db.cardinality/many})

(registry/register!
 :type-ref/experience-journey-delivers-value
 :atlas/type-ref
 #{:meta/ref-experience-journey-delivers-value}
 {:type-ref/source      :atlas/experience-journey
  :type-ref/property    :experience-journey/delivers-value
  :type-ref/datalog-verb :journey/delivers-value
  :type-ref/cardinality :db.cardinality/one})

(registry/register!
 :type-ref/experience-journey-replaces
 :atlas/type-ref
 #{:meta/ref-experience-journey-replaces}
 {:type-ref/source      :atlas/experience-journey
  :type-ref/property    :experience-journey/replaces
  :type-ref/datalog-verb :journey/replaces
  :type-ref/cardinality :db.cardinality/one})

(registry/register!
 :type-ref/experience-journey-friction
 :atlas/type-ref
 #{:meta/ref-experience-journey-friction}
 {:type-ref/source      :atlas/experience-journey
  :type-ref/property    :experience-journey/friction-points
  :type-ref/datalog-verb :journey/friction-points
  :type-ref/cardinality :db.cardinality/many})

;; Datalog extractor
(registry/register!
 :datalog-extractor/experience-journey
 :atlas/datalog-extractor
 #{:meta/experience-journey-extractor}
 {:datalog-extractor/fn (fn [compound-id props]
                          (when (contains? compound-id :atlas/experience-journey)
                            (type-ref/extract-reference-facts
                             :atlas/experience-journey
                             compound-id
                             props)))
  :datalog-extractor/schema {:journey/risk-failure-mode {:db/cardinality :db.cardinality/many}
   :journey/delivers-value {:db/cardinality :db.cardinality/one}
   :journey/replaces {:db/cardinality :db.cardinality/one}
   :journey/friction-points {:db/cardinality :db.cardinality/many}}})
