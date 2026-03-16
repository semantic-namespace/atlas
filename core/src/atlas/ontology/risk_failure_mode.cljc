(ns atlas.ontology.risk-failure-mode
  "Risk-failure-mode ontology module. Auto-registers on require."
  (:require [atlas.registry :as registry]
            [atlas.ontology.type-ref :as type-ref]))

(registry/register!
 :atlas/risk-failure-mode
 :atlas/ontology
 #{:atlas/risk-failure-mode}
 {:ontology/for :atlas/risk-failure-mode
  :ontology/keys [:risk-failure-mode/triggered-by
                  :risk-failure-mode/detection
                  :risk-failure-mode/user-experiences
                  :risk-failure-mode/recovery-path
                  :risk-failure-mode/recovery-steps
                  :risk-failure-mode/data-loss
                  :risk-failure-mode/business-impact
                  :risk-failure-mode/frequency
                  :risk-failure-mode/preventable
                  :risk-failure-mode/why-not-preventable
                  :risk-failure-mode/prevention-strategy
                  :risk-failure-mode/security-event
                  :risk-failure-mode/logged
                  :risk-failure-mode/log-details]})

;; Type-refs
(registry/register!
 :type-ref/risk-failure-mode-triggered-by
 :atlas/type-ref
 #{:meta/ref-risk-failure-mode-triggered-by}
 {:type-ref/source      :atlas/risk-failure-mode
  :type-ref/property    :risk-failure-mode/triggered-by
  :type-ref/datalog-verb :risk/triggered-by
  :type-ref/cardinality :db.cardinality/many})

(registry/register!
 :type-ref/risk-failure-mode-preventable
 :atlas/type-ref
 #{:meta/ref-risk-failure-mode-preventable}
 {:type-ref/source      :atlas/risk-failure-mode
  :type-ref/property    :risk-failure-mode/preventable
  :type-ref/datalog-verb :risk/preventable
  :type-ref/cardinality :db.cardinality/one})

(registry/register!
 :type-ref/risk-failure-mode-security-event
 :atlas/type-ref
 #{:meta/ref-risk-failure-mode-security-event}
 {:type-ref/source      :atlas/risk-failure-mode
  :type-ref/property    :risk-failure-mode/security-event
  :type-ref/datalog-verb :risk/security-event
  :type-ref/cardinality :db.cardinality/one})

(registry/register!
 :type-ref/risk-failure-mode-logged
 :atlas/type-ref
 #{:meta/ref-risk-failure-mode-logged}
 {:type-ref/source      :atlas/risk-failure-mode
  :type-ref/property    :risk-failure-mode/logged
  :type-ref/datalog-verb :risk/logged
  :type-ref/cardinality :db.cardinality/one})

;; Datalog extractor
(registry/register!
 :datalog-extractor/risk-failure-mode
 :atlas/datalog-extractor
 #{:meta/risk-failure-mode-extractor}
 {:datalog-extractor/fn (fn [compound-id props]
                          (when (contains? compound-id :atlas/risk-failure-mode)
                            (type-ref/extract-reference-facts
                             :atlas/risk-failure-mode
                             compound-id
                             props)))
  :datalog-extractor/schema {:risk/triggered-by {:db/cardinality :db.cardinality/many}
                             :risk/preventable {:db/cardinality :db.cardinality/one}
                             :risk/security-event {:db/cardinality :db.cardinality/one}
                             :risk/logged {:db/cardinality :db.cardinality/one}}})
