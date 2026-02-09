(ns atlas.ontology.risk-failure-mode
  "Risk-failure-mode ontology module. Auto-registers on require."
  (:require [atlas.registry :as registry]))

(def ontology-definition
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

;; =============================================================================
;; DATALOG INTEGRATION
;; =============================================================================

(def datalog-schema
  "Datascript schema for risk-failure-mode properties."
  {:risk/triggered-by {:db/cardinality :db.cardinality/many}
   :risk/preventable {:db/cardinality :db.cardinality/one}
   :risk/security-event {:db/cardinality :db.cardinality/one}
   :risk/logged {:db/cardinality :db.cardinality/one}})

;; =============================================================================
;; AUTO-REGISTRATION (top-level, like clojure.spec)
;; =============================================================================

(registry/register!
 :atlas/risk-failure-mode
 :atlas/ontology
 #{:atlas/risk-failure-mode}
 ontology-definition)

;; Datalog extractor
(registry/register!
 :datalog-extractor/risk-failure-mode
 :atlas/datalog-extractor
 #{:meta/risk-failure-mode-extractor}
 {:datalog-extractor/fn (fn [compound-id props]
                          (when (contains? compound-id :atlas/risk-failure-mode)
                            (let [dev-id (:atlas/dev-id props)
                                  triggered-by (:risk-failure-mode/triggered-by props)
                                  preventable (:risk-failure-mode/preventable props)
                                  security-event (:risk-failure-mode/security-event props)
                                  logged (:risk-failure-mode/logged props)]
                              (cond-> []
                                ;; Triggered by (collection)
                                triggered-by
                                (concat (map (fn [trigger]
                                               [:db/add dev-id :risk/triggered-by trigger])
                                             (if (coll? triggered-by) triggered-by [triggered-by])))

                                ;; Preventable (boolean)
                                preventable
                                (conj [:db/add dev-id :risk/preventable preventable])

                                ;; Security event (boolean)
                                security-event
                                (conj [:db/add dev-id :risk/security-event security-event])

                                ;; Logged (boolean)
                                logged
                                (conj [:db/add dev-id :risk/logged logged])))))
  :datalog-extractor/schema datalog-schema})
