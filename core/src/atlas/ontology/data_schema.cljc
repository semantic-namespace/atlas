(ns atlas.ontology.data-schema
  "Data-schema ontology module. Auto-registers on require."
  (:require [atlas.registry :as registry]))

(def ontology-definition
  {:ontology/for :atlas/data-schema
   :ontology/keys [:data-schema/fields]})

;; =============================================================================
;; DATALOG INTEGRATION
;; =============================================================================

(def datalog-schema
  "Datascript schema for data-schema properties."
  {:schema/field {:db/cardinality :db.cardinality/many}})

;; =============================================================================
;; AUTO-REGISTRATION (top-level, like clojure.spec)
;; =============================================================================

(registry/register!
 :atlas/data-schema
 :atlas/ontology
 #{:atlas/data-schema}
 ontology-definition)

;; Datalog extractor
(registry/register!
 :datalog-extractor/data-schema
 :atlas/datalog-extractor
 #{:meta/data-schema-extractor}
 {:datalog-extractor/fn (fn [compound-id props]
                          (when (contains? compound-id :atlas/data-schema)
                            (let [dev-id (:atlas/dev-id props)
                                  fields (:data-schema/fields props)]
                              (when fields
                                (map (fn [field]
                                       [:db/add dev-id :schema/field field])
                                     fields)))))
  :datalog-extractor/schema datalog-schema})
