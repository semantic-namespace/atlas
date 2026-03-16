(ns atlas.ontology.data-schema
  "Data-schema ontology module. Auto-registers on require."
  (:require [atlas.registry :as registry]
            [atlas.ontology.type-ref :as type-ref]))

(registry/register!
 :atlas/data-schema
 :atlas/ontology
 #{:atlas/data-schema}
 {:ontology/for :atlas/data-schema
   :ontology/keys [:data-schema/fields]})

;; Type-ref: data-schema fields
(registry/register!
 :type-ref/data-schema-fields
 :atlas/type-ref
 #{:meta/ref-data-schema-fields}
 {:type-ref/source      :atlas/data-schema
  :type-ref/property    :data-schema/fields
  :type-ref/datalog-verb :schema/field
  :type-ref/cardinality :db.cardinality/many})

;; Datalog extractor
(registry/register!
 :datalog-extractor/data-schema
 :atlas/datalog-extractor
 #{:meta/data-schema-extractor}
 {:datalog-extractor/fn (fn [compound-id props]
                          (when (contains? compound-id :atlas/data-schema)
                            (type-ref/extract-reference-facts
                             :atlas/data-schema
                             compound-id
                             props)))
  :datalog-extractor/schema {:schema/field {:db/cardinality :db.cardinality/many}}})
