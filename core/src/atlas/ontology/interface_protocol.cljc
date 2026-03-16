(ns atlas.ontology.interface-protocol
  "Interface-protocol ontology module. Auto-registers on require."
  (:require [atlas.registry :as registry]
            [atlas.ontology.type-ref :as type-ref]))

;; Ontology
(registry/register!
 :atlas/interface-protocol
 :atlas/ontology
 #{:atlas/interface-protocol}
 {:ontology/for :atlas/interface-protocol
   :ontology/keys [:interface-protocol/functions]})

;; Type-ref: protocol functions
(registry/register!
 :type-ref/interface-protocol-functions
 :atlas/type-ref
 #{:meta/ref-interface-protocol-functions}
 {:type-ref/source      :atlas/interface-protocol
  :type-ref/property    :interface-protocol/functions
  :type-ref/datalog-verb :protocol/function
  :type-ref/cardinality :db.cardinality/many})

;; Datalog extractor
(registry/register!
 :datalog-extractor/interface-protocol
 :atlas/datalog-extractor
 #{:meta/interface-protocol-extractor}
 {:datalog-extractor/fn (fn [compound-id props]
                          (when (contains? compound-id :atlas/interface-protocol)
                            (type-ref/extract-reference-facts
                             :atlas/interface-protocol
                             compound-id
                             props)))
  :datalog-extractor/schema {:protocol/function {:db/cardinality :db.cardinality/many}}})
