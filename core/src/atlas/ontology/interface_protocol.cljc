(ns atlas.ontology.interface-protocol
  "Interface-protocol ontology module. Auto-registers on require."
  (:require [atlas.registry :as registry]))

;; Ontology
(registry/register!
 :atlas/interface-protocol
 :atlas/ontology
 #{:atlas/interface-protocol}
 {:ontology/for :atlas/interface-protocol
   :ontology/keys [:interface-protocol/functions]})

;; Datalog extractor
(registry/register!
 :datalog-extractor/interface-protocol
 :atlas/datalog-extractor
 #{:meta/interface-protocol-extractor}
 {:datalog-extractor/fn (fn [compound-id props]
                          (when (contains? compound-id :atlas/interface-protocol)
                            (let [dev-id (:atlas/dev-id props)]
                              (when-let [functions (:interface-protocol/functions props)]
                                (map (fn [protocol-fn]
                                       [:db/add dev-id :protocol/function protocol-fn])
                                     functions)))))
  :datalog-extractor/schema {:protocol/function {:db/cardinality :db.cardinality/many}}})
