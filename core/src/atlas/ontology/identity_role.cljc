(ns atlas.ontology.identity-role
  "Identity-role ontology module. Auto-registers on require."
  (:require [atlas.registry :as registry]))

(def ontology-definition
  {:ontology/for :atlas/identity-role
   :ontology/keys [:identity-role/description
                   :identity-role/cannot-access
                   :identity-role/responsibilities
                   :identity-role/expectations
                   :identity-role/data-access
                   :identity-role/granted-by
                   :identity-role/security-requirement
                   :identity-role/audit-logged
                   :identity-role/typical-users
                   :identity-role/privacy-constraint]})

;; =============================================================================
;; DATALOG INTEGRATION
;; =============================================================================

(def datalog-schema
  "Datascript schema for identity-role properties."
  {:role/data-access {:db/cardinality :db.cardinality/many}
   :role/granted-by {:db/cardinality :db.cardinality/one}
   :role/audit-logged {:db/cardinality :db.cardinality/one}})

;; =============================================================================
;; AUTO-REGISTRATION (top-level, like clojure.spec)
;; =============================================================================

(registry/register!
 :atlas/identity-role
 :atlas/ontology
 #{:atlas/identity-role}
 ontology-definition)

;; Datalog extractor
(registry/register!
 :datalog-extractor/identity-role
 :atlas/datalog-extractor
 #{:meta/identity-role-extractor}
 {:datalog-extractor/fn (fn [compound-id props]
                          (when (contains? compound-id :atlas/identity-role)
                            (let [dev-id (:atlas/dev-id props)
                                  data-access (:identity-role/data-access props)
                                  granted-by (:identity-role/granted-by props)
                                  audit-logged (:identity-role/audit-logged props)]
                              (cond-> []
                                ;; Data access (collection)
                                data-access
                                (concat (map (fn [access]
                                               [:db/add dev-id :role/data-access access])
                                             (if (coll? data-access) data-access [data-access])))

                                ;; Granted by (reference)
                                granted-by
                                (conj [:db/add dev-id :role/granted-by granted-by])

                                ;; Audit logged (boolean)
                                audit-logged
                                (conj [:db/add dev-id :role/audit-logged audit-logged])))))
  :datalog-extractor/schema datalog-schema})
