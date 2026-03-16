(ns atlas.ontology.identity-role
  "Identity-role ontology module. Auto-registers on require."
  (:require [atlas.registry :as registry]
            [atlas.ontology.type-ref :as type-ref]))

(registry/register!
 :atlas/identity-role
 :atlas/ontology
 #{:atlas/identity-role}
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

;; Type-refs
(registry/register!
 :type-ref/identity-role-data-access
 :atlas/type-ref
 #{:meta/ref-identity-role-data-access}
 {:type-ref/source      :atlas/identity-role
  :type-ref/property    :identity-role/data-access
  :type-ref/datalog-verb :role/data-access
  :type-ref/cardinality :db.cardinality/many})

(registry/register!
 :type-ref/identity-role-granted-by
 :atlas/type-ref
 #{:meta/ref-identity-role-granted-by}
 {:type-ref/source      :atlas/identity-role
  :type-ref/property    :identity-role/granted-by
  :type-ref/datalog-verb :role/granted-by
  :type-ref/cardinality :db.cardinality/one})

(registry/register!
 :type-ref/identity-role-audit-logged
 :atlas/type-ref
 #{:meta/ref-identity-role-audit-logged}
 {:type-ref/source      :atlas/identity-role
  :type-ref/property    :identity-role/audit-logged
  :type-ref/datalog-verb :role/audit-logged
  :type-ref/cardinality :db.cardinality/one})

;; Datalog extractor
(registry/register!
 :datalog-extractor/identity-role
 :atlas/datalog-extractor
 #{:meta/identity-role-extractor}
 {:datalog-extractor/fn (fn [compound-id props]
                          (when (contains? compound-id :atlas/identity-role)
                            (type-ref/extract-reference-facts
                             :atlas/identity-role
                             compound-id
                             props)))
  :datalog-extractor/schema {:role/data-access {:db/cardinality :db.cardinality/many}
                             :role/granted-by {:db/cardinality :db.cardinality/one}
                             :role/audit-logged {:db/cardinality :db.cardinality/one}}})
