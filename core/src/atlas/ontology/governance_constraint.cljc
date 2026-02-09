(ns atlas.ontology.governance-constraint
  "Governance-constraint ontology module. Auto-registers on require."
  (:require [atlas.registry :as registry]))

(def ontology-definition
  {:ontology/for :atlas/governance-constraint
   :ontology/keys [:governance-constraint/enforced-by
                   :governance-constraint/rationale
                   :governance-constraint/compliance-requirement
                   :governance-constraint/violation-response
                   :governance-constraint/user-sees
                   :governance-constraint/business-impact
                   :governance-constraint/google-oauth-scope
                   :governance-constraint/user-benefit
                   :governance-constraint/alternative-rejected
                   :governance-constraint/why-rejected
                   :governance-constraint/user-experiences
                   :governance-constraint/recovery-path
                   :governance-constraint/revocable
                   :governance-constraint/revocation-path]})

;; =============================================================================
;; DATALOG INTEGRATION
;; =============================================================================

(def datalog-schema
  "Datascript schema for governance-constraint properties."
  {:constraint/enforced-by {:db/cardinality :db.cardinality/many}
   :constraint/compliance-requirement {:db/cardinality :db.cardinality/many}
   :constraint/oauth-scope {:db/cardinality :db.cardinality/many}
   :constraint/revocable {:db/cardinality :db.cardinality/one}})

;; =============================================================================
;; AUTO-REGISTRATION (top-level, like clojure.spec)
;; =============================================================================

(registry/register!
 :atlas/governance-constraint
 :atlas/ontology
 #{:atlas/governance-constraint}
 ontology-definition)

;; Datalog extractor
(registry/register!
 :datalog-extractor/governance-constraint
 :atlas/datalog-extractor
 #{:meta/governance-constraint-extractor}
 {:datalog-extractor/fn (fn [compound-id props]
                          (when (contains? compound-id :atlas/governance-constraint)
                            (let [dev-id (:atlas/dev-id props)
                                  enforced-by (:governance-constraint/enforced-by props)
                                  compliance (:governance-constraint/compliance-requirement props)
                                  oauth-scopes (:governance-constraint/google-oauth-scope props)
                                  revocable (:governance-constraint/revocable props)]
                              (cond-> []
                                ;; Enforced by (references to components/functions)
                                enforced-by
                                (concat (map (fn [enforcer]
                                               [:db/add dev-id :constraint/enforced-by enforcer])
                                             (if (coll? enforced-by) enforced-by [enforced-by])))

                                ;; Compliance requirements
                                compliance
                                (concat (map (fn [req]
                                               [:db/add dev-id :constraint/compliance-requirement req])
                                             (if (coll? compliance) compliance [compliance])))

                                ;; OAuth scopes
                                oauth-scopes
                                (concat (map (fn [scope]
                                               [:db/add dev-id :constraint/oauth-scope scope])
                                             (if (coll? oauth-scopes) oauth-scopes [oauth-scopes])))

                                ;; Revocable flag
                                revocable
                                (conj [:db/add dev-id :constraint/revocable revocable])))))
  :datalog-extractor/schema datalog-schema})
