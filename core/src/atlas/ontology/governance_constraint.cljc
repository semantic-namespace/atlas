(ns atlas.ontology.governance-constraint
  "Governance-constraint ontology module. Auto-registers on require."
  (:require [atlas.registry :as registry]
            [atlas.ontology.type-ref :as type-ref]))

(registry/register!
 :atlas/governance-constraint
 :atlas/ontology
 #{:atlas/governance-constraint}
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

;; Type-refs
(registry/register!
 :type-ref/governance-constraint-enforced-by
 :atlas/type-ref
 #{:meta/ref-governance-constraint-enforced-by}
 {:type-ref/source      :atlas/governance-constraint
  :type-ref/property    :governance-constraint/enforced-by
  :type-ref/datalog-verb :constraint/enforced-by
  :type-ref/cardinality :db.cardinality/many})

(registry/register!
 :type-ref/governance-constraint-compliance
 :atlas/type-ref
 #{:meta/ref-governance-constraint-compliance}
 {:type-ref/source      :atlas/governance-constraint
  :type-ref/property    :governance-constraint/compliance-requirement
  :type-ref/datalog-verb :constraint/compliance-requirement
  :type-ref/cardinality :db.cardinality/many})

(registry/register!
 :type-ref/governance-constraint-oauth-scope
 :atlas/type-ref
 #{:meta/ref-governance-constraint-oauth-scope}
 {:type-ref/source      :atlas/governance-constraint
  :type-ref/property    :governance-constraint/google-oauth-scope
  :type-ref/datalog-verb :constraint/oauth-scope
  :type-ref/cardinality :db.cardinality/many})

(registry/register!
 :type-ref/governance-constraint-revocable
 :atlas/type-ref
 #{:meta/ref-governance-constraint-revocable}
 {:type-ref/source      :atlas/governance-constraint
  :type-ref/property    :governance-constraint/revocable
  :type-ref/datalog-verb :constraint/revocable
  :type-ref/cardinality :db.cardinality/one})

;; Datalog extractor
(registry/register!
 :datalog-extractor/governance-constraint
 :atlas/datalog-extractor
 #{:meta/governance-constraint-extractor}
 {:datalog-extractor/fn (fn [compound-id props]
                          (when (contains? compound-id :atlas/governance-constraint)
                            (type-ref/extract-reference-facts
                             :atlas/governance-constraint
                             compound-id
                             props)))
  :datalog-extractor/schema {:constraint/enforced-by {:db/cardinality :db.cardinality/many}
                             :constraint/compliance-requirement {:db/cardinality :db.cardinality/many}
                             :constraint/oauth-scope {:db/cardinality :db.cardinality/many}
                             :constraint/revocable {:db/cardinality :db.cardinality/one}}})
