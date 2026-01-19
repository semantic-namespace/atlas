(ns atlas.ontology.governance-constraint
  "Governance-constraint ontology module."
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

(defonce ^:private loaded? (atom false))

(defn load!
  "Load the governance-constraint ontology."
  []
  (when-not @loaded?
    (registry/register!
     :atlas/governance-constraint
     :atlas/ontology
     #{:atlas/governance-constraint}
     ontology-definition)
    (reset! loaded? true))
  :loaded)

(defn loaded?* [] @loaded?)

(defn unload! []
  (when @loaded? (reset! loaded? false))
  :unloaded)

(defn reset-loaded-state! [] (reset! loaded? false))
