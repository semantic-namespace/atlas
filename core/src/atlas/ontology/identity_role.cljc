(ns atlas.ontology.identity-role
  "Identity-role ontology module."
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

(defonce ^:private loaded? (atom false))

(defn load!
  "Load the identity-role ontology."
  []
  (when-not @loaded?
    (registry/register!
     :atlas/identity-role
     :atlas/ontology
     #{:atlas/identity-role}
     ontology-definition)
    (reset! loaded? true))
  :loaded)

(defn loaded?* [] @loaded?)

(defn unload! []
  (when @loaded? (reset! loaded? false))
  :unloaded)

(defn reset-loaded-state! [] (reset! loaded? false))
