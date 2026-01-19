(ns atlas.ontology.experience-journey
  "Experience-journey ontology module."
  (:require [atlas.registry :as registry]))

(def ontology-definition
  {:ontology/for :atlas/experience-journey
   :ontology/keys [:experience-journey/user-journey
                   :experience-journey/time-to-complete
                   :experience-journey/friction-points
                   :experience-journey/why-designed-this-way
                   :experience-journey/user-sentiment
                   :experience-journey/risk-failure-mode
                   :experience-journey/recovery-time
                   :experience-journey/delivers-value
                   :experience-journey/replaces]})

(defonce ^:private loaded? (atom false))

(defn load!
  "Load the experience-journey ontology."
  []
  (when-not @loaded?
    (registry/register!
     :atlas/experience-journey
     :atlas/ontology
     #{:atlas/experience-journey}
     ontology-definition)
    (reset! loaded? true))
  :loaded)

(defn loaded?* [] @loaded?)

(defn unload! []
  (when @loaded? (reset! loaded? false))
  :unloaded)

(defn reset-loaded-state! [] (reset! loaded? false))
