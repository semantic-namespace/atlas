(ns atlas.ontology.business-pattern
  "Business-pattern ontology module."
  (:require [atlas.registry :as registry]))

(def ontology-definition
  {:ontology/for :atlas/business-pattern
   :ontology/keys [:business-pattern/principle
                   :business-pattern/justification
                   :business-pattern/experience-journey
                   :business-pattern/failure-recovery
                   :business-pattern/alternative-rejected
                   :business-pattern/why-rejected
                   :business-pattern/business-value
                   :business-pattern/metrics-improved]})

(defonce ^:private loaded? (atom false))

(defn load!
  "Load the business-pattern ontology."
  []
  (when-not @loaded?
    (registry/register!
     :atlas/business-pattern
     :atlas/ontology
     #{:atlas/business-pattern}
     ontology-definition)
    (reset! loaded? true))
  :loaded)

(defn loaded?* [] @loaded?)

(defn unload! []
  (when @loaded? (reset! loaded? false))
  :unloaded)

(defn reset-loaded-state! [] (reset! loaded? false))
