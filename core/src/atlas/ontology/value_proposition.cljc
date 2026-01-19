(ns atlas.ontology.value-proposition
  "Value-proposition ontology module."
  (:require [atlas.registry :as registry]))

(def ontology-definition
  {:ontology/for :atlas/value-proposition
   :ontology/keys [:value-proposition/business-problem
                   :value-proposition/before-state
                   :value-proposition/after-state
                   :value-proposition/time-saved
                   :value-proposition/solution
                   :value-proposition/metrics-improved
                   :value-proposition/user-segment
                   :value-proposition/business-value-quantified
                   :value-proposition/business-value
                   :value-proposition/competitive-advantage
                   :value-proposition/implements-pattern
                   :value-proposition/trust-factors
                   :value-proposition/risk-mitigation
                   :value-proposition/compliance-benefit]})

(defonce ^:private loaded? (atom false))

(defn load!
  "Load the value-proposition ontology."
  []
  (when-not @loaded?
    (registry/register!
     :atlas/value-proposition
     :atlas/ontology
     #{:atlas/value-proposition}
     ontology-definition)
    (reset! loaded? true))
  :loaded)

(defn loaded?* [] @loaded?)

(defn unload! []
  (when @loaded? (reset! loaded? false))
  :unloaded)

(defn reset-loaded-state! [] (reset! loaded? false))
