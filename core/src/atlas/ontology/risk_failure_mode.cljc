(ns atlas.ontology.risk-failure-mode
  "Risk-failure-mode ontology module."
  (:require [atlas.registry :as registry]))

(def ontology-definition
  {:ontology/for :atlas/risk-failure-mode
   :ontology/keys [:risk-failure-mode/triggered-by
                   :risk-failure-mode/detection
                   :risk-failure-mode/user-experiences
                   :risk-failure-mode/recovery-path
                   :risk-failure-mode/recovery-steps
                   :risk-failure-mode/data-loss
                   :risk-failure-mode/business-impact
                   :risk-failure-mode/frequency
                   :risk-failure-mode/preventable
                   :risk-failure-mode/why-not-preventable
                   :risk-failure-mode/prevention-strategy
                   :risk-failure-mode/security-event
                   :risk-failure-mode/logged
                   :risk-failure-mode/log-details]})

(defonce ^:private loaded? (atom false))

(defn load!
  "Load the risk-failure-mode ontology."
  []
  (when-not @loaded?
    (registry/register!
     :atlas/risk-failure-mode
     :atlas/ontology
     #{:atlas/risk-failure-mode}
     ontology-definition)
    (reset! loaded? true))
  :loaded)

(defn loaded?* [] @loaded?)

(defn unload! []
  (when @loaded? (reset! loaded? false))
  :unloaded)

(defn reset-loaded-state! [] (reset! loaded? false))
