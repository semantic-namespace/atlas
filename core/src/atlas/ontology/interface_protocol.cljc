(ns atlas.ontology.interface-protocol
  "Interface-protocol ontology module."
  (:require [atlas.registry :as registry]))

(def ontology-definition
  {:ontology/for :atlas/interface-protocol
   :ontology/keys [:interface-protocol/functions]})

(defonce ^:private loaded? (atom false))

(defn load!
  "Load the interface-protocol ontology."
  []
  (when-not @loaded?
    (registry/register!
     :atlas/interface-protocol
     :atlas/ontology
     #{:atlas/interface-protocol}
     ontology-definition)
    (reset! loaded? true))
  :loaded)

(defn loaded?* [] @loaded?)

(defn unload! []
  (when @loaded? (reset! loaded? false))
  :unloaded)

(defn reset-loaded-state! [] (reset! loaded? false))
