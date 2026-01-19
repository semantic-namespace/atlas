(ns atlas.ontology.data-schema
  "Data-schema ontology module."
  (:require [atlas.registry :as registry]))

(def ontology-definition
  {:ontology/for :atlas/data-schema
   :ontology/keys [:data-schema/fields]})

(defonce ^:private loaded? (atom false))

(defn load!
  "Load the data-schema ontology."
  []
  (when-not @loaded?
    (registry/register!
     :atlas/data-schema
     :atlas/ontology
     #{:atlas/data-schema}
     ontology-definition)
    (reset! loaded? true))
  :loaded)

(defn loaded?* [] @loaded?)

(defn unload! []
  (when @loaded? (reset! loaded? false))
  :unloaded)

(defn reset-loaded-state! [] (reset! loaded? false))
