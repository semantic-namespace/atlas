(ns atlas.ontology.interface-protocol
  "Interface-protocol ontology module."
  (:require [atlas.registry :as registry]
            [atlas.datalog :as datalog]))

(def ontology-definition
  {:ontology/for :atlas/interface-protocol
   :ontology/keys [:interface-protocol/functions]})

;; =============================================================================
;; DATALOG INTEGRATION
;; =============================================================================

(defn extract-facts
  "Extract Datascript facts from interface-protocol properties.
   Called by atlas.datalog when building database."
  [compound-id props]
  (when (contains? compound-id :atlas/interface-protocol)
    (let [dev-id (:atlas/dev-id props)]
      (when-let [functions (:interface-protocol/functions props)]
        (map (fn [protocol-fn]
               [:db/add dev-id :protocol/function protocol-fn])
             functions)))))

(def datalog-schema
  "Datascript schema for interface-protocol properties."
  {:protocol/function {:db/cardinality :db.cardinality/many}})

(defn- register-datalog!
  "Register datalog extensions for interface-protocol properties."
  []
  (datalog/register-fact-extractor! extract-facts)
  (datalog/register-schema! datalog-schema))

;; =============================================================================
;; LOADING
;; =============================================================================

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
    (register-datalog!)
    (reset! loaded? true))
  :loaded)

(defn loaded?* [] @loaded?)

(defn unload! []
  (when @loaded? (reset! loaded? false))
  :unloaded)

(defn reset-loaded-state! [] (reset! loaded? false))
