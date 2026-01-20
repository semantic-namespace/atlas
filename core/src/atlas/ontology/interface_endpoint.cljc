(ns atlas.ontology.interface-endpoint
  "Interface-endpoint ontology module.

   This module defines the `:atlas/interface-endpoint` entity type and related
   functionality. It is an optional ontology that must be explicitly loaded
   before using interface-endpoint features.

   Usage:
     (require '[atlas.ontology.interface-endpoint :as ie])
     (ie/load!)

   After loading, you can:
   - Register interface-endpoints with :interface-endpoint/context, :response, :deps
   - Use ontology/context-for, ontology/response-for, ontology/deps-for
   - Run invariants like invariant-endpoints-are-api-tier"
  (:require [atlas.registry :as registry]
            [atlas.registry.lookup :as entity]
            [atlas.ontology :as ontology]
            [atlas.invariant :as invariant]
            [clojure.set :as set]))

;; =============================================================================
;; ONTOLOGY DEFINITION
;; =============================================================================

(def ontology-definition
  "The ontology definition for :atlas/interface-endpoint"
  {:ontology/for :atlas/interface-endpoint
   :ontology/keys [:interface-endpoint/context
                   :interface-endpoint/response
                   :interface-endpoint/deps]
   :dataflow/context-key :interface-endpoint/context
   :dataflow/response-key :interface-endpoint/response
   :dataflow/deps-key :interface-endpoint/deps})

;; =============================================================================
;; INVARIANTS - Interface-endpoint specific rules
;; =============================================================================

(defn invariant-endpoints-are-api-tier
  "Endpoints should be :tier/api."
  []
  (let [endpoints (->> (entity/all-with-aspect :atlas/interface-endpoint)
                       (remove #(entity/has-aspect? % :atlas/ontology)))
        violations (remove #(entity/has-aspect? % :tier/api) endpoints)]
    (when (seq violations)
      {:invariant :endpoints-are-api-tier
       :violation :wrong-tier
       :endpoints violations
       :severity :error
       :message (str "Endpoints should be :tier/api: " violations)})))

(defn invariant-all-fns-reachable
  "Every execution-function should be reachable from some endpoint.

   This invariant expresses that all business logic (execution-functions)
   must be accessible through the API layer (interface-endpoints).
   Unreachable functions are dead code.

   Note: This makes explicit that interface-endpoint depends on
   execution-function in the ontology module hierarchy."
  []
  (let [endpoints (entity/all-with-aspect :atlas/interface-endpoint)
        all-fns (set (entity/all-with-aspect :atlas/execution-function))
        ;; Find reachable via BFS from endpoints
        reachable (atom #{})
        collect-reachable (fn collect [id]
                            (when-not (@reachable id)
                              (swap! reachable conj id)
                              (doseq [dep (ontology/deps-for id)]
                                (collect dep))))]
    (doseq [ep endpoints]
      (collect-reachable ep))
    (let [unreachable (set/difference all-fns @reachable)]
      (when (seq unreachable)
        {:invariant :all-fns-reachable
         :violation :unreachable-functions
         :functions unreachable
         :severity :warning
         :message (str "Functions not reachable from any endpoint: " unreachable)}))))

(def invariants
  "All invariants specific to interface-endpoint ontology"
  [invariant-endpoints-are-api-tier
   invariant-all-fns-reachable])

;; =============================================================================
;; LOADING
;; =============================================================================

(defn- register-ontology!
  "Register the interface-endpoint ontology in the registry."
  []
  (registry/register!
   :atlas/interface-endpoint
   :atlas/ontology
   #{:atlas/interface-endpoint}
   ontology-definition))

(defn- register-invariants!
  "Register interface-endpoint invariants with the invariant module."
  []
  (doseq [inv invariants]
    (invariant/register-ontology-invariant! inv)))

(defonce ^:private loaded? (atom false))

(defn load!
  "Load the interface-endpoint ontology.

   This must be called before using interface-endpoint features:
   - Registering entities with :interface-endpoint/* properties
   - Using ontology/context-for, ontology/response-for, ontology/deps-for
   - Running invariant-endpoints-are-api-tier

   Safe to call multiple times - subsequent calls are no-ops."
  []
  (when-not @loaded?
    (register-ontology!)
    (register-invariants!)
    (reset! loaded? true))
  :loaded)

(defn loaded?*
  "Check if the interface-endpoint ontology has been loaded."
  []
  @loaded?)

(defn unload!
  "Unload the interface-endpoint ontology (primarily for testing).

   WARNING: This does not remove already-registered interface-endpoints
   from the registry. Use reset! on registry/registry for a full reset."
  []
  (when @loaded?
    (doseq [inv invariants]
      (invariant/unregister-ontology-invariant! inv))
    (reset! loaded? false))
  :unloaded)

(defn reset-loaded-state!
  "Reset the loaded state to false (for testing).

   Use this before calling load! in test fixtures when you've also reset
   the registry. This ensures load! will re-register the ontology."
  []
  (reset! loaded? false))
