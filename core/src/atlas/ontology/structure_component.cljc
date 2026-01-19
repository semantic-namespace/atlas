(ns atlas.ontology.structure-component
  "Structure-component ontology module.

   This module defines the `:atlas/structure-component` entity type and related
   functionality. It is an optional ontology that must be explicitly loaded
   before using structure-component features.

   Usage:
     (require '[atlas.ontology.structure-component :as sc])
     (sc/load!)

   After loading, you can:
   - Register structure-components with :structure-component/deps, :provides, etc.
   - Use templates like template:foundation-component
   - Run invariants like invariant-components-are-foundation"
  (:require [atlas.registry :as registry]
            [atlas.registry.lookup :as entity]
            [atlas.invariant :as invariant]
            [clojure.set :as set]))

;; =============================================================================
;; ONTOLOGY DEFINITION
;; =============================================================================

(def ontology-definition
  "The ontology definition for :atlas/structure-component"
  {:ontology/for :atlas/structure-component
   :ontology/keys [:structure-component/deps
                   :structure-component/consumes
                   :structure-component/emits
                   :structure-component/visual-purpose
                   :structure-component/rendering-features
                   :structure-component/provides]
   :dataflow/deps-key :structure-component/deps})

;; =============================================================================
;; TEMPLATES - Reducing boilerplate
;; =============================================================================

(defn template:foundation-component
  "Template for foundation-tier components.

   Usage:
     (template:foundation-component :domain/db
       :external? true :async? true :traced? true)"
  [domain & {:keys [external? async? traced?]
             :or {external? false async? false traced? false}}]
  (cond-> #{:atlas/structure-component
            :semantic/docs
            :tier/foundation
            :effect/read}
    domain (conj domain)
    external? (conj :integration/external)
    (not external?) (conj :integration/internal)
    async? (conj :temporal/async :temporal/timeout-configured)
    traced? (conj :observability/traced)
    (not traced?) (conj :observability/metered)))

;; =============================================================================
;; PROTOCOL HELPERS
;; =============================================================================

(defn- registered-protocol-ids
  "Return protocol dev-ids registered as :atlas/interface-protocol."
  []
  (->> @registry/registry
       (filter (fn [[id _]] (contains? id :atlas/interface-protocol)))
       (map (fn [[_ v]] (:atlas/dev-id v)))
       set))

(defn get-protocol-aspects
  "Extract protocol aspects from a compound identity and classify against the registry.
   Returns {:declared [...], :known [...], :unknown [...]}."
  ([compound-id]
   (get-protocol-aspects compound-id (registered-protocol-ids)))
  ([compound-id known-protocols]
   (let [declared (->> compound-id
                       (filter #(= "protocol" (namespace %)))
                       sort
                       vec)
         known (filterv known-protocols declared)
         unknown (filterv (complement known-protocols) declared)]
     {:declared declared
      :known known
      :unknown unknown})))

;; =============================================================================
;; INVARIANTS - Structure-component specific rules
;; =============================================================================

(defn invariant-components-are-foundation
  "Components should be :tier/foundation."
  []
  (let [components (entity/all-with-aspect :atlas/structure-component)
        violations (remove #(entity/has-aspect? % :tier/foundation) components)]
    (when (seq violations)
      {:invariant :components-are-foundation
       :violation :wrong-tier
       :components violations
       :severity :error
       :message (str "Components should be :tier/foundation: " violations)})))

(defn invariant-protocol-exists
  "Components that declare protocol aspects must have those protocols registered."
  []
  (let [known-protocols (registered-protocol-ids)
        components-with-protocols (->> @registry/registry
                                       (filter (fn [[id _]]
                                                 (and (contains? id :atlas/structure-component)
                                                      (seq (:declared (get-protocol-aspects id known-protocols))))))
                                       (map (fn [[id v]]
                                              (let [{:keys [declared unknown]} (get-protocol-aspects id known-protocols)]
                                                {:dev-id (:atlas/dev-id v)
                                                 :protocols declared
                                                 :missing unknown}))))
        violations (for [{:keys [dev-id missing]} components-with-protocols
                         protocol missing]
                     {:component dev-id :missing-protocol protocol})]
    (when (seq violations)
      {:invariant :protocol-exists
       :violation :missing-protocol-definition
       :details violations
       :severity :error
       :message (str "Components reference undefined protocols: "
                     (set (map :missing-protocol violations)))})))

(defn invariant-protocol-conformance
  "Components implementing a protocol must provide all required protocol functions.

   Checks that component's implementation map contains all methods declared
   in the protocol's :protocol/functions."
  []
  (let [protocols (->> @registry/registry
                       (filter (fn [[id _]] (contains? id :atlas/interface-protocol)))
                       (map (fn [[_ v]]
                              {:protocol-id (:atlas/dev-id v)
                               :required-fns (set (:interface-protocol/functions v))}))
                       (remove #(empty? (:required-fns %))))

        violations (for [{:keys [protocol-id required-fns]} protocols
                         :when required-fns
                         [compound-id value] @registry/registry
                         :when (and (contains? compound-id :atlas/structure-component)
                                    (contains? compound-id protocol-id))
                         :let [dev-id (:atlas/dev-id value)
                               provided-methods (when (map? value)
                                                  (set (filter #(namespace %) (keys value))))
                               missing (when provided-methods
                                         (set/difference required-fns provided-methods))]
                         :when (and missing (seq missing))]
                     {:component dev-id
                      :protocol protocol-id
                      :missing-methods (vec missing)})]
    (when (seq violations)
      {:invariant :protocol-conformance
       :violation :incomplete-protocol-implementation
       :details violations
       :severity :warning
       :message (str "Components don't implement all protocol methods. "
                     "This is a warning because implementations may be provided at runtime.")})))

(def invariants
  "All invariants specific to structure-component ontology"
  [invariant-components-are-foundation
   invariant-protocol-exists
   invariant-protocol-conformance])

;; =============================================================================
;; LOADING
;; =============================================================================

(defn- register-ontology!
  "Register the structure-component ontology in the registry."
  []
  (registry/register!
   :atlas/structure-component
   :atlas/ontology
   #{:atlas/structure-component}
   ontology-definition))

(defn- register-invariants!
  "Register structure-component invariants with the invariant module."
  []
  (doseq [inv invariants]
    (invariant/register-ontology-invariant! inv)))

(defonce ^:private loaded? (atom false))

(defn load!
  "Load the structure-component ontology.

   This must be called before using structure-component features:
   - Registering entities with :structure-component/* properties
   - Using templates like template:foundation-component
   - Running component invariants

   Safe to call multiple times - subsequent calls are no-ops."
  []
  (when-not @loaded?
    (register-ontology!)
    (register-invariants!)
    (reset! loaded? true))
  :loaded)

(defn loaded?*
  "Check if the structure-component ontology has been loaded."
  []
  @loaded?)

(defn unload!
  "Unload the structure-component ontology (primarily for testing).

   WARNING: This does not remove already-registered structure-components
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
