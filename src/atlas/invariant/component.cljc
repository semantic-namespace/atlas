(ns atlas.invariant.component
  "Component-related invariants and helpers."
  (:require [atlas.registry :as cid]
            [atlas.entity :as rt]
            [clojure.set :as set]))

;; =============================================================================
;; PROTOCOL HELPERS
;; =============================================================================

(defn- registered-protocol-ids
  "Return protocol dev-ids registered as :semantic-namespace/interface-protocol."
  []
  (->> @cid/registry
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
;; COMPONENT INVARIANTS
;; =============================================================================

(defn invariant-components-are-foundation
  "Components should be :tier/foundation."
  []
  (let [components (rt/all-with-aspect :atlas/structure-component)
        violations (remove #(rt/has-aspect? % :tier/foundation) components)]
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
        components-with-protocols (->> @cid/registry
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
  (let [protocols (->> @cid/registry
                       (filter (fn [[id _]] (contains? id :atlas/interface-protocol)))
                       (map (fn [[_ v]]
                              {:protocol-id (:atlas/dev-id v)
                               :required-fns (set (:interface-protocol/functions v))}))
                       (remove #(empty? (:required-fns %))))

        ;; Note: This invariant is for design-time checking. At runtime, implementations
        ;; are typically functions/closures, not maps with protocol methods.
        ;; This invariant validates that component REGISTRATIONS declare conformance.
        violations (for [{:keys [protocol-id required-fns]} protocols
                         :when required-fns
                         [compound-id value] @cid/registry
                         :when (and (contains? compound-id :atlas/structure-component)
                                    (contains? compound-id protocol-id))
                         :let [dev-id (:atlas/dev-id value)
                               ;; Components may provide protocol methods in their value map
                               ;; or may be validated elsewhere (implementation-specific)
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

(def component-invariants
  "Component invariants in check order."
  [invariant-components-are-foundation
   invariant-protocol-exists
   invariant-protocol-conformance])

(defn- result-level [result]
  (or (:level result) (:severity result)))

(defn- normalize-result
  "Ensure invariant results carry both :level and :severity."
  [result]
  (cond-> result
    (and (:severity result) (not (:level result)))
    (assoc :level (:severity result))
    (and (:level result) (not (:severity result)))
    (assoc :severity (:level result))))

(defn check
  "Run provided component invariants and return {:valid? :errors :warnings :violations}."
  [invariants]
  (let [results (keep #(%) invariants)
        normalized (map normalize-result results)
        errors (filter #(= :error (result-level %)) normalized)
        warnings (filter #(= :warning (result-level %)) normalized)]
    {:violations normalized
     :errors errors
     :warnings warnings
     :violations-flat normalized
     :errors-flat errors
     :warnings-flat warnings
     :valid? (empty? errors)}))

(defn check-invariants
  "Run component invariants."
  []
  (check component-invariants))
