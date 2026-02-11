(ns atlas.ontology.structure-component
  "Structure-component ontology module.

   This module defines the `:atlas/structure-component` entity type and related
   functionality. Auto-registers on require.

   Usage:
     (require '[atlas.ontology.structure-component])"
  (:require [atlas.registry :as registry]
            [atlas.registry.lookup :as entity]
            [atlas.ontology.type-ref :as type-ref]
            [clojure.spec.alpha :as s]
            [clojure.set :as set]))

(s/def :structure-component/deps (s/coll-of :atlas/dev-id))

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

;; Ontology
(registry/register!
 :atlas/structure-component
 :atlas/ontology
 #{:atlas/structure-component}
 {:ontology/for :atlas/structure-component
  :ontology/keys [:structure-component/deps]
  :dataflow/deps-key :structure-component/deps})

;; Type-ref: structure-component → structure-component (self-ref deps)
(registry/register!
 :type-ref/structure-component-deps
 :atlas/type-ref
 #{:meta/ref-structure-component-deps}
 {:type-ref/source :atlas/structure-component
  :type-ref/property :structure-component/deps
  :type-ref/datalog-verb :entity/depends
  :type-ref/cardinality :db.cardinality/many})

;; Datalog extractor
(registry/register!
 :datalog-extractor/structure-component
 :atlas/datalog-extractor
 #{:meta/structure-component-extractor}
 {:datalog-extractor/fn (fn [compound-id props]
                          (when (contains? compound-id :atlas/structure-component)
                            (let [dev-id (:atlas/dev-id props)
                                  consumes (:structure-component/consumes props)
                                  emits (:structure-component/emits props)
                                  provides (:structure-component/provides props)]
                              (concat
                               ;; Automatic reference extraction via type-ref
                               (type-ref/extract-reference-facts
                                :atlas/structure-component
                                compound-id
                                props)

                               ;; Manual extraction for non-reference properties
                               (cond-> []
                                 ;; Consumes (component-specific)
                                 consumes
                                 (concat (map (fn [c]
                                                [:db/add dev-id :component/consumes c])
                                              consumes))

                                 ;; Emits (component-specific)
                                 emits
                                 (concat (map (fn [e]
                                                [:db/add dev-id :component/emits e])
                                              emits))

                                 ;; Provides (services/capabilities)
                                 provides
                                 (concat (map (fn [p]
                                                [:db/add dev-id :component/provides p])
                                              provides)))))))
  :datalog-extractor/schema {:entity/depends {:db/cardinality :db.cardinality/many}
   :component/consumes {:db/cardinality :db.cardinality/many}
   :component/emits {:db/cardinality :db.cardinality/many}
   :component/provides {:db/cardinality :db.cardinality/many}}})

;; Invariants
(registry/register!
 :invariant/components-are-foundation
 :atlas/invariant
 #{:meta/foundation-tier-check}
 {:invariant/fn (fn []
                  ;; "Components should be :tier/foundation."
                  (let [components (entity/all-with-aspect :atlas/structure-component)

                        violations (remove #(or (entity/has-aspect? % :atlas/ontology)
                                                (entity/has-aspect? % :tier/foundation)) components)]
                    (when (seq violations)
                      {:invariant :components-are-foundation
                       :violation :wrong-tier
                       :components violations
                       :severity :error
                       :message (str "Components should be :tier/foundation: " (pr-str violations))})))})

(registry/register!
 :invariant/protocol-exists
 :atlas/invariant
 #{:meta/protocol-existence-check}
 {:invariant/fn (fn 
                  []
                  ;; "Components that declare protocol aspects must have those protocols registered."
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
                                     (set (map :missing-protocol violations)))})))})

(registry/register!
 :invariant/protocol-conformance
 :atlas/invariant
 #{:meta/protocol-conformance-check}
 {:invariant/fn (fn []
                  "Components implementing a protocol must provide all required protocol functions.

   Checks that component's implementation map contains all methods declared
   in the protocol's :protocol/functions."

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
                                     "This is a warning because implementations may be provided at runtime.")})))})
