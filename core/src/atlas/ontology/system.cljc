(ns atlas.ontology.system
  "System ontology module — declares a product or application as the root of the
   business-value graph.

   A :atlas/system entity lists the value-propositions it delivers via
   :system/delivers. This creates :entity/depends edges from the system to each
   VP, so:
     - blast-radius(VP)   → surfaces the system (product depends on this VP)
     - trace-causes(system) → full dependency chain: VPs → BPs → boundary endpoints

   Design principle: existence implies wiring.
   A system that exists must declare its delivered VPs; a VP that exists must be
   listed in at least one system. Both invariants are enforced here.

   Auto-registers on require."
  (:require [atlas.registry :as registry]
            [atlas.registry.lookup :as entity]
            [atlas.ontology.type-ref :as type-ref]
            [clojure.spec.alpha :as s]))

;; ---------------------------------------------------------------------------
;; Specs
;; ---------------------------------------------------------------------------

(s/def :system/name string?)
(s/def :system/delivers (s/coll-of qualified-keyword? :min-count 1))

;; ---------------------------------------------------------------------------
;; Entity type
;; ---------------------------------------------------------------------------

(registry/register!
 :atlas/system
 :atlas/ontology
 #{:atlas/system}
 {:ontology/for  :atlas/system
  :ontology/keys [:system/name
                  :system/delivers]
  :dataflow/deps-key :system/delivers})

;; Type-ref: system/delivers → VP (cardinality many, stored as :entity/depends)
(registry/register!
 :type-ref/system-delivers
 :atlas/type-ref
 #{:meta/ref-system-delivers}
 {:type-ref/source      :atlas/system
  :type-ref/property    :system/delivers
  :type-ref/datalog-verb :entity/depends
  :type-ref/cardinality  :db.cardinality/many})

;; Datalog extractor
(registry/register!
 :datalog-extractor/system
 :atlas/datalog-extractor
 #{:meta/system-extractor}
 {:datalog-extractor/fn
  (fn [compound-id props]
    (when (contains? compound-id :atlas/system)
      (type-ref/extract-reference-facts :atlas/system compound-id props)))
  :datalog-extractor/schema
  {:entity/depends {:db/cardinality :db.cardinality/many}}})

;; ---------------------------------------------------------------------------
;; Invariants — existence implies wiring
;; ---------------------------------------------------------------------------

(defn- all-systems
  "All registered :atlas/system entities, excluding the ontology definition."
  []
  (->> (entity/all-with-aspect :atlas/system)
       (filter #(= :atlas/system (:atlas/type (entity/props-for %))))))

(defn- all-value-propositions
  "All registered :atlas/value-proposition entities, excluding the ontology definition."
  []
  (->> (entity/all-with-aspect :atlas/value-proposition)
       (filter #(= :atlas/value-proposition (:atlas/type (entity/props-for %))))))

(registry/register!
 :invariant/system-delivers-valid-vps
 :atlas/invariant
 #{:meta/system-delivers-valid-vps}
 {:invariant/fn
  (fn []
    (let [violations (for [sys-id (all-systems)
                           :let   [props    (entity/props-for sys-id)
                                   delivers (:system/delivers props)]
                           vp-id  delivers
                           :when  (nil? (entity/props-for vp-id))]
                       {:system sys-id :missing-vp vp-id})]
      (when (seq violations)
        {:invariant :system-delivers-valid-vps
         :violation :delivers-references-missing-entity
         :details   violations
         :severity  :error
         :message   (str "System entities reference non-existent value-propositions: "
                         (mapv :missing-vp violations))})))})

(registry/register!
 :invariant/value-proposition-wired-to-system
 :atlas/invariant
 #{:meta/value-proposition-wired-to-system}
 {:invariant/fn
  (fn []
    (let [delivered (into #{}
                          (mapcat #(:system/delivers (entity/props-for %)))
                          (all-systems))
          violations (for [vp-id (all-value-propositions)
                           :when (not (delivered vp-id))]
                       {:value-proposition vp-id})]
      (when (seq violations)
        {:invariant :value-proposition-wired-to-system
         :violation :value-proposition-not-declared-by-any-system
         :details   violations
         :severity  :error
         :message   (str "Value propositions not declared in any system: "
                         (mapv :value-proposition violations))})))})
