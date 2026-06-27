(ns atlas.ontology.business-pattern
  "Business-pattern ontology module. Auto-registers on require."
  (:require [atlas.registry :as registry]
            [atlas.registry.lookup :as entity]
            [atlas.ontology.type-ref :as type-ref]))

(registry/register!
 :atlas/business-pattern
 :atlas/ontology
 #{:atlas/business-pattern}
 {:ontology/for :atlas/business-pattern
  :ontology/keys [:business-pattern/principle
                  :business-pattern/justification
                  :business-pattern/experience-journey
                  :business-pattern/failure-recovery
                  :business-pattern/alternative-rejected
                  :business-pattern/why-rejected
                  :business-pattern/business-value
                  :business-pattern/metrics-improved]})

;; Type-refs
(registry/register!
 :type-ref/business-pattern-journey
 :atlas/type-ref
 #{:meta/ref-business-pattern-journey}
 {:type-ref/source      :atlas/business-pattern
  :type-ref/property    :business-pattern/experience-journey
  :type-ref/datalog-verb :business-pattern/experience-journey
  :type-ref/cardinality :db.cardinality/one})

(registry/register!
 :type-ref/business-pattern-metrics
 :atlas/type-ref
 #{:meta/ref-business-pattern-metrics}
 {:type-ref/source      :atlas/business-pattern
  :type-ref/property    :business-pattern/metrics-improved
  :type-ref/datalog-verb :business-pattern/metrics-improved
  :type-ref/cardinality :db.cardinality/many})

;; Invariant — existence implies wiring

(defn- all-business-patterns
  "All registered business-pattern entities, excluding the ontology definition itself."
  []
  (->> (entity/all-with-aspect :atlas/business-pattern)
       (filter #(= :atlas/business-pattern (:atlas/type (entity/props-for %))))))

(defn- all-value-propositions
  "All registered value-proposition entities, excluding the ontology definition itself."
  []
  (->> (entity/all-with-aspect :atlas/value-proposition)
       (filter #(= :atlas/value-proposition (:atlas/type (entity/props-for %))))))

(registry/register!
 :invariant/business-pattern-has-implementors
 :atlas/invariant
 #{:meta/business-pattern-invariant}
 {:invariant/fn
  (fn []
    (let [implemented (set (keep #(:value-proposition/implements-pattern
                                   (entity/props-for %))
                                 (all-value-propositions)))
          violations  (for [bp-id (all-business-patterns)
                            :when (not (implemented bp-id))]
                        {:business-pattern bp-id})]
      (when (seq violations)
        {:invariant :business-pattern-has-implementors
         :violation :no-implementing-value-proposition
         :details   violations
         :severity  :error
         :message   (str "Business patterns without an implementing value proposition: "
                         (mapv :business-pattern violations))})))})

;; Datalog extractor
(registry/register!
 :datalog-extractor/business-pattern
 :atlas/datalog-extractor
 #{:meta/business-pattern-extractor}
 {:datalog-extractor/fn (fn [compound-id props]
                          (when (contains? compound-id :atlas/business-pattern)
                            (type-ref/extract-reference-facts
                             :atlas/business-pattern
                             compound-id
                             props)))
  :datalog-extractor/schema {:business-pattern/experience-journey {:db/cardinality :db.cardinality/one}
                             :business-pattern/metrics-improved {:db/cardinality :db.cardinality/many}}})
