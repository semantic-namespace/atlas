(ns atlas.ontology.allium-rule
  "EXPERIMENTAL — registers :atlas/allium-rule as a first-class entity type.

   An allium rule is one behavioural rule of a JUXT Allium spec
   (https://github.com/juxt/allium), carried in the registry so the spec is a
   projection of the registry rather than a file beside it. The rule's clauses
   are opaque allium-syntax strings: atlas owns the rule's identity, what
   implements it and its version history; `allium check` owns what the clauses
   mean. atlas.adapter.allium emits these entities in :project mode.

   Compound-id = semantic identity (domain, operation, ...).
   Props       = :rule/name, :rule/when, :rule/requires, :rule/ensures,
                 :rule/implemented-by (the dev-id of the code that implements it).

   :rule/implemented-by is an :entity/depends edge, so consumers-of,
   blast-radius and change-risk on the implementing entity list the rules it
   implements: a change to the code points at the rules to re-read.

   Usage:
     (require '[atlas.ontology.allium-rule])"
  (:require [atlas.registry :as registry]
            [atlas.registry.lookup :as entity]
            [atlas.ontology.type-ref :as type-ref]
            [clojure.spec.alpha :as s]))

;; ---------------------------------------------------------------------------
;; Specs
;; ---------------------------------------------------------------------------

(s/def :rule/name string?)
(s/def :rule/when string?)
(s/def :rule/requires (s/coll-of string? :kind vector?))
(s/def :rule/ensures (s/coll-of string? :kind vector? :min-count 1))
(s/def :rule/implemented-by qualified-keyword?)

;; ---------------------------------------------------------------------------
;; :atlas/allium-rule
;; ---------------------------------------------------------------------------

(registry/register!
 :atlas/allium-rule
 :atlas/ontology
 #{:atlas/allium-rule}
 {:ontology/for  :atlas/allium-rule
  :ontology/keys [:rule/name
                  :rule/when
                  :rule/ensures]})

;; Type-ref: rule → the entity that implements it (one)
;; Uses :entity/depends so consumers-of / blast-radius work on the implementor
(registry/register!
 :type-ref/allium-rule-implemented-by
 :atlas/type-ref
 #{:meta/ref-allium-rule-implemented-by}
 {:type-ref/source       :atlas/allium-rule
  :type-ref/property     :rule/implemented-by
  :type-ref/datalog-verb :entity/depends
  :type-ref/cardinality  :db.cardinality/one})

;; Datalog extractor
(registry/register!
 :datalog-extractor/allium-rule
 :atlas/datalog-extractor
 #{:meta/allium-rule-extractor}
 {:datalog-extractor/fn
  (fn [compound-id props]
    (when (contains? compound-id :atlas/allium-rule)
      (type-ref/extract-reference-facts :atlas/allium-rule compound-id props)))})

;; ---------------------------------------------------------------------------
;; Invariants
;; ---------------------------------------------------------------------------

(defn- rule-ids []
  (for [id (entity/all-with-aspect :atlas/allium-rule)
        :when (not (contains? (entity/identity-for id) :atlas/ontology))]
    id))

(registry/register!
 :invariant/allium-rule-implemented-by-exists
 :atlas/invariant
 #{:meta/allium-rule-implemented-by-check}
 {:invariant/rule {:doc "An allium rule's :rule/implemented-by exists"
                   :severity :error
                   :when {:or [{:= {:atlas/type :atlas/allium-rule}} {:= {:atlas/aspects :atlas/allium-rule}}]
                          :not {:or [{:= {:atlas/type :atlas/ontology}} {:= {:atlas/aspects :atlas/ontology}}
                                     {:= {:atlas/type :atlas/type}} {:= {:atlas/aspects :atlas/type}}]}}
                   :require [{:resolves :rule/implemented-by}]}
  :invariant/fn
  (fn []
    (let [violations (for [id    (rule-ids)
                           :let  [impl (:rule/implemented-by (entity/props-for id))]
                           :when impl
                           :when (nil? (entity/identity-for impl))]
                       {:entity id :missing-implementor impl})]
      (when (seq violations)
        {:invariant :allium-rule-implemented-by-exists
         :violation :rule-implementor-not-found
         :details   (vec violations)
         :severity  :error
         :message   "An allium rule's :rule/implemented-by names an entity that is not in the registry — the spec points at code that no longer exists"})))})

(registry/register!
 :invariant/allium-rule-names-unique
 :atlas/invariant
 #{:meta/allium-rule-names-unique-check}
 {:invariant/fn
  (fn []
    (let [by-name    (group-by #(:rule/name (entity/props-for %)) (rule-ids))
          violations (for [[nm ids] by-name
                           :when (> (count ids) 1)]
                       {:rule-name nm :entities (vec (sort ids))})]
      (when (seq violations)
        {:invariant :allium-rule-names-unique
         :violation :duplicate-rule-name
         :details   (vec violations)
         :severity  :error
         :message   "Two allium rules share a :rule/name — emitted into one module they would collide"})))})
