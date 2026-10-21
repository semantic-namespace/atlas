(ns atlas.invariant-rules-test
  "An invariant's :invariant/rule fails the same entities as its
   :invariant/fn. The fns run here, on fixtures.invariant-rules.entities;
   bb atlas-pack-smoke runs the rules on the same file."
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [atlas.registry :as registry]
   [atlas.datalog :as datalog]
   [fixtures.invariant-rules.entities :as fixtures]))

(def ^:private ontologies
  '[atlas.ontology.interface-endpoint atlas.ontology.structure-component
    atlas.ontology.execution-function atlas.ontology.decision atlas.ontology.value-proposition
    atlas.ontology.business-pattern atlas.ontology.system atlas.ontology.deployment
    atlas.ontology.allium-rule atlas.ontology.test-case atlas.ontology.shapeup
    atlas.ontology.llm-prompt atlas.ontology.workflow-producer atlas.ontology.workflow])

(use-fixtures :each
  (fn [f]
    (registry/reset-all!)
    (datalog/reset-all!)
    (doseq [o ontologies] (require o :reload))
    (fixtures/register-all!)
    (f)
    (registry/reset-all!)
    (datalog/reset-all!)))

(defn- invariant [id]
  (get @registry/registry (get @registry/dev-id-index id)))

(defn- type-of [id]
  (:atlas/type (get @registry/registry (get @registry/dev-id-index id))))

(defn- failing
  "The fixtures an invariant's fn reports, wherever in its result they are,
   of the types `expected` is: a result also names what a violation points
   at (a missing system, a component depended on)."
  [id expected]
  (let [types (set (map type-of expected))
        result ((:invariant/fn (invariant id)))]
    (set (filter #(and (= "fixture" (namespace %)) (types (type-of %)))
                 (filter keyword? (tree-seq coll? seq result))))))

(deftest fixtures-are-distinct-entities
  (is (empty? (filter #(and (= "fixture" (namespace %)) (nil? (type-of %)))
                      (keys @registry/dev-id-index)))
      "every fixture dev-id resolves to its own entity")
  (is (= (count (filter #(= "fixture" (namespace %)) (keys @registry/dev-id-index)))
         (count (set (keep #(when (= "fixture" (namespace %)) (get @registry/dev-id-index %))
                           (keys @registry/dev-id-index)))))
      "no two fixtures share a compound identity"))

(deftest each-rule-carrying-invariant-fails-its-failing-fixtures
  (doseq [[id expected] fixtures/failing]
    (testing id
      (is (map? (:invariant/rule (invariant id))) "carries a rule")
      (is (= expected (failing id expected))))))
