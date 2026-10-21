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
    atlas.ontology.decision atlas.ontology.value-proposition])

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

(defn- failing
  "The fixtures an invariant's fn reports, wherever in its result they are."
  [id]
  (let [fixture-ids (set (mapcat val fixtures/failing))
        all-fixtures (into fixture-ids (map #(keyword "fixture" (name %)))
                           [:endpoint-api :component-foundation :decision-retired-with-reason
                            :decision-active-realized :authority-revoked-with-reason :vp-bounded])
        result ((:invariant/fn (invariant id)))
        ids (set (filter all-fixtures (filter keyword? (tree-seq coll? seq result))))]
    ids))

(deftest each-rule-carrying-invariant-fails-its-failing-fixtures
  (doseq [[id expected] fixtures/failing]
    (testing id
      (is (map? (:invariant/rule (invariant id))) "carries a rule")
      (is (= expected (failing id))))))
