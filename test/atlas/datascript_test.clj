(ns atlas.datascript-test
  (:require [clojure.test :refer [deftest is use-fixtures testing]]
            [atlas.registry :as cid]
            [atlas.datalog :as sut]
            [atlas.ontology.execution-function :as ef]
            [atlas.ontology.interface-endpoint :as ie]
            [atlas.ontology.interface-protocol :as ip]))

(use-fixtures :each
  (fn [f]
    (reset! cid/registry {})
    (sut/reset-db-cache!)
    (require 'atlas.ontology.execution-function :reload)
    (require 'atlas.ontology.interface-endpoint :reload)
    (require 'atlas.ontology.interface-protocol :reload)
    (f)
    (reset! cid/registry {})
    (sut/reset-db-cache!)))

(defn- seed-registry! []
  ;; Endpoint exposing one context key
  (cid/register!

   :endpoint/orders

   :atlas/interface-endpoint

   #{ :tier/api}
   {:interface-endpoint/context [:ctx/order]})

  ;; Function that consumes the endpoint input and produces an output
  (cid/register!

   :fn/alpha

   :atlas/execution-function

   #{ :domain/alpha}
   {:interface-endpoint/context [:ctx/order]
    :interface-endpoint/response [:out/alpha]
    :execution-function/deps #{:endpoint/orders}})

  ;; Foundation function that should be filtered out by logic query
  (cid/register!

   :fn/beta

   :atlas/execution-function

   #{ :tier/foundation}
   {:interface-endpoint/context [:out/alpha]
    :interface-endpoint/response []
    :execution-function/deps #{:fn/alpha}}))

(deftest registry-facts-emit-relations
  (seed-registry!)
  (let [facts (set (sut/registry-facts))]
    (testing "core relations are emitted"
      (is (facts [:db/add :fn/alpha :entity/consumes :ctx/order]))
      (is (facts [:db/add :fn/alpha :entity/produces :out/alpha]))
      (is (facts [:db/add :fn/alpha :entity/depends :endpoint/orders])))
    (testing "endpoint context gets mapped"
      (is (facts [:db/add :endpoint/orders :endpoint-context :ctx/order])))))

(deftest has-aspect-evaluates-against-db
  (seed-registry!)
  (let [db (sut/create-db)]
    (is (true? (sut/has-aspect db :fn/alpha :domain/alpha)))
    (is (false? (sut/has-aspect db :fn/alpha :tier/foundation)))))

(deftest compile-logic-query-translates-dsl
  (let [dsl '[:find [:arg/dev ...]
              :where
              [:logic/has-aspect :arg/dev :atlas/execution-function]
              {:not [[:logic/has-aspect :arg/dev :tier/foundation]]}]
        compiled (sut/compile-logic-query dsl)]
    ;; Check structure - :logic/has-aspect expands into [?e :atlas/dev-id ?dev] [?e :entity/aspect ...]
    (is (vector? compiled) "compiled query is a vector")
    (is (= :find (first compiled)) "starts with :find")
    (is (= '[?dev ...] (second compiled)) "find spec is correct")
    (is (= :in (nth compiled 2)) "has :in")
    (is (= '$ (nth compiled 3)) "in clause is $")
    (is (= :where (nth compiled 4)) "has :where")
    ;; Check that where clauses contain the expected patterns
    (let [where-clauses (drop 5 compiled)]
      (is (>= (count where-clauses) 2) "has at least 2 where clauses")
      (is (some #(and (vector? %) (= :atlas/dev-id (second %)) (= '?dev (nth % 2))) where-clauses) "has dev-id binding")
      (is (some #(and (vector? %) (= :entity/aspect (second %)) (= :atlas/execution-function (nth % 2))) where-clauses) "has function aspect")
      (is (some list? where-clauses) "has not clause"))))

(deftest run-logic-query-filters-foundation-functions
  (seed-registry!)
  (let [dsl '[:find [:arg/dev ...]
              :where
              [:logic/has-aspect :arg/dev :atlas/execution-function]
              {:not [[:logic/has-aspect :arg/dev :tier/foundation]]}]
        results (sut/run-logic-query dsl)]
    (is (= [:fn/alpha] results) "only non-foundation functions match")))
