(ns atlas.source-impl-key-test
  "An ontology's :ontology/impl-key: where atlas.source/load! puts an
   entity's annotated var, and that entities of that type need one."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [atlas.registry :as registry]
            [atlas.source :as source]))

(defn ^{:atlas.source/id :endpoint/get-order} get-order [_request] {:status 200})
(defn ^{:atlas.source/id :fn/price-order} price-order [_ctx _deps] {:order/total 1})

(defn- ontologies! []
  (registry/register! :atlas/execution-function :atlas/ontology #{:atlas/execution-function}
                      {:ontology/for :atlas/execution-function})
  (registry/register! :atlas/test-endpoint :atlas/ontology #{:atlas/test-endpoint}
                      {:ontology/for :atlas/test-endpoint :ontology/impl-key :endpoint/handler}))

(use-fixtures :each (fn [f] (registry/reset-all!) (source/forget-loads!) (ontologies!) (f)
                      (registry/reset-all!) (source/forget-loads!)))

(def ^:private entities
  {:endpoint/get-order {:atlas/type :atlas/test-endpoint :atlas/aspects #{:domain/orders :tier/api}}
   :fn/price-order {:atlas/type :atlas/execution-function :atlas/aspects #{:domain/orders :tier/service}}})

(defn- props [id] (get @registry/registry (get @registry/dev-id-index id)))

(deftest the-var-goes-under-the-ontologys-impl-key
  (source/load! [{:source "test" :entities entities}] '[atlas.source-impl-key-test])
  (testing "an endpoint's handler is its var, under :endpoint/handler"
    (is (= #'get-order (:endpoint/handler (props :endpoint/get-order))))
    (is (not (contains? (props :endpoint/get-order) :atlas/impl))))
  (testing "a type whose ontology declares none keeps :atlas/impl"
    (is (= #'price-order (:atlas/impl (props :fn/price-order))))))

(deftest a-type-with-an-impl-key-needs-a-var
  (is (= :endpoint/handler (source/impl-key :atlas/test-endpoint)))
  (is (= :atlas/impl (source/impl-key :atlas/execution-function)))
  (is (source/impl-required? :atlas/test-endpoint))
  (is (= [{:problem :entity-without-impl :dev-id :endpoint/get-order :type :atlas/test-endpoint}]
         (vec (source/linkage-problems (select-keys entities [:endpoint/get-order]) {})))))
