(ns atlas.meta-source-inline-impl-test
  "atlas's meta-source pack: a register! call with an inline fn is
   implemented by its own rows."
  (:require [clojure.test :refer [deftest is testing]]
            [atlas.meta-source]))

(def ^:private read-forms #'atlas.meta-source/read-forms)

(defn- read-one [form]
  (first (read-forms [{:row 3 :end-row 9 :name-row 3 :name-col 2}] {:form-at (constantly form)} {})))

(deftest an-inline-fn-is-implemented-by-the-call-rows
  (testing "an :execution-function/impl written inline gives the call's rows"
    (is (= {:row 3 :end-row 9}
           (:impl-range (read-one '(registry/register! :fn/load :atlas/execution-function #{:domain/x}
                                     {:execution-function/impl (fn [arg] (other arg))}))))))
  (testing "a #() is a fn too"
    (is (:impl-range (read-one '(registry/register! :fn/load :atlas/execution-function #{:domain/x}
                                  {:execution-function/impl (fn* [p] (other p))})))))
  (testing "an impl that is a var, or none, gives no rows: a var carrying the meta key links it"
    (is (nil? (:impl-range (read-one '(registry/register! :fn/load :atlas/execution-function #{:domain/x}
                                        {:execution-function/impl load-impl})))))
    (is (nil? (:impl-range (read-one '(registry/register! :data/x :atlas/data-schema #{:domain/x}
                                        {:data-schema/fields [:a/b]})))))))
