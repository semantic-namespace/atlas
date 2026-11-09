(ns atlas.source-test-case-impl-test
  "A test case from a registry file: its data in EDN, what isn't data in a
   var of no args carrying :atlas.source/id, under :test-case/impl."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.spec.alpha :as s]
            [atlas.registry :as registry]
            [atlas.source :as source]
            [atlas.ontology.test-case :as test-case]
            [atlas.test.runner :as runner]))

(s/def :demo/text string?)
(s/def :demo/error any?)
(s/def :demo/kind keyword?)

(defn ^{:atlas.source/id :fn/classify-error} classify-error
  [{:demo/keys [error]}]
  {:demo/kind (if (instance? IllegalStateException error) :state :other)})

(defn ^{:atlas.source/id :test/classify-error--state} state-error
  []
  {:test-case/fixture {:demo/error (IllegalStateException. "boom")}
   :test-case/expectations [{:expectation/kind :result
                             :expectation/pred #(= :state (:demo/kind %))
                             :expectation/docs "an IllegalStateException is a :state error"}]})

(defn ^{:atlas.source/id :test/classify-error--clash} clashing-fixture
  []
  {:test-case/fixture {:demo/text "from the var"}
   :test-case/expectations []})

(def ^:private entities
  {:fn/classify-error {:atlas/type :atlas/execution-function
                       :atlas/aspects #{:domain/demo :action/classify}
                       :execution-function/context [:demo/error]
                       :execution-function/response [:demo/kind]}
   :test/classify-error--state {:atlas/type :atlas/test-case
                                :atlas/aspects #{:test/demo :test/demo-state}
                                :test-case/target :fn/classify-error
                                :test-case/fixture {:demo/text "kept from the file"}}
   :test/classify-error--clash {:atlas/type :atlas/test-case
                                :atlas/aspects #{:test/demo :test/demo-clash}
                                :test-case/target :fn/classify-error
                                :test-case/fixture {:demo/text "from the file"}}})

(defn- fresh! []
  (registry/reset-all!)
  (source/forget-loads!)
  (doseq [n '[atlas.ontology atlas.ontology.execution-function atlas.ontology.test-case]]
    (require n :reload)))

(use-fixtures :each (fn [f] (fresh!) (f) (registry/reset-all!) (source/forget-loads!)))

(defn- props [id] (get @registry/registry (get @registry/dev-id-index id)))

(deftest the-var-goes-under-test-case-impl
  (source/load! [{:source "test" :entities entities}] '[atlas.source-test-case-impl-test])
  (is (= :test-case/impl (source/impl-key :atlas/test-case)))
  (is (= #'state-error (:test-case/impl (props :test/classify-error--state))))
  (testing "case-of merges the var's return into the case, fixtures by key"
    (let [c (test-case/case-of :test/classify-error--state)]
      (is (= :fn/classify-error (:test-case/target c)))
      (is (= "kept from the file" (get-in c [:test-case/fixture :demo/text])))
      (is (instance? IllegalStateException (get-in c [:test-case/fixture :demo/error])))
      (is (= 1 (count (:test-case/expectations c)))))))

(deftest the-runner-runs-a-case-from-a-registry-file
  (source/load! [{:source "test" :entities entities}] '[atlas.source-test-case-impl-test])
  (let [report (runner/run-test {:test/case-dev-id :test/classify-error--state})]
    (is (:test/pass? report) (pr-str report))
    (is (= 1 (:test/checks-passed report)))))

(deftest a-test-case-from-a-file-needs-its-var
  (is (source/impl-required? :atlas/test-case))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"disagree"
                        (source/load! [{:source "test"
                                        :entities (assoc entities :test/classify-error--orphan
                                                         {:atlas/type :atlas/test-case
                                                          :atlas/aspects #{:test/demo :test/demo-orphan}
                                                          :test-case/target :fn/classify-error})}]
                                      '[atlas.source-test-case-impl-test]))))

(deftest file-and-var-giving-one-fixture-key-two-values-is-a-conflict
  (source/load! [{:source "test" :entities entities}] '[atlas.source-test-case-impl-test])
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"disagree"
                        (test-case/case-of :test/classify-error--clash))))

(deftest a-test-case-registered-inline-is-unchanged
  (registry/register! :test/inline :atlas/test-case #{:test/demo :test/demo-inline}
                      {:test-case/target :fn/classify-error
                       :test-case/fixture {:demo/error :x}
                       :test-case/expectations []})
  (is (= {:demo/error :x} (:test-case/fixture (test-case/case-of :test/inline)))))
