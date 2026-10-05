(ns atlas.review.risk-test
  (:require [clojure.test :refer [deftest is]]
            [atlas.registry :as registry]
            [atlas.review.risk :as risk]))

(defn- with-entities [f]
  (let [saved-log @registry/registrations saved-reg @registry/registry saved-idx @registry/dev-id-index]
    (try
      (registry/register! :fn.t/core :atlas/execution-function #{:services/mail :domain/t}
                          {:execution-function/context [] :execution-function/response [:t/out] :execution-function/deps #{}})
      (registry/register! :endpoint.t/api :atlas/yorba-endpoint #{:domain/t :endpoint/t}
                          {:endpoint/deps [:fn.t/core]})
      (registry/register! :fn.t/leaf :atlas/execution-function #{:domain/t :leaf/t}
                          {:execution-function/context [] :execution-function/response [] :execution-function/deps #{}})
      (registry/register! :test/t-core :atlas/test-case #{:test/t :domain/t} {:test-case/target :fn.t/core})
      (f)
      (finally (reset! registry/registrations saved-log) (reset! registry/registry saved-reg) (reset! registry/dev-id-index saved-idx)))))

(deftest risk-is-ranked-with-its-reasons
  (with-entities
    (fn []
      (let [core (risk/assess :fn.t/core :contract)
            leaf (risk/assess :fn.t/leaf :code)]
        (is (some #(re-find #"contract changed" %) (:reasons core)))
        (is (some #(re-find #"talks to mail" %) (:reasons core)))
        (is (not-any? #(re-find #"no test case" %) (:reasons core)) "a test case targets it")
        (is (some #(re-find #"no test case" %) (:reasons leaf)))
        (is (> (:score core) (:score leaf)))
        (is (#{:low :medium :high} (:level leaf)))))))
