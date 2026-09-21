(ns grain-demo.counter-tests
  "Declarative test-cases for the counter app — one per declared outcome
   (the verdict alphabet IS the test matrix). Pure data: Given events, When
   command, Then expected events or anomaly. Executed by
   grain-demo.test-runner; serializable, so the suite travels in cloud
   snapshots."
  (:require [atlas.registry :as registry]
            [atlas.ontology.test-case]))

(def counter-a-id #uuid "00000000-0000-0000-0000-00000000000a")

(defn init-tests!
  "Register the counter test-cases (one per :grain/outcomes verdict).

   Identity carries :operation/* as well as :outcome/* — a verdict like
   :outcome/counter-not-found is shared by increment and decrement, so without
   the operation aspect the two test-cases would collapse into one compound
   identity (same lesson the commands learned: differ by aspect, not name).
   The :grain-outcome-coverage invariant checks every command verdict has a
   matching test-case."
  []

  ;; --- create-counter: #{:outcome/created :outcome/name-conflict} ---------

  (registry/register!
   :test.example/create-counter--created
   :atlas/test-case
   #{:domain/counter :operation/create :outcome/created}
   {:test-case/target :command.example/create-counter
    :test-case/fixture {:given-events []
                        :command {:command/name :example/create-counter
                                  :name "Counter A"}}
    :test-case/expectations [{:expectation/kind :result
                              :expectation/docs "fresh name → counter-created"
                              :expect/events [:example/counter-created]}]})

  (registry/register!
   :test.example/create-counter--name-conflict
   :atlas/test-case
   #{:domain/counter :operation/create :outcome/name-conflict}
   {:test-case/target :command.example/create-counter
    :test-case/fixture {:given-events [{:type :example/counter-created
                                        :body {:counter-id counter-a-id
                                               :name "Counter A"}}]
                        :command {:command/name :example/create-counter
                                  :name "Counter A"}}
    :test-case/expectations [{:expectation/kind :result
                              :expectation/docs "duplicate name → conflict anomaly"
                              :expect/anomaly :conflict}]})

  ;; --- increment-counter: #{:outcome/incremented :outcome/counter-not-found}

  (registry/register!
   :test.example/increment-counter--incremented
   :atlas/test-case
   #{:domain/counter :operation/increment :outcome/incremented}
   {:test-case/target :command.example/increment-counter
    :test-case/fixture {:given-events [{:type :example/counter-created
                                        :body {:counter-id counter-a-id
                                               :name "Counter A"}}]
                        :command {:command/name :example/increment-counter
                                  :counter-id counter-a-id}}
    :test-case/expectations [{:expectation/kind :result
                              :expectation/docs "existing counter → incremented"
                              :expect/events [:example/counter-incremented]}]})

  (registry/register!
   :test.example/increment-counter--not-found
   :atlas/test-case
   #{:domain/counter :operation/increment :outcome/counter-not-found}
   {:test-case/target :command.example/increment-counter
    :test-case/fixture {:given-events []
                        :command {:command/name :example/increment-counter
                                  :counter-id counter-a-id}}
    :test-case/expectations [{:expectation/kind :result
                              :expectation/docs "unknown counter → not-found anomaly"
                              :expect/anomaly :not-found}]})

  ;; --- decrement-counter: #{:outcome/decremented :outcome/counter-not-found} -

  (registry/register!
   :test.example/decrement-counter--decremented
   :atlas/test-case
   #{:domain/counter :operation/decrement :outcome/decremented}
   {:test-case/target :command.example/decrement-counter
    :test-case/fixture {:given-events [{:type :example/counter-created
                                        :body {:counter-id counter-a-id
                                               :name "Counter A"}}]
                        :command {:command/name :example/decrement-counter
                                  :counter-id counter-a-id}}
    :test-case/expectations [{:expectation/kind :result
                              :expectation/docs "existing counter → decremented"
                              :expect/events [:example/counter-decremented]}]})

  (registry/register!
   :test.example/decrement-counter--not-found
   :atlas/test-case
   #{:domain/counter :operation/decrement :outcome/counter-not-found}
   {:test-case/target :command.example/decrement-counter
    :test-case/fixture {:given-events []
                        :command {:command/name :example/decrement-counter
                                  :counter-id counter-a-id}}
    :test-case/expectations [{:expectation/kind :result
                              :expectation/docs "unknown counter → not-found anomaly"
                              :expect/anomaly :not-found}]})

  ;; --- calculate-average: #{:outcome/average-calculated :outcome/no-counters} -

  (registry/register!
   :test.example/calculate-average--average-calculated
   :atlas/test-case
   #{:domain/counter :operation/calculate :outcome/average-calculated}
   {:test-case/target :command.example/calculate-average-counter-value
    :test-case/fixture {:given-events [{:type :example/counter-created
                                        :body {:counter-id counter-a-id
                                               :name "Counter A"}}
                                       {:type :example/counter-incremented
                                        :body {:counter-id counter-a-id}}]
                        :command {:command/name :example/calculate-average-counter-value}}
    :test-case/expectations [{:expectation/kind :result
                              :expectation/docs "counters exist → average-calculated"
                              :expect/events [:example/average-calculated]}]})

  (registry/register!
   :test.example/calculate-average--no-counters
   :atlas/test-case
   #{:domain/counter :operation/calculate :outcome/no-counters}
   {:test-case/target :command.example/calculate-average-counter-value
    :test-case/fixture {:given-events []
                        :command {:command/name :example/calculate-average-counter-value}}
    :test-case/expectations [{:expectation/kind :result
                              :expectation/docs "no counters → no-counters anomaly"
                              :expect/anomaly :no-counters}]})

  :done)
