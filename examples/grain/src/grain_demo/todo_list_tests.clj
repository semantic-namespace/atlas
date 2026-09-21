(ns grain-demo.todo-list-tests
  "Declarative test-cases for the grain-todo-list :todo Task-lifecycle slice —
   one per declared :grain/outcomes verdict (the verdict alphabet IS the test
   matrix). Pure data: Given events, When command, Then expected events or
   anomaly. Serializable, so the suite travels in cloud snapshots.

   Identity carries :operation/* as well as :outcome/* — :outcome/task-not-found
   is shared by rename/complete/cancel/archive/reactivate, so without the
   operation aspect those cases would collapse into one compound identity (the
   same lesson the commands and the counter test-cases learned)."
  (:require [atlas.registry :as registry]
            [atlas.ontology.test-case]))

(def task-a-id #uuid "00000000-0000-0000-0000-0000000000a1")

(defn- captured-event []
  {:type :todo/task-captured
   :body {:user-id #uuid "00000000-0000-0000-0000-0000000000f0"
          :task-id task-a-id :title "Task A" :status :active :order 1000}})

(defn- completed-event []
  {:type :todo/task-completed
   :body {:user-id #uuid "00000000-0000-0000-0000-0000000000f0" :task-id task-a-id}})

(defn init-todo-tests!
  "Register the Task-lifecycle test-cases (one per command verdict)."
  []

  ;; --- capture-task: #{:outcome/captured} ---------------------------------
  (registry/register!
   :test.todo/capture-task--captured
   :atlas/test-case
   #{:domain/todo :operation/create :outcome/captured}
   {:test-case/target :command.todo/capture-task
    :test-case/fixture {:given-events []
                        :command {:command/name :todo/capture-task :title "Task A"}}
    :test-case/expectations [{:expectation/kind :result
                              :expectation/docs "fresh capture → task-captured"
                              :expect/events [:todo/task-captured]}]})

  ;; --- rename-task: #{:outcome/renamed :outcome/task-not-found} ------------
  (registry/register!
   :test.todo/rename-task--renamed
   :atlas/test-case
   #{:domain/todo :operation/rename :outcome/renamed}
   {:test-case/target :command.todo/rename-task
    :test-case/fixture {:given-events [(captured-event)]
                        :command {:command/name :todo/rename-task
                                  :task-id task-a-id :title "Task A2"}}
    :test-case/expectations [{:expectation/kind :result
                              :expectation/docs "existing task → task-renamed"
                              :expect/events [:todo/task-renamed]}]})

  (registry/register!
   :test.todo/rename-task--not-found
   :atlas/test-case
   #{:domain/todo :operation/rename :outcome/task-not-found}
   {:test-case/target :command.todo/rename-task
    :test-case/fixture {:given-events []
                        :command {:command/name :todo/rename-task
                                  :task-id task-a-id :title "X"}}
    :test-case/expectations [{:expectation/kind :result
                              :expectation/docs "unknown task → not-found anomaly"
                              :expect/anomaly :not-found}]})

  ;; --- complete-task: #{:outcome/completed :outcome/task-not-found} --------
  (registry/register!
   :test.todo/complete-task--completed
   :atlas/test-case
   #{:domain/todo :operation/complete :outcome/completed}
   {:test-case/target :command.todo/complete-task
    :test-case/fixture {:given-events [(captured-event)]
                        :command {:command/name :todo/complete-task :task-id task-a-id}}
    :test-case/expectations [{:expectation/kind :result
                              :expectation/docs "active task → task-completed"
                              :expect/events [:todo/task-completed]}]})

  (registry/register!
   :test.todo/complete-task--not-found
   :atlas/test-case
   #{:domain/todo :operation/complete :outcome/task-not-found}
   {:test-case/target :command.todo/complete-task
    :test-case/fixture {:given-events []
                        :command {:command/name :todo/complete-task :task-id task-a-id}}
    :test-case/expectations [{:expectation/kind :result
                              :expectation/docs "unknown task → not-found anomaly"
                              :expect/anomaly :not-found}]})

  ;; --- cancel-task: #{:outcome/canceled :outcome/task-not-found} -----------
  (registry/register!
   :test.todo/cancel-task--canceled
   :atlas/test-case
   #{:domain/todo :operation/cancel :outcome/canceled}
   {:test-case/target :command.todo/cancel-task
    :test-case/fixture {:given-events [(captured-event)]
                        :command {:command/name :todo/cancel-task :task-id task-a-id}}
    :test-case/expectations [{:expectation/kind :result
                              :expectation/docs "active task → task-canceled"
                              :expect/events [:todo/task-canceled]}]})

  (registry/register!
   :test.todo/cancel-task--not-found
   :atlas/test-case
   #{:domain/todo :operation/cancel :outcome/task-not-found}
   {:test-case/target :command.todo/cancel-task
    :test-case/fixture {:given-events []
                        :command {:command/name :todo/cancel-task :task-id task-a-id}}
    :test-case/expectations [{:expectation/kind :result
                              :expectation/docs "unknown task → not-found anomaly"
                              :expect/anomaly :not-found}]})

  ;; --- archive-task: #{:outcome/archived :outcome/task-not-found} ----------
  ;; lifecycle: completed → archived, so the given history must complete first.
  (registry/register!
   :test.todo/archive-task--archived
   :atlas/test-case
   #{:domain/todo :operation/archive :outcome/archived}
   {:test-case/target :command.todo/archive-task
    :test-case/fixture {:given-events [(captured-event) (completed-event)]
                        :command {:command/name :todo/archive-task :task-id task-a-id}}
    :test-case/expectations [{:expectation/kind :result
                              :expectation/docs "completed task → task-archived"
                              :expect/events [:todo/task-archived]}]})

  (registry/register!
   :test.todo/archive-task--not-found
   :atlas/test-case
   #{:domain/todo :operation/archive :outcome/task-not-found}
   {:test-case/target :command.todo/archive-task
    :test-case/fixture {:given-events []
                        :command {:command/name :todo/archive-task :task-id task-a-id}}
    :test-case/expectations [{:expectation/kind :result
                              :expectation/docs "unknown task → not-found anomaly"
                              :expect/anomaly :not-found}]})

  ;; --- reactivate-task: #{:outcome/reactivated :outcome/task-not-found} ----
  ;; lifecycle: completed → active, so the given history must complete first.
  (registry/register!
   :test.todo/reactivate-task--reactivated
   :atlas/test-case
   #{:domain/todo :operation/reactivate :outcome/reactivated}
   {:test-case/target :command.todo/reactivate-task
    :test-case/fixture {:given-events [(captured-event) (completed-event)]
                        :command {:command/name :todo/reactivate-task :task-id task-a-id}}
    :test-case/expectations [{:expectation/kind :result
                              :expectation/docs "completed task → task-reactivated"
                              :expect/events [:todo/task-reactivated]}]})

  (registry/register!
   :test.todo/reactivate-task--not-found
   :atlas/test-case
   #{:domain/todo :operation/reactivate :outcome/task-not-found}
   {:test-case/target :command.todo/reactivate-task
    :test-case/fixture {:given-events []
                        :command {:command/name :todo/reactivate-task :task-id task-a-id}}
    :test-case/expectations [{:expectation/kind :result
                              :expectation/docs "unknown task → not-found anomaly"
                              :expect/anomaly :not-found}]})

  :done)
