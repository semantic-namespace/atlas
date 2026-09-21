(ns grain-demo.todo-list
  "grain-todo-list's :todo service — Task lifecycle vertical slice, authored as
   Atlas semantic entities via atlas.ontology.grain.

   Mirrors the real grain defs (cjbarre.grain-todo-list.service.todo-list-service):
     defcommand   :todo capture-task     -> :command.todo/capture-task
     event        :todo/task-captured    -> :event.todo/task-captured
     defreadmodel :todo tasks            -> :read-model.todo/tasks
     defquery     :todo tasks-page       -> :query.todo/tasks-page

   Scope of THIS slice: the core Task state machine — capture / rename / complete /
   cancel / archive / reactivate — plus the tasks read-model and its two queries.
   (Project, WeeklyReview, due-within, ordering, assignment are follow-on slices.)

   Unlike the counter (a public demo), todo entities are owner-scoped, so the
   commands are :access/enforced. All commands are :effect/write; the read-model
   and queries are :effect/read. Verdict alphabets (:grain/outcomes) are modelled
   from the lifecycle + not-found guard; reconcile against the real handlers'
   anomalies when materialising.

   Load order note: requires the grain ontology (kinds + dataflow type-refs) and
   the execution-function / data-schema ontologies, exactly like grain_demo.counter."
  (:require
   [atlas.registry :as registry]
   [atlas.ontology :as ontology]
   [atlas.invariant :as invariant]
   [atlas.ontology.execution-function]
   [atlas.ontology.data-schema]
   [atlas.ontology.grain]))

(defn init-todo!
  "Register the Task-lifecycle slice of grain-todo-list's :todo service."
  []

  ;; ==========================================================================
  ;; EVENTS (grain: defschemas event-schemas)
  ;; ==========================================================================

  (registry/register!
   :event.todo/task-captured
   :atlas/data-schema
   #{:grain/event :domain/todo :operation/create}
   {:data-schema/fields [:task/user-id :task/id :task/title :task/status :task/order]})

  (registry/register!
   :event.todo/task-renamed
   :atlas/data-schema
   #{:grain/event :domain/todo :operation/rename}
   {:data-schema/fields [:task/user-id :task/id :task/title]})

  (registry/register!
   :event.todo/task-completed
   :atlas/data-schema
   #{:grain/event :domain/todo :operation/complete}
   {:data-schema/fields [:task/user-id :task/id]})

  (registry/register!
   :event.todo/task-canceled
   :atlas/data-schema
   #{:grain/event :domain/todo :operation/cancel}
   {:data-schema/fields [:task/user-id :task/id]})

  (registry/register!
   :event.todo/task-archived
   :atlas/data-schema
   #{:grain/event :domain/todo :operation/archive}
   {:data-schema/fields [:task/user-id :task/id]})

  (registry/register!
   :event.todo/task-reactivated
   :atlas/data-schema
   #{:grain/event :domain/todo :operation/reactivate}
   {:data-schema/fields [:task/user-id :task/id]})

  ;; ==========================================================================
  ;; COMMANDS (grain: defcommand :todo … — owner-scoped, hence :access/enforced)
  ;; ==========================================================================

  (registry/register!
   :command.todo/capture-task
   :atlas/execution-function
   #{:grain/command :domain/todo :effect/write :access/enforced :operation/create}
   {:execution-function/context [:task/title]
    :grain/produces #{:event.todo/task-captured}
    :execution-function/response [:event.todo/task-captured]
    :grain/outcomes #{:outcome/captured}})

  (registry/register!
   :command.todo/rename-task
   :atlas/execution-function
   #{:grain/command :domain/todo :effect/write :access/enforced :operation/rename}
   {:execution-function/context [:task/id :task/title]
    :grain/produces #{:event.todo/task-renamed}
    :execution-function/response [:event.todo/task-renamed]
    :grain/outcomes #{:outcome/renamed :outcome/task-not-found}})

  (registry/register!
   :command.todo/complete-task
   :atlas/execution-function
   #{:grain/command :domain/todo :effect/write :access/enforced :operation/complete}
   {:execution-function/context [:task/id]
    :grain/produces #{:event.todo/task-completed}
    :execution-function/response [:event.todo/task-completed]
    :grain/outcomes #{:outcome/completed :outcome/task-not-found}})

  (registry/register!
   :command.todo/cancel-task
   :atlas/execution-function
   #{:grain/command :domain/todo :effect/write :access/enforced :operation/cancel}
   {:execution-function/context [:task/id]
    :grain/produces #{:event.todo/task-canceled}
    :execution-function/response [:event.todo/task-canceled]
    :grain/outcomes #{:outcome/canceled :outcome/task-not-found}})

  (registry/register!
   :command.todo/archive-task
   :atlas/execution-function
   #{:grain/command :domain/todo :effect/write :access/enforced :operation/archive}
   {:execution-function/context [:task/id]
    :grain/produces #{:event.todo/task-archived}
    :execution-function/response [:event.todo/task-archived]
    :grain/outcomes #{:outcome/archived :outcome/task-not-found}})

  (registry/register!
   :command.todo/reactivate-task
   :atlas/execution-function
   #{:grain/command :domain/todo :effect/write :access/enforced :operation/reactivate}
   {:execution-function/context [:task/id]
    :grain/produces #{:event.todo/task-reactivated}
    :execution-function/response [:event.todo/task-reactivated]
    :grain/outcomes #{:outcome/reactivated :outcome/task-not-found}})

  ;; ==========================================================================
  ;; READ MODEL (grain: defreadmodel :todo tasks)
  ;; ==========================================================================

  (registry/register!
   :read-model.todo/tasks
   :atlas/execution-function
   #{:grain/read-model :domain/todo :effect/read}
   {:grain/consumes #{:event.todo/task-captured
                      :event.todo/task-renamed
                      :event.todo/task-completed
                      :event.todo/task-canceled
                      :event.todo/task-archived
                      :event.todo/task-reactivated}
    :grain/version 1
    :execution-function/context [:event.todo/task-captured
                                 :event.todo/task-renamed
                                 :event.todo/task-completed
                                 :event.todo/task-canceled
                                 :event.todo/task-archived
                                 :event.todo/task-reactivated]
    :execution-function/response [:task/id :task/title :task/status :task/order]})

  ;; ==========================================================================
  ;; QUERIES (grain: defquery :todo tasks-page / task-page)
  ;; ==========================================================================

  (registry/register!
   :query.todo/tasks-page
   :atlas/execution-function
   #{:grain/query :domain/todo :effect/read :access/enforced :operation/list}
   {:grain/reads #{:read-model.todo/tasks}
    :execution-function/deps [:read-model.todo/tasks]})

  (registry/register!
   :query.todo/task-page
   :atlas/execution-function
   #{:grain/query :domain/todo :effect/read :access/enforced :operation/lookup}
   {:execution-function/context [:task/id]
    :grain/reads #{:read-model.todo/tasks}
    :execution-function/deps [:read-model.todo/tasks]})

  (ontology/register-entity-types!)
  :done)

(comment
  (init-todo!)
  (invariant/report)
  (require '[atlas.query :as query])
  (query/find-by-aspect @registry/registry #{:domain/todo :effect/write})
  (reset! registry/registry {}))
