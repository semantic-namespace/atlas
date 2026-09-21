(ns app.grain-counter
  "Grain's example counter app (components/example-service in ObneyAI/grain),
   authored as Atlas semantic entities via atlas.ontology.grain.

   Mirrors the grain registrations one-to-one, kind-prefixed:

     defcommand   :example create-counter   -> :command.example/create-counter
     defquery     :example counters         -> :query.example/counters
     defreadmodel :example counters         -> :read-model.example/counters
     defprocessor :example calculate-...    -> :processor.example/calculate-...
     defperiodic  :example example-...      -> :periodic.example/example-...
     event        :example/counter-created  -> :event.example/counter-created

   Expected invariant findings against this registry:
     - :grain-no-public-write (warning) — all four commands register
       {:authorized? (constantly true)} in grain, i.e. :access/public + :effect/write
     - :grain-orphan-events (warning) — :event.example/average-calculated is
       produced by the calculate-average command but consumed by nothing:
       no read model subscribes to it, no processor listens. Dead data flow
       in grain's own example app, invisible to grain's per-kind registries."
  (:require
   [atlas.registry :as registry]
   [atlas.ontology :as ontology]
   [atlas.invariant :as invariant]
   [atlas.ontology.execution-function]
   [atlas.ontology.data-schema]
   [atlas.ontology.grain]))

(defn init-registry!
  "Register the grain counter example app."
  []

  ;; ==========================================================================
  ;; EVENTS (grain: defschemas events)
  ;; ==========================================================================

  ;; Each event carries the :operation/* aspect of the fact it records — the
  ;; same aspect as its producing command. Without it all four events share
  ;; one compound identity and collapse into a single registry entry.

  (registry/register!
   :event.example/counter-created
   :atlas/data-schema
   #{:grain/event :domain/counter :operation/create}
   {:data-schema/fields [:counter/id :counter/name]})

  (registry/register!
   :event.example/counter-incremented
   :atlas/data-schema
   #{:grain/event :domain/counter :operation/increment}
   {:data-schema/fields [:counter/id]})

  (registry/register!
   :event.example/counter-decremented
   :atlas/data-schema
   #{:grain/event :domain/counter :operation/decrement}
   {:data-schema/fields [:counter/id]})

  (registry/register!
   :event.example/average-calculated
   :atlas/data-schema
   #{:grain/event :domain/counter :operation/calculate}
   {:data-schema/fields [:average/value]})

  ;; ==========================================================================
  ;; COMMANDS (grain: defcommand — all registered with
  ;; {:authorized? (constantly true)}, hence :access/public)
  ;; ==========================================================================

  (registry/register!
   :command.example/create-counter
   :atlas/execution-function
   #{:grain/command :domain/counter :effect/write :access/public :operation/create}
   {:execution-function/context [:counter/name]
    :grain/produces #{:event.example/counter-created}
    :execution-function/response [:event.example/counter-created]
    :grain/outcomes #{:outcome/created :outcome/name-conflict}})

  (registry/register!
   :command.example/increment-counter
   :atlas/execution-function
   ;; :operation/increment (not a shared :operation/update): increment and
   ;; decrement would otherwise carry identical compound identities — atlas
   ;; requires semantically distinct entities to differ by aspect, not name.
   #{:grain/command :domain/counter :effect/write :access/public :operation/increment}
   {:execution-function/context [:counter/id]
    :grain/produces #{:event.example/counter-incremented}
    :execution-function/response [:event.example/counter-incremented]
    :grain/outcomes #{:outcome/incremented :outcome/counter-not-found}})

  (registry/register!
   :command.example/decrement-counter
   :atlas/execution-function
   #{:grain/command :domain/counter :effect/write :access/public :operation/decrement}
   {:execution-function/context [:counter/id]
    :grain/produces #{:event.example/counter-decremented}
    :execution-function/response [:event.example/counter-decremented]
    :grain/outcomes #{:outcome/decremented :outcome/counter-not-found}})

  (registry/register!
   :command.example/calculate-average-counter-value
   :atlas/execution-function
   #{:grain/command :domain/counter :effect/write :access/public :operation/calculate}
   {:grain/produces #{:event.example/average-calculated}
    :execution-function/response [:event.example/average-calculated]
    :grain/outcomes #{:outcome/average-calculated :outcome/no-counters}})

  ;; A deprecated command exercising the terminal-state invariant (passes —
  ;; reason present). Not part of the upstream grain example.
  (registry/register!
   :command.example/reset-counter
   :atlas/execution-function
   #{:grain/command :domain/counter :effect/write :access/enforced
     :operation/update :status/deprecated}
   {:grain/deprecated-reason
    "Removed upstream: replaced by explicit decrement; kept here to exercise the deprecation invariant."})

  ;; ==========================================================================
  ;; READ MODEL (grain: defreadmodel :example counters)
  ;; ==========================================================================

  (registry/register!
   :read-model.example/counters
   :atlas/execution-function
   #{:grain/read-model :domain/counter :effect/read}
   {:grain/consumes #{:event.example/counter-created
                      :event.example/counter-incremented
                      :event.example/counter-decremented}
    :grain/version 1
    :execution-function/context [:event.example/counter-created
                                 :event.example/counter-incremented
                                 :event.example/counter-decremented]
    :execution-function/response [:counter/id :counter/name :counter/value]})

  ;; ==========================================================================
  ;; QUERIES (grain: defquery)
  ;; ==========================================================================

  (registry/register!
   :query.example/counters
   :atlas/execution-function
   #{:grain/query :domain/counter :effect/read :access/public :operation/list}
   {:grain/reads #{:read-model.example/counters}
    :execution-function/deps [:read-model.example/counters]})

  (registry/register!
   :query.example/counter
   :atlas/execution-function
   #{:grain/query :domain/counter :effect/read :access/public :operation/lookup}
   {:execution-function/context [:counter/id]
    :grain/reads #{:read-model.example/counters}
    :execution-function/deps [:read-model.example/counters]})

  ;; ==========================================================================
  ;; TODO PROCESSOR (grain: defprocessor — subscribes via :topics, dispatches
  ;; the calculate-average command)
  ;; ==========================================================================

  (registry/register!
   :processor.example/calculate-average-counter-value
   :atlas/execution-function
   #{:grain/todo-processor :domain/counter :temporal/async}
   {:grain/consumes #{:event.example/counter-incremented
                      :event.example/counter-decremented}
    :grain/dispatches #{:command.example/calculate-average-counter-value}
    :execution-function/deps [:command.example/calculate-average-counter-value
                              :read-model.example/counters]
    :execution-function/context [:event.example/counter-incremented
                                 :event.example/counter-decremented]})

  ;; ==========================================================================
  ;; PERIODIC TASK (grain: defperiodic — 30s heartbeat, no-op)
  ;; ==========================================================================

  (registry/register!
   :periodic.example/example-periodic-task
   :atlas/execution-function
   #{:grain/periodic-task :domain/counter :temporal/async}
   {:grain/schedule {:every 30 :duration :seconds}})

  (ontology/register-entity-types!)
  :done)

(comment
  (init-registry!)

  ;; The report grain cannot produce:
  (invariant/report)

  ;; Cross-kind semantic queries:
  (require '[atlas.query :as query])
  (query/find-by-aspect @registry/registry :grain/command)
  (query/find-by-aspect @registry/registry #{:domain/counter :effect/write})

  ;; Consumers of an event — the join grain's five registries cannot do:
  (query/where @registry/registry
               (fn [_ props]
                 (contains? (or (:grain/consumes props) #{})
                            :event.example/counter-incremented)))
  ;; => :processor.example/calculate-average-counter-value
  ;;    :read-model.example/counters

  (reset! registry/registry {}))
