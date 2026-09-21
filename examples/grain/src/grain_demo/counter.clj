(ns grain-demo.counter
  "Grain's example counter app, authored as Atlas semantic entities — the
   executable twin of test/app/grain_counter.clj in the atlas repo.

   The atlas registration is the single defining act. Each entity carries:
     - compound identity  (kind, domain, operation, effect, access policy)
     - data-flow          (:grain/produces / :grain/consumes / :grain/dispatches / :grain/reads)
     - payload schema     (:grain/schema — malli, as grain expects)
     - implementation     (:atlas/impl — the handler/reducer fn)

   atlas.adapter.grain.materialize then projects these into grain's own
   registries (register-command!, register-query!, ...). Grain's defcommand /
   defquery / defreadmodel macros are never used — grain becomes the
   execution substrate, atlas the definition layer.

   Handler bodies are ports of components/example-service in ObneyAI/grain."
  (:require
   [atlas.registry :as registry]
   [atlas.ontology :as ontology]
   [atlas.ontology.execution-function]
   [atlas.ontology.data-schema]
   [atlas.ontology.grain]
   [ai.obney.grain.event-store-v3.interface :refer [->event]]
   [ai.obney.grain.read-model-processor-v2.interface :as rmp]
   [ai.obney.grain.command-processor-v2.interface :as cp]
   [ai.obney.grain.time.interface :as time]
   [cognitect.anomalies :as anom]))

(defn- counters-root
  "Project the counters read model (grain name :example/counters)."
  [context]
  (rmp/project context :example/counters))

;; =============================================================================
;; IMPLEMENTATIONS (ports of example-service core namespaces)
;; =============================================================================

(defn create-counter
  "Creates a new counter. Counter name must be unique."
  [context]
  (let [counter-name (get-in context [:command :name])
        counter-id (random-uuid)
        unique-counter-names (->> (counters-root context)
                                  vals
                                  (map :counter/name)
                                  set)]
    (if (contains? unique-counter-names counter-name)
      {::anom/category ::anom/conflict
       ::anom/message (format "Counter with name '%s' already exists." counter-name)}
      {:command-result/events
       [(->event {:type :example/counter-created
                  :tags #{[:counter counter-id]}
                  :body {:counter-id counter-id
                         :name counter-name}})]
       :command/result {:counter-id counter-id}})))

(defn increment-counter
  "Increments an existing counter by 1."
  [{{:keys [counter-id]} :command :as context}]
  (if (get (counters-root context) counter-id)
    {:command-result/events
     [(->event {:type :example/counter-incremented
                :tags #{[:counter counter-id]}
                :body {:counter-id counter-id}})]}
    {::anom/category ::anom/not-found
     ::anom/message (format "Counter with ID '%s' not found." counter-id)}))

(defn decrement-counter
  "Decrements an existing counter by 1."
  [{{:keys [counter-id]} :command :as context}]
  (if (get (counters-root context) counter-id)
    {:command-result/events
     [(->event {:type :example/counter-decremented
                :tags #{[:counter counter-id]}
                :body {:counter-id counter-id}})]}
    {::anom/category ::anom/not-found
     ::anom/message (format "Counter with ID '%s' not found." counter-id)}))

(defn calculate-average-counter-value
  "Calculates the average value of all initialized counters."
  [context]
  (let [state (->> (counters-root context)
                   (filter (fn [[_ v]] (:counter/value v)))
                   (into {}))]
    (if (empty? state)
      {}
      {:command-result/events
       [(->event
         {:type :example/average-calculated
          :body {:value (/ (double (->> state
                                        vals
                                        (map :counter/value)
                                        (reduce + 0)))
                           (double (count state)))}})]})))

(defn counters-reducer
  "Read model reducer: (state, event) -> state."
  [state event]
  (case (:event/type event)
    :example/counter-created
    (assoc state (:counter-id event)
           {:counter/id (:counter-id event)
            :counter/name (:name event)})

    :example/counter-incremented
    (update state (:counter-id event) update :counter/value (fnil inc 0))

    :example/counter-decremented
    (update state (:counter-id event) update :counter/value (fnil dec 0))

    state))

(defn counters-query
  "Returns all counters."
  [context]
  {:query/result (vals (counters-root context))})

(defn counter-query
  "Returns a single counter by id."
  [{{:keys [counter-id]} :query :as context}]
  (if-let [counter (get (counters-root context) counter-id)]
    {:query/result counter}
    {::anom/category ::anom/not-found
     ::anom/message (format "Counter with ID '%s' not found." counter-id)}))

(defn recalculate-average-processor
  "Recomputes the average counter value whenever a counter changes."
  [context]
  (cp/process-command
   (assoc context
          :command {:command/id (random-uuid)
                    :command/timestamp (time/now)
                    :command/name :example/calculate-average-counter-value}))
  {})

(defn heartbeat-periodic
  "Example periodic task. Runs every 30s per tenant; no-op heartbeat."
  [tenant-id _time]
  (println "[periodic] heartbeat for tenant" tenant-id)
  {})

;; =============================================================================
;; ATLAS REGISTRATIONS — the single defining act
;; =============================================================================

(defn init-registry!
  "Register the counter app as atlas entities (semantics + schema + impl)."
  []

  ;; --- EVENTS ---------------------------------------------------------------
  ;; Each event carries the :operation/* of the fact it records — the same
  ;; aspect as its producing command (identical aspect sets would otherwise
  ;; collapse into one compound identity).

  (registry/register!
   :event.example/counter-created
   :atlas/data-schema
   #{:grain/event :domain/counter :operation/create}
   {:data-schema/fields [:counter/id :counter/name]
    :grain/schema [:map [:counter-id :uuid] [:name :string]]})

  (registry/register!
   :event.example/counter-incremented
   :atlas/data-schema
   #{:grain/event :domain/counter :operation/increment}
   {:data-schema/fields [:counter/id]
    :grain/schema [:map [:counter-id :uuid]]})

  (registry/register!
   :event.example/counter-decremented
   :atlas/data-schema
   #{:grain/event :domain/counter :operation/decrement}
   {:data-schema/fields [:counter/id]
    :grain/schema [:map [:counter-id :uuid]]})

  (registry/register!
   :event.example/average-calculated
   :atlas/data-schema
   #{:grain/event :domain/counter :operation/calculate}
   {:data-schema/fields [:average/value]
    :grain/schema [:map [:value :double]]})

  ;; --- COMMANDS ---------------------------------------------------------------
  ;; Upstream grain registers all of these with {:authorized? (constantly true)}
  ;; — semantically :access/public. The :grain-no-public-write invariant flags
  ;; every one of them.

  (registry/register!
   :command.example/create-counter
   :atlas/execution-function
   #{:grain/command :domain/counter :effect/write :access/public :operation/create}
   {:execution-function/context [:counter/name]
    :grain/produces #{:event.example/counter-created}
    :execution-function/response [:event.example/counter-created]
    :grain/outcomes #{:outcome/created :outcome/name-conflict}
    :grain/schema [:map [:name :string]]
    :atlas/impl create-counter})

  (registry/register!
   :command.example/increment-counter
   :atlas/execution-function
   #{:grain/command :domain/counter :effect/write :access/public :operation/increment}
   {:execution-function/context [:counter/id]
    :grain/produces #{:event.example/counter-incremented}
    :execution-function/response [:event.example/counter-incremented]
    :grain/outcomes #{:outcome/incremented :outcome/counter-not-found}
    :grain/schema [:map [:counter-id :uuid]]
    :atlas/impl increment-counter})

  (registry/register!
   :command.example/decrement-counter
   :atlas/execution-function
   #{:grain/command :domain/counter :effect/write :access/public :operation/decrement}
   {:execution-function/context [:counter/id]
    :grain/produces #{:event.example/counter-decremented}
    :execution-function/response [:event.example/counter-decremented]
    :grain/outcomes #{:outcome/decremented :outcome/counter-not-found}
    :grain/schema [:map [:counter-id :uuid]]
    :atlas/impl decrement-counter})

  (registry/register!
   :command.example/calculate-average-counter-value
   :atlas/execution-function
   #{:grain/command :domain/counter :effect/write :access/public :operation/calculate}
   {:grain/produces #{:event.example/average-calculated}
    :execution-function/response [:event.example/average-calculated]
    :grain/outcomes #{:outcome/average-calculated :outcome/no-counters}
    :grain/schema [:map]
    :atlas/impl calculate-average-counter-value})

  ;; Deprecated — the materializer must NOT register this in grain.
  ;; Deprecation is not documentation here; it has operational meaning.
  (registry/register!
   :command.example/reset-counter
   :atlas/execution-function
   #{:grain/command :domain/counter :effect/write :access/enforced
     :operation/update :status/deprecated}
   {:grain/deprecated-reason
    "Removed upstream: replaced by explicit decrement; kept to exercise deprecation semantics."})

  ;; --- READ MODEL -------------------------------------------------------------

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
    :execution-function/response [:counter/id :counter/name :counter/value]
    :atlas/impl counters-reducer})

  ;; --- QUERIES ----------------------------------------------------------------

  (registry/register!
   :query.example/counters
   :atlas/execution-function
   #{:grain/query :domain/counter :effect/read :access/public :operation/list}
   {:grain/reads #{:read-model.example/counters}
    :execution-function/deps [:read-model.example/counters]
    :grain/schema [:map]
    :atlas/impl counters-query})

  (registry/register!
   :query.example/counter
   :atlas/execution-function
   #{:grain/query :domain/counter :effect/read :access/public :operation/lookup}
   {:execution-function/context [:counter/id]
    :grain/reads #{:read-model.example/counters}
    :execution-function/deps [:read-model.example/counters]
    :grain/schema [:map [:counter-id :uuid]]
    :atlas/impl counter-query})

  ;; --- TODO PROCESSOR ---------------------------------------------------------

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
                                 :event.example/counter-decremented]
    :atlas/impl recalculate-average-processor})

  ;; --- PERIODIC TASK ----------------------------------------------------------

  (registry/register!
   :periodic.example/example-periodic-task
   :atlas/execution-function
   #{:grain/periodic-task :domain/counter :temporal/async}
   {:grain/schedule {:every 30 :duration :seconds}
    :atlas/impl heartbeat-periodic})

  (ontology/register-entity-types!)
  :done)
