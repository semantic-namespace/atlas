(ns atlas.adapter.tilakone
  "Adapter: bidirectional transformer between tilakone FSMs and atlas workflows.

   tilakone FSM → atlas workflow registrations:
     (tilakone->atlas fsm-def opts)

   atlas workflow → tilakone FSM:
     (atlas->tilakone :wf/my-workflow)

   Roundtrip guarantee:
     (= (atlas->tilakone (register-atlas! (tilakone->atlas fsm opts) opts))
        (normalize-tilakone fsm))

   Conventions:
     - tilakone catch-all `_` maps to `:signal/any` in atlas
     - Terminal transitions (no ::tk/to) map to target `:workflow/exit`
     - Extra state keys (e.g. ::spec.workflow/log-level) are preserved
       in workflow-producer props under :tilakone/state-meta
     - Runtime keys (:context-uuid, :workflow-id, etc.) are preserved
       in workflow props under :tilakone/runtime-meta"
  (:require [atlas.registry :as registry]
            [atlas.registry.lookup :as entity]
            [clojure.set :as set]))

;; =============================================================================
;; CONSTANTS
;; =============================================================================

;; tilakone ns-qualified keys (we use keywords, not the tilakone.core vars,
;; so this adapter doesn't depend on tilakone at compile time)
(def ^:private tk-states :tilakone.core/states)
(def ^:private tk-state :tilakone.core/state)
(def ^:private tk-name :tilakone.core/name)
(def ^:private tk-transitions :tilakone.core/transitions)
(def ^:private tk-on :tilakone.core/on)
(def ^:private tk-to :tilakone.core/to)

;; tilakone catch-all: the symbol '_ or the tilakone.core/_ var value
;; In data literals, _ appears as the symbol '_
(defn- catch-all?
  [v]
  (or (= v '_)
      (= v 'ANY_SIGNAL)
      (and (symbol? v) (= (name v) "_"))))

;; Atlas signal for catch-all
(def catch-all-signal :signal/any)

;; Atlas target for terminal transitions
(def exit-target :workflow/exit)

;; Keys that are part of tilakone's own structure (not user metadata)
(def ^:private tilakone-structural-keys
  #{:tilakone.core/states :tilakone.core/state
    :tilakone.core/name :tilakone.core/transitions
    :tilakone.core/on :tilakone.core/to})

;; =============================================================================
;; TILAKONE → ATLAS
;; =============================================================================

(defn- state-name->producer-dev-id
  "Convert a tilakone state name keyword to a producer dev-id.
   Uses the provided name-fn or defaults to identity."
  [state-name name-fn]
  (if name-fn
    (name-fn state-name)
    state-name))

(defn- parse-transition
  "Parse a single tilakone transition map into [signal target].
   Converts _ to :signal/any, missing ::tk/to to :workflow/exit."
  [transition name-fn]
  (let [signal-raw (get transition tk-on)
        target-raw (get transition tk-to)
        signal (if (catch-all? signal-raw)
                 catch-all-signal
                 signal-raw)
        target (if target-raw
                 (state-name->producer-dev-id target-raw name-fn)
                 exit-target)]
    [signal target]))

(defn- parse-state
  "Parse a single tilakone state map into atlas data.
   Returns {:dev-id kw :transitions {signal target} :meta {...}}."
  [state-map name-fn]
  (let [state-name (get state-map tk-name)
        dev-id (state-name->producer-dev-id state-name name-fn)
        transitions-raw (get state-map tk-transitions [])
        transitions (into {} (map #(parse-transition % name-fn) transitions-raw))
        ;; Collect non-tilakone keys as state metadata
        meta-keys (remove tilakone-structural-keys (keys state-map))
        state-meta (when (seq meta-keys)
                     (select-keys state-map meta-keys))]
    (cond-> {:dev-id dev-id
             :state-name state-name
             :transitions transitions
             :signals (set (vals transitions))}
      state-meta (assoc :state-meta state-meta))))

(defn tilakone->atlas
  "Convert a tilakone FSM definition to atlas registration data.

   Returns a map:
     {:producers [{:dev-id :wp/x
                   :state-name ::original-name
                   :signals #{:signal/a :signal/b}
                   :state-meta {...}}]
      :workflow {:producers #{:wp/x :wp/y ...}
                 :initial-producer :wp/x
                 :transitions {:wp/x {:signal/a :wp/y ...}}
                 :runtime-meta {...}}}

   Options:
     :name-fn       - (fn [state-name] -> producer-dev-id), default: identity
     :workflow-id   - dev-id for the workflow entity
     :aspects       - aspects set for the workflow compound-id"
  ([fsm-def]
   (tilakone->atlas fsm-def {}))
  ([fsm-def {:keys [name-fn workflow-id aspects]}]
   (let [states (get fsm-def tk-states)
         initial-state (get fsm-def tk-state)
         parsed-states (mapv #(parse-state % name-fn) states)
         initial-dev-id (state-name->producer-dev-id initial-state name-fn)
         ;; Collect all producer dev-ids (including exit if referenced)
         all-producer-ids (set (map :dev-id parsed-states))
         ;; Build per-producer transition map (only for states that have transitions)
         transitions (into {}
                           (for [{:keys [dev-id transitions]} parsed-states
                                 :when (seq transitions)]
                             [dev-id transitions]))
         ;; Collect per-producer signals (what each producer can emit)
         producer-signals (into {}
                                (for [{:keys [dev-id transitions]} parsed-states
                                      :when (seq transitions)]
                                  [dev-id (set (keys transitions))]))
         ;; Runtime metadata (non-structural top-level keys)
         runtime-keys (remove tilakone-structural-keys (keys fsm-def))
         runtime-meta (when (seq runtime-keys)
                        (select-keys fsm-def runtime-keys))]
     {:producers (mapv (fn [{:keys [dev-id state-name state-meta]}]
                         (cond-> {:dev-id dev-id
                                  :state-name state-name
                                  :signals (get producer-signals dev-id #{})}
                           state-meta (assoc :state-meta state-meta)))
                       parsed-states)
      :workflow (cond-> {:producers all-producer-ids
                         :initial-producer initial-dev-id
                         :transitions transitions}
                  workflow-id (assoc :workflow-id workflow-id)
                  aspects (assoc :aspects aspects)
                  runtime-meta (assoc :runtime-meta runtime-meta))})))

;; =============================================================================
;; ATLAS → TILAKONE
;; =============================================================================

(defn- signal->tilakone
  "Convert an atlas signal keyword back to tilakone format.
   :signal/any → _ (catch-all symbol)"
  [signal]
  (if (= signal catch-all-signal)
    '_
    signal))

(defn- target->tilakone
  "Convert an atlas transition target back to tilakone state name.
   Uses the reverse name map if provided."
  [target reverse-name-map]
  (if (= target exit-target)
    nil
    (get reverse-name-map target target)))

(defn atlas->tilakone
  "Convert an atlas workflow back to a tilakone FSM definition.

   Reads the workflow and its producers from the registry.
   Returns a tilakone FSM map.

   Options:
     :reverse-name-fn - (fn [producer-dev-id] -> state-name), default: identity
     :include-runtime-meta? - include :tilakone/runtime-meta from workflow (default: true)"
  ([workflow-dev-id]
   (atlas->tilakone workflow-dev-id {}))
  ([workflow-dev-id {:keys [reverse-name-fn include-runtime-meta?]
                     :or {include-runtime-meta? true}}]
   (let [wf-props (entity/props-for workflow-dev-id)
         producers (:workflow/producers wf-props)
         initial (:workflow/initial-producer wf-props)
         transitions (:workflow/transitions wf-props)
         runtime-meta (:tilakone/runtime-meta wf-props)
         reverse-fn (or reverse-name-fn identity)
         ;; Build state maps
         states (mapv
                 (fn [producer-id]
                   (let [p-props (entity/props-for producer-id)
                         state-name (reverse-fn producer-id)
                         producer-transitions (get transitions producer-id {})
                         state-meta (:tilakone/state-meta p-props)
                         reverse-map (into {}
                                           (map (fn [p] [p (reverse-fn p)])
                                                producers))
                         built-transitions (mapv
                                            (fn [[signal target]]
                                              (let [tk-target (target->tilakone target reverse-map)]
                                                (cond-> {tk-on (signal->tilakone signal)}
                                                  tk-target (assoc tk-to tk-target))))
                                            producer-transitions)]
                     (cond-> {tk-name state-name}
                       (seq built-transitions) (assoc tk-transitions built-transitions)
                       state-meta (merge state-meta))))
                 ;; Sort producers to maintain stable order
                 (sort-by str producers))
         initial-state (reverse-fn initial)]
     (cond-> {tk-states states
              tk-state initial-state}
       (and include-runtime-meta? runtime-meta) (merge runtime-meta)))))

;; =============================================================================
;; REGISTRATION HELPERS
;; =============================================================================

(defn register-atlas!
  "Register the atlas data produced by tilakone->atlas into the registry.

   Options:
     :workflow-dev-id   - dev-id for the workflow (required)
     :workflow-aspects  - aspects set for the workflow compound-id (default #{})
     :producer-aspects-fn - (fn [producer-data] -> aspects), default: unique meta aspect
     :producer-output-fn  - (fn [producer-data] -> [keyword...]), default: []
     :ef-mapping        - map of {producer-dev-id -> execution-function-dev-id}

   Returns the workflow dev-id."
  [atlas-data {:keys [workflow-dev-id workflow-aspects
                      producer-aspects-fn producer-output-fn
                      ef-mapping]
               :or {workflow-aspects #{}
                    producer-aspects-fn (fn [{:keys [dev-id]}]
                                          ;; Default: use dev-id as meta aspect for uniqueness
                                          #{(keyword "meta" (str (namespace dev-id) "." (name dev-id)))})
                    producer-output-fn (constantly [])}}]
  (let [{:keys [producers workflow]} atlas-data]
    ;; Register workflow-producers
    (doseq [{:keys [dev-id signals state-meta] :as p-data} producers]
      (let [ef-id (get ef-mapping dev-id)]
        (registry/register!
         dev-id
         :atlas/workflow-producer
         (producer-aspects-fn p-data)
         (cond-> {:workflow-producer/signals (if (seq signals) signals #{:signal/any})
                  :workflow-producer/output (producer-output-fn p-data)}
           ef-id (assoc :workflow-producer/execution-function ef-id)
           state-meta (assoc :tilakone/state-meta state-meta)))))
    ;; Register workflow
    (let [runtime-meta (:runtime-meta workflow)]
      (registry/register!
       workflow-dev-id
       :atlas/workflow
       workflow-aspects
       (cond-> {:workflow/producers (:producers workflow)
                :workflow/initial-producer (:initial-producer workflow)
                :workflow/transitions (:transitions workflow)}
         runtime-meta (assoc :tilakone/runtime-meta runtime-meta))))
    workflow-dev-id))

(defn def-register-atlas
  "alias to register-atlas! but for returning only the workflow entity"
  [atlas-data {:keys [workflow-dev-id workflow-aspects]
               :or {workflow-aspects #{}}}]
  (let [{:keys [workflow]} atlas-data
        runtime-meta (:runtime-meta workflow)]
    [workflow-dev-id
     :atlas/workflow
     workflow-aspects
     (cond-> {:workflow/producers (:producers workflow)
              :workflow/initial-producer (:initial-producer workflow)
              :workflow/transitions (:transitions workflow)}
       runtime-meta (assoc :tilakone/runtime-meta runtime-meta))]))

;; =============================================================================
;; EXECUTION BRIDGE (migration helper)
;; =============================================================================

(defn exec
  "Build an :atlas/impl function that delegates to a tilakone-style integrant
   producer.

   dispatch-key: integrant dispatch value (e.g. :spec.workers.ob.fees/banks-accounts)

   Returns a (fn [deps context]) that:
     1. (ig/init-key dispatch-key deps) -> [_ producer-fn]
     2. (producer-fn context) -> [signal delay result]
     3. zipmap with [:workflow/signal :async/seconds-to-delay dispatch-key]

   Result map shape: {:workflow/signal :fsm/success
                      :async/seconds-to-delay 0
                      <dispatch-key> data}"
  [dispatch-key]
  (let [response-keys [:workflow/signal :async/seconds-to-delay dispatch-key]]
    (fn [deps context]
      (let [init-fn (requiring-resolve 'integrant.core/init-key)
            [_ producer-fn] (init-fn dispatch-key deps)
            result (producer-fn context)]
        (zipmap response-keys result)))))

;; =============================================================================
;; NORMALIZATION (for roundtrip comparison)
;; =============================================================================

(defn normalize-tilakone
  "Normalize a tilakone FSM for comparison.
   - Sorts states by name
   - Sorts transitions by signal
   - Strips runtime metadata (or keeps it, controlled by opts)"
  ([fsm-def]
   (normalize-tilakone fsm-def {}))
  ([fsm-def {:keys [keep-runtime?] :or {keep-runtime? true}}]
   (let [states (get fsm-def tk-states)
         normalized-states (->> states
                                (mapv (fn [state]
                                        (let [transitions (get state tk-transitions [])]
                                          (assoc state tk-transitions
                                                 (vec (sort-by #(str (get % tk-on)) transitions))))))
                                (sort-by #(str (get % tk-name)))
                                vec)
         base {tk-states normalized-states
               tk-state (get fsm-def tk-state)}
         runtime-keys (remove tilakone-structural-keys (keys fsm-def))]
     (if (and keep-runtime? (seq runtime-keys))
       (merge base (select-keys fsm-def runtime-keys))
       base))))
