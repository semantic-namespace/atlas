(ns atlas.adapter.grain
  "Materialize grain-kind atlas entities into grain's live registries.

   Atlas is the defining layer; grain's registration functions (all public,
   all plain data + fn) are the projection target. Grain's defcommand /
   defquery / defreadmodel / defprocessor / defperiodic macros are bypassed
   entirely — kind is data (a :grain/* aspect), not a macro name.

   Mapping (kind aspect → grain registry):
     :grain/command        → command-processor-v2   register-command!
     :grain/query          → query-processor        register-query!
     :grain/read-model     → read-model-processor-v2 register-read-model!
     :grain/todo-processor → todo-processor-v2      register-processor!
     :grain/periodic-task  → periodic-task          register-periodic-trigger!
     :grain/event          → schema-util            register! (malli)

   Dev-id → grain name strips the kind prefix:
     :command.example/create-counter → :example/create-counter

   Semantics with operational meaning:
     - :status/deprecated entities are NOT materialized.
     - :access/public   → {:authorized? (constantly true)}
       :access/enforced → the entity's :grain/authorized? fn, else deny.
       (grain is deny-by-default; so is this projection.)

   verify! closes the loop: reads grain's own code-agent-tools catalog back
   and diffs it against what atlas declared."
  (:require
   [atlas.registry :as registry]
   [atlas.registry.serialise :as serialise]
   [atlas.ontology :as ontology]
   [ai.obney.grain.command-processor-v2.interface :as cp]
   [ai.obney.grain.query-processor.interface :as qp]
   [ai.obney.grain.read-model-processor-v2.interface :as rmp]
   [ai.obney.grain.todo-processor-v2.interface :as tp]
   [ai.obney.grain.periodic-task.interface :as pt]
   [ai.obney.grain.schema-util.interface :as schema-util]
   [ai.obney.grain.code-agent-tools.interface :as agent-tools]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as str]))

(defn grain-name
  "Strip the kind prefix from an atlas dev-id to recover the grain name.
   :command.example/create-counter → :example/create-counter"
  [dev-id]
  (let [ns' (namespace dev-id)
        i (str/index-of ns' ".")]
    (keyword (if i (subs ns' (inc i)) ns') (name dev-id))))

(defn- authorized-fn
  [compound-id props]
  (cond
    (contains? compound-id :access/public) (constantly true)
    (contains? compound-id :access/enforced) (or (:grain/authorized? props)
                                                 (constantly false))
    ;; No access policy declared — deny, like grain itself.
    :else (constantly false)))

(defn- register-schema!
  [gname props]
  (when-let [schema (:grain/schema props)]
    (schema-util/register! {gname schema})))

;; Runtime observations, recorded by the wrapper materialize! installs
;; around every command impl: which event types each command actually
;; emitted, which anomaly categories it actually returned. The ground truth
;; that observed-vs-declared audits declarations against.
(def observations* (atom {}))

(defn reset-observations! [] (reset! observations* {}))

(defn- record-observation! [gname result]
  (let [event-types (set (map :event/type (:command-result/events result)))
        anomaly (some-> (:cognitect.anomalies/category result) name keyword)]
    (swap! observations* update gname
           (fn [obs]
             (-> (or obs {:invocations 0 :events #{} :anomalies #{}})
                 (update :invocations inc)
                 (update :events into event-types)
                 (update :anomalies (fnil conj #{}) anomaly)
                 (update :anomalies disj nil))))))

(defn- observing [gname impl]
  (fn [context]
    (let [result (impl context)]
      (record-observation! gname result)
      result)))

(defn materialize!
  "Walk the atlas registry and register every live grain-kind entity in the
   corresponding grain registry. Returns a summary of what was projected
   and what was skipped.

   Command impls are wrapped with an observation recorder feeding
   observed-vs-declared — the event store's ground truth against the
   registry's declarations."
  []
  (let [summary (atom {:skipped-deprecated []})]
    (doseq [[compound-id props] @registry/registry
            :let [dev-id (:atlas/dev-id props)
                  impl (:atlas/impl props)]
            :when dev-id]
      (if (contains? compound-id :status/deprecated)
        (when (some #(= "grain" (namespace %)) compound-id)
          (swap! summary update :skipped-deprecated conj dev-id))
        (let [gname (grain-name dev-id)]
          (cond
            (contains? compound-id :grain/command)
            (do (cp/register-command! gname (observing gname impl)
                                      {:authorized? (authorized-fn compound-id props)})
                (register-schema! gname props)
                (swap! summary update :commands (fnil conj []) gname))

            (contains? compound-id :grain/query)
            (do (qp/register-query! gname impl
                                    {:authorized? (authorized-fn compound-id props)})
                (register-schema! gname props)
                (swap! summary update :queries (fnil conj []) gname))

            (contains? compound-id :grain/read-model)
            (do (rmp/register-read-model! gname impl
                                          {:events (set (map grain-name (:grain/consumes props)))
                                           :version (:grain/version props 1)})
                (swap! summary update :read-models (fnil conj []) gname))

            (contains? compound-id :grain/todo-processor)
            (do (tp/register-processor! gname impl
                                        {:topics (set (map grain-name (:grain/consumes props)))})
                (swap! summary update :processors (fnil conj []) gname))

            (contains? compound-id :grain/periodic-task)
            (do (pt/register-periodic-trigger! gname impl
                                               {:schedule (:grain/schedule props)})
                (swap! summary update :periodic-tasks (fnil conj []) gname))

            (contains? compound-id :grain/event)
            (do (register-schema! gname props)
                (swap! summary update :events (fnil conj []) gname))))))
    @summary))

;; =============================================================================
;; VERIFICATION — read grain's catalog back, diff against atlas declarations
;; =============================================================================

(defn- atlas-names
  "Grain names of live (non-deprecated) atlas entities carrying a kind aspect."
  [kind-aspect]
  (->> @registry/registry
       (keep (fn [[compound-id props]]
               (when (and (contains? compound-id kind-aspect)
                          (not (contains? compound-id :status/deprecated))
                          (:atlas/dev-id props))
                 (grain-name (:atlas/dev-id props)))))
       set))

(defn verify!
  "Diff grain's live registries (via code-agent-tools catalog) against the
   atlas declarations. Returns {:in-sync? bool :kinds {...}}.

   In sync means every atlas declaration is live in grain (declared ⊆ live).
   Live entries atlas did not declare are reported as :unmanaged rather than
   drift — grain components register framework internals in the same
   registries (e.g. the control plane's :grain.control/* read models)."
  []
  (let [cat (agent-tools/catalog)
        check (fn [kind-aspect cat-key]
                (let [declared (atlas-names kind-aspect)
                      live (set (keys (get cat cat-key)))]
                  {:declared declared
                   :live live
                   :missing (set/difference declared live)
                   :unmanaged (set/difference live declared)
                   :match? (empty? (set/difference declared live))}))
        kinds {:commands (check :grain/command :commands)
               :queries (check :grain/query :queries)
               :read-models (check :grain/read-model :read-models)
               :processors (check :grain/todo-processor :processors)
               :periodic-tasks (check :grain/periodic-task :periodic-triggers)}]
    {:in-sync? (every? :match? (vals kinds))
     :kinds kinds}))

;; =============================================================================
;; IMPORT — existing grain app (captured registries) → draft atlas entities
;; =============================================================================
;;
;; The reverse leg: adopt atlas over a grain app you didn't author. Input is a
;; capture map read from the app's own JVM (its global registries + schema
;; registry + schema classification from the defschemas var names):
;;
;;   {:commands {..} :queries {..} :read-models {..} :processors {..}
;;    :periodic {..}
;;    :schema-classes {:event-schemas #{..} :command-schemas #{..} ..}
;;    :schemas {name malli-form ..}
;;    :source-root "/path/to/app/src"}
;;
;; Inference policy — no fake semantics:
;;   - Kind, domain, effect, access, declared consumes are FACTS from the
;;     registries and are imported as such.
;;   - :operation/* and :entity/* aspects are NOT guessed. Every imported
;;     entity instead carries a :draft/<name> aspect: mechanically unique
;;     compound-ids, explicitly marked as awaiting human/LLM enrichment
;;     (suggest-placement / refactor-aspect are the follow-up tools).
;;   - produces / reads / dispatches are recovered by scanning each handler's
;;     source slice (var file/line from the capture) for occurrences of
;;     registered event / read-model / command names. Heuristic — mentions
;;     are treated as use — so entities found this way carry
;;     :import/scanned-dataflow true in props for review.

(defn- kind-dev-id [kind gname]
  (keyword (str (name kind) "." (namespace gname)) (name gname)))

(defn- draft-aspect [gname]
  (keyword "draft" (name gname)))

(defn- domain-aspect [gname]
  (keyword "domain" (namespace gname)))

(defn- source-slices
  "{grain-name source-text-slice} — each entry's file sliced from its
   definition line to the next definition in the same file."
  [entries source-root]
  (let [with-src (filter #(get-in (val %) [:source :file]) entries)]
    (into {}
          (mapcat (fn [[file es]]
                    (let [f (io/file source-root file)]
                      (when (.exists f)
                        (let [lines (vec (str/split-lines (slurp f)))
                              sorted (sort-by #(get-in (val %) [:source :line]) es)
                              starts (map #(get-in (val %) [:source :line]) sorted)
                              ends (concat (rest starts) [(inc (count lines))])]
                          (map (fn [[gname _] start end]
                                 [gname (str/join "\n" (subvec lines (dec start) (dec end)))])
                               sorted starts ends))))))
          (group-by #(get-in (val %) [:source :file]) with-src))))

(defn- names-in [slice names]
  (set (filter #(str/includes? (or slice "") (str %)) names)))

(defn- top-level-fields [malli-form]
  (when (and (vector? malli-form) (= :map (first malli-form)))
    (vec (keep #(when (vector? %) (first %)) (rest malli-form)))))

(defn import-catalog!
  "Register a captured grain app as draft atlas entities. Returns a report."
  [{:keys [commands queries read-models processors periodic
           schema-classes schemas source-root]}]
  (let [events (:event-schemas schema-classes)
        event-dev-id #(kind-dev-id :event %)
        cmd-names (set (keys commands))
        rm-names (set (keys read-models))
        cmd-slices (source-slices commands source-root)
        qry-slices (source-slices queries source-root)
        proc-slices (source-slices processors source-root)
        access-aspect #(when (:authorized?/present? %) :access/enforced)
        base-props (fn [gname opts]
                     (cond-> {}
                       (get schemas gname) (assoc :grain/schema (get schemas gname))
                       (:source opts) (assoc :import/source (:source opts))))
        scanned (atom 0)
        reg! (fn [dev-id aspects props]
               (registry/register! dev-id :atlas/execution-function
                                   (set (remove nil? aspects)) props))]
    ;; events
    (doseq [ev events]
      (registry/register!
       (event-dev-id ev) :atlas/data-schema
       #{:grain/event (domain-aspect ev) (draft-aspect ev)}
       (cond-> {}
         (get schemas ev) (assoc :grain/schema (get schemas ev)
                                 :data-schema/fields (or (top-level-fields (get schemas ev)) [])))))
    ;; commands: declared facts + scanned produces/reads
    (doseq [[gname opts] commands
            :let [slice (get cmd-slices gname)
                  produces (names-in slice events)
                  reads (names-in slice rm-names)]]
      (when (or (seq produces) (seq reads)) (swap! scanned inc))
      (reg! (kind-dev-id :command gname)
            [:grain/command :effect/write (domain-aspect gname)
             (draft-aspect gname) (access-aspect opts)]
            (cond-> (base-props gname opts)
              ;; Mirror produces into :execution-function/response — events
              ;; are data, and the response key is what feeds both the
              ;; data-key trace tools (consumers-of/producers-of) and the
              ;; datalog :entity/produces facts.
              (seq produces) (assoc :grain/produces (set (map event-dev-id produces))
                                    :execution-function/response (vec (map event-dev-id produces))
                                    :import/scanned-dataflow true)
              ;; reads are runtime dependencies — mirror into deps so
              ;; blast-radius / dependent-tree traverse them
              (seq reads) (assoc :grain/reads (set (map #(kind-dev-id :read-model %) reads))
                                 :execution-function/deps (vec (map #(kind-dev-id :read-model %) reads))))))
    ;; queries: scanned reads
    (doseq [[gname opts] queries
            :let [reads (names-in (get qry-slices gname) rm-names)]]
      (when (seq reads) (swap! scanned inc))
      (reg! (kind-dev-id :query gname)
            [:grain/query :effect/read (domain-aspect gname)
             (draft-aspect gname) (access-aspect opts)]
            (cond-> (base-props gname opts)
              (seq reads) (assoc :grain/reads (set (map #(kind-dev-id :read-model %) reads))
                                 :execution-function/deps (vec (map #(kind-dev-id :read-model %) reads))
                                 :import/scanned-dataflow true))))
    ;; read models: declared consumes
    (doseq [[gname opts] read-models]
      (reg! (kind-dev-id :read-model gname)
            [:grain/read-model :effect/read (domain-aspect gname) (draft-aspect gname)]
            (cond-> (base-props gname opts)
              ;; Mirror consumes into :execution-function/context (events are
              ;; the data a read model consumes) for the trace tools.
              (:events opts) (assoc :grain/consumes (set (map event-dev-id (:events opts)))
                                    :execution-function/context (vec (map event-dev-id (:events opts))))
              (:version opts) (assoc :grain/version (:version opts)))))
    ;; processors: declared topics + scanned dispatches
    (doseq [[gname opts] processors
            :let [dispatches (names-in (get proc-slices gname) cmd-names)]]
      (when (seq dispatches) (swap! scanned inc))
      (reg! (kind-dev-id :processor gname)
            [:grain/todo-processor :temporal/async (domain-aspect gname) (draft-aspect gname)]
            (cond-> (base-props gname opts)
              (:topics opts) (assoc :grain/consumes (set (map event-dev-id (:topics opts)))
                                    :execution-function/context (vec (map event-dev-id (:topics opts))))
              (seq dispatches) (assoc :grain/dispatches (set (map #(kind-dev-id :command %) dispatches))
                                      :execution-function/deps (vec (map #(kind-dev-id :command %) dispatches))
                                      :import/scanned-dataflow true))))
    ;; periodic tasks
    (doseq [[gname opts] periodic]
      (reg! (kind-dev-id :periodic gname)
            [:grain/periodic-task :temporal/async (domain-aspect gname) (draft-aspect gname)]
            (cond-> (base-props gname opts)
              (:schedule opts) (assoc :grain/schedule (:schedule opts)))))
    (ontology/register-entity-types!)
    {:events (count events)
     :commands (count commands)
     :queries (count queries)
     :read-models (count read-models)
     :processors (count processors)
     :periodic (count periodic)
     :entities-with-scanned-dataflow @scanned}))

;; =============================================================================
;; EXPORT — cloud-pushable snapshot, correct by construction
;; =============================================================================

(defn exportable-snapshot
  "Registry snapshot for a cloud push. Encodes the integration lessons:

   1. Includes the fn-free META entities (ontology descriptors + type-refs) —
      under cloud--version the trace tools resolve dataflow keys through the
      ontology entry IN THE PULLED SNAPSHOT; without these every trace
      silently returns empty (the gap an ontology audit catches).
   2. Includes INVARIANT entities — sanitised to their semantics. An invariant's
      executable :invariant/fn can't serialise and stays in-process (that is what
      check-against runs). But its MEANING — :invariant/severity, :invariant/subject
      (the aspects it governs) and :invariant/docs — is data, so it travels: the
      snapshot carries what each rule MEANS for a human/LLM to reason about, even
      though the body was stripped. Only enriched invariants (those with
      :invariant/docs) are included; a bare :invariant/fn sanitises to a shell.
   3. Non-serialisable values (:atlas/impl, :invariant/fn, …) are stripped by
      atlas.registry.serialise/sanitize-props.

   Selects app entities by the kind-prefixed dev-id namespaces this adapter
   emits, plus :atlas/test-case entities (fully declarative)."
  ([] (exportable-snapshot @registry/registry))
  ([registry]
   (let [fn-free? (fn [props] (not-any? fn? (vals props)))
         meta? (fn [cid] (or (contains? cid :atlas/ontology)
                             (contains? cid :atlas/type-ref)))
         app-ns? (fn [dev-id]
                   (some #(str/starts-with? (namespace dev-id) %)
                         ["command." "query." "read-model." "processor."
                          "periodic." "event." "test."]))
         metas (filter (fn [[cid props]] (and (meta? cid) (fn-free? props)))
                       registry)
         invariants (keep (fn [[cid props]]
                            (when (and (= :atlas/invariant (:atlas/type props))
                                       (:invariant/docs props))
                              [cid (serialise/sanitize-props props)]))
                          registry)
         app (keep (fn [[cid props]]
                     (let [props' (serialise/sanitize-props props)]
                       (when (and (:atlas/dev-id props)
                                  (or (and (app-ns? (:atlas/dev-id props))
                                           (fn-free? props'))
                                      (and (contains? cid :atlas/test-case)
                                           (fn-free? props'))))
                         [cid props'])))
                   registry)]
     (into {} (concat metas invariants app)))))

;; =============================================================================
;; OBSERVED VS DECLARED — the event store audits the registry
;; =============================================================================

(defn observed-vs-declared
  "Audit runtime observations against registry declarations, per command.

   - :undeclared-events — the command emitted event types its
     :grain/produces does not declare. The declaration lies; severity error.
   - :unexercised-events — declared produces never observed despite the
     command having run. Not drift, but untested surface; severity warning.
   - :observed — verdict summary (invocations, events, anomalies) alongside
     the declared :grain/outcomes alphabet, for human/LLM review.

   Commands with no observations are skipped — this audit only speaks about
   what actually ran."
  []
  (let [obs @observations*]
    (->> @registry/registry
         (keep (fn [[compound-id props]]
                 (when (and (contains? compound-id :grain/command)
                            (not (contains? compound-id :status/deprecated))
                            (:atlas/dev-id props))
                   (let [gname (grain-name (:atlas/dev-id props))
                         o (get obs gname)
                         declared (set (map grain-name (:grain/produces props)))]
                     (when o
                       (let [undeclared (set/difference (:events o) declared)
                             unexercised (set/difference declared (:events o))]
                         {:command gname
                          :undeclared-events undeclared
                          :unexercised-events unexercised
                          :observed (assoc o :declared-outcomes (:grain/outcomes props))}))))))
         vec)))

(registry/register!
 :invariant/grain-observed-vs-declared
 :atlas/invariant
 #{:meta/grain-observed-vs-declared-check}
 {:invariant/fn
  (fn []
    "Declared event production must match observed event production.

   The event store is ground truth: a command that emits event types its
   :grain/produces does not declare has a lying declaration."
    (let [audit (observed-vs-declared)
          lying (filter #(seq (:undeclared-events %)) audit)
          unexercised (filter #(seq (:unexercised-events %)) audit)]
      (cond
        (seq lying)
        {:invariant :grain-observed-vs-declared
         :violation :undeclared-event-production
         :details (mapv #(select-keys % [:command :undeclared-events]) lying)
         :severity :error
         :message (str "Commands emitted events their declarations omit: "
                       (mapv :command lying))}

        (seq unexercised)
        {:invariant :grain-observed-vs-declared
         :violation :declared-events-unexercised
         :details (mapv #(select-keys % [:command :unexercised-events]) unexercised)
         :severity :warning
         :message (str "Declared events never observed despite invocations: "
                       (mapv :command unexercised))})))})

;; =============================================================================
;; SCHEMA-EVOLUTION GOVERNANCE — event schema changed ⇒ consumer version bump
;; =============================================================================

(defn schema-evolution-check
  "Compare two registry snapshots (e.g. cloud versions). For every
   :grain/event whose :grain/schema changed, every read model consuming it
   must have bumped :grain/version — grain's cache-invalidation discipline
   as a checked rule. Returns a seq of violations."
  [old-registry new-registry]
  (let [by-dev-id (fn [reg]
                    (into {} (keep (fn [[cid props]]
                                     (when (:atlas/dev-id props)
                                       [(:atlas/dev-id props) {:cid cid :props props}])))
                          reg))
        old-ids (by-dev-id old-registry)
        new-ids (by-dev-id new-registry)
        changed-events (for [[id {:keys [cid props]}] new-ids
                             :let [old (get old-ids id)]
                             :when (and (contains? cid :grain/event)
                                        old
                                        (:grain/schema (:props old))
                                        (:grain/schema props)
                                        (not= (:grain/schema (:props old))
                                              (:grain/schema props)))]
                         id)]
    (vec
     (for [ev changed-events
           [rm-id {:keys [props]}] new-ids
           :when (contains? (or (:grain/consumes props) #{}) ev)
           :let [old-version (:grain/version (:props (get old-ids rm-id)))
                 new-version (:grain/version props)]
           :when (and old-version new-version (<= new-version old-version))]
       {:event ev
        :consumer rm-id
        :version {:old old-version :new new-version}
        :message (str ev " schema changed but " rm-id
                      " :grain/version not bumped (" old-version " → " new-version ")")}))))
