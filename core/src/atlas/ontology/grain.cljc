(ns atlas.ontology.grain
  "Grain (CQRS / Event Sourcing) ontology module. Auto-registers on require.

   Maps the registrable kinds of https://github.com/ObneyAI/grain onto Atlas
   entity types. No new entity types are introduced — grain kinds are aspects
   on the existing types:

     grain kind        Atlas type                    kind aspect
     ----------        ----------                    -----------
     defcommand        :atlas/execution-function     :grain/command
     defquery          :atlas/execution-function     :grain/query
     defreadmodel      :atlas/execution-function     :grain/read-model
     defprocessor      :atlas/execution-function     :grain/todo-processor
     defperiodic       :atlas/execution-function     :grain/periodic-task
     event schema      :atlas/data-schema            :grain/event

   Because grain keeps five separate registries, the same qualified name may
   denote different kinds (e.g. :example/counters is both a query and a read
   model). Atlas dev-ids are global, so the mapping prefixes the namespace
   with the kind: :command.example/create-counter, :query.example/counters,
   :read-model.example/counters, :event.example/counter-created. Stripping
   the first segment recovers the grain name — the mapping is round-trippable.

   Access policy is expressed with :access/public / :access/enforced aspects
   (a fresh namespace — :auth/* is already used for data descriptor keys like
   :auth/token and must not be overloaded with policy aspects).

   Data-flow edges (produces / consumes / dispatches / reads) are declared as
   type-refs sourced at :atlas/execution-function, so the existing
   execution-function datalog extractor picks them up generically — this
   module needs no extractor of its own.

   Usage:
     (require '[atlas.ontology.grain])"
  (:require [atlas.registry :as registry]
            [atlas.registry.lookup :as entity]
            [atlas.ontology.type-ref :as type-ref]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]))

;; =============================================================================
;; SPECS
;; =============================================================================

;; Event entities this entity appends to the event store
;; (commands, todo-processors, periodic-tasks)
(s/def :grain/produces (s/coll-of qualified-keyword? :kind set?))

;; Event entities this entity subscribes to
;; (read models via :events, todo-processors via :topics)
(s/def :grain/consumes (s/coll-of qualified-keyword? :kind set?))

;; Command entities a todo-processor dispatches via process-command
(s/def :grain/dispatches (s/coll-of qualified-keyword? :kind set?))

;; Read-model entities a query projects from
(s/def :grain/reads (s/coll-of qualified-keyword? :kind set?))

;; Grain schedule map, e.g. {:cron "0 0 * * *"} or {:every 30 :duration :seconds}
(s/def :grain/schedule map?)

;; Read-model cache version (bump to invalidate)
(s/def :grain/version pos-int?)

;; Mandatory when an entity carries :status/deprecated
(s/def :grain/deprecated-reason string?)

;; Verdict alphabet — the finite set of outcomes a command can decide
;; (success verdicts + one per guarded failure, e.g. grain anomaly
;; categories). Declares the guard's interface as data; the decision
;; procedure stays in the impl (or a behavioral spec).
(s/def :grain/outcomes (s/coll-of qualified-keyword? :kind set?))

;; Justification for an event that is produced but intentionally has no
;; consumer (a decision, not a condition — present = the orphan warning is
;; knowingly retired; cite the design source, e.g. the allium spec).
(s/def :dataflow/unconsumed-reason string?)

;; =============================================================================
;; TYPE-REFS — data-flow edges, extracted by the existing
;; execution-function extractor via type-ref/extract-reference-facts
;; =============================================================================

(registry/register!
 :type-ref/grain-produces
 :atlas/type-ref
 #{:meta/ref-grain-produces :domain/grain}
 {:type-ref/source :atlas/execution-function
  :type-ref/property :grain/produces
  :type-ref/datalog-verb :entity/produces
  :type-ref/cardinality :db.cardinality/many})

(registry/register!
 :type-ref/grain-consumes
 :atlas/type-ref
 #{:meta/ref-grain-consumes :domain/grain}
 {:type-ref/source :atlas/execution-function
  :type-ref/property :grain/consumes
  :type-ref/datalog-verb :entity/consumes
  :type-ref/cardinality :db.cardinality/many})

(registry/register!
 :type-ref/grain-dispatches
 :atlas/type-ref
 #{:meta/ref-grain-dispatches :domain/grain}
 {:type-ref/source :atlas/execution-function
  :type-ref/property :grain/dispatches
  :type-ref/datalog-verb :entity/depends
  :type-ref/cardinality :db.cardinality/many})

(registry/register!
 :type-ref/grain-reads
 :atlas/type-ref
 #{:meta/ref-grain-reads :domain/grain}
 {:type-ref/source :atlas/execution-function
  :type-ref/property :grain/reads
  :type-ref/datalog-verb :entity/depends
  :type-ref/cardinality :db.cardinality/many})

;; Invariant subject as an edge (grain owns this — grain's invariants declare
;; :invariant/subject; core provides only the generic :atlas/invariant extractor
;; seam). A domain that is NOT the atlas author adds invariant edges exactly like
;; this: a type-ref sourced at :atlas/invariant, loaded in the domain's REPL.
;; Values are ASPECTS (not entity dev-ids), so the verb :invariant/governs stores
;; keyword values — rules become discoverable by governed aspect
;; ([?inv :invariant/governs :effect/write]) without pretending aspects are entities.
(registry/register!
 :type-ref/invariant-subject
 :atlas/type-ref
 #{:meta/ref-invariant-subject :domain/grain}
 {:type-ref/source :atlas/invariant
  :type-ref/property :invariant/subject
  :type-ref/datalog-verb :invariant/governs
  :type-ref/cardinality :db.cardinality/many})

;; =============================================================================
;; INVARIANTS
;; =============================================================================

(defn- grain-kind?
  "True when the entity carries any :grain/* kind aspect."
  [dev-id]
  (some #(= "grain" (namespace %))
        (or (entity/identity-for dev-id) #{})))

(def ^:private grain-edge-source-kinds
  "Which grain kind may DECLARE which data-flow property — the source side of
   the CQRS connection grammar. `grain-refs-valid` checks the target kind of
   each ref; this checks that the entity carrying the edge is even allowed to."
  {:grain/produces   #{:grain/command :grain/periodic-task}
   :grain/consumes   #{:grain/read-model :grain/todo-processor}
   :grain/dispatches #{:grain/todo-processor :grain/periodic-task}
   :grain/reads      #{:grain/query :grain/command}})

(def ^:private grain-prefix->kind
  "Dev-id kind-prefix → the :grain/* kind aspect it must carry. Enforces the
   round-trippable naming the module docstring promises."
  {"command"    :grain/command
   "query"      :grain/query
   "read-model" :grain/read-model
   "processor"  :grain/todo-processor
   "periodic"   :grain/periodic-task
   "event"      :grain/event})

(registry/register!
 :invariant/grain-command-declares-access
 :atlas/invariant
 #{:meta/grain-access-check :domain/grain}
 {:invariant/severity :error
  :invariant/subject #{:grain/command :access/public :access/enforced}
  :invariant/docs
  "Every grain command must declare an access policy. Grain rejects commands
   without an :authorized? predicate at the adapter level (deny by default). The
   semantic mirror: every :grain/command must carry :access/public or
   :access/enforced."
  :invariant/fn
  (fn []
    (let [violations (->> (entity/all-with-aspect :grain/command)
                          (remove #(or (entity/has-aspect? % :access/public)
                                       (entity/has-aspect? % :access/enforced))))]
      (when (seq violations)
        {:invariant :grain-command-declares-access
         :violation :command-without-access-policy
         :commands (vec violations)
         :severity :error
         :message (str "Grain commands must declare :access/public or :access/enforced: "
                       (vec violations))})))})

(registry/register!
 :invariant/grain-no-public-write
 :atlas/invariant
 #{:meta/grain-public-write-check :domain/grain}
 {:invariant/severity :warning
  :invariant/subject #{:grain/command :effect/write :access/public}
  :invariant/docs
  "Write commands should not be publicly accessible. A :grain/command with
   :effect/write and :access/public corresponds to a grain command registered
   with a trivially-true :authorized? predicate — anyone can mutate state. Grain
   itself cannot see this: :authorized? presence is checked, its meaning is not."
  :invariant/fn
  (fn []
    (let [violations (->> (entity/all-with-aspect :grain/command)
                          (filter #(and (entity/has-aspect? % :effect/write)
                                        (entity/has-aspect? % :access/public))))]
      (when (seq violations)
        {:invariant :grain-no-public-write
         :violation :public-write-command
         :commands (vec violations)
         :severity :warning
         :message (str "Write commands open to the public: " (vec violations))})))})

(registry/register!
 :invariant/grain-orphan-events
 :atlas/invariant
 #{:meta/grain-orphan-event-check :domain/grain}
 {:invariant/severity :warning
  :invariant/subject #{:grain/event :grain/consumes :dataflow/unconsumed-reason}
  :invariant/docs
  "Every produced event should have at least one consumer. An event that no read
   model, processor, or other consumer subscribes to is a dead data flow —
   appended to the store forever, projected by nothing. Grain's per-kind
   registries cannot join producers against consumers. Events carrying
   :dataflow/unconsumed-reason are skipped: the orphan is a documented decision,
   not an oversight."
  :invariant/fn
  (fn []
    (let [events (set (entity/all-with-aspect :grain/event))
          consumed (->> (entity/all-with-aspect :atlas/execution-function)
                        (mapcat #(:grain/consumes (entity/props-for %)))
                        set)
          justified (set (filter #(:dataflow/unconsumed-reason (entity/props-for %))
                                 events))
          orphans (set/difference events consumed justified)]
      (when (seq orphans)
        {:invariant :grain-orphan-events
         :violation :event-without-consumer
         :events (vec orphans)
         :severity :warning
         :message (str "Events produced but never consumed: " (vec orphans))})))})

(registry/register!
 :invariant/grain-event-has-producer
 :atlas/invariant
 #{:meta/grain-event-producer-check :domain/grain}
 {:invariant/severity :warning
  :invariant/subject #{:grain/event :grain/produces}
  :invariant/docs
  "Every registered event should have at least one producer — otherwise it is a
   schema no command, processor, or periodic-task ever appends."
  :invariant/fn
  (fn []
    (let [events (set (entity/all-with-aspect :grain/event))
          produced (->> (entity/all-with-aspect :atlas/execution-function)
                        (mapcat #(:grain/produces (entity/props-for %)))
                        set)
          unproduced (set/difference events produced)]
      (when (seq unproduced)
        {:invariant :grain-event-has-producer
         :violation :event-without-producer
         :events (vec unproduced)
         :severity :warning
         :message (str "Events registered but never produced: " (vec unproduced))})))})

(registry/register!
 :invariant/grain-refs-valid
 :atlas/invariant
 #{:meta/grain-ref-integrity-check :domain/grain}
 {:invariant/severity :warning
  :invariant/subject #{:grain/produces :grain/consumes :grain/dispatches :grain/reads}
  :invariant/docs
  "Referential integrity of grain data-flow properties. :grain/produces and
   :grain/consumes must reference registered :grain/event entities;
   :grain/dispatches must reference :grain/command entities; :grain/reads must
   reference :grain/read-model entities. (This checks the TARGET kind of each
   edge; grain-edge-source-legality checks the source side.)"
  :invariant/fn
  (fn []
    (let [check (fn [dev-id prop required-aspect]
                  (for [ref (get (entity/props-for dev-id) prop)
                        :when (or (nil? (entity/identity-for ref))
                                  (not (entity/has-aspect? ref required-aspect)))]
                    {:entity dev-id :property prop :ref ref :expected required-aspect}))
          violations (->> (entity/all-with-aspect :atlas/execution-function)
                          (mapcat (fn [id]
                                    (concat (check id :grain/produces :grain/event)
                                            (check id :grain/consumes :grain/event)
                                            (check id :grain/dispatches :grain/command)
                                            (check id :grain/reads :grain/read-model)))))]
      (when (seq violations)
        {:invariant :grain-refs-valid
         :violation :dangling-or-mistyped-ref
         :details (vec violations)
         :severity :warning
         :message "Grain data-flow refs must point at registered entities of the right kind"})))})

(registry/register!
 :invariant/grain-deprecated-requires-reason
 :atlas/invariant
 #{:meta/grain-deprecation-check :domain/grain}
 {:invariant/severity :error
  :invariant/subject #{:status/deprecated :grain/deprecated-reason}
  :invariant/docs
  "Deprecated grain entities must record why. Institutional memory: a
   :status/deprecated entity without a :grain/deprecated-reason loses the context
   future maintainers need."
  :invariant/fn
  (fn []
    (let [violations (->> (entity/all-with-aspect :status/deprecated)
                          (filter grain-kind?)
                          (remove #(:grain/deprecated-reason (entity/props-for %))))]
      (when (seq violations)
        {:invariant :grain-deprecated-requires-reason
         :violation :deprecated-without-reason
         :entities (vec violations)
         :severity :error
         :message (str "Deprecated grain entities missing :grain/deprecated-reason: "
                       (vec violations))})))})

(registry/register!
 :invariant/grain-edge-source-legality
 :atlas/invariant
 #{:meta/grain-edge-source-check :domain/grain}
 {:invariant/severity :error
  :invariant/subject #{:grain/produces :grain/consumes :grain/dispatches :grain/reads}
  :invariant/docs
  "Source side of grain's CQRS connection grammar. grain-refs-valid type-checks
   the TARGET of each data-flow ref; this checks the other half: that the entity
   DECLARING the edge is a kind allowed to carry it — a read-model must not
   :grain/produces, a query must not :grain/dispatches. Together they type the
   whole grammar. Grain enforces this by having five separate registries; Atlas
   has one, so the grammar must be asserted."
  :invariant/fn
  (fn []
    (let [violations (for [id   (entity/all-with-aspect :atlas/execution-function)
                           :let [props (entity/props-for id)]
                           [prop legal-kinds] grain-edge-source-kinds
                           :when (seq (get props prop))
                           :when (not (some #(entity/has-aspect? id %) legal-kinds))]
                       {:entity id :property prop :legal-source-kinds legal-kinds})]
      (when (seq violations)
        {:invariant :grain-edge-source-legality
         :violation :illegal-edge-source
         :details (vec violations)
         :severity :error
         :message (str "Data-flow edges declared by a grain kind not allowed to carry them: "
                       (vec violations))})))})

(registry/register!
 :invariant/grain-outcome-coverage
 :atlas/invariant
 #{:meta/grain-outcome-coverage-check :domain/grain}
 {:invariant/severity :warning
  :invariant/subject #{:grain/command :grain/outcomes :atlas/test-case}
  :invariant/docs
  "The verdict alphabet IS the test matrix. Every verdict a :grain/command
   declares in :grain/outcomes must have a matching :atlas/test-case — one whose
   :test-case/target is the command and whose identity carries that :outcome/*
   aspect. An uncovered verdict is declared-but-unproven behaviour. Grain cannot
   express this join: outcomes and test-cases are not registry concepts there."
  :invariant/fn
  (fn []
    (let [covered    (set (for [tc   (entity/all-with-aspect :atlas/test-case)
                                :let [target  (:test-case/target (entity/props-for tc))
                                      verdict (first (filter #(= "outcome" (namespace %))
                                                             (entity/identity-for tc)))]
                                :when (and target verdict)]
                            [target verdict]))
          violations (for [cmd     (entity/all-with-aspect :grain/command)
                           verdict (:grain/outcomes (entity/props-for cmd))
                           :when   (not (contains? covered [cmd verdict]))]
                       {:command cmd :uncovered-verdict verdict})]
      (when (seq violations)
        {:invariant :grain-outcome-coverage
         :violation :verdict-without-test-case
         :details (vec violations)
         :severity :warning
         :message (str "Command verdicts with no test-case: " (vec violations))})))})

(registry/register!
 :invariant/grain-kind-prefix-consistency
 :atlas/invariant
 #{:meta/grain-prefix-consistency-check :domain/grain}
 {:invariant/severity :error
  :invariant/subject #{:grain/command :grain/query :grain/read-model
                       :grain/todo-processor :grain/periodic-task :grain/event}
  :invariant/docs
  "Kind-prefix must match kind-aspect (round-trippable naming). The ontology maps
   grain names to dev-ids by prefixing the kind (:command.example/create-counter).
   That recovery is only round-trippable if the prefix agrees with the entity's
   :grain/* kind aspect. A :query.example/foo carrying :grain/command would
   silently break name recovery — no other invariant catches it."
  :invariant/fn
  (fn []
    (let [violations (for [[prefix kind] grain-prefix->kind
                           id  (entity/all-with-aspect kind)
                           :let [seg (first (str/split (namespace id) #"\."))]
                           :when (not= seg prefix)]
                       {:entity id :expected-prefix prefix :actual-prefix seg :kind kind})]
      (when (seq violations)
        {:invariant :grain-kind-prefix-consistency
         :violation :prefix-kind-mismatch
         :details (vec violations)
         :severity :error
         :message (str "Dev-id kind-prefix disagrees with :grain/* kind aspect: "
                       (vec violations))})))})
