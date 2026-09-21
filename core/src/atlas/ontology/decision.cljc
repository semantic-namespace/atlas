(ns atlas.ontology.decision
  "Decision ontology module.

   Defines `:atlas/decision` and `:atlas/decision-authority` — ENGINE-NEUTRAL
   decision entities, distilled from fifty years of convergent traditions
   (decision tables, production rules, DMN knowledge sources, policy engines,
   LLM judges): a decision is a question with a CLOSED, business-meaningful
   outcome alphabet, declared inputs, an inspectable logic body, an authority
   that mandates it, and zero or more realizations in actual engines.

   What makes a decision a decision (vs. an ordinary function): the outcome
   space is closed and enumerable — `#{:verdict/approve :verdict/reject}`
   decides; `(* n 2)` computes. Closedness is what makes coverage checkable,
   which is why decision tables could verify completeness in 1965 and why
   grain's outcome-coverage invariant works today.

   The decision is deliberately SEPARATE from whichever dispatcher consumes
   its verdict (DMN's split of decisions out of BPMN, restated): the same
   decision may be realized by an ORC condition node, an FSM transition, a
   grain command's outcome set, or a phase of an :atlas/llm-prompt protocol.
   `:decision/realized-by` (cardinality-many) carries that binding.

   Usage:
     (require '[atlas.ontology.decision])

   Example registration:
     (registry/register!
       :decision.review/atlas-review-verdict
       :atlas/decision
       #{:domain/review :decision/verdict :status/active}
       {:decision/question    \"Should this registry change be accepted?\"
        :decision/inputs      [:review/cloud-diff :review/aspect-anomalies]
        :decision/outcomes    #{:verdict/approve :verdict/reject}
        :decision/logic-style :llm
        :decision/realized-by #{:atlas.prompts.review/registry}
        :decision/mandated-by #{:authority.review/atlas-review-protocol}})"
  (:require [atlas.registry :as registry]
            [atlas.registry.lookup :as entity]
            [atlas.ontology.type-ref :as type-ref]
            [clojure.set :as set]
            [clojure.spec.alpha :as s]))

;; =============================================================================
;; SPECS
;; =============================================================================

;; Human phrasing of what is being decided (the compound identity carries the
;; semantic naming; the question is the prose complement).
(s/def :decision/question string?)

;; Information requirements — the data keys the decision needs to be made.
;; Declared as the ontology's :dataflow/context-key so the trace layer
;; (consumers-of / data-flow) sees decisions as consumers for free.
(s/def :decision/inputs (s/coll-of qualified-keyword? :kind vector?))

;; The CLOSED outcome alphabet. Minimum 2 — one outcome is a computation.
(s/def :decision/outcomes (s/coll-of qualified-keyword? :kind set? :min-count 2))

;; How the mapping from inputs to outcome is expressed. The style names the
;; tradition; the body carries its content as plain data:
;;   :predicate — {:key k :op :equals/... :value v}  (ORC condition shape)
;;   :table     — vector of {:when {...} :then outcome} rows
;;   :llm       — instruction string (the prompt IS the logic body)
;;   :code      — fully-qualified fn symbol string (Path-A discipline)
(s/def :decision/logic-style #{:predicate :table :llm :code})
(s/def :decision/logic-body any?)

;; References (values are dev-ids of other registered entities)
(s/def :decision/realized-by (s/coll-of :atlas/dev-id :kind set?))
(s/def :decision/mandated-by (s/coll-of :atlas/dev-id :kind set?))
(s/def :decision/mitigates (s/coll-of :atlas/dev-id :kind set?))

;; Terminal-state memory: why the org stopped deciding this way.
(s/def :decision/retirement-reason string?)

;; --- decision-authority ------------------------------------------------------

(s/def :authority/kind #{:policy :regulation :protocol :heuristic})
(s/def :authority/statement string?)
(s/def :authority/source string?)
(s/def :authority/revocation-reason string?)

;; =============================================================================
;; ONTOLOGY DESCRIPTORS
;; =============================================================================

(registry/register!
 :atlas/decision
 :atlas/ontology
 #{:atlas/decision}
 {:ontology/for :atlas/decision
  ;; Lifecycle is a property of THIS entity — a decision is active, or
  ;; superseded, or retired; never two at once. Without this, a
  ;; :status/superseded decision inherits :status/active from its
  ;; :decision/mandated-by authority and carries both, and the
  ;; decision-active-has-realization invariant fires a false positive on a
  ;; correctly-retired decision. (Observed 2026-07-16 on
  ;; :decision.qa/since-date-corrupted.)
  :ontology/intrinsic-aspects #{:status/active :status/superseded
                                :status/retired :status/proposed
                                :status/revoked}
  :ontology/keys [:decision/question
                  :decision/inputs
                  :decision/outcomes
                  :decision/logic-style
                  :decision/logic-body
                  :decision/realized-by
                  :decision/mandated-by
                  :decision/mitigates
                  :decision/retirement-reason]
  ;; decisions consume their inputs — trace tools see them as consumers
  :dataflow/context-key :decision/inputs
  :dataflow/context-verb :entity/consumes})

(registry/register!
 :atlas/decision-authority
 :atlas/ontology
 #{:atlas/decision-authority}
 {:ontology/for :atlas/decision-authority
  :ontology/keys [:authority/kind
                  :authority/statement
                  :authority/source
                  :authority/revocation-reason]})

;; =============================================================================
;; TYPE-REFS
;; =============================================================================

;; Decision -> the entity/entities that execute it (ORC node, FSM transition
;; host, grain command, llm-prompt). Cardinality-many is the multi-engine
;; claim made structural: one decision, N realizations.
(registry/register!
 :type-ref/decision-realized-by
 :atlas/type-ref
 #{:meta/ref-decision-realized-by}
 {:type-ref/source :atlas/decision
  :type-ref/property :decision/realized-by
  :type-ref/datalog-verb :decision/realized-by
  :type-ref/cardinality :db.cardinality/many})

;; Decision -> the authority that mandates it (DMN knowledge source).
(registry/register!
 :type-ref/decision-mandated-by
 :atlas/type-ref
 #{:meta/ref-decision-mandated-by}
 {:type-ref/source :atlas/decision
  :type-ref/property :decision/mandated-by
  :type-ref/datalog-verb :decision/mandated-by
  :type-ref/cardinality :db.cardinality/many})

;; Decision -> the failure mode it guards against. Same join shape as the
;; ORC PoC's :node/mitigates, sourced at the decision. Deliberately its own
;; verb (honest naming); cross-source queries union the two verbs.
(registry/register!
 :type-ref/decision-mitigates
 :atlas/type-ref
 #{:meta/ref-decision-mitigates}
 {:type-ref/source :atlas/decision
  :type-ref/property :decision/mitigates
  :type-ref/datalog-verb :decision/mitigates
  :type-ref/cardinality :db.cardinality/many})

;; =============================================================================
;; DATALOG EXTRACTOR
;; =============================================================================

;; Beyond the generic type-ref extraction, each declared outcome becomes a
;; fact — `[?d :decision/outcome :verdict/reject]` — so "every decision that
;; can yield this outcome" is a one-clause datalog query across the registry.
(registry/register!
 :datalog-extractor/decision
 :atlas/datalog-extractor
 #{:meta/decision-extractor}
 {:datalog-extractor/fn
  (fn [compound-id props]
    (when (and (contains? compound-id :atlas/decision)
               (not (contains? compound-id :atlas/ontology)))
      (let [dev-id (:atlas/dev-id props)]
        (vec
         (concat
          (type-ref/extract-reference-facts :atlas/decision compound-id props)
          (map (fn [o] [:db/add dev-id :decision/outcome o])
               (:decision/outcomes props)))))))
  :datalog-extractor/schema
  {:decision/realized-by {:db/cardinality :db.cardinality/many}
   :decision/mandated-by {:db/cardinality :db.cardinality/many}
   :decision/mitigates   {:db/cardinality :db.cardinality/many}
   :decision/outcome     {:db/cardinality :db.cardinality/many}}})

;; =============================================================================
;; INVARIANTS
;; =============================================================================

(defn- user-entities-with-aspect
  "All non-meta entities carrying an aspect (skips ontology descriptors and
   the :atlas/type registration that shares the type keyword as dev-id)."
  [aspect]
  (->> (entity/all-with-aspect aspect)
       (remove #(or (entity/has-aspect? % :atlas/ontology)
                    (entity/has-aspect? % :atlas/type)))))

(registry/register!
 :invariant/decision-outcomes-closed
 :atlas/invariant
 #{:meta/decision-outcomes-closed}
 {:invariant/severity :error
  :invariant/subject  #{:atlas/decision}
  :invariant/docs
  "Every decision declares a CLOSED outcome alphabet of >= 2 qualified
   keywords. One outcome is a computation, not a decision; zero is a
   registration mistake. This is the property that makes coverage checkable
   at all — the decision-table completeness lesson, restated as a gate."
  :invariant/fn
  (fn []
    (let [violations
          (for [d (user-entities-with-aspect :atlas/decision)
                :let [outcomes (:decision/outcomes (entity/props-for d))]
                :when (not (s/valid? :decision/outcomes outcomes))]
            {:decision d :outcomes outcomes})]
      (when (seq violations)
        {:invariant :decision-outcomes-closed
         :violation :open-or-trivial-alphabet
         :details (vec violations)
         :severity :error
         :message (str "Decisions without a closed >=2 outcome alphabet: "
                       (mapv :decision violations))})))})

(registry/register!
 :invariant/decision-terminal-requires-reason
 :atlas/invariant
 #{:meta/decision-terminal-reason}
 {:invariant/severity :error
  :invariant/subject  #{:atlas/decision :status/superseded :status/retired}
  :invariant/docs
  "A superseded or retired decision must carry :decision/retirement-reason.
   Institutional memory depends on it: a retired decision documents why the
   org stopped deciding this way, which is exactly the knowledge that
   evaporates otherwise."
  :invariant/fn
  (fn []
    (let [violations
          (for [d (user-entities-with-aspect :atlas/decision)
                :when (or (entity/has-aspect? d :status/superseded)
                          (entity/has-aspect? d :status/retired))
                :when (not (string? (:decision/retirement-reason
                                     (entity/props-for d))))]
            {:decision d})]
      (when (seq violations)
        {:invariant :decision-terminal-requires-reason
         :violation :terminal-without-reason
         :details (vec violations)
         :severity :error
         :message (str "Terminal decisions missing :decision/retirement-reason: "
                       (mapv :decision violations))})))})

(registry/register!
 :invariant/authority-terminal-requires-reason
 :atlas/invariant
 #{:meta/authority-terminal-reason}
 {:invariant/severity :error
  :invariant/subject  #{:atlas/decision-authority :status/revoked}
  :invariant/docs
  "A revoked authority must carry :authority/revocation-reason — the policy
   half of the same institutional-memory discipline."
  :invariant/fn
  (fn []
    (let [violations
          (for [a (user-entities-with-aspect :atlas/decision-authority)
                :when (entity/has-aspect? a :status/revoked)
                :when (not (string? (:authority/revocation-reason
                                     (entity/props-for a))))]
            {:authority a})]
      (when (seq violations)
        {:invariant :authority-terminal-requires-reason
         :violation :terminal-without-reason
         :details (vec violations)
         :severity :error
         :message (str "Revoked authorities missing :authority/revocation-reason: "
                       (mapv :authority violations))})))})

(registry/register!
 :invariant/decision-refs-resolve
 :atlas/invariant
 #{:meta/decision-refs-resolve}
 {:invariant/severity :warning
  :invariant/subject  #{:atlas/decision}
  :invariant/docs
  "Referential integrity: :decision/realized-by, :decision/mandated-by and
   :decision/mitigates targets should resolve to registered entities, and
   mandated-by targets should be :atlas/decision-authority. Warning, not
   error — a dangling realized-by is legitimate while the realizing engine
   entity hasn't been ingested yet (same stance as :bt/delegate-to)."
  :invariant/fn
  (fn []
    (let [check (fn [d prop authority?]
                  (for [target (get (entity/props-for d) prop)
                        :let [id (entity/identity-for target)]
                        :when (or (nil? id)
                                  (and authority?
                                       (not (contains? id :atlas/decision-authority))))]
                    {:decision d :property prop :target target
                     :problem (if (nil? id) :unresolved :not-an-authority)}))
          violations
          (vec (mapcat (fn [d]
                         (concat (check d :decision/realized-by false)
                                 (check d :decision/mandated-by true)
                                 (check d :decision/mitigates false)))
                       (user-entities-with-aspect :atlas/decision)))]
      (when (seq violations)
        {:invariant :decision-refs-resolve
         :violation :dangling-or-mistyped-reference
         :details violations
         :severity :warning
         :message (str "Decision references that don't resolve as expected: "
                       violations)})))})

(registry/register!
 :invariant/decision-realization-covers-inputs
 :atlas/invariant
 #{:meta/decision-realization-inputs}
 {:invariant/severity :warning
  :invariant/subject  #{:atlas/decision}
  :invariant/docs
  "Every :decision/realized-by target should consume at least the evidence the
   decision declares in :decision/inputs — compared name-wise, since a
   realization's engine namespaces its keys differently (an ORC leaf reads
   :bb/foo where the decision declares :review/foo).

   A realization consuming materially LESS evidence than the decision declares
   is the tell for a real modeling error: it is either an under-powered
   realization, or — more often — a DIFFERENT decision wearing the wrong name.
   Different inputs mean a different question, and a decision's identity is
   question + alphabet + inputs; a thin projection of the evidence answers a
   thinner question, however similar its outcome alphabet looks.

   Warning, not error: a realization may legitimately derive a declared input
   from another (computing it rather than reading it), and prompt-realizations
   consume their inputs through tool calls rather than a declared context.
   The signal is worth surfacing; the judgement stays with the author."
  :invariant/fn
  (fn []
    (let [;; compare by NAME — engines namespace their blackboard keys freely
          names   (fn [ks] (set (map name (or ks []))))
          covers? (fn [target declared]
                    (let [p (entity/props-for target)
                          consumed (names (concat (:execution-function/context p)
                                                  (:decision/inputs p)))]
                      ;; unresolved targets are decision-refs-resolve's business
                      (or (nil? (entity/identity-for target))
                          (empty? declared)
                          (seq (set/intersection declared consumed)))))
          violations
          (for [d (user-entities-with-aspect :atlas/decision)
                :let [props    (entity/props-for d)
                      declared (names (:decision/inputs props))]
                target (:decision/realized-by props)
                :when (not (covers? target declared))
                :let [consumed (names (:execution-function/context
                                       (entity/props-for target)))]]
            {:decision d :realization target
             :declared-inputs declared :consumed consumed
             :missing (set/difference declared consumed)})]
      (when (seq violations)
        {:invariant :decision-realization-covers-inputs
         :violation :realization-under-consumes-inputs
         :details (vec violations)
         :severity :warning
         :message (str "Realizations consuming less evidence than their decision "
                       "declares (possible mis-scoped decision): "
                       (mapv (juxt :decision :realization) violations))})))})

(registry/register!
 :invariant/decision-active-has-realization
 :atlas/invariant
 #{:meta/decision-active-realization}
 {:invariant/severity :warning
  :invariant/subject  #{:atlas/decision :status/active}
  :invariant/docs
  "An ACTIVE decision should be realized somewhere — at least one
   :decision/realized-by target. A decision no engine executes is either
   :status/proposed (fine, say so) or dead vocabulary. Warning: aspirational
   registries are legitimate, silent ones drift."
  :invariant/fn
  (fn []
    (let [violations
          (for [d (user-entities-with-aspect :atlas/decision)
                :when (entity/has-aspect? d :status/active)
                :when (empty? (:decision/realized-by (entity/props-for d)))]
            {:decision d})]
      (when (seq violations)
        {:invariant :decision-active-has-realization
         :violation :active-without-realization
         :details (vec violations)
         :severity :warning
         :message (str "Active decisions with no realization: "
                       (mapv :decision violations))})))})
