(ns atlas.ontology.behavior-tree
  "The behaviour-tree ONTOLOGY — an Atlas vocabulary for modeling behaviour
   trees. Declarations only: no engine, no executor, no grain/ORC code. It
   requires atlas core and nothing else.

   Everything here reuses :atlas/execution-function (leaves + dataflow) and
   :atlas/risk-failure-mode (failure concepts). All type-refs are sourced at
   existing base types, so the generic extractor picks them up with NO core
   change — this is a downstream specialization of core's seams, which is why
   it is a module and not part of core.

   HISTORY: lived in examples/grain as `orc-demo.behavior-tree` until
   2026-07-16. It never used anything from that module — but that module pulls
   grain-core-v2 + grain-code-agent-tools via :local/root, LMDB, --add-opens
   and a Clojure 1.12 floor, so nobody could take this vocabulary without the
   whole event-sourcing stack and two unpublishable local checkouts. Extracted
   so a consumer that only wants to MODEL trees (e.g. an application modeling
   its LLM skills as decision trees) can depend on just this.

   The vocabulary is domain-agnostic. The node-type enum below was verified
   against real ORC's dsl.clj, but nothing here is ORC-specific: `:domain/orc`
   is provenance on the ORC demo's own entities, not a requirement of this
   ontology. bt-contract-complete walks any BT workflow.

   Four type-refs + one invariant:
     :behavior-tree/children     tree structure (parent -> ordered children)
     :behavior-tree/delegate-to  cross-tree call (delegate leaf -> the sheet it invokes)
     :node/mitigates  the knowledge<->structure JOIN (node -> failure concept)
     :concept/broader concept hierarchy (failure concept -> broader concept)
     bt-contract-complete  ORDERING-AWARE dataflow check (writes must precede
                           reads; parallel/fallback branches are isolated) —
                           the check ORC's sheet validator stubs out

   Kind vocabulary — verified against real ORC's dsl.clj, which defines
   :node-type as ONE of #{leaf condition llm-condition repl-researcher delegate
   sequence fallback parallel map-each}. :behavior-tree/leaf, :behavior-tree/condition,
   :behavior-tree/llm-condition, :behavior-tree/repl-researcher, :behavior-tree/delegate, and the four composite
   kinds are node-type-level aspects — mutually exclusive per node. :behavior-tree/llm and
   :behavior-tree/code are a DIFFERENT axis: real ORC's :leaf node-type carries a further
   :executor :ai|:code field (dsl.clj `llm`/`code` fns both emit :node-type
   :leaf), so :behavior-tree/llm/:behavior-tree/code compose WITH :behavior-tree/leaf, not alongside it as a
   sixth sibling kind — #{:behavior-tree/leaf :behavior-tree/llm ...} is the leaf+executor pair;
   #{:behavior-tree/condition ...} / #{:behavior-tree/llm-condition ...} / #{:behavior-tree/delegate ...} are
   their own node-types with no executor sub-split.

   :behavior-tree/condition and :behavior-tree/llm-condition are branch gates: they READ (a single
   :check key for :behavior-tree/condition; a :reads vector for :behavior-tree/llm-condition) but
   WRITE nothing — their output is control flow, not a blackboard key. Model
   the read via :execution-function/context (empty :execution-function/response)
   so bt-contract-complete's ordering check covers them like any other node —
   a condition gating on an unproduced key is exactly the class of bug the
   invariant exists to catch."
  (:require [atlas.registry :as registry]
            [atlas.registry.lookup :as entity]
            [atlas.ontology.execution-function]
            [atlas.ontology.risk-failure-mode]))

;; The node-type enum and the executor axis are INTRINSIC — each is a property
;; of the node itself, and both are mutually exclusive per node (see the ns
;; docstring). Without this declaration they inherit through
;; :behavior-tree/children, so a sequence acquires :behavior-tree/leaf from its
;; children and every node answers to every node-type — `by-aspect
;; :behavior-tree/leaf` returns the whole tree rather than the leaves.
;;
;; :behavior-tree/inputs / :as are properties, not aspects, so they are not
;; listed. Nothing else here is intrinsic: a subtree containing an llm leaf
;; genuinely does involve an llm, which is why the executor aspects are only
;; blocked from INHERITANCE, not from being carried by the node that declares
;; them.
(registry/register!
 :atlas/behavior-tree
 :atlas/ontology
 #{:atlas/behavior-tree}
 {:ontology/for :atlas/behavior-tree
  :ontology/intrinsic-aspects #{;; node-type — mutually exclusive per node
                                :behavior-tree/leaf
                                :behavior-tree/condition
                                :behavior-tree/llm-condition
                                :behavior-tree/repl-researcher
                                :behavior-tree/delegate
                                :behavior-tree/sequence
                                :behavior-tree/fallback
                                :behavior-tree/parallel
                                :behavior-tree/map-each
                                ;; executor axis — mutually exclusive per leaf
                                :behavior-tree/llm
                                :behavior-tree/code}})

(registry/register!
 :type-ref/bt-children
 :atlas/type-ref
 #{:meta/ref-bt-children :domain/orc}
 {:type-ref/source :atlas/execution-function
  :type-ref/property :behavior-tree/children
  :type-ref/datalog-verb :entity/depends
  :type-ref/cardinality :db.cardinality/many})

;; Delegate leaf -> the sheet it invokes. Real ORC's `delegate` fn (dsl.clj)
;; carries :target-sheet-id (a raw sheet UUID, grain's own identity for a whole
;; other tree) alongside ordinary :reads/:writes. Modeled as a nilable,
;; card-one edge: when the target sheet is itself ingested/registered as an
;; Atlas entity its dev-id resolves here; otherwise this is a dangling ref like
;; any other out-of-scope external reference — not an error, just unresolved.
(registry/register!
 :type-ref/bt-delegate-to
 :atlas/type-ref
 #{:meta/ref-bt-delegate-to :domain/orc}
 {:type-ref/source :atlas/execution-function
  :type-ref/property :behavior-tree/delegate-to
  :type-ref/datalog-verb :entity/depends
  :type-ref/cardinality :db.cardinality/one})

(registry/register!
 :type-ref/node-mitigates
 :atlas/type-ref
 #{:meta/ref-node-mitigates :domain/orc}
 {:type-ref/source :atlas/execution-function
  :type-ref/property :node/mitigates
  :type-ref/datalog-verb :node/mitigates
  :type-ref/cardinality :db.cardinality/many})

(registry/register!
 :type-ref/concept-broader
 :atlas/type-ref
 #{:meta/ref-concept-broader :domain/orc}
 {:type-ref/source :atlas/risk-failure-mode
  :type-ref/property :concept/broader
  :type-ref/datalog-verb :concept/broader
  :type-ref/cardinality :db.cardinality/many})

(registry/register!
 :invariant/bt-contract-complete
 :atlas/invariant
 #{:meta/bt-contract-complete :domain/orc}
 {:invariant/severity :error
  :invariant/subject  #{:behavior-tree/leaf :domain/orc}
  :invariant/docs
  "ORDERING-AWARE dataflow completeness: every blackboard key a BT node reads
   (:execution-function/context) must be available AT THE POINT THE NODE RUNS —
   written by an EARLIER sibling under an enclosing ORDERED composite, by an
   ancestor, or declared a workflow input (:behavior-tree/inputs).

   ORDERED composites thread writes forward: :behavior-tree/sequence,
   :behavior-tree/fallback, and :behavior-tree/map-each (per-item sequential
   body, which also binds its :behavior-tree/as item key for children). Fallback
   counts because it runs children in order until one SUCCEEDS — a later child
   runs only because the earlier ones ran and failed, so their writes are
   guaranteed.

   Only :behavior-tree/parallel is concurrent: its children see just what was
   available at branch entry, never each other's writes.

   Subsumes the flat 'produced anywhere' check and additionally catches
   reads-before-writes."
  :invariant/fn
  (fn []
    (let [bb?      (fn [k] (and (keyword? k) (= "bb" (namespace k))))
          ;; A BT node is anything carrying a :behavior-tree/* aspect — the
          ;; marker is the namespace, not a domain. (Pre-v0.9.0 this checked
          ;; "bt"; the :bt/* -> :behavior-tree/* rename left this predicate
          ;; pointing at the extinct namespace, so it silently matched nothing
          ;; and the invariant vacuously passed. Fixed 2026-07-16.)
          bt-node? (fn [n] (some #(= "behavior-tree" (namespace %)) (entity/identity-for n)))
          ;; Enumerate every node by unioning over the closed node-type enum —
          ;; domain-agnostic, so this ordering check covers any BT workflow
          ;; (ORC, inbox-search QA, …), not only :domain/orc.
          kinds    [:behavior-tree/leaf :behavior-tree/condition :behavior-tree/llm-condition
                    :behavior-tree/repl-researcher :behavior-tree/delegate :behavior-tree/sequence
                    :behavior-tree/fallback :behavior-tree/parallel :behavior-tree/map-each]
          nodes    (->> kinds (mapcat entity/all-with-aspect) distinct (filter bt-node?))
          p        entity/props-for
          reads    (fn [n] (filter bb? (:execution-function/context (p n))))
          writes   (fn [n] (set (filter bb? (:execution-function/response (p n)))))
          kids     (fn [n] (:behavior-tree/children (p n)))
          ;; ORDERED = children run one after another, so a later child sees an
          ;; earlier one's writes. That is sequence, map-each AND fallback.
          ;;
          ;; Fallback belongs here: it runs children in order until one
          ;; SUCCEEDS, so child N runs precisely BECAUSE children 1..N-1 ran and
          ;; failed — their writes are guaranteed, not speculative. Sequence and
          ;; fallback differ in why they stop (failure vs success), not in
          ;; ordering. The retry pattern depends on exactly this: the failing
          ;; call writes :bb/call-error and the fallback branch reads it.
          ;;
          ;; Only :behavior-tree/parallel is genuinely concurrent, so only it
          ;; denies siblings each other's writes.
          ;;
          ;; (Fixed 2026-07-16. This check was masked until then: node-type
          ;; aspects inherited through :behavior-tree/children, so every node
          ;; carried :behavior-tree/sequence and `ordered?` was true for almost
          ;; everything. Making node-type intrinsic exposed it as a false
          ;; positive on :retry/open-circuit.)
          ordered? (fn [n] (let [id (entity/identity-for n)]
                             (or (contains? id :behavior-tree/sequence)
                                 (contains? id :behavior-tree/map-each)
                                 (contains? id :behavior-tree/fallback))))
          ;; walk returns {:produced #{...} :violations [...]} for the subtree
          walk     (fn walk [n avail]
                     (let [avail (-> avail
                                     (into (:behavior-tree/inputs (p n)))
                                     (cond-> (:behavior-tree/as (p n)) (conj (:behavior-tree/as (p n)))))
                           own   (writes n)
                           ;; a node may read keys it itself writes (read-modify-
                           ;; write / load-if-absent) — ORC's real load-csv does
                           ;; exactly this
                           ok?   (into avail own)
                           viol  (vec (for [k (reads n) :when (not (ok? k))]
                                        {:node n :reads-unproduced k}))]
                       (if-let [cs (seq (kids n))]
                         (if (ordered? n)
                           ;; sequence: each child sees earlier siblings' writes
                           (reduce (fn [acc c]
                                     (let [r (walk c (into avail (:produced acc)))]
                                       (-> acc
                                           (update :produced into (:produced r))
                                           (update :violations into (:violations r)))))
                                   {:produced own :violations viol} cs)
                           ;; parallel / fallback / map-each: entry availability only
                           (reduce (fn [acc c]
                                     (let [r (walk c avail)]
                                       (-> acc
                                           (update :produced into (:produced r))
                                           (update :violations into (:violations r)))))
                                   {:produced own :violations viol} cs))
                         {:produced own :violations viol})))
          child?   (set (mapcat kids nodes))
          roots    (remove child? nodes)
          violations (vec (mapcat #(:violations (walk % #{})) roots))]
      (when (seq violations)
        {:invariant :bt-contract-complete :violation :dead-read
         :details violations :severity :error
         :message (str "BT nodes read blackboard keys not available when they run: "
                       violations)})))})
