(ns orc-demo.contract-check
  "The atlas-derived ORDERING-AWARE contract check as a TEST GATE for ORC
   workflows.

   ORC's own sheet validator (mcp-sheet-builder/core/validator.clj) checks
   reads ∪ writes ⊆ blackboard — but only for LLM-GENERATED sheets, and its
   data-flow check is stubbed ('full data flow analysis would be more
   complex'). This gate is that stub, implemented: every read must be
   available AT THE POINT THE NODE RUNS — written by an earlier sibling in an
   enclosing sequence, an ancestor, or a declared input. Parallel siblings
   cannot see each other's writes; fallback siblings may never have run;
   map-each bodies run per-item sequentially with the item key bound. And it
   runs on EVERY workflow, hand-written included.

   The whole ORC<->atlas bridge is `ingest-tree!` (+ `orc->ingest` for ORC's
   native `dsl/workflow` data). In ORC's CI it becomes:
   `(is (nil? (check-tree (orc->ingest my-workflow))))` after each defworkflow.

   `ingest-tree!` also takes an ENRICHMENT overlay — a hand-maintained map of
   dev-id -> {:aspects #{..} :props {..}} merged onto each ingest. Structure
   stays machine-fresh (re-ingested from code, never drifts); meaning
   (:node/mitigates links, :operation/* aspects) is declared once and re-applied
   on every ingest. This is how machine-ingested trees join the knowledge graph."
  (:require [clojure.test :refer [deftest is]]
            [atlas.registry :as registry]
            [atlas.registry.lookup :as entity]
            [atlas.ontology.behavior-tree]))

(defn ingest-tree!
  "Register an ORC-shaped tree node (and its descendants) as atlas entities.
   Node = {:id kw :type :sequence|:fallback|:parallel|:map-each|:leaf
           :children [...] :reads [...] :writes [...] :inputs #{...}
           :as kw :mitigates #{...}}.
   `enrichment` (optional) = {dev-id {:aspects #{..} :props {..}}} — the
   semantic overlay merged onto the mechanically-derived entity."
  ([node] (ingest-tree! node {}))
  ([{:keys [id type children reads writes inputs mitigates as] :as _node} enrichment]
   (let [e (get enrichment id)]
     ;; a per-node identity aspect (:node/<ns>-<name>) — atlas requires unique
     ;; compound-ids, and two behaviour-tree leaves that differ only by dataflow
     ;; would otherwise collapse into one. Enrichment :aspects are the real
     ;; semantic distinguishers; the name aspect is scaffolding until then.
     (registry/register!
      id :atlas/execution-function
      (into (cond-> #{(keyword "bt" (name type)) :domain/orc
                      (keyword "node" (str (namespace id) "-" (name id)))}
              (empty? children) (conj :bt/leaf))
            (:aspects e))
      (merge
       (cond-> {}
         (seq children)  (assoc :bt/children (mapv :id children))
         (seq reads)     (assoc :execution-function/context (vec reads))
         (seq writes)    (assoc :execution-function/response (vec writes))
         (seq inputs)    (assoc :bt/inputs (set inputs))
         as              (assoc :bt/as as)
         (seq mitigates) (assoc :node/mitigates (set mitigates)))
       (:props e))))
   (doseq [c children] (ingest-tree! c enrichment))
   id))

(defn orc->ingest
  "Adapt ORC's NATIVE workflow data — {:name :blackboard [ks] :root {:node-type
   :name :reads :writes :children (:from :as :into on map-each)}} (exactly what
   `(dsl/workflow …)` returns) — into the ingest shape. Blackboard keys are
   namespaced to :bb/*; map-each :from counts as a read, :into as a write, :as
   is carried as the bound item key; workflow inputs are derived as the declared
   blackboard keys nothing writes (external inputs)."
  [{:keys [name blackboard root]}]
  (let [bb   (fn [k] (keyword "bb" (clojure.core/name k)))
        idns (str "bt." name)
        conv (fn conv [node]
               (cond-> {:id (keyword idns (:name node)) :type (:node-type node)}
                 (seq (:reads node))    (assoc :reads (mapv bb (:reads node)))
                 (:from node)           (update :reads (fnil conj []) (bb (:from node)))
                 (seq (:writes node))   (assoc :writes (mapv bb (:writes node)))
                 (:into node)           (update :writes (fnil conj []) (bb (:into node)))
                 (:as node)             (assoc :as (bb (:as node)))
                 (seq (:children node)) (assoc :children (mapv conv (:children node)))))
        tree    (conv root)
        written (set (mapcat :writes (tree-seq :children :children tree)))
        inputs  (remove written (map bb blackboard))]
    (assoc tree :inputs (set inputs))))

(defn- load-ontology! []
  (reset! registry/registry {})
  (require 'atlas.ontology.type-ref 'atlas.ontology 'atlas.ontology.execution-function
           'atlas.ontology.risk-failure-mode 'atlas.ontology.behavior-tree :reload))

(defn check-tree
  "Ingest an ORC tree (data) into a clean registry; return the ordering-aware
   contract result (nil = complete, else the :dead-read violation). The CI gate.
   Safe inside a live process: the ambient registry is restored on exit."
  ([tree] (check-tree tree {}))
  ([tree enrichment]
   (let [before @registry/registry]
     (try
       (load-ontology!)
       (ingest-tree! tree enrichment)
       ((:invariant/fn (entity/props-for :invariant/bt-contract-complete)))
       (finally (reset! registry/registry before))))))

;; --- fixtures ---------------------------------------------------------------

(def good-tree
  {:id :wf/good :type :sequence :inputs #{:bb/in}
   :children [{:id :wf/a :type :leaf :reads [:bb/in] :writes [:bb/x]}
              {:id :wf/b :type :leaf :reads [:bb/x]  :writes [:bb/out]}]})

;; realistic bug: :wf/d reads :bb/tpo — a typo'd/renamed key nothing writes
(def broken-tree
  {:id :wf/broken :type :sequence :inputs #{:bb/in}
   :children [{:id :wf/c :type :leaf :reads [:bb/in]  :writes [:bb/x]}
              {:id :wf/d :type :leaf :reads [:bb/tpo] :writes [:bb/out]}]})

;; ordering bug: :bb/x IS produced — but by a LATER sibling. The flat
;; 'produced anywhere' check (and a blackboard-coverage check) passes this;
;; only the ordering-aware walk catches it.
(def out-of-order-tree
  {:id :wf/ooo :type :sequence :inputs #{:bb/in}
   :children [{:id :wf/reader :type :leaf :reads [:bb/x]  :writes [:bb/out]}
              {:id :wf/writer :type :leaf :reads [:bb/in] :writes [:bb/x]}]})

;; isolation bug: parallel siblings cannot observe each other's writes —
;; :wf/q reading :wf/p's write is a race, not a dataflow.
(def parallel-cross-read-tree
  {:id :wf/par-root :type :sequence :inputs #{:bb/in}
   :children [{:id :wf/par :type :parallel
               :children [{:id :wf/p :type :leaf :reads [:bb/in] :writes [:bb/a]}
                          {:id :wf/q :type :leaf :reads [:bb/a]  :writes [:bb/b]}]}
              {:id :wf/join :type :leaf :reads [:bb/a :bb/b] :writes [:bb/out]}]})

;; map-each: the body reads the bound item key (:as) — legal, per-item.
(def map-each-tree
  {:id :wf/batch :type :map-each :inputs #{:bb/items} :as :bb/item
   :children [{:id :wf/score :type :leaf :reads [:bb/item] :writes [:bb/scores]}
              {:id :wf/tally :type :leaf :reads [:bb/scores] :writes [:bb/report]}]})

;; --- the gate ---------------------------------------------------------------

(deftest good-workflow-is-contract-complete
  (is (nil? (check-tree good-tree))
      "a workflow whose every read is produced upstream passes"))

(deftest dead-read-fails-the-build
  (let [v (check-tree broken-tree)]
    (is (= :dead-read (:violation v))
        "a leaf reading a key nothing writes must be caught")
    (is (= {:node :wf/d :reads-unproduced :bb/tpo} (first (:details v)))
        "and pinpointed to the offending node + key")))

(deftest read-before-write-fails-the-build
  (let [v (check-tree out-of-order-tree)]
    (is (= :dead-read (:violation v))
        "a read whose producer runs LATER in the sequence must be caught")
    (is (= {:node :wf/reader :reads-unproduced :bb/x} (first (:details v)))
        "flat 'produced anywhere' checks pass this — ordering-awareness catches it")))

(deftest parallel-siblings-are-isolated
  (let [v (check-tree parallel-cross-read-tree)]
    (is (= [{:node :wf/q :reads-unproduced :bb/a}] (:details v))
        "a parallel sibling reading another branch's write is a race — caught;
         the downstream :wf/join reading both AFTER the parallel is legal")))

(deftest map-each-binds-item-and-threads-writes
  (is (nil? (check-tree map-each-tree))
      "the per-item body may read the :as key and later steps see earlier writes"))

(deftest enrichment-overlay-carries-meaning
  (let [before @registry/registry]
    (try
      (load-ontology!)
      (ingest-tree! good-tree
                    {:wf/b {:aspects #{:operation/judge :judge/grounding}
                            :props   {:node/mitigates #{:failure.orc/hallucination}}}})
      (is (= #{:failure.orc/hallucination} (:node/mitigates (entity/props-for :wf/b)))
          "enrichment props ride on the machine-ingested entity")
      (is (contains? (entity/identity-for :wf/b) :judge/grounding)
          "enrichment aspects become part of the compound identity")
      (finally (reset! registry/registry before)))))
