(ns orc-demo.evaluation
  "PoC part 2 — ORC's REAL evaluation suite
   (components/evaluation/src/ai/obney/orc/evaluation/core/sheets.clj) as Atlas
   entities. Demonstrates:

   - a concept HIERARCHY (get-hierarchy): four failure modes roll up under
     :failure.orc/output-quality via :concept/broader;
   - the KILLER 3-way join: every workflow node mitigating any failure UNDER
     output-quality — hierarchy (knowledge) + mitigates (link) + tree
     (structure), across workflows, in one query.

   Audit finding (2026-07-13): the prior version of this file modeled
   `run-judges`/`aggregate` as ONE node instance literally shared (via
   :behavior-tree/children) between `evaluation-suite` and `evaluation-batch-suite`. Real
   ORC has no cross-sheet node-sharing primitive — every `dsl/workflow` call
   builds an independent tree. Verified against source: `batch-evaluation-suite`
   RE-DECLARES its own `parallel \"run-judges\"` + `code \"aggregate\"` inside a
   `sequence \"evaluate-one\"`, nested inside `map-each \"evaluate-all\"` (three
   levels — map-each > sequence > parallel — not two), reading `:current-trace`
   (the real :as binding; the prior version wrongly used :trace-data) and
   writing `:current-aggregate`, not `:aggregate-result`. Rebuilt below as two
   INDEPENDENTLY-DECLARED, structurally-near-identical trees — which is a more
   accurate AND more interesting demonstration: `cloud-near-intent` /
   `suggest-placement` can now surface the batch judges as genuine structural
   near-duplicates of the suite judges, which is what ORC's own
   `find-trees-for-problem` gestures at, done as a real reuse-detection query
   instead of an invented shared node.

   Each judge :code leaf mitigates the failure mode it checks for — grounding ↔
   hallucination, instruction ↔ instruction-violation, etc. — in BOTH trees;
   the judges ARE the structural realisation of the failure taxonomy."
  (:require [atlas.registry :as registry]
            [atlas.ontology :as ontology]
            [atlas.ontology.behavior-tree]))

(defn init-evaluation!
  []
  ;; --- KNOWLEDGE: the failure taxonomy (risk-failure-mode + :concept/broader) ---
  (registry/register!
   :failure.orc/output-quality :atlas/risk-failure-mode
   #{:domain/orc :failure/quality}
   {:risk-failure-mode/detection "the LLM's output is low-quality along some dimension"})

  (registry/register!
   :failure.orc/instruction-violation :atlas/risk-failure-mode
   #{:domain/orc :failure/compliance}
   {:risk-failure-mode/detection "output ignores or violates the given instruction"
    :concept/broader #{:failure.orc/output-quality}})

  (registry/register!
   :failure.orc/reasoning-error :atlas/risk-failure-mode
   #{:domain/orc :failure/reasoning}
   {:risk-failure-mode/detection "the reasoning chain is invalid or unsupported"
    :concept/broader #{:failure.orc/output-quality}})

  (registry/register!
   :failure.orc/omission :atlas/risk-failure-mode
   #{:domain/orc :failure/completeness}
   {:risk-failure-mode/detection "the output omits required parts of the answer"
    :concept/broader #{:failure.orc/output-quality}})

  ;; --- STRUCTURE: evaluation-suite's own 4 judges (real dsl/code fn calls) -------
  (registry/register!
   :bt.eval/grounding-judge :atlas/execution-function
   #{:behavior-tree/leaf :behavior-tree/code :domain/orc :workflow/evaluation-suite :operation/judge :judge/grounding}
   {:execution-function/context [:bb/trace-data] :execution-function/response [:bb/grounding-result]
    :node/mitigates #{:failure.orc/hallucination}})

  (registry/register!
   :bt.eval/instruction-judge :atlas/execution-function
   #{:behavior-tree/leaf :behavior-tree/code :domain/orc :workflow/evaluation-suite :operation/judge :judge/instruction-following}
   {:execution-function/context [:bb/trace-data] :execution-function/response [:bb/instruction-result]
    :node/mitigates #{:failure.orc/instruction-violation}})

  (registry/register!
   :bt.eval/reasoning-judge :atlas/execution-function
   #{:behavior-tree/leaf :behavior-tree/code :domain/orc :workflow/evaluation-suite :operation/judge :judge/reasoning}
   {:execution-function/context [:bb/trace-data] :execution-function/response [:bb/reasoning-result]
    :node/mitigates #{:failure.orc/reasoning-error}})

  (registry/register!
   :bt.eval/completeness-judge :atlas/execution-function
   #{:behavior-tree/leaf :behavior-tree/code :domain/orc :workflow/evaluation-suite :operation/judge :judge/completeness}
   {:execution-function/context [:bb/trace-data] :execution-function/response [:bb/completeness-result]
    :node/mitigates #{:failure.orc/omission}})

  (registry/register!
   :bt.eval/run-judges :atlas/execution-function
   #{:behavior-tree/parallel :domain/orc :workflow/evaluation-suite}
   {:behavior-tree/children [:bt.eval/grounding-judge :bt.eval/instruction-judge
                  :bt.eval/reasoning-judge :bt.eval/completeness-judge]})

  (registry/register!
   :bt.eval/aggregate :atlas/execution-function
   #{:behavior-tree/leaf :behavior-tree/code :domain/orc :workflow/evaluation-suite :operation/aggregate}
   {:execution-function/context [:bb/grounding-result :bb/instruction-result
                                 :bb/reasoning-result :bb/completeness-result]
    :execution-function/response [:bb/aggregate-result]})

  (registry/register!
   :bt.eval/root :atlas/execution-function
   #{:behavior-tree/sequence :domain/orc :workflow/evaluation-suite}
   {:behavior-tree/inputs #{:bb/trace-data}
    :behavior-tree/children [:bt.eval/run-judges :bt.eval/aggregate]})

  ;; --- STRUCTURE: evaluation-batch-suite's OWN 4 judges — a separate tree, not
  ;; a reference to the suite's nodes. Same :fn targets, same :writes key names,
  ;; but :reads :current-trace (the map-each item binding) instead of :trace-data.
  (registry/register!
   :bt.eval/grounding-judge-batch :atlas/execution-function
   #{:behavior-tree/leaf :behavior-tree/code :domain/orc :workflow/evaluation-batch-suite :operation/judge :judge/grounding}
   {:execution-function/context [:bb/current-trace] :execution-function/response [:bb/grounding-result]
    :node/mitigates #{:failure.orc/hallucination}})

  (registry/register!
   :bt.eval/instruction-judge-batch :atlas/execution-function
   #{:behavior-tree/leaf :behavior-tree/code :domain/orc :workflow/evaluation-batch-suite :operation/judge :judge/instruction-following}
   {:execution-function/context [:bb/current-trace] :execution-function/response [:bb/instruction-result]
    :node/mitigates #{:failure.orc/instruction-violation}})

  (registry/register!
   :bt.eval/reasoning-judge-batch :atlas/execution-function
   #{:behavior-tree/leaf :behavior-tree/code :domain/orc :workflow/evaluation-batch-suite :operation/judge :judge/reasoning}
   {:execution-function/context [:bb/current-trace] :execution-function/response [:bb/reasoning-result]
    :node/mitigates #{:failure.orc/reasoning-error}})

  (registry/register!
   :bt.eval/completeness-judge-batch :atlas/execution-function
   #{:behavior-tree/leaf :behavior-tree/code :domain/orc :workflow/evaluation-batch-suite :operation/judge :judge/completeness}
   {:execution-function/context [:bb/current-trace] :execution-function/response [:bb/completeness-result]
    :node/mitigates #{:failure.orc/omission}})

  (registry/register!
   :bt.eval/run-judges-batch :atlas/execution-function
   #{:behavior-tree/parallel :domain/orc :workflow/evaluation-batch-suite}
   {:behavior-tree/children [:bt.eval/grounding-judge-batch :bt.eval/instruction-judge-batch
                  :bt.eval/reasoning-judge-batch :bt.eval/completeness-judge-batch]})

  (registry/register!
   :bt.eval/aggregate-batch :atlas/execution-function
   #{:behavior-tree/leaf :behavior-tree/code :domain/orc :workflow/evaluation-batch-suite :operation/aggregate}
   {:execution-function/context [:bb/grounding-result :bb/instruction-result
                                 :bb/reasoning-result :bb/completeness-result]
    :execution-function/response [:bb/current-aggregate]})

  ;; the per-item body: sequence "evaluate-one" — real nesting level the prior
  ;; version skipped (map-each pointed straight at parallel+aggregate before).
  (registry/register!
   :bt.eval/evaluate-one :atlas/execution-function
   #{:behavior-tree/sequence :domain/orc :workflow/evaluation-batch-suite}
   {:behavior-tree/children [:bt.eval/run-judges-batch :bt.eval/aggregate-batch]})

  ;; map-each binds its item key (:behavior-tree/as) for the per-item body, which runs
  ;; sequentially — ORC's :from/:as/:into semantics. Real :as is :current-trace.
  (registry/register!
   :bt.eval/batch-suite :atlas/execution-function
   #{:behavior-tree/map-each :domain/orc :workflow/evaluation-batch-suite}
   {:behavior-tree/inputs #{:bb/traces}
    :behavior-tree/as :bb/current-trace
    :behavior-tree/children [:bt.eval/evaluate-one]})

  (ontology/register-entity-types!)
  :done)
