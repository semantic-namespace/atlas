(ns orc-demo.contract-analysis
  "PoC part 1 — ORC's contract-analysis tree as Atlas entities, re-aligned to the
   REAL, currently-shipped walkthrough (docs/GETTING-STARTED.md in the ObneyAI/orc
   checkout), not the top-level README's illustrative mermaid diagram.

   Audit finding (2026-07-13): the original version of this file modeled the
   README diagram — a :behavior-tree/fallback root with an :escalate guard, a route-by-type
   branch, :risk/:persist leaves — none of which exist as real code anywhere in
   the orc checkout (verified: zero non-DSL-definition hits for :node-type
   :condition/:llm-condition/:delegate, and no :behavior-tree/fallback-shaped
   contract-analysis tree in GETTING-STARTED.md). The real, runnable tree is a
   FLAT 5-node LLM sequence — no branching, no delegate, no code leaf. Rebuilt
   here to match it exactly (blackboard key names, :reads/:writes, and the
   reason-before-score discipline — :reasoning is always first in :writes —
   taken verbatim from the walkthrough).

   Leaves are plain :atlas/execution-function (reads=context/consumes,
   writes=response/produces). No node in this (real) tree mitigates a failure
   concept directly — :failure.orc/hallucination is kept registered here only
   because orc-demo.evaluation's grounding-judge :node/mitigates it and some
   namespace must own the concept entity; dropping it would dangle that
   reference. Ontology (type-refs + contract-complete invariant) lives in
   atlas.ontology.behavior-tree."
  (:require [atlas.registry :as registry]
            [atlas.ontology :as ontology]
            [atlas.ontology.behavior-tree]))

(defn init-contract-analysis!
  []
  ;; KNOWLEDGE: a failure concept, reusing :atlas/risk-failure-mode. Owned here
  ;; (see docstring) but not mitigated by any node in THIS tree — the real
  ;; contract-analysis walkthrough has no guard/escalation leaf.
  (registry/register!
   :failure.orc/hallucination
   :atlas/risk-failure-mode
   #{:domain/orc :failure/factuality}
   {:risk-failure-mode/detection "output asserts facts not grounded in the input contract"
    :risk-failure-mode/prevention-strategy "escalate to human when confidence is low"
    :risk-failure-mode/business-impact "wrong contract advice acted upon"
    :concept/broader #{:failure.orc/output-quality}})

  ;; STRUCTURE: the real tree is one flat sequence — no fallback, no branching.
  (registry/register!
   :bt.contract/root :atlas/execution-function
   #{:behavior-tree/sequence :domain/orc :workflow/contract-analysis}
   {:behavior-tree/inputs #{:bb/contract-v2 :bb/contract-v3}
    :behavior-tree/children [:bt.contract/survey :bt.contract/diff :bt.contract/classify
                  :bt.contract/impact :bt.contract/summarize]})

  ;; STRUCTURE: leaves — names, blackboard keys, reads/writes verbatim from
  ;; docs/GETTING-STARTED.md. All five are :behavior-tree/llm (the real tree has no
  ;; :behavior-tree/code leaf and no repl-researcher).
  (registry/register!
   :bt.contract/survey :atlas/execution-function
   #{:behavior-tree/leaf :behavior-tree/llm :domain/orc :workflow/contract-analysis :operation/extract}
   {:execution-function/context [:bb/contract-v2 :bb/contract-v3]
    :execution-function/response [:bb/reasoning :bb/document-survey]})

  (registry/register!
   :bt.contract/diff :atlas/execution-function
   #{:behavior-tree/leaf :behavior-tree/llm :domain/orc :workflow/contract-analysis :operation/diff}
   {:execution-function/context [:bb/contract-v2 :bb/contract-v3 :bb/document-survey]
    :execution-function/response [:bb/reasoning :bb/section-diffs]})

  (registry/register!
   :bt.contract/classify :atlas/execution-function
   #{:behavior-tree/leaf :behavior-tree/llm :domain/orc :workflow/contract-analysis :operation/classify}
   {:execution-function/context [:bb/section-diffs]
    :execution-function/response [:bb/reasoning :bb/major-changes]})

  (registry/register!
   :bt.contract/impact :atlas/execution-function
   #{:behavior-tree/leaf :behavior-tree/llm :domain/orc :workflow/contract-analysis :operation/analyze}
   {:execution-function/context [:bb/major-changes :bb/section-diffs]
    :execution-function/response [:bb/reasoning :bb/impact-analysis]})

  (registry/register!
   :bt.contract/summarize :atlas/execution-function
   #{:behavior-tree/leaf :behavior-tree/llm :domain/orc :workflow/contract-analysis :operation/summarize}
   {:execution-function/context [:bb/document-survey :bb/major-changes :bb/impact-analysis]
    :execution-function/response [:bb/reasoning :bb/summary]})

  (ontology/register-entity-types!)
  :done)
