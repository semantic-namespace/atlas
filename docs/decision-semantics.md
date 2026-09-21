# Decision Semantics — Onboarding

> For a developer who wants to **reuse** existing decisions or **author** new
> ones. Ontology source: `core/src/atlas/ontology/decision.cljc`. Authoring
> assistant: the `/author-decision` skill. Live examples:
> `atlas/core@snap-003` (5 decisions, 2 authorities).

## 1. What a decision is (and what it is NOT)

A **decision** is an engine-neutral entity: a *question* answered by selecting
from a **closed, business-meaningful outcome alphabet**. That closedness is the
whole point — it's what separates a decision from an ordinary computation.

```
:atlas/decision       → "eligible / referred / rejected"   (selects an outcome)
:atlas/execution-function → "risk score = 0.42"            (computes a value)
```

**The gate**: if you can only name *one* outcome, or the result is an
open-ended value/document/score, it's a **computation** — register an
`:atlas/execution-function`, not a decision. The `decision-outcomes-closed`
invariant enforces `≥ 2` outcomes; the `/author-decision` skill checks it
conversationally before you ever register.

The decision is deliberately **separate from the engine that runs it** (the
DMN principle). The same decision can be realized by an ORC behavior-tree
condition, an FSM transition, a grain command's outcome set, or a phase of an
LLM prompt — via `:decision/realized-by`.

## 2. The six parts

| Prop | Meaning | Example |
|---|---|---|
| compound identity | the question, named by aspects | `#{:atlas/decision :domain/resilience :decision/routing}` |
| `:decision/question` | prose phrasing | `"Did the downstream call succeed?"` |
| `:decision/inputs` | information required (a `:dataflow/context-key`, so trace tools see the decision as a consumer) | `[:resilience/call-error]` |
| `:decision/outcomes` | the **closed alphabet** (≥2) | `#{:outcome/proceed :outcome/fall-back}` |
| `:decision/logic-style` + `:decision/logic-body` | how the answer is chosen | `:predicate` + `{:key :call-error :op :equals :value nil}` |
| `:decision/mandated-by` / `:decision/mitigates` / `:decision/realized-by` | authority / risk / engine links | see §4 |

Logic-style options: `:predicate` (a `{:key :op :value}` map), `:table`
(rows), `:llm` (an instruction string — the prompt *is* the logic), `:code`
(a fully-qualified fn symbol string).

## 3. Reuse first — before authoring anything

Decisions live in the registry to be **found, read, and reused**. Always check
what exists before minting new vocabulary:

```
by-aspect          query/aspect: decision--verdict      ;; existing decisions of a kind
by-aspect          query/aspect: outcome--reject        ;; who already yields this outcome
suggest-placement  entity/intended-aspects: [...]       ;; overlap with existing decisions
aspect-anomalies                                        ;; near-duplicate outcome names
```

Two reuse rules that matter:
- **Reuse outcome keywords.** If `:outcome/reject` exists, don't mint
  `:verdict/rejected`. The `by-aspect` check on your candidate outcome is the
  guardrail.
- **`suggest-placement` overlap ≥ 0.6** with an existing decision → you may be
  duplicating it. Extend/supersede rather than add.

Query the graph for cross-cutting insight the individual engines can't give:
- *"Every decision that can yield `:outcome/reject`"* → one `by-aspect` (each
  outcome is extracted as a `[?d :decision/outcome ...]` fact).
- *"Every decision mandated by policy X"* → `:decision/mandated-by` traversal.
- *"Every decision guarding failure mode Y"* → `:decision/mitigates` traversal.

## 4. Governance and realization links

- `:decision/mandated-by` → an **`:atlas/decision-authority`** (the policy /
  regulation / protocol / heuristic that requires the decision). Register the
  authority in the same pass if it doesn't exist (kind/statement/source).
- `:decision/mitigates` → an `:atlas/risk-failure-mode` it guards against
  (same join shape as the ORC PoC's `:node/mitigates`).
- `:decision/realized-by` → the engine entity that executes it
  (**cardinality-many** — one decision, N realizations across engines). A
  not-yet-ingested target is fine; it warns, doesn't error.

## 5. The rules you must satisfy (invariants)

| Invariant | Severity | Checks |
|---|---|---|
| `decision-outcomes-closed` | error | ≥ 2 outcomes — the decision-vs-computation gate |
| `decision-terminal-requires-reason` | error | superseding/retiring needs `:decision/retirement-reason` (institutional memory) |
| `authority-terminal-requires-reason` | error | a revoked authority needs its reason |
| `decision-refs-resolve` | warning | `realized-by`/`mandated-by`/`mitigates` targets resolve; `mandated-by` is an authority |
| `decision-active-has-realization` | warning | `:status/active` decisions run somewhere — use `:status/proposed` for aspirational ones |

Lifecycle aspects (reused from the shapeup convention):
`:status/proposed → :status/active → :status/superseded | :status/retired`.

## 6. Authoring — the fast path

Run **`/author-decision`**. It walks the 7 phases (question → decision-or-
computation gate → inputs → outcome-vocab reuse check → authority/risk →
logic-body + realizations → validate + `cloud-propose`) and won't let you
register a single-outcome "decision" or an unchecked outcome keyword.

Manual shape, if you prefer:

```clojure
(registry/register!
 :decision.<domain>/<name> :atlas/decision
 #{:domain/<x> :decision/<kind> :status/proposed}   ;; :active only once realized
 {:decision/question    "<one sentence>"
  :decision/inputs      [:<ns>/<key> ...]
  :decision/outcomes    #{:<ns>/<a> :<ns>/<b>}       ;; ≥ 2, reuse-checked
  :decision/logic-style :predicate                    ;; or :table / :llm / :code
  :decision/logic-body  {:key :<k> :op :equals :value nil}
  :decision/mandated-by #{:authority.<x>/<name>}
  :decision/realized-by #{:<engine-entity>}})         ;; omit if none yet → :status/proposed
```

## 7. Worked examples (all live in `atlas/core@snap-003`)

- **`:decision.resilience/call-succeeded`** — `:predicate`, alphabet
  `#{:outcome/proceed :outcome/fall-back}`, realized-by the ORC retry-tree's
  `check-success` condition, mandated-by the circuit-breaker heuristic. Shows
  a decision whose logic body is lifted verbatim from a real engine node.
- **`:decision.review/atlas-review-verdict`** — `:llm`, alphabet
  `#{:verdict/approve :verdict/reject}`, realized-by the `atlas-review` prompt.
  Shows a prompt phase modeled as a decision. (Its superseded predecessor
  `...-with-notes` carries a `:decision/retirement-reason` documenting why the
  three-way alphabet was collapsed — read it as an example of the terminal
  invariant in action.)
- **`:decision.ontology/reuse-verdict`** — `:llm`, alphabet
  `#{:verdict/reuse-candidate :verdict/neighbor :verdict/novel}`, realized-by
  the `define-ontology` prompt. This is the reuse gate from §3, modeled as a
  decision.

## 8. Mental model in one line

Atlas doesn't *make* decisions — it makes every decision a **first-class,
queryable address**: findable on the system graph, readable down to its
outcome alphabet and governance links, and callable through whichever engine
realizes it.
