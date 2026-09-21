# ORC Adapter (Design + PoC)

> **ORC** (ObneyAI) is a behavior-tree workflow engine on Grain — *"a behavior
> modeling framework more than an LLM framework."* This doc argues that Atlas is
> ORC's natural **definition layer**, and it leads with the two claims that
> matter: (1) ORC's two separate ontology stores — workflow structure and
> knowledge concepts — become **one queryable graph**, enabling queries neither
> store can answer; (2) most of ORC's bespoke `ontology-mcp` tooling becomes
> **deletable** — replaced by generic registry queries. A working PoC
> (`examples/grain/src/orc_demo/`) backs every claim, including runs against
> ORC's *literal* workflow code.

## The headline: two stores → one graph

ORC keeps workflow structure (trees, sheets) in one store and knowledge
(failure/problem concepts) in another, with no join between them. In Atlas both
are entities in one registry, so **structure and knowledge query against each
other**. The proof, verified against ORC's real workflows:

- **★ 3-way join** — *every node, across all workflows, that guards any failure
  under `output-quality`*:
  ```
  [?n :node/mitigates ?f] [?f :concept/broader :failure.orc/output-quality]
  => escalate→hallucination (contract-analysis), grounding-judge→hallucination,
     instruction-judge→instruction-violation, reasoning-judge→reasoning-error,
     completeness-judge→omission, validate-schema→hallucination, …
     ;; 7 nodes, 3 workflows — hierarchy + knowledge + structure in ONE query
  ```
  `escalate` (contract-analysis), `grounding-judge` (evaluation) and
  `validate-schema` (csv-to-ontology) all surface as hallucination-guards —
  a cross-workflow, cross-store insight neither ORC store can produce.
  *"Every guard against hallucination across my whole system"* is one query.
- **Concept hierarchy** (ORC's `get-hierarchy`): failures under `output-quality`
  → `{hallucination, instruction-violation, reasoning-error, omission,
  ambiguity}` — a `:concept/broader` type-ref traversal.
- **Shared subbehavior** (ORC's `find-trees-for-problem`): workflows sharing
  `run-judges` → `{evaluation-suite, evaluation-batch-suite}` — cross-workflow
  reuse as a plain graph query.
- A failure's `prevention-strategy` (*"escalate to human when confidence is
  low"*) is **structurally backed** by the actual `escalate` node — a checkable
  link, not prose.

## The deletion pitch: ontology-mcp tools → registry queries

Against ORC's `ontology-mcp` (13 tools), roughly **ten become derived**:

- **Memory tools** (`get-tree-profile`, `get-node-patterns`,
  `find-trees-for-problem`) and **execution tools** (`list-available`,
  `get-description`, `find-for-problem`) → `entity-detail` /
  `suggest-placement` / `near-intent` / `entities-by-type` /
  `producers-of` / `consumers-of`. Bespoke code → registry queries, and they
  work **cross-workflow**, which the per-tree originals don't.
- **Knowledge tools** (`get-concept`, `list-concepts`, `get-hierarchy`) →
  concepts are entities + aspects + type-refs; a taxonomy is exactly what an
  extensible ontology system holds. And unified with structure (above).
- **The one genuine bolt-on**: `search-concepts` / `hybrid-search` is
  embedding/text retrieval. Atlas models the concepts and relationships
  natively; free-text search stays an **embedding-index adapter** over concept
  descriptions — the same shape as every other Atlas adapter (meaning in Atlas,
  an engine consumes a projection), not a modelling gap.

## Why ORC fits Atlas almost for free

ORC's own DSL reference says a workflow is *"defined as data structures"* with
`:reads`/`:writes` contracts and *"deterministic identity, stable across
rebuilds."* That **is** Atlas's model — dataflow + compound identity. The
mapping is mostly reuse:

| ORC | Atlas | verdict |
|---|---|---|
| leaf (`:reads`/`:writes` + `:llm`/`:code` executor) | `:atlas/execution-function`; `:reads`→`:execution-function/context`→`:entity/consumes`, `:writes`→`response`→`:entity/produces` | **reuse** |
| `:llm` leaf | `:atlas/llm-prompt` (GEPA / judge = meta-ops over the prompt entity) | **reuse** |
| blackboard keys | data keys (`:bb/*`) / `:atlas/data-schema` | **reuse** |
| failure concept (`failure:Hallucination`) | **`:atlas/risk-failure-mode`** — already richer than ORC's concept schema (`detection`, `prevention-strategy`, `triggered-by`, `business-impact`) | **reuse** |
| tree structure (`:sequence`/`:fallback`/`:map-each`, child order) | a small `:bt/children` type-ref + node-kind aspects (`:bt/leaf`, `:bt/sequence`…) | **new (small)** |
| node ↔ concept link | a `:node/mitigates` type-ref | **new (small)** |

Both new type-refs are sourced at `:atlas/execution-function`, so the **generic
extractor picks them up with no core change** (see `ontology-extensibility.md`);
both are tagged `:domain/orc` for provenance. For an adopter this means: *you
never wait on Atlas maintainers merging your PR* — the whole ORC ontology
(3 type-refs + 1 invariant) loads from ORC's own namespace.

## The on-ramp: a CI gate that fills ORC's own TODO

Honest context first: ORC already ships a validator
(`mcp-sheet-builder/core/validator.clj`) whose `check-blackboard-coverage`
verifies *reads ∪ writes ⊆ blackboard*. Two gaps in it, both real:

1. It runs only on **LLM-generated sheets** inside the builder workflow —
   hand-written workflows (`evaluation-suite`, `csv-to-ontology`, …) go through
   no gate at all.
2. Its data-flow check is **stubbed**: *"Simplified check — full data flow
   analysis would be more complex"* — it returns valid unconditionally.

The Atlas gate (`orc-demo.contract-check`) is that stub, implemented, for every
workflow: **ordering-aware** contract completeness. Every read must be available
*at the point the node runs* — written by an earlier sibling in an enclosing
`:sequence`, by an ancestor, or declared an input. `:parallel` siblings cannot
observe each other's writes (that's a race, not a dataflow); `:fallback`
siblings may never have run; `:map-each` bodies run per-item sequentially with
the item key (`:as`) bound. A node may read keys it itself writes (the
read-modify-write / load-if-absent idiom — ORC's real `load-csv` declares
`:reads [:csv-path :csv-data] :writes [:csv-data]`, and running the gate on
real code is what surfaced the idiom).

**Run on ORC's literal code** (not a hand-authored mirror): an ORC REPL evals
the real `dsl/workflow` data and `orc->ingest` (~15 lines, consuming ORC's
native `{:node-type :reads :writes :children}` + map-each `:from/:as/:into`)
feeds it to the gate. All verified:

| real workflow | nodes | gate | injected bug | caught? |
|---|---|---|---|---|
| `evaluation-suite` | 7 | passes | `aggregate` reads `:grounding-reslt` (typo) | ✅ pinpointed |
| `csv-to-ontology` | 25 | passes | first leaf reads a typo'd key | ✅ pinpointed |
| `csv-to-ontology` | 25 | — | **reorder**: `detect-temporal-columns` moved before its producer `analyze-structure` | ✅ `{:node detect-temporal-columns :reads-unproduced :bb/column-analysis}` |

The third row is the one ORC's coverage check **cannot** catch — every key is
in the blackboard; only ordering-awareness sees that the producer now runs
later. That's a realistic refactor mistake in a 25-node tree, invisible to
per-leaf review. In ORC's CI the gate is one line after each workflow:
`(is (nil? (check-tree (orc->ingest my-workflow))))`.

Local fixtures prove each semantic class separately (6 tests, all green):
dead read, read-before-write within a sequence, parallel-sibling isolation
(with the post-join read legal), map-each item binding, and the enrichment
overlay below.

**Bonus finding (for free):** the first naive ingest gave every leaf the same
compound-id (kind + domain, no distinguisher), so Atlas *refused* to let two
"identical" nodes coexist — a real modeling bug in the ingest that Atlas's
identity-uniqueness surfaced immediately.

## Enrichment overlay: machine-fresh structure, declared-once meaning

Automated ingest produces faithful but semantically thin entities (name-derived
`:node/*` aspects, no concept links). Hand-authoring is rich but drifts. The
overlay dissolves the fork: `ingest-tree!` takes
`{dev-id {:aspects #{…} :props {…}}}` — structure is re-ingested from code on
every run (never drifts), meaning (`:node/mitigates`, `:operation/*`,
`:judge/*` aspects) is declared once and re-applied on every ingest. Verified:
the **machine-ingested** real `evaluation-suite`, enriched with two judge
links, answers the 3-way join —

```clojure
;; enrichment, declared once:
{:bt.evaluation-suite/grounding-judge
 {:aspects #{:operation/judge :judge/grounding}
  :props   {:node/mitigates #{:failure.orc/hallucination}}} …}
;; query over machine-ingested entities:
=> #{[:bt.evaluation-suite/grounding-judge   :failure.orc/hallucination]
     [:bt.evaluation-suite/instruction-judge :failure.orc/instruction-violation]}
```

The synthetic per-node name aspect is scaffolding for exactly this: it keeps
ingested nodes unique until enrichment supplies the real semantic
distinguishers.

## The PoC contents

- `orc-demo.contract-analysis` — the README's `contract-analysis` tree
  (`:bt/fallback` over a `:bt/sequence` + `escalate` leaf) plus
  `:failure.orc/hallucination`, which `escalate` `:node/mitigates`.
- `orc-demo.evaluation` — ORC's **actual** eval workflow: both suites, four
  judges under a `:parallel run-judges`, each judge mitigating the failure it
  checks; the four failures roll up under `:failure.orc/output-quality` via
  `:concept/broader`. `batch-suite` models map-each faithfully
  (`:bt/as :bb/trace-data`).
- `orc-demo.csv-ontology` — a faithful 8-node slice of the real
  `csv-to-ontology` sheet; `validate-schema` guards the **same**
  hallucination concept as the eval `grounding-judge` — the cross-workflow
  queries grow with zero new machinery.
- `orc-demo.behavior-tree` — the entire ORC ontology: 3 type-refs + the
  ordering-aware invariant. No core changes.
- `orc-demo.contract-check` — `ingest-tree!` + `orc->ingest` + `check-tree`
  (the CI gate; restores the ambient registry on exit, so it is safe inside a
  live process) + fixtures for every semantic class.

**Live registry:** `grain/orc`, layered so every version diff is purely
additive and *is* the pitch:

| version | entities | layer |
|---|---|---|
| `v0.1.0` | 17 | atlas base alone (ontology descriptors, type registrations, base type-refs) |
| `v0.2.0` | 21 | + the whole ORC ontology — 3 type-refs + the ordering-aware invariant, no core change |
| `v0.3.0` | 30 | + contract-analysis (README example) — knowledge↔structure join works |
| `v0.4.0` | 42 | + evaluation suite — failure taxonomy, judges, shared subbehavior |
| `v0.5.0` | 52 | + csv-to-ontology — cross-workflow queries scale, zero new machinery |

Browsable with `cloud-overview` / `cloud-diff` /
`cloud-branch-entities aspect=domain/orc` / the Atlas UI.

*Adapter note:* the grain `exportable-snapshot` filters app entities by grain's
kind-prefixes (`command./query./…`), so a general ORC/other-domain adapter
exports by `:domain/<x>` + meta instead (what this PoC does) — a small,
expected adapter seam, not a core change.

## Honest boundaries / migration path

- **Atlas holds structure and meaning, not execution**: the tree *ticks* in ORC
  (Grain event-sourced); Atlas defines and checks it. Same split as grain
  (execute) / allium (verify) / atlas (define).
- **Child order lives in the prop, not the graph edge.** `:bt/children` maps to
  the `:entity/depends` verb (cardinality-many, unordered) so blast-radius and
  reachability come free; the ordered vector is preserved in the registry prop
  and is what the ordering-aware invariant walks. Atlas never materializes back
  to ORC, so nothing is lost — but the edge alone does not encode sequence
  order.
- ORC's runtime **memory** (event-store-backed patterns of *what actually ran*)
  is observed history — Atlas's `observed-vs-declared` complements it but
  doesn't replace the event store.
- **Single-workflow dataflow checks are writable without Atlas** (~15 lines of
  tree-walking). The gate's point is not that the check is hard — it's that it
  is *derived* from definitions that also feed the cross-workflow queries,
  blast-radius, semantic diffs, and the knowledge join above. One definition,
  many derivations.
- Migration is incremental: ingest one workflow's structure (the reads/writes
  are already there), add concept links via the enrichment overlay, and let the
  derived checks earn trust — exactly how the grain-todo-list and counter
  rebuilds went.

## The pitch, in one line

**Define ORC in Atlas and your two ontology stores become one graph** — workflow
structure and failure/problem knowledge query against each other — while the
data-flow check your validator stubs out, dataflow analysis, blast-radius, and
semantic diffs come free from the definition, and ORC keeps its executor, GEPA,
judges, and (if wanted) an embedding index as adapters over the Atlas
definitions.

## Related

- `examples/grain/src/orc_demo/README.md` — the 5-minute try-it path.
- `docs/ontology-extensibility.md` — why the new type-refs need no core change.
- `docs/adapter-grain.md`, `docs/adapter-allium.md` — the same "reuse the base,
  add one seam" discipline for grain and allium.
- Live registry: `grain/orc` (`v0.1.0` → `v0.5.0`, layered).
