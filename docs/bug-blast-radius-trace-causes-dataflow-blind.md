# blast-radius / trace-causes are blind to dataflow edges

Status: **confirmed, reproduced on the live yorba-clj registry (dev), 2026-09-21**
Affects: `:atlas.llm-ide/blast-radius`, `:atlas.llm-ide/batch-blast-radius`,
`:atlas.llm-ide/trace-causes`, `:atlas.llm-ide/explain-area`.

## Symptom

Both tools return empty or partial results for entities that have real,
registry-confirmed dependents. The report that triggered this investigation told
readers to distrust them and cross-check with `bottlenecks` or direct dependency
listings — that workaround is correct, and this explains why it works.

## Reproduction (dev yorba-clj registry)

`fn.mailing-lists/populated` — no explicit `:execution-function/deps`, consumes
`co.yorba.spec.mailing-lists/accounts`, which `fn.cache/mailing-lists-accounts`
produces.

| tool | result |
|---|---|
| `data-flow fn.mailing-lists/populated` | needs `…/accounts`, produced-by `fn.cache/mailing-lists-accounts`, satisfied |
| `trace-causes fn.mailing-lists/populated` | `{:upstream [] :components [] :failure-modes []}` — **empty** |
| `blast-radius fn.cache/mailing-lists-accounts` | affected does **not** contain `fn.mailing-lists/populated` |
| `entity-snapshot fn.cache/mailing-lists-accounts` → `:atlas/blast-radius` | affected **does** contain `fn_mailing-lists--populated` and `mcp-fn_fn_mailing-lists--populated` |

The last two rows are the same question, asked of the same registry, answered
differently by two implementations that live in the same product.

## Root cause: two dependency graphs, each missing half the edge kinds

**Graph A — registry model** (`atlas.ide.trace`, via `effective-dependencies-for`):
explicit deps (`ot/deps-for`) ∪ dataflow-derived deps (producers of my context
keys). Used by `dependents-of`, `dependencies-of`, `in-degree`/`out-degree`
(so `bottlenecks`, `change-risk`), and `entity-snapshot`'s `:atlas/blast-radius`
(`core.cljc:88` → `trace/recursive-dependents-summary`).

**Graph B — datalog model** (`atlas.datalog`): the `:entity/depends` verb only —
i.e. `:execution-function/deps` plus every type-ref whose
`:type-ref/datalog-verb` is `:entity/depends`. Used by
`query-upstream-closure` / `query-downstream-closure`
(`datalog.cljc:553,582`), which are the *only* thing `blast-radius` and
`trace-causes` call (`llm_ide.cljc:137-147, 263, 281`).

The dataflow facts **are in the same datalog DB** — the execution-function
extractor emits `:entity/consumes` / `:entity/produces`
(`ontology/execution_function.cljc`) and `query-consumers-of` /
`query-producers-of` read them. The closure queries simply never join through
them: they walk `[?e :entity/depends ?target]` and nothing else.

So any dependency that exists only because A consumes a key B produces is
invisible to blast-radius and trace-causes. For a registry where most
execution-functions chain by data key rather than by declared `deps`, that is
most of the graph.

The divergence runs **both ways**: Graph A misses type-ref-declared edges
(e.g. llm-prompt → tool, VP → BP), which is why the datalog blast-radius above
found `fn_llm-prompt--test-business-value` / `qa-inbox-search` while
entity-snapshot's did not. Neither graph is a superset of the other, and neither
is the whole truth.

## Second, independent bug: set arguments silently return empty

`blast-radius` normalises its entity argument by hand:

```clojure
changing-set (if (set? dev-id-or-set) dev-id-or-set #{dev-id-or-set})
```

A JSON array from the MCP boundary arrives as a vector, so this yields
`#{["a" "b"]}` — a set containing a vector — which matches nothing. Verified
against the local atlas-cloud server: the same call with one string dev-id
returns an affected set; with a two-element array it returns
`{:affected [] :tiers-hit #{} :domains-hit #{}}`. No error, no warning.

`llm_ide.cljc` already has `ensure-keyword-set` (line 91) for exactly this, and
applies it at lines 730, 755, 811, 915. `blast-radius` (281) and `trace-causes`
(263) are the two trace tools that skip it.

## Third, minor: `batch-blast-radius` demands `max-hops`

`blast-radius` defaults `max-hops` to 3, but calling `batch-blast-radius`
without it fails with `{:error "required arguments are missing" :missing
[":query/max-hops"]}`. The MCP boundary derives required args from the context
spec with no notion of optionality, so a defaulted parameter reads as mandatory.

## Proposed fix

1. **Make the closures dataflow-aware** (contained blast radius of its own —
   only the two closure queries change, so invariants that assert on
   `:entity/depends` keep their current meaning):
   add `query-effective-dependencies` / `query-effective-dependents` to
   `atlas.datalog`, each the union of the `:entity/depends` edge and the
   consumes/produces join, and have `query-upstream-closure` /
   `query-downstream-closure` traverse those instead.

   Rejected alternative: materialising dataflow edges as extra `:entity/depends`
   facts at DB build time. It fixes the closures but silently changes every
   invariant and adapter that reads `:entity/depends` (`adapter/overarch.clj`,
   `adapter/yaml_ld.clj`, `invariant/unified.cljc`).

2. **Normalise the entity argument** in `blast-radius` and `trace-causes` with
   the existing `ensure-keyword-set` / `ensure-keyword`.

3. Optionally, converge Graph A and Graph B on one definition of "effective
   dependency" so `entity-snapshot` and `blast-radius` can no longer disagree.
   This is the real cure; 1 and 2 are the stop-the-bleeding.

## Regression test to add

`test/atlas/llm_ide_test.clj` currently exercises blast-radius only on entities
wired with explicit `:execution-function/deps`, which is why this passed CI. A
case with two functions linked *only* by a shared data key — producer's response
key = consumer's context key, no `deps` on either — reproduces the bug in one
assertion.
