# ORC Execution Bridge — Path B: Ephemeral RLM Trees (Design + Proof)

> Companion to `adapter-orc.md`, which covers the **read direction**: mirroring
> ORC's real trees into Atlas for analysis (`orc->ingest`), Atlas as a passive
> semantic layer over code that runs elsewhere. This doc covers the **write
> direction**: Atlas as the *authoring source*, with ORC's own engine actually
> executing what Atlas defines — proven against ORC's real, unmodified
> `execute-tree` entrypoint, not a mock. This is "Path B". "Path A" — Atlas
> driving ORC's other lane, *persisted* production-shaped workflows
> (`build-workflow!`/`execute`), including full `fallback`/`condition` support
> this path doesn't have — is written up separately and not yet published.

## The headline

A `:behavior-tree/*` entity already living in the Atlas registry —
`:retry/attempt-call`, `#{:behavior-tree/leaf :behavior-tree/code
:domain/resilience :operation/call}`, `:execution-function/context
[:bb/request]` — was reshaped by a small adapter into ORC's canonical DSL and
handed to ORC's real `execute-tree` (`rlm_tree_executor.clj`). It ran for
real: real ephemeral sheet created via real grain commands, real event-sourced
tick, real async dispatch through ORC's own todo-processors, real function
invocation.

```clojure
;; the exact result, unedited
{:status :success
 :outputs {:request "payment-gateway"
           :response "called-payment-gateway"
           :call-error nil}}
```

This mirrors what `atlas.adapter.tilakone` already does for `:atlas/workflow`
(atlas definition → real tilakone FSM execution) — the same pattern now has a
proof-of-concept for the behavior-tree side, targeting ORC's own engine
instead of a general-purpose library (no generic Clojure BT engine besides
[aido](https://github.com/mmower/aido) was found; ORC is the real, working
one already in hand).

## The adapter: `bt->orc-sexpr`

~25 lines. Walks `:behavior-tree/children` from a root dev-id and emits
ORC's native s-expression DSL — the same shape `sheet/code`/`sheet/sequence`
macros in ORC's own `dsl.clj` produce, skipped entirely (the executor consumes
plain list-and-symbol data, not macro-expanded forms):

```clojure
(defn bt->orc-sexpr
  [dev-id fn-table]
  (let [id     (entity/identity-for dev-id)
        props  (entity/props-for dev-id)
        kids   (:behavior-tree/children props)
        reads  (mapv #(keyword (name %)) (:execution-function/context props))
        writes (mapv #(keyword (name %)) (:execution-function/response props))]
    (cond
      (contains? id :behavior-tree/sequence)
      (apply list 'sheet/sequence (map #(bt->orc-sexpr % fn-table) kids))

      (contains? id :behavior-tree/parallel)
      (apply list 'sheet/parallel (map #(bt->orc-sexpr % fn-table) kids))

      (and (contains? id :behavior-tree/leaf) (contains? id :behavior-tree/code))
      (list 'sheet/code :reads reads :writes writes :fn (get fn-table dev-id))

      (and (contains? id :behavior-tree/leaf) (contains? id :behavior-tree/llm))
      (list 'sheet/llm :reads reads :writes writes :instruction (:llm-prompt/instruction props ""))

      ;; :behavior-tree/fallback, :behavior-tree/condition — see Scope Boundary below
      :else
      (throw (ex-info "Unsupported node kind" {:dev-id dev-id :identity id})))))
```

`reads`/`writes` come straight from `:execution-function/context`/`:response`
— the same primitive shared with `:atlas/workflow`'s wrapped functions (see
the FSM-vs-BT discussion this doc's design grew out of). `:bb/*` namespacing
is stripped back to ORC's bare-keyword blackboard convention on the way out
(the inverse of what `orc->ingest` adds on the way in).

## Three real findings along the way (ORC/grain-internal, not Atlas-specific)

Getting a *correct* result took more than writing the reshaper — three real
gaps in exercising ORC's engine outside its own test harness, each confirmed
by direct evidence, not guesswork:

1. **`create-test-context` is LMDB-backed**, and `kv-store-lmdb`'s native lib
   needs glibc 2.36+; this host has 2.31 (`UnsatisfiedLinkError:
   GLIBC_ABI_DT_RELR not found`). Same wall hit earlier with the grain
   materializer demo. Fix: substitute the atom-based `MemKV` record from
   `examples/grain/src/grain_demo/mem_kv.clj` (already proven), implementing
   `ai.obney.grain.kv-store.interface.protocol/KVStore` directly.
2. **`create-test-context` alone starts no pubsub or todo-processors** — only
   `create-async-test-context` does. Without them, a tick command is recorded
   as an event but nothing ever consumes it: silent `{:status :timeout}` after
   the full timeout budget, no error. Fix: replicate
   `create-async-test-context`'s processor-starting loop
   (`(reduce-kv ... (tp/start ...) @tp/processor-registry*)`) by hand against
   the mem-kv-backed context — 16 processors start
   (`execute-leaf-node`, `execute-composite-node`, `update-blackboard`, …).
3. **ORC's own auto-inferred blackboard schema for numbers is invalid Malli**
   — `execute-tree`'s inference (`(number? v) :number`) produces `:schema
   :number`, which isn't a real Malli schema. `:sheet/declare-key` rejects it
   with an anomaly that `run-command!`/`compile-tree-node` never check — so
   the failure doesn't surface until three steps downstream, as a confusing
   `Cannot invoke "Object.getClass()" because "x" is null` NPE when a node
   tries to read a value that was silently never declared. Root-caused by
   diffing raw event-store contents between a working run and a failing one
   (the failing run was simply missing a `:sheet/node-io-set` event — the
   command never landed). Real bug in ORC's own code; worked around via
   `execute-tree`'s own `:blackboard-schemas` override (`{:n :int}`) rather
   than relying on inference.

## A second example: `:behavior-tree/parallel`, and a repeat offender

`sequence` + `leaf`/`code` alone doesn't exercise composite fan-out. A
health-check tree does — sequence(root) → parallel(three independent checks)
→ aggregate leaf:

```
:health/root     #{:behavior-tree/sequence :domain/resilience :workflow/health-check}
                 children: [:health/checks :health/aggregate]

:health/checks   #{:behavior-tree/parallel :domain/resilience :workflow/health-check}
                 children: [:health/check-db :health/check-cache :health/check-queue]

:health/check-db     #{:behavior-tree/leaf :behavior-tree/code :operation/check :target/db}
                      reads: [:db-endpoint] → writes: [:db-status]
:health/check-cache  #{:behavior-tree/leaf :behavior-tree/code :operation/check :target/cache}
                      reads: [:cache-endpoint] → writes: [:cache-status]
:health/check-queue  #{:behavior-tree/leaf :behavior-tree/code :operation/check :target/queue}
                      reads: [:queue-endpoint] → writes: [:queue-status]

:health/aggregate #{:behavior-tree/leaf :behavior-tree/code :operation/aggregate}
                 reads: [:db-status :cache-status :queue-status] → writes: [:overall-status]
                 mitigates: undetected-partial-outage
```

Ran for real, one dependency deliberately down:

```clojure
{:status :success
 :outputs {:db-status :up, :cache-status :up, :queue-status :down
           :overall-status :degraded}}
```

`node-trace` confirms genuine parallel composite semantics — all three checks
complete independently, timestamps overlapping, before the aggregate runs.

**Building it hit the exact aspect-collision bug that recurred all session**
(counter test-cases, todo-list test-cases, csv-ontology leaves): the first
registration gave `:health/check-db`/`:health/check-cache`/`:health/check-queue`
the *identical* compound-id — `#{:behavior-tree/leaf :behavior-tree/code
:domain/resilience :workflow/health-check :operation/check}`, no distinguishing
aspect. They silently collapsed onto one registry slot; the reshaped output
showed all three parallel children with `check-queue`'s `reads`/`writes`. Same
fix as every prior occurrence: an extra distinguishing aspect per leaf
(`:target/db`/`:target/cache`/`:target/queue`) — a standing reminder that this
bridge inherits the registry's identity discipline, for better (no accidental
silent duplicates survive) and worse (you must actively avoid the collision).

## Scope boundary — confirmed, not assumed

`compile-tree-node` (this specific executor, `rlm_tree_executor.clj`) only
recognizes `sheet/sequence`, `sheet/parallel`, `sheet/llm`, `sheet/map-each`,
`sheet/code`, `final!`. **No `fallback`, no `condition`.** Confirmed directly:
reshaping the *full* `:retry/root` tree (which uses `:behavior-tree/fallback`
+ `:behavior-tree/condition`) fails immediately and legibly:

```
Unknown tree node type: sheet/fallback
```

That vocabulary belongs to ORC's *other* execution pipeline — the
statically-authored sheet DSL (`dsl.clj`) that runs hand-written workflows
like `evaluation-suite`/`csv-to-ontology-pipeline` (the ones mirrored via
`orc->ingest` in `adapter-orc.md`). `execute-tree`/`rlm_tree_executor.clj` is
specifically the **RLM dynamic sub-tree** pipeline — built for LLM-generated
ad-hoc trees compiled and run on the fly, a different lane than the one that
executes ORC's hand-authored sheets, despite both ultimately building on the
same sheet/node/blackboard primitives.

Practical consequence for anyone extending `bt->orc-sexpr`: `sequence` /
`parallel` (both confirmed running for real) / `code` / `llm` atlas trees can
run through this bridge today; `map-each` is supported by `compile-tree-node`
but not yet wired into the reshaper (no atlas example has exercised it).
`fallback` / `condition` atlas trees need either restructuring (e.g. compile a
condition into a `code` node whose write encodes the branch decision) or
targeting ORC's *other* executor — a different, unexplored bridge.

## Verified end to end (this session, both positive and negative)

| Tree | Source | Result |
|---|---|---|
| `(sheet/sequence (sheet/code ...))`, hand-built, `:reads []` bug | synthetic | `:status :failure` — my own bug (empty reads), correctly diagnosed via raw event-store diff |
| same, `:reads [:n]` fixed, bare `:number` schema | synthetic | `:status :failure` — ORC's own inference bug, root-caused |
| same, `:blackboard-schemas {:n :int}` | synthetic | **`:status :success`, `{:doubled 42}`** |
| full `:retry/root` (`fallback`+`condition`+2×`code`) | live atlas registry | `:status :failure` — clean, immediate, correctly-attributed `Unknown tree node type: sheet/fallback` |
| `:retry/attempt-call` alone, wrapped in `sequence` | live atlas registry | **`:status :success`, real ORC execution of a real atlas-authored entity** |
| `:health/root` (`sequence`→`parallel`×3→`code`), collision bug present | live atlas registry | wrong result — all 3 parallel children silently show `check-queue`'s `reads`/`writes` |
| same, fixed with `:target/*` disambiguating aspects | live atlas registry | **`:status :success`, `{:overall-status :degraded}` — real parallel fan-out** |

## Where this leaves the "atlas → orc" thesis

The mechanism is real and proven, not hypothetical: an atlas-authored
`:behavior-tree/*` entity can be the actual source of truth for a real ORC
execution, the same relationship `:atlas/workflow` already has with tilakone.
The gap that remains is coverage, not feasibility — `fallback`/`condition`
support would need either a second reshaper targeting ORC's other executor,
or a compilation strategy that lowers those node kinds into `code`-node
equivalents for this one.
