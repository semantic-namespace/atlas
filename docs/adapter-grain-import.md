# Grain Import: grain-todo-list Case Study

> **Status: Experimental.** The import leg of the [Grain adapter](adapter-grain.md)
> (roadmap item 8, executed 2026-07-10). Importer: `import-catalog!` in
> `atlas.adapter.grain` (`examples/grain/src/atlas/adapter/grain.clj`).
> Target: [grain-todo-list](https://github.com/ObneyAI/grain-todo-list) — a
> real grain application we did not author (two services, auth, sessions,
> Datastar UI).

Where the counter demo shows Atlas *authoring* a grain app from scratch, the
import leg is the **adoption path**: take an existing grain application and
lift it into the Atlas registry — data-flow, invariants, and cloud tooling
included — without touching its source.

## Two-phase design

**Phase 1 — capture** (runs in the app's own JVM; needs nothing but grain):
a short script requires the service namespaces (populating grain's global
registries at load time), then writes a plain-EDN capture:

- the five kind registries, opts sans functions, `:authorized?` reduced to a
  presence flag
- each handler's source `{:file :line}` from var metadata
- schema classification straight from the app's own `defschemas` var names
  (`event-schemas`, `command-schemas`, …) — no guessing which schema is an
  event
- the malli forms themselves (EDN-round-trippable ones only)

**Phase 2 — import** (runs on the atlas side): `import-catalog!` turns the
capture into registered atlas entities and returns a report.

## Inference policy — facts, drafts, and flagged heuristics

Three tiers, kept strictly apart:

1. **Facts** (imported as-is): kind aspect from which registry the entry
   lives in; `:domain/<ns>` from the grain name; `:effect/write|read` from
   kind; `:access/enforced` from `:authorized?` presence; declared consumes
   (read models' `:events`, processors' `:topics`); `:version`, schedules,
   schemas.
2. **Drafts** (explicitly unreviewed): no `:operation/*` / `:entity/*`
   aspects are guessed — inventing semantics from name-tokenizing would
   pollute the vocabulary. Every imported entity carries a `:draft/<name>`
   aspect instead: compound-ids stay mechanically unique, and the aspect
   doubles as a work queue for human/LLM enrichment via `suggest-placement`
   and `refactor-aspect`.
3. **Flagged heuristics**: produces (commands), reads (queries/commands),
   and dispatches (processors) are not declared anywhere in grain — they are
   recovered by slicing each handler's source (file/line from the capture,
   sliced to the next definition) and scanning for occurrences of registered
   event / read-model / command names. Entities carrying scan-derived
   data-flow are marked `:import/scanned-dataflow true`.

## Results on grain-todo-list

73 entities: 28 events, 27 commands, 12 queries, 4 read models,
2 processors; 33 with scan-recovered data-flow. Snapshotted as
`grain/todo-list/main/v0.1.0` (14 fn-free meta entities + 73 app entities).

- **All error-level invariants pass** — every command and query has a real
  `:authorized?` predicate. A properly gated app, in instructive contrast to
  grain's own example-service where every command is `(constantly true)`.
- **One real finding:** `:event.user/logged-in` is an **orphan** — produced
  by `:command.user/login`, consumed by nothing. Two independent invariant
  engines converge on it: the grain ontology's `grain-orphan-events` and
  core's pre-existing `internal-fn-outputs-consumed`.
- Data-flow is answerable through the standard cloud tool surface:

  ```
  consumers-of :event.todo/task-captured → :read-model.todo/tasks
  producers-of :event.user/logged-in     → :command.user/login
  consumers-of :event.user/logged-in     → (none)
  ```

## Two integration lessons (both encoded in the importer)

These cost a debugging session each; they are now behavior, not lore.

**1. The trace tools read data keys, not type-refs.**
`consumers-of` / `producers-of` / `trace-data-flow` build their index from
`:execution-function/context` and `:execution-function/response`
(`atlas.ide.trace`), not from the datalog edges that type-refs produce. The
fix is semantically honest rather than a workaround: *events are data*, so
the importer mirrors produced events into `:execution-function/response`
(commands) and consumed events into `:execution-function/context` (read
models, processors). No key-mixing conflicts arise: payload context exists
only on commands/queries; event-consumption context only on read models and
processors. `:grain/produces`/`:grain/consumes` remain the grain-facing
declaration that `materialize!` projects.

**2. Snapshots must carry their ontology descriptors.**
Under `cloud--version`, `context-for` resolves dataflow keys through the
ontology entry (`:ontology/for :atlas/execution-function`) found **in the
pulled snapshot** — if the snapshot holds only app entities, every trace
tool silently returns empty. This is exactly the gap the
tooling audits, and why production registries carry their
`:ontology/for` entries as data. Exports must
include the fn-free meta entities (ontology descriptors + type-refs)
alongside app entities.

## Reproducing

```bash
# Phase 1 — in the grain app checkout (grain-todo-list); edit the config
# map in the script for other apps:
clojure -M:dev -e "(load-file \"<atlas>/examples/grain/scripts/capture.clj\")"

# Phase 2 — in examples/grain:
clojure -M -e "
(require '[atlas.adapter.grain :as adapter] 'atlas.ontology.grain
         'atlas.ontology.execution-function 'atlas.ontology.data-schema)
(adapter/import-catalog! (read-string (slurp \"capture.edn\")))
(require '[atlas.invariant :as inv]) (inv/report)"
```

## Known limits & follow-ups

- The source scan treats *mention as use*: an event keyword appearing in a
  handler (e.g. in a comparison) is counted as produced. Events built
  through helper functions are missed. The observed-vs-declared invariant
  (roadmap H2) is the principled replacement — audit declarations against
  the event store itself.
- `:draft/*` enrichment is the designed human/LLM follow-up: pick a domain,
  run `suggest-placement` per entity, replace drafts with real vocabulary.
- The counter demo app predates the data-key mirroring and should adopt it
  (`test/app/grain_counter.clj`, `examples/grain/src/grain_demo/counter.clj`).
- Both lessons above belong in the roadmap's `exportable-snapshot` fn (H1.3)
  so pushes are correct by construction.
