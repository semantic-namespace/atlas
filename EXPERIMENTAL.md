# Stability & Experimental Surfaces

> Atlas is **alpha** overall — see the status banner in the README. This file
> classifies stability *within* the repo so you know what to depend on.

## Stable-ish (core semantic kernel)

The compound-identity model and its primary operations are the most settled part
of Atlas and the intended public surface:

- `atlas.core` — public API facade
- `atlas.registry` — registration, compound identity
- `atlas.query` — query by aspect
- `atlas.ontology` — entity types, data-flow resolution
- `atlas.invariant` (+ `atlas.invariant.dsl.*`) — architectural validation
- `atlas.datalog` — Datascript projection & graph queries

APIs may still change before 1.0, but these are exercised by the test suite and
the example apps in `test/app/`.

## Experimental

Usable, but interfaces are unsettled and may move or be extracted into separate
modules. Marked `EXPERIMENTAL` in their namespace docstrings.

### Adapters — `atlas.adapter.*`
Bridges between Atlas and third-party libraries/tools. These are candidates to
become **separate modules** so `core` carries no optional dependencies:

| Namespace | Bridges to |
|---|---|
| `atlas.adapter.integrant` | Integrant system configs |
| `atlas.adapter.tilakone` | tilakone FSMs ↔ atlas workflows |
| `atlas.adapter.port-workflow` | `port.workflow/producer` multimethods |
| `atlas.adapter.spec-qualifier` | clojure.spec → qualified map keys |
| `atlas.adapter.overarch` | Overarch C4/UML/GraphViz export (see [docs](docs/adapter-overarch.md)) |

Note: several adapters use `requiring-resolve` and are JVM-only despite the
`.cljc` extension — they will not load under ClojureScript.

### Extension ontologies — `atlas.ontology.*`
Higher-level vocabularies beyond the built-in entity types:

- `atlas.ontology.deployment` — deployment snapshots linked to `:atlas/system`
- `atlas.ontology.shapeup` — Shape Up process artifacts
- `atlas.ontology.system` — product/application root of the business-value graph

### LLM / IDE tooling
- `atlas.llm-ide` + `atlas.llm-ide.view` — LLM-facing tools (MCP)
- `atlas.ide` + `atlas.ide.narrative`, `atlas.ide.trace` — IDE-facing tools

`atlas.ide` and `atlas.llm-ide` overlap and are a consolidation candidate.

### Tooling surfaces
- **UI** (`ui/`) — three client generations exist; **v3** (SSE browser) is the
  current direction and is experimental. v1/v2 are older PoCs.
- **Emacs** (`emacs/`) — experimental; `atlas-llm-daemon.sh` is a machine-specific
  example script, not a turnkey tool.
- **dev-tools** (`dev-tools/`) — `atlas.source-tracker` is on-demand only (no file
  watchers), assumes the JVM cwd is the repo root.

## Not part of the published library

- `cloud/` — the team-sync service (separate repo; planned as a paid service).
- `dev/` — monorepo dev harness (not shipped in any JAR).
- `docs/internal/` — internal design notes and working documents.
- `js/` — ClojureScript→ES-module build of core (see `js/README.md`).
