# Atlas Documentation

> **Status: Proof of Concept (Alpha).** APIs may change.

Atlas is a semantic registry for software architecture: describe your system's
components, functions, and relationships as **compound identities** (sets of
semantic aspects), then query, validate, and visualize your architecture as a
single queryable graph.

## Start here

| Doc | What |
|---|---|
| [Getting Started](getting-started.md) | 5-minute tutorial |
| [Core Concepts](concepts.md) | Compound identities, tiers, data flow |
| [API Reference](api-reference.md) | Function reference |
| [Examples](examples.md) | Annotated example registries |

## Guides

| Doc | What |
|---|---|
| [Authoring Guide](authoring-guide.md) | Building entity definitions interactively |
| [Compound-ID Refactoring](compound-id-refactor.md) | Reviewing/improving compound-ids to avoid drift |
| [Dependency Traversal](dependency-traversal.md) | Forward/reverse recursive dependency analysis |
| [Semantic Aggregation](semantic-aggregation.md) | Rolling aspects up the dependency graph |
| [Declared Dispatch](declared-dispatch.md) | The Atlas execution pattern |
| [Source Tracker](source-tracker-design.md) | Correlating registry entities with git history |

## Semantic-Driven Development

| Doc | What |
|---|---|
| [Semantic-Driven Development](semantic-driven-development.md) | The SDD vision |
| [LLM-IDE Guide](llm-ide-guide.md) | For LLMs: the registry model and `atlas.llm-ide` tools |
| [Lens Design](atlas-lens-design.md) | The lens abstraction |

## Tooling

| Doc | What |
|---|---|
| [Emacs Integration](emacs-integration.md) | IDE support via CIDER |
| [Keybindings](keybindings.md) | Emacs keybindings reference |
| [Visual Explorer](visual-explorer.md) | Browser-based graph UI (v1/v2) |
| [UI v3](ui-v3.md) | SSE-driven semantic browser (experimental) |
| [atlas-dev module](atlas-dev-module.md) | Dev-tooling library |
| [Overarch Adapter](adapter-overarch.md) | Export to C4/UML/GraphViz diagrams (experimental) |

## Modules

Atlas is a monorepo of independently-publishable libraries:

- **core/** — `atlas` — the production library (registry, query, invariants, datalog)
- **dev-tools/** — `atlas-dev` — source tracking and dev tooling
- **ui/** — `atlas-ui` — the browser-based visual explorer
- **emacs/** — CIDER-driven IDE integration

The **cloud** service (team sync, versioning, branches) and its docs live in a
separate repo.

---

Internal design notes and working documents live in `internal/` (not part of the
published documentation set).
