# Atlas Semantic Registry — LLM Prompt

You have access to an Atlas semantic registry via MCP tools. Atlas is a semantic registry for software architecture where every entity has a **compound identity** — a set of qualified keywords that express what the entity *means*.

```
#{:atlas/execution-function :domain/auth :operation/validate :tier/service}
```

This identity says: "execution function, auth domain, validate operation, service tier."

## Key Concepts

- **dev-id**: Human-readable handle (e.g., `:fn/validate-token`). Use this to reference entities in tool calls.
- **compound identity**: The full set of semantic keywords (entity type + aspects). This IS the entity's meaning.
- **entity type**: One `:atlas/*` keyword per entity (e.g., `:atlas/execution-function`, `:atlas/structure-component`).
- **aspects**: All other keywords — reusable semantic tags organized by namespace (`:domain/*`, `:tier/*`, `:operation/*`, `:effect/*`, etc.).
- **deps**: Explicit dependency edges between entities. These form the graph that tools traverse.
- **context/response**: Data keys an entity consumes (input) and produces (output). These create implicit dataflow links.

### Tier Hierarchy

```
:tier/api         ← entry points (endpoints, external interfaces)
:tier/service     ← business logic
:tier/foundation  ← infrastructure (DB, cache, external APIs)
:tier/worker      ← background processing (queue consumers, scheduled jobs)
```

## Available Tools

### Query — Exploration and Lookup

| Tool | Args | Use When |
|---|---|---|
| `entity-detail` | `entity/dev-id` | "What is entity X?" — full props, deps, dependents |
| `by-aspect` | `query/aspect` | "What's in domain X?" — all entities with an aspect |
| `data-flow` | `entity/dev-id` | "What data does X consume/produce?" |
| `producers-of` | `data/key` | "Who produces data key X?" |
| `consumers-of` | `data/key` | "Who consumes data key X?" |
| `list-entity-types` | *(none)* | "What types of entities exist?" |
| `entities-by-type` | `entity/type` | "List all endpoints / all components" |
| `entities-by-all-types` | *(none)* | System overview — counts by type |
| `ontology-info` | `entity/type` | "What fields does this entity type have?" |
| `aggregate-by-aspect` | `query/aspect` | Statistics for entities sharing an aspect |

### Trace — Impact and Causation

| Tool | Args | Use When |
|---|---|---|
| `blast-radius` | `entity/dev-id-or-set`, `query/max-hops` | "What breaks if X changes?" — downstream dependents |
| `trace-causes` | `symptom/dev-id`, `query/max-hops` | "What could cause failure in X?" — upstream deps |
| `change-risk` | `entity/dev-id-set` | "How risky is changing these entities?" — 0.0–1.0 score |
| `trace-data-key` | `data/key` | "Trace data key X end-to-end" — producers → consumers |
| `batch-blast-radius` | `entity/dev-id-set`, `query/max-hops` | Combined blast radius for multiple entities |
| `batch-change-risk` | `entity/dev-id-set` | Individual + combined risk scores |

### Diagnose — Structural Health

| Tool | Args | Use When |
|---|---|---|
| `orphans` | *(optional aspect overrides)* | "What's unconnected?" — no inbound deps |
| `islands` | *(none)* | "Are there isolated subgraphs?" |
| `bottlenecks` | *(none)* | "What are the riskiest nodes?" — high in×out degree |
| `aspect-anomalies` | *(none)* | "Any typos or naming drift?" — aspects used once, similar names |
| `structural-gaps` | *(none)* | "Any missing connections between domains?" |

### Explain — Area Overview

| Tool | Args | Use When |
|---|---|---|
| `explain-area` | `query/focus`, `query/depth` | "Explain domain X" — entry points, key entities, components |

### Suggest — Placement

| Tool | Args | Use When |
|---|---|---|
| `suggest-placement` | `entity/intended-aspects`, `dataflow/consumes`, `dataflow/produces` | "Where should I add this new entity?" — similar entities, suggested deps |

### Batch — Multi-Entity Analysis

| Tool | Args | Use When |
|---|---|---|
| `batch-entity-detail` | `entity/dev-id-set` | Get details for multiple entities at once |
| `compare-entities` | `entity/dev-id-set` | Compare aspects, types, connectivity metrics |

### History — Registry Evolution

| Tool | Args | Use When |
|---|---|---|
| `snapshot-registry` | `history/version-label` | Take a named snapshot of current state |
| `history-versions` | *(none)* | List all snapshot labels |
| `diff-versions` | `history/version-old`, `history/version-new` | Compare two snapshots |
| `version-summary` | `history/version-label` | Statistics for a snapshot |
| `entity-timeline` | `entity/dev-id` | Track one entity across snapshots |
| `snapshot-and-diff` | `history/version-label` | Snapshot + immediate diff vs previous |

## Reasoning Patterns

### "What does this system do?"
1. `entities-by-all-types` → count and distribution
2. `by-aspect` with `:tier/api` → entry points
3. `by-aspect` with `:tier/foundation` → infrastructure

### "Tell me about domain X"
1. `by-aspect` with `:domain/X` → all entities
2. `explain-area` with focus `:domain/X` → structured overview

### "What is entity Y?"
1. `entity-detail` → identity, props, deps, dependents
2. `data-flow` → consumes/produces
3. `blast-radius` → downstream impact

### "Is this system healthy?"
1. `orphans` → unconnected entities
2. `islands` → isolated subgraphs
3. `bottlenecks` → high-risk central nodes
4. `aspect-anomalies` → vocabulary drift
5. `structural-gaps` → missing domain connections

### "What's risky to change?"
1. `change-risk` for the specific entity set
2. `blast-radius` to see downstream impact
3. `bottlenecks` for system-wide risk nodes

### "Where should I add new functionality?"
1. `suggest-placement` with intended aspects and data keys
2. Review `:similar` for patterns to follow
3. Review `:suggested-deps` for wiring

## Ontology-Agnostic Parameters

Tools accept overrides for projects using different aspect vocabularies:

| Parameter | Default | Effect |
|---|---|---|
| `aspect/entry-point` | `:tier/api` | What counts as an entry point |
| `aspect/key-component` | `:tier/foundation` | What counts as key infrastructure |
| `aspect/high-risk` | `:tier/foundation` | What is considered high-risk |
| `aspect/external-integration` | `:integration/external` | What marks external integrations |
| `aspect/domain-namespace` | `"domain"` | Namespace prefix for domain aspects |
| `aspect/type-namespace` | `"atlas"` | Namespace prefix for entity types |
| `risk/centrality-threshold` | `3` | Threshold for high-centrality detection |

## Rules

- Always use **dev-ids** (`:fn/my-function`) to reference entities, not compound identity sets.
- Aspects are **reusable tags** — the same aspect appears on many entities. Use `by-aspect` to find them.
- Data keys create **implicit links** between entities (producer → consumer) beyond explicit deps.
- If `entity-detail` returns nil, the dev-id does not exist — check spelling.
- Start broad (`entities-by-all-types`), then narrow (`by-aspect`, `entity-detail`).
- For impact questions, always check `blast-radius` before recommending changes.
