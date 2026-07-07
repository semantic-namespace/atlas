# Atlas LLM-IDE Guide

This document is intended to be read by an LLM (via MCP or prompt injection) to understand the Atlas registry, its semantic model, and how to use the available `atlas.llm-ide` tools effectively.

---

## Part 1: Atlas Mental Model

### What is Atlas?

Atlas is a semantic registry for software architecture. Every entity in a codebase is registered with a **compound identity** — a set of qualified keywords that express what the entity *means*, not where it lives.

**The core insight:** Identity is not a file path or namespace. It is the intersection of semantic dimensions.

```
#{:atlas/execution-function :domain/auth :operation/validate :tier/service}
```

This identity says: "I am an execution function, in the auth domain, doing a validate operation, at the service tier." That set IS the entity's meaning.

### The Registry

The registry is a global map from compound identity → properties:

```
{#{:atlas/execution-function :domain/auth :tier/service} {:atlas/dev-id :fn/validate-token
                                                           :execution-function/context [:auth/token]
                                                           :execution-function/response [:auth/valid?]
                                                           :execution-function/deps #{:component/oauth}}
 #{:atlas/structure-component :tier/foundation :domain/auth} {:atlas/dev-id :component/oauth
                                                               :component/provides [:oauth/token]}
 ...}
```

Every entity has:
- **`:atlas/dev-id`** — a human-readable keyword handle (e.g., `:fn/validate-token`). This is the primary way to reference entities in tool calls.
- **compound identity** — the full set of semantic keywords (entity type + aspects)
- **properties** — domain-specific data (context, response, deps, etc.)

### Entity Types

The entity type is always one keyword in the compound identity, in the `:atlas/*` namespace:

| Entity Type | Meaning |
|---|---|
| `:atlas/execution-function` | Business logic function |
| `:atlas/interface-endpoint` | API endpoint (HTTP, GraphQL, etc.) |
| `:atlas/structure-component` | Infrastructure (DB, cache, external service) |
| `:atlas/data-schema` | Data structure definition |
| `:atlas/interface-protocol` | Interface / contract |
| `:atlas/business-pattern` | Business pattern (saga, workflow) |
| `:atlas/governance-constraint` | Compliance or governance rule |

### Aspects

Aspects are all the other keywords in the compound identity beyond the entity type. They are semantic tags organized by namespace:

| Namespace | Examples | Meaning |
|---|---|---|
| `:domain/*` | `:domain/auth`, `:domain/payments` | Business domain |
| `:tier/*` | `:tier/api`, `:tier/service`, `:tier/foundation` | Architectural layer |
| `:operation/*` | `:operation/create`, `:operation/validate` | Action type |
| `:effect/*` | `:effect/write`, `:effect/read`, `:effect/async` | Side effect type |
| `:integration/*` | `:integration/external` | Integration kind |
| `:http/*` | `:http/get`, `:http/post` | HTTP method |

Any qualified keyword can be an aspect. Projects define their own aspect vocabularies.

**Standard tier hierarchy:**
```
:tier/api         <- HTTP endpoints, external interfaces
:tier/service     <- Business logic functions
:tier/foundation  <- DB, cache, external APIs (components)
```

### Data Flow

Execution functions declare their data contracts:

- **`:execution-function/context`** — list of input data keys the function needs
- **`:execution-function/response`** — list of output data keys the function produces
- **`:execution-function/deps`** — set of dev-ids (components or functions) it depends on

```clojure
{:execution-function/context  [:user/id :auth/token]
 :execution-function/response [:user/profile]
 :execution-function/deps     #{:component/db :fn/validate-token}}
```

This enables Atlas to trace data flow across the entire system: who produces `:user/profile`? who consumes `:auth/token`?

### Compound Identity Rules

1. Exactly **one entity type** keyword (`:atlas/*` namespace)
2. Zero or more **aspect** keywords (any qualified keyword)
3. All keywords must be **qualified** (have a namespace)
4. The set is **unordered** — `#{:a :b}` and `#{:b :a}` are the same identity

### Querying Logic

- Finding by a single aspect returns all entities whose identity *contains* that aspect
- Finding by a set of aspects returns entities whose identity *contains all* of them (AND logic)
- dev-id is a convenient alias — always resolve to compound identity under the hood

---

## Part 2: LLM-IDE Tools

### How Tools Work

All tools are called via a single frontal controller:

```
handle-tool({:tool/name  <qualified-keyword>
             :tool/args  {<qualified-keyword> <value> ...}})
```

**Returns on success:**
```
{:success? true
 :data     {...}}
```

**Returns on failure:**
```
{:success? false
 :error    {:message "..." :tool <name>}}
```

When a tool call fails, the `:error` map will tell you what went wrong. If the tool name is unknown, `:available` lists all registered tools.

### Discovering Available Tools

Before using tools, you can discover what is registered:

```
handle-tool({:tool/name :atlas.llm-ide/list-entity-types, :tool/args {}})
handle-tool({:tool/name :atlas.llm-ide/by-aspect, :tool/args {:query/aspect :domain/llm-ide}})
```

The llm-ide tools themselves are registered in the Atlas registry with aspects `#{:domain/llm-ide :tier/tooling}`, so they are self-describing and discoverable.

---

## Part 3: Tool Reference

### Intent: query — Exploration and Lookup

#### `atlas.llm-ide/entity-detail`
Get full information about one entity.

```
args:    {:entity/dev-id :fn/validate-token}
returns: {:atlas/dev-id ...
          :identity #{...}
          :type :atlas/execution-function
          :aspects #{...}
          :dependencies [...]
          :dependents [...]
          :context [...] :response [...] :deps #{...}}
```

Use this first when asked about a specific entity.

#### `atlas.llm-ide/by-aspect`
List all entities that have a given aspect.

```
args:    {:query/aspect :domain/auth}
returns: {:entities [:fn/validate-token :fn/login :component/oauth ...]}
```

Use when asked "what's in the auth domain?" or "show me all API endpoints".

#### `atlas.llm-ide/data-flow`
Show input/output data flow for an entity.

```
args:    {:entity/dev-id :fn/process-payment}
returns: {:flow {:context [...] :response [...] :deps [...]}}
```

#### `atlas.llm-ide/producers-of`
Find which entities produce a given data key.

```
args:    {:data/key :user/profile}
returns: {:data-key :user/profile :producers [:fn/get-user ...] :count 1}
```

#### `atlas.llm-ide/consumers-of`
Find which entities consume a given data key.

```
args:    {:data/key :auth/token}
returns: {:data-key :auth/token :consumers [:fn/validate-token :fn/get-user] :count 2}
```

#### `atlas.llm-ide/list-entity-types`
List all entity types registered in the system.

```
args:    {}
returns: {:types [:atlas/execution-function :atlas/structure-component ...] :count N}
```

#### `atlas.llm-ide/entities-by-type`
List all entities of a specific type.

```
args:    {:entity/type :atlas/interface-endpoint}
returns: {:entity-type :atlas/interface-endpoint :entities [...] :count N}
```

#### `atlas.llm-ide/entities-by-all-types`
Full breakdown of the registry grouped by entity type. Good for a system overview.

```
args:    {}
returns: {:by-type {:atlas/execution-function {:entities [...] :count N} ...}
          :type-count N :total-entities N :summary {...}}
```

#### `atlas.llm-ide/ontology-info`
Get the ontology definition (schema) for an entity type.

```
args:    {:entity/type :atlas/execution-function}
returns: {:entity-type ... :ontology-keys [...] :ontology-definition {...} :available? true}
```

#### `atlas.llm-ide/batch-entity-detail`
Get details for multiple entities at once.

```
args:    {:entity/dev-id-set #{:fn/login :fn/validate-token :component/oauth}}
returns: {:results [{:entity ... :info {...} :exists? true} ...]
          :missing [...] :summary {:total N :found N :missing N}}
```

#### `atlas.llm-ide/compare-entities`
Compare aspects, types, and connectivity metrics across a set of entities.

```
args:    {:entity/dev-id-set #{:fn/login :fn/register}}
returns: {:entities [{:entity ... :identity #{...} :type ... :domains [...] :tiers [...]
                      :in-degree N :out-degree N} ...]
          :common-aspects [...]   ; aspects shared by all
          :unique-aspects [...]   ; aspects unique to each
          :metrics {:avg-in-degree ... :types {...} :domains {...} :tiers {...}}
          :summary {:requested N :found N :missing N}}
```

#### `atlas.llm-ide/aggregate-by-aspect`
Aggregate statistics for all entities sharing an aspect.

```
args:    {:query/aspect :domain/payments}
returns: {:total N :entities [...]
          :by-type {:atlas/execution-function N ...}
          :by-tier {:tier/service N ...}
          :by-domain {:domain/payments N ...}
          :metrics {:avg-in-degree ... :max-out-degree ...}}
```

---

### Intent: trace — Impact and Causation

#### `atlas.llm-ide/blast-radius`
Find all entities downstream of a change (what would break).

```
args:    {:entity/dev-id-or-set :fn/validate-token   ; or a set #{:fn/a :fn/b}
          :query/max-hops       3}                    ; optional, default 3
returns: {:affected  [{:entity ... :hops N} ...]
          :tiers-hit #{:tier/service :tier/api}
          :domains-hit #{:domain/auth}}
```

Use when asked "what breaks if I change X?" or "what's the impact of modifying this?"

#### `atlas.llm-ide/trace-causes`
Find all entities upstream of a symptom (what could cause it).

```
args:    {:symptom/dev-id :fn/process-payment
          :query/max-hops 5}                         ; optional, default 5
returns: {:upstream    [{:entity ... :hops N} ...]
          :components  [...]    ; upstream structure-components
          :failure-modes [...]} ; upstream risk-failure-modes
```

Use when asked "what could cause a failure in X?" or "trace the root cause of Y."

#### `atlas.llm-ide/change-risk`
Score the risk of changing a set of entities.

```
args:    {:entity/dev-id-set         #{:fn/validate-token}
          :risk/centrality-threshold  3       ; optional, default 3
          :aspect/domain-namespace   "domain" ; optional
          :aspect/high-risk          :tier/foundation ; optional
          :aspect/external-integration :integration/external} ; optional
returns: {:risk-score  0.75    ; 0.0–1.0
          :reasons     [{:type :high-centrality :entities [...] :detail "..."}
                        {:type :crosses-domain :detail "..."}
                        {:type :high-risk-aspect :aspect :tier/foundation :detail "..."}
                        {:type :external-integration :detail "..."}]
          :suggestions ["Consider smaller incremental changes" ...]}
```

Risk factors checked: high number of dependents, crosses multiple domains, touches foundation tier, touches external integrations.

#### `atlas.llm-ide/trace-data-key`
Trace a data key end-to-end: who produces it and who consumes it.

```
args:    {:data/key :user/profile}
returns: {:data-key      :user/profile
          :produced-by   [:fn/get-user]
          :consumed-by   [:fn/process-payment :endpoint/get-profile]
          :connected?    true
          :producers-count 1 :consumers-count 2}
```

#### `atlas.llm-ide/batch-blast-radius`
Run blast-radius for each entity in a set and aggregate the results.

```
args:    {:entity/dev-id-set #{:fn/login :fn/validate-token}
          :query/max-hops   3}
returns: {:results   [{:entity ... :blast-radius {:affected [...] ...}} ...]
          :aggregate {:total-affected N :tiers-hit #{...} :domains-hit #{...}
                      :overlapping-entities [...] :overlap-count N}
          :summary   {:entities-analyzed N :combined-blast-radius N}}
```

#### `atlas.llm-ide/batch-change-risk`
Risk score each entity individually and combined.

```
args:    {:entity/dev-id-set #{:fn/a :fn/b} ...same optional args as change-risk...}
returns: {:individual-results [{:entity ... :risk {:data {:risk-score ...}}} ...]
          :combined-risk      {:risk-score ... :reasons [...]}
          :high-risk-entities [...]
          :summary {:total-entities N :high-risk-count N :combined-risk-score ...
                    :recommendation "..."}}
```

---

### Intent: diagnose — Structural Health

#### `atlas.llm-ide/orphans`
Find entities with no inbound dependencies (nothing depends on them).

```
args:    {:aspect/exclude-from-orphan-check :tier/api   ; optional — API entities are expected orphans
          :aspect/type-namespace            "atlas"}    ; optional
returns: {:orphans [{:entity ... :type :atlas/execution-function} ...]
          :count N}
```

API endpoints are legitimately orphaned (nothing depends on them — they are entry points). Exclude them with `:aspect/exclude-from-orphan-check`.

#### `atlas.llm-ide/islands`
Find disconnected subgraphs (clusters of entities with no links to the rest).

```
args:    {}
returns: {:islands [[...entities in component...] [...] ...]   ; sorted largest first
          :count N}
```

A healthy system typically has one large connected component. Multiple components suggest isolated subsystems.

#### `atlas.llm-ide/bottlenecks`
Find high-centrality entities (many inbound AND outbound dependencies).

```
args:    {}
returns: {:bottlenecks [{:entity ... :in-degree N :out-degree N :score N} ...]}
```

Score = in-degree × out-degree. High scores indicate entities that are risky to change and hard to test in isolation.

#### `atlas.llm-ide/aspect-anomalies`
Find aspects used only once (possibly typos or orphan vocabulary) and aspects with suspiciously similar names.

```
args:    {}
returns: {:sparse       [:operation/procss ...]   ; used only once — possible typo
          :similar-names [[:operation/create :operation/creates] ...]}
```

#### `atlas.llm-ide/structural-gaps`
Find domain pairs that have no connections between them despite being related.

```
args:    {}
returns: {:gaps [{:domains [...] :expected-connections N :actual-connections N} ...]}
```

---

### Intent: explain — Area Overview

#### `atlas.llm-ide/explain-area`
Explain a bounded area of the system around a focus point.

```
args:    {:query/focus             :domain/payments   ; aspect keyword OR dev-id
          :query/depth             2                  ; optional, hops to explore
          :aspect/entry-point      :tier/api          ; optional
          :aspect/key-component    :tier/foundation}  ; optional
returns: {:focus        :domain/payments
          :entity-count N
          :entry-points [...]     ; entities with :tier/api aspect
          :key-entities [...]     ; top 5 by in-degree within the area
          :components   [...]}    ; entities with :tier/foundation aspect
```

When `:query/focus` is an aspect (like `:domain/payments`), it returns all entities with that aspect. When it is a dev-id, it walks up and down the dependency graph to build the neighborhood.

---

### Intent: suggest — Placement Advice

#### `atlas.llm-ide/suggest-placement`
Given intended aspects for a new entity, find similar existing entities and suggested dependencies.

```
args:    {:entity/intended-aspects  #{:domain/auth :tier/service :operation/create}
          :dataflow/consumes        [:user/email :user/password]  ; optional
          :dataflow/produces        [:auth/token]}                 ; optional
returns: {:similar       [{:entity ... :overlap 0.8 :shared [...] :missing [...]} ...]
          :suggested-deps [...]   ; entities that produce the consumed data keys
          :patterns       [...]}  ; top 3 similar entities as reference patterns
```

---

### Intent: history — Registry Snapshots and Diffs

The history tools track how the registry evolves over time. Typical workflow:

1. Call `snapshot-registry` with label `"before-refactor"` → baseline
2. Developer makes changes to the codebase
3. Call `snapshot-registry` with label `"after-refactor"` → new state
4. Call `diff-versions` or `snapshot-and-diff` → what changed

#### `atlas.llm-ide/snapshot-registry`
Take a named snapshot of the current registry state.

```
args:    {:history/version-label "before-refactor"}  ; optional — auto-generated if omitted
returns: {:snapshot {...}
          :versions ["before-refactor" ...]}
```

#### `atlas.llm-ide/history-versions`
List all snapshot labels in order.

```
args:    {}
returns: {:versions ["v1" "before-refactor" "after-refactor"]}
```

#### `atlas.llm-ide/diff-versions`
Compare two snapshots.

```
args:    {:history/version-old "before-refactor"
          :history/version-new "after-refactor"}
returns: {:snap-changes   {:old {...} :new {...}}
          :edge-diff      {:added [...] :removed [...]}
          :vocabulary-diff {:added-aspects [...] :removed-aspects [...]}
          :summary        {:old {...} :new {...}}}
```

#### `atlas.llm-ide/version-summary`
Summary statistics for a single snapshot.

```
args:    {:history/version-label "after-refactor"}
returns: {:summary {:entity-count N :aspect-count N ...}
          :diff    {...}    ; changes from previous version
          :renames [...]}   ; detected entity renames
```

#### `atlas.llm-ide/entity-timeline`
Track a single entity's changes across all snapshots.

```
args:    {:entity/dev-id :fn/validate-token}
returns: {:timeline [{:version "v1" :aspects #{...} :added [...] :removed [...]} ...]}
```

#### `atlas.llm-ide/snapshot-and-diff`
Convenience: snapshot the current state and immediately diff against the previous snapshot.

```
args:    {:history/version-label "post-refactor"}  ; optional
returns: {:snapshot         {...}
          :version          "post-refactor"
          :previous-version "pre-refactor"
          :diff             {:snap-changes ... :edge-diff ... :vocabulary-diff ... :summary ...}
          :invariants       {:valid? true :violations []}
          :versions         [...]}
```

---

## Part 4: Ontology-Agnostic Parameters

Several tools accept parameters to override the default assumptions about your registry's ontology. This makes the tools work with any aspect vocabulary.

| Parameter | Default | Used by |
|---|---|---|
| `:aspect/entry-point` | `:tier/api` | `explain-area`, `orphans` |
| `:aspect/key-component` | `:tier/foundation` | `explain-area` |
| `:aspect/high-risk` | `:tier/foundation` | `change-risk`, `batch-change-risk` |
| `:aspect/external-integration` | `:integration/external` | `change-risk`, `batch-change-risk` |
| `:aspect/exclude-from-orphan-check` | `:tier/api` | `orphans` |
| `:aspect/domain-namespace` | `"domain"` | `change-risk`, `blast-radius` |
| `:aspect/type-namespace` | `"atlas"` | `orphans`, `compare-entities` |
| `:risk/centrality-threshold` | `3` | `change-risk` |

Example — project using `:layer/*` instead of `:tier/*`:

```
handle-tool({:tool/name :atlas.llm-ide/change-risk
             :tool/args {:entity/dev-id-set         #{:fn/process-payment}
                         :aspect/high-risk          :layer/core
                         :aspect/external-integration :vendor/external}})
```

---

## Part 5: Reasoning Patterns

### "What does this system do?"

Start broad:
1. `entities-by-all-types` → count and distribution of entity types
2. `by-aspect` with `:tier/api` → entry points (what's exposed)
3. `by-aspect` with `:tier/foundation` → infrastructure (what's depended on)

### "Tell me about domain X"

1. `by-aspect` with `:domain/X` → all entities in domain
2. `explain-area` with `:query/focus :domain/X` → structured overview with entry points, key entities, components

### "What is entity Y?"

1. `entity-detail` → full picture: identity, context, response, deps, dependents
2. `data-flow` → what data it consumes and produces
3. `blast-radius` → what depends on it

### "Is this system healthy?"

1. `orphans` → unconnected entities
2. `islands` → isolated subgraphs
3. `bottlenecks` → high-risk central nodes
4. `aspect-anomalies` → vocabulary drift / typos
5. `structural-gaps` → missing domain connections

Or call the invariant check via `snapshot-and-diff` which includes `:invariants`.

### "What's risky to change?"

1. `change-risk` for the specific set of entities you're changing
2. `blast-radius` to see downstream impact
3. `bottlenecks` to understand system-wide risk nodes

### "What changed?"

Use history tools:
1. If snapshots exist: `history-versions` → list versions, then `diff-versions`
2. For a specific entity: `entity-timeline`
3. Combined: `snapshot-and-diff` for a live snapshot + immediate diff

### "Where should I add this new function?"

1. `suggest-placement` with the intended aspects and data keys
2. Review `:similar` for patterns to follow
3. Review `:suggested-deps` for what to wire it to

---

## Part 6: Key Expectations

- **dev-ids are stable handles.** Always use `:fn/my-function` style dev-ids to reference entities in tool calls, not compound identity sets.
- **Aspects are reusable tags.** The same aspect keyword (e.g., `:domain/auth`) appears on many entities. Use `by-aspect` to find all of them.
- **Data keys connect entities.** If `:fn/a` responds with `:user/id` and `:fn/b` consumes `:user/id` in its context, they are semantically linked even if not in `:execution-function/deps`.
- **Invariants are architectural rules.** A system with `{:valid? false :violations [...]}` has declared architectural problems — report them.
- **Tools are themselves in the registry.** The llm-ide tools are registered as `:atlas/execution-function` entities with `:domain/llm-ide`. You can query them like any other entity.
- **Missing entities.** If `entity-detail` returns `nil` or `batch-entity-detail` reports `:missing`, the dev-id does not exist in the registry. The entity may not have been registered, or the dev-id may be misspelled.
- **Empty registry.** If queries return empty results and `entities-by-all-types` shows zero entities, the registry is empty. The user needs to load their ontology (e.g., `(require '[my.app :as app]) (app/init-registry!)`).
