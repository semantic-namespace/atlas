> **Status: Proof of Concept (Alpha)**
>
> Atlas is in early development. APIs may change. Feedback welcome!

# Atlas

> "There is no reason to limit our specifications to what we can prove, yet that is primarily what type systems do. There is so much more we want to communicate and verify about our systems."
>
> — [clojure.org/about/spec](https://clojure.org/about/spec#_expressivity_proof)

A semantic registry for software architecture. Describe your system's components, functions, and relationships using **compound identities** (sets of semantic aspects), then query, validate, and visualize your architecture as a single queryable graph.


## The Problem

Understanding, maintaining, and communicating system design are constant challenges in any medium-to-large project. Complexity grows with every new feature, rule, or condition, eventually overwhelming developers with unmanageable cognitive load.

In most software systems, "identity" is bound to **place** - a thing's name implies where it lives (folder, file, namespace, memory address). This is an accident of hardware history, not a conceptual necessity.

What if identity was determined by **semantics** - what something means and how it relates to other things?

## What Atlas Does

Atlas offers a semantic registry and tooling layer for software architecture: you describe components, functions, endpoints, protocols, and business concepts as **compound identities** (sets of aspects) plus structured properties - without thinking about implementation details.

```clojure
;; A function's identity IS its semantic meaning
(registry/register!
  :fn/validate-token                    ; dev-id (for humans)
  :atlas/execution-function             ; entity type
  #{:domain/auth                        ; semantic aspects
    :operation/validate
    :tier/service}
  {:execution-function/context [:auth/token]
   :execution-function/response [:auth/valid? :auth/user-id]
   :execution-function/deps #{:component/oauth-provider}})
```

From this registry, Atlas can:
- **Query** - Find all entities in `:domain/auth`, all `:tier/service` functions, etc.
- **Validate** - Check that data flows are satisfiable, dependencies exist, tiers are respected
- **Visualize** - Explore the semantic graph interactively
- **Document** - Generate architecture docs from the registry

It's a **data-first approach** where the system's structure is queryable, visualizable, and validated via a shared ontology - rather than hard-coded diagrams or ad-hoc documentation.

## Technical Approach

Atlas embraces Clojure namespaced keywords (like `clojure.spec`) and depends on a mutable global registry (also like `clojure.spec`). Beyond that, Atlas pursues two intents:

1. **Entities as Datalog facts** - Represent your system entities (functions, components, endpoints, etc.) as Datalog facts, making them queryable via Datalog.

2. **Semantics as Datalog facts** - Represent semantic relations as Datalog facts, so entities and their meanings become a single queryable graph.

Every identity and relation lives in a global registry that can be translated into a graph database, queryable via Datalog.

## Quick Start

### Installation

Add to your `deps.edn`:


```clojure
{:deps {com.github.semantic-namespace/atlas {:mvn/version "0.0.1-SNAPSHOT"}}
```


### Basic Usage

```clojure
(require '[atlas.registry :as registry])
(require '[atlas.query :as query])
(require '[atlas.ontology :as ont])

;; Register entity types first
(ont/register-entity-types!)

;; Register a component
(registry/register!
  :component/database
  :atlas/structure-component
  #{:tier/foundation :domain/storage}
  {:component/provides [:db/query :db/transact]})

;; Register a function that uses it
(registry/register!
  :fn/get-user
  :atlas/execution-function
  #{:domain/users :tier/service}
  {:execution-function/context [:user/id]
   :execution-function/response [:user/data]
   :execution-function/deps #{:component/database}})

;; Register an API endpoint
(registry/register!
  :endpoint/get-user
  :atlas/interface-endpoint
  #{:domain/users :tier/api :http/get}
  {:endpoint/context [:http/path-params]
   :endpoint/response [:http/response]
   :endpoint/deps #{:fn/get-user}})
```

### Querying

```clojure
;; Find all entities in auth domain
(query/find-by-aspect @registry/registry :domain/auth)

;; Find by multiple aspects
(query/find-by-aspect @registry/registry #{:domain/users :tier/service})

;; Get an entity's semantic identity
(registry/identity-for :fn/get-user)
;; => #{:atlas/execution-function :domain/users :tier/service}
```

### Checking Invariants

```clojure
(require '[atlas.invariant :as inv])

;; Check all architectural invariants
(inv/check-all)

;; Get human-readable report
(inv/report)
```

## Core Concepts

### Compound Identity

Every entity has a **compound identity** - a set of qualified keywords:

```clojure
#{:atlas/execution-function :domain/auth :operation/validate :tier/service}
```

This set **is** the entity's semantic meaning. It enables:
- Multi-dimensional querying (find by any aspect combination)
- Semantic similarity (how related are two entities?)
- Architectural reasoning (what domains touch what tiers?)

### Entity Types

Built-in types (register your own with `:atlas/type`):
- `:atlas/execution-function` - Business logic functions
- `:atlas/interface-endpoint` - API endpoints
- `:atlas/structure-component` - Infrastructure components
- `:atlas/data-schema` - Data structures
- `:atlas/interface-protocol` - Protocols/interfaces

### Data Flow

Functions declare what data they consume and produce:

```clojure
{:execution-function/context [:user/id :auth/token]   ; inputs
 :execution-function/response [:user/data :user/roles] ; outputs
 :execution-function/deps #{:component/db}}            ; dependencies
```

Atlas validates that every input is produced somewhere and every output is consumed or terminal.

### Tiers

Standard architectural tiers:
- `:tier/foundation` - Components (DB, cache, external APIs)
- `:tier/service` - Business logic functions
- `:tier/api` - HTTP endpoints

Atlas enforces tier boundaries (services can't skip to foundation, etc.).

## Tooling

### POC Visual Explorer v-1

![Atlas UI graph visualization](https://github.com/user-attachments/assets/7b0259bd-d272-4b20-a834-16357c708583)

Interactive graph visualization for exploring semantic registries:

```clojure
(require '[atlas.atlas-ui.server :as ui])
(ui/start! registry/registry {:port 8082 :ui-version :v1}) ; Opens browser at http://localhost:8082
```

Features: multi-aspect query builder, lens system for filtering, dependency tracing, shareable URLs.

### POC Visual Explorer v-2

![Atlas UI aspects & entities](https://github.com/user-attachments/assets/750cb572-9ee5-4b13-a295-4a4496cd8f9f)

Interactive visualization for exploring semantic registries using 2 divs, aspects by aspects groups and entities by entities-types:

```clojure
(require '[atlas.atlas-ui.server :as ui])
(ui/start! registry/registry {:port 8082 :ui-version :v2}) ; Opens browser at http://localhost:8082
```

### Emacs Integration (PoC)

![Emacs transient menu](https://github.com/user-attachments/assets/1af2d36d-6fb9-44b8-b77d-2b262e396da0)

Explore registries directly from your editor via CIDER:

```elisp
  (add-to-list 'load-path "..../semantic-namespace/atlas/emacs")
  (require 'atlas)
M-x atlas  ; Opens transient menu
```

Features: entity browsing, data flow tracing, invariant checking, completion.

## Documentation

- [Getting Started](docs/getting-started.md) - 5-minute tutorial
- [Core Concepts](docs/concepts.md) - Detailed concept explanations
- [API Reference](docs/api-reference.md) - Function reference
- [Examples](docs/examples.md) - Annotated example registries
- [Emacs Integration](docs/emacs-integration.md) - IDE support via CIDER
- [Visual Explorer](docs/visual-explorer.md) - Browser-based graph UI

## Example Applications

See `test/app/` for complete examples:
- `calendar_availability.clj` - Calendar system with OAuth, users, events
- `cart.clj` - Shopping cart with sessions, payments
- `pet_shop.clj` - Multi-domain system with external integrations

## Development

```bash
# Start REPL
clojure -M:repl

# Run tests
clojure -M:test

# Build JAR
clojure -T:build jar
```

## License

MIT - See [LICENSE](LICENSE)

## Status & Roadmap

This is an early proof-of-concept exploring semantic approaches to architecture description. Current limitations:

- API stability not guaranteed
- Documentation in progress
- Tooling (UI, Emacs) is experimental
- Performance not yet optimized for large registries

Feedback, ideas, and contributions welcome!
