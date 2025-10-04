# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

> **Status: Proof of Concept (Alpha)** - APIs may change.

## Project Overview

Atlas is a semantic registry and tooling layer for software architecture. Instead of binding identity to place (files, namespaces), Atlas binds identity to **semantics** - what entities mean and how they relate.

**Key concept:** Every entity has a compound identity like `#{:atlas/execution-function :domain/auth :operation/validate :tier/service}` that IS its semantic meaning, enabling multi-dimensional querying and architectural reasoning.

**Documentation:**
- [Getting Started](docs/getting-started.md) - 5-minute tutorial
- [Core Concepts](docs/concepts.md) - Compound identities, tiers, data flow
- [API Reference](docs/api-reference.md) - Function reference
- [Examples](docs/examples.md) - Annotated patterns
- [Emacs Integration](docs/emacs-integration.md) - IDE support via CIDER
- [Visual Explorer](docs/visual-explorer.md) - Browser-based graph UI (PoC)

## Clojure REPL Evaluation

The command `clj-nrepl-eval` is installed on your path for evaluating Clojure code via nREPL.

```bash
# Discover nREPL servers
clj-nrepl-eval --discover-ports

# Evaluate code
clj-nrepl-eval -p <port> "<clojure-code>"

# With timeout (milliseconds)
clj-nrepl-eval -p <port> --timeout 5000 "<clojure-code>"
```

The REPL session persists between evaluations. Always use `:reload` when requiring namespaces to pick up changes.

## Development Commands

### Running Tests
```bash
# Run all tests with Kaocha
clojure -M:test

# Run specific test namespace
clojure -M:test --focus atlas.registry-test

# Run tests via nREPL
clj-nrepl-eval -p <port> "(require 'atlas.registry-test :reload) (clojure.test/run-tests 'atlas.registry-test)"
```

### REPL Development
```bash
# Start REPL
clojure -M:repl

# Load example app:
(require '[app.calendar-availability :as app])
(app/init-registry!)
```

### Project Build
```bash
clojure -T:build jar      # Build JAR
clojure -T:build install  # Install to local Maven
clojure -X:deploy         # Deploy to Clojars (maintainers)
```

### Visual Explorer (PoC)

```clojure
(require '[atlas.atlas-ui.server :as ui])
(ui/start!)  ; Opens browser at http://localhost:8082
(ui/stop! 8082)
```

## Code Architecture

### Source Structure

```
src/atlas/
├── core.cljc          # Public API entry point
├── registry.cljc      # Semantic kernel, compound identity algebra
├── query.cljc         # Querying by aspects
├── entity.cljc        # Runtime entity resolution
├── invariant.cljc     # Architectural validation
├── ontology.cljc      # Tools, templates, discovery
├── graph.cljc         # Graph algorithms
├── datalog.cljc       # Datascript backend
└── atlas_ui/          # Visual explorer (PoC)
```

### Example Applications

**Location:** `test/app/`

- `calendar_availability.clj` - OAuth, users, calendar events
- `cart.clj` - Shopping cart with sessions, payments
- `pet_shop.clj` - Multi-domain with external integrations

## Key Patterns

### Registration API

```clojure
;; 4-arity: explicit dev-id
(registry/register!
  :fn/validate-token              ; dev-id
  :atlas/execution-function       ; entity type
  #{:domain/auth                  ; aspects (set)
    :operation/validate
    :tier/service}
  {:execution-function/context [:auth/token]
   :execution-function/response [:auth/valid?]
   :execution-function/deps #{:component/oauth}})

;; 3-arity: auto-generate dev-id
(registry/register!
  :atlas/execution-function
  #{:tier/service :domain/cart}
  {...})
```

### Compound Identity Rules

- **One entity type** - e.g., `:atlas/execution-function`
- **Zero or more aspects** - e.g., `:domain/auth`, `:tier/service`
- **All qualified** - `:ns/name` not `:name`

### Entity Types

Built-in (register with `(ont/register-entity-types!)`):
- `:atlas/execution-function` - Business logic functions
- `:atlas/interface-endpoint` - API endpoints
- `:atlas/structure-component` - Infrastructure components
- `:atlas/data-schema` - Data structures
- `:atlas/interface-protocol` - Interfaces/contracts

Custom types: register with `:atlas/type`

### Always Use `:reload`

Since the registry is stateful (atom-based):

```clojure
(require '[atlas.registry :as registry] :reload)
```

### Registry Fixture Pattern

Tests should reset the registry:

```clojure
(use-fixtures :each
  (fn [f]
    (reset! atlas.registry/registry {})
    (f)
    (reset! atlas.registry/registry {})))
```

### Querying

```clojure
(require '[atlas.query :as query])

;; By aspect
(query/find-by-aspect @registry/registry :domain/auth)

;; By multiple aspects (AND)
(query/find-by-aspect @registry/registry #{:domain/auth :tier/service})

;; By predicate
(query/where @registry/registry
  #(contains? (:identity %) :effect/write))
```

### Invariant Checking

```clojure
(require '[atlas.invariant :as inv])

(inv/check-all)  ; => {:valid? bool :violations [...]}
(inv/report)     ; Human-readable
```

### Custom Invariants

**DSL (declarative):**
```clojure
(dsl/dsl-axiom
  :my/rule-id
  :error
  "Description"
  {:op :dsl.op/entity-has-aspect :args :effect/write}    ; when
  {:op :dsl.op/entity-has-aspect :args :audit/enabled})  ; then
```

**Function (imperative):**
```clojure
(dsl/fn-axiom
  :my/rule-id
  :error
  "Description"
  (fn [db]
    [{:entity :some/entity :message "Violation"}]))
```

## Key Dependencies

- `org.clojure/clojure "1.11.1"`
- `datascript/datascript "1.6.5"` - Semantic queries
- `org.clojure/data.json "2.5.1"`

## Testing Philosophy

Tests validate:
1. Algebraic properties (closure, determinism)
2. Registry operations
3. Query correctness
4. Invariant validation
5. Semantic relationships

All tests should be pure and deterministic.
