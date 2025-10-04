# API Reference

> **Note:** Atlas is a Proof of Concept. APIs may change.

## Namespaces Overview

| Namespace | Purpose |
|-----------|---------|
| `atlas.registry` | Core registration and identity operations |
| `atlas.query` | Querying entities by aspects |
| `atlas.entity` | Runtime entity resolution |
| `atlas.invariant` | Architectural validation |
| `atlas.ontology` | Tools and templates |

---

## atlas.registry

The semantic kernel - registration and compound identity operations.

### register!

Register an entity in the registry.

```clojure
;; 4-arity: explicit dev-id
(register! dev-id type aspects props)

;; 3-arity: auto-generate dev-id
(register! type aspects props)
```

**Parameters:**
- `dev-id` - Keyword, human-readable identifier (e.g., `:fn/login`)
- `type` - Qualified keyword, entity type (e.g., `:atlas/execution-function`)
- `aspects` - Set of qualified keywords (e.g., `#{:domain/auth :tier/service}`)
- `props` - Map of properties

**Example:**
```clojure
(registry/register!
  :fn/validate-token
  :atlas/execution-function
  #{:domain/auth :operation/validate :tier/service}
  {:execution-function/context [:auth/token]
   :execution-function/response [:auth/valid?]
   :execution-function/deps #{:component/oauth}})
```

### identity-for

Get the compound identity for a dev-id.

```clojure
(identity-for dev-id) => #{...} or nil
```

**Example:**
```clojure
(registry/identity-for :fn/validate-token)
;; => #{:atlas/execution-function :domain/auth :operation/validate :tier/service}
```

### fetch

Get the properties for a compound identity.

```clojure
(fetch registry compound-identity) => map or nil
```

**Example:**
```clojure
(registry/fetch @registry/registry
  #{:atlas/execution-function :domain/auth :operation/validate :tier/service})
;; => {:atlas/dev-id :fn/validate-token, :execution-function/context [...], ...}
```

### entity-type

Extract the entity type from a compound identity.

```clojure
(entity-type compound-identity) => keyword or nil
```

**Example:**
```clojure
(registry/entity-type #{:atlas/execution-function :domain/auth :tier/service})
;; => :atlas/execution-function
```

### aspects

Extract non-type aspects from a compound identity.

```clojure
(aspects compound-identity) => #{...}
```

**Example:**
```clojure
(registry/aspects #{:atlas/execution-function :domain/auth :tier/service})
;; => #{:domain/auth :tier/service}
```

### valid?

Check if a compound identity is structurally valid.

```clojure
(valid? compound-identity) => boolean
```

### registered-types

Get all registered entity types.

```clojure
(registered-types) => #{:atlas/execution-function :atlas/data-schema ...}
```

### summary

Get registry statistics.

```clojure
(summary) => {:total-entities n, :by-type {...}, ...}
```

---

## atlas.query

Pure functions for querying the registry.

### find-by-aspect

Find all entities with given aspect(s).

```clojure
(find-by-aspect registry aspect) => [{:identity #{...} :props {...}} ...]
(find-by-aspect registry aspects-set) => [...]
```

**Example:**
```clojure
;; Single aspect
(query/find-by-aspect @registry/registry :domain/auth)

;; Multiple aspects (AND)
(query/find-by-aspect @registry/registry #{:domain/auth :tier/service})
```

### find-by-dev-id

Find entity by its dev-id.

```clojure
(find-by-dev-id registry dev-id) => {:identity #{...} :props {...}} or nil
```

### find-exact

Find entity with exact compound identity match.

```clojure
(find-exact registry compound-identity) => {:identity #{...} :props {...}} or nil
```

### where

Filter registry by predicate.

```clojure
(where registry pred) => [{:identity #{...} :props {...}} ...]
```

**Example:**
```clojure
;; Find entities with :effect/write
(query/where @registry/registry
  #(contains? (:identity %) :effect/write))

;; Find functions with more than 2 deps
(query/where @registry/registry
  #(> (count (:execution-function/deps (:props %))) 2))
```

### semantic-similarity

Find entities similar to target identity.

```clojure
(semantic-similarity registry target) => [{:identity #{...} :similarity 0.75} ...]
(semantic-similarity registry target min-similarity) => [...]
```

Uses Jaccard similarity (intersection / union).

### dependency-graph

Build a dependency graph from the registry.

```clojure
(dependency-graph registry id-key deps-key) => {dev-id #{dep-ids ...} ...}
```

**Example:**
```clojure
(query/dependency-graph @registry/registry :atlas/dev-id :execution-function/deps)
;; => {:fn/login #{:component/db :fn/validate}, :fn/validate #{:component/oauth}, ...}
```

### by-tier

Group entities by tier.

```clojure
(by-tier registry) => {:tier/api [...] :tier/service [...] :tier/foundation [...]}
```

### impact-of-change

Find all entities affected by changing an entity.

```clojure
(impact-of-change registry dev-id) => #{affected-dev-ids ...}
```

---

## atlas.entity

Runtime entity resolution.

### identity-for

Get semantic identity for dev-id.

```clojure
(identity-for dev-id) => #{...}
```

### props-for

Get properties for dev-id.

```clojure
(props-for dev-id) => {...}
```

### context-for

Get context (input data keys) for dev-id.

```clojure
(context-for dev-id) => [:key1 :key2 ...]
```

### response-for

Get response (output data keys) for dev-id.

```clojure
(response-for dev-id) => [:key1 :key2 ...]
```

### deps-for

Get dependencies for dev-id.

```clojure
(deps-for dev-id) => #{:dep1 :dep2 ...}
```

### has-aspect?

Check if entity has an aspect.

```clojure
(has-aspect? dev-id aspect) => boolean
```

### all-with-aspect

Find all entities with aspect.

```clojure
(all-with-aspect aspect) => [dev-ids ...]
```

### trace-data-flow

Trace what produces each context key for an entity.

```clojure
(trace-data-flow dev-id) => {:key1 :producer-dev-id, :key2 :external, ...}
```

---

## atlas.invariant

Architectural validation.

### check-all

Run all built-in invariants.

```clojure
(check-all) => {:valid? boolean :violations [...]}
```

### check

Run specific invariants.

```clojure
(check invariant-fns) => {:valid? boolean :violations [...]}
```

**Example:**
```clojure
(inv/check [inv/invariant-deps-exist
            inv/invariant-no-circular-deps])
```

### report

Generate human-readable report.

```clojure
(report) => "string report"
```

### Built-in Invariants

**Structural:**
- `invariant-deps-exist` - All dependencies reference existing entities
- `invariant-no-circular-deps` - No cycles in dependency graph
- `invariant-all-fns-reachable` - Functions reachable from endpoints

**Data Flow:**
- `invariant-context-satisfiable` - Context keys are producible
- `invariant-no-orphan-responses` - Response keys are consumed

**Semantic:**
- `invariant-endpoints-are-api-tier` - Endpoints are `:tier/api`
- `invariant-components-are-foundation` - Components are `:tier/foundation`
- `invariant-external-is-async` - External integrations are async
- `invariant-pure-has-no-deps` - Pure functions have no deps

**Protocol:**
- `invariant-protocol-exists` - Referenced protocols exist
- `invariant-protocol-conformance` - Implementations conform

---

## atlas.ontology

Development tools and templates.

### register-entity-types!

Register all built-in entity types.

```clojure
(register-entity-types!) => nil
```

Call this once at startup.

### aspect-catalog

Browse all aspects with usage statistics.

```clojure
(aspect-catalog) => {:domain/auth {:count 5, :entities [...]} ...}
```

### suggest-aspects

Get aspect suggestions based on similar entities.

```clojure
(suggest-aspects partial-identity) => [:suggested/aspect1 :suggested/aspect2 ...]
```

### impact-analysis

Analyze what's affected by changes to an aspect.

```clojure
(impact-analysis aspect) => {:direct [...] :transitive [...]}
```

### Templates

Pre-built templates for common patterns:

```clojure
;; Foundation component
(template:foundation-component :component/my-db :domain/users
  {:component/provides [:db/query]})

;; Service function
(template:service-function :fn/my-fn :domain/users :operation/read
  {:execution-function/context [:user/id]
   :execution-function/response [:user/data]
   :execution-function/deps #{}})

;; API endpoint
(template:api-endpoint :endpoint/my-ep :domain/users :http/get
  {:endpoint/context [:http/params]
   :endpoint/response [:http/json]
   :endpoint/deps #{:fn/my-fn}})
```

### where->

Fluent query builder.

```clojure
(-> (where-> :domain/auth)
    (and-aspect :tier/service)
    (execute-query))
```

### inspect

Quick inspection of a dev-id.

```clojure
(inspect :fn/login)
;; Prints identity, props, deps, consumers, etc.
```

---

## Global State

Atlas uses a global mutable registry:

```clojure
atlas.registry/registry  ; the atom
@atlas.registry/registry ; current state (map)
```

For testing, reset between tests:

```clojure
(use-fixtures :each
  (fn [f]
    (reset! atlas.registry/registry {})
    (f)
    (reset! atlas.registry/registry {})))
```

---

## Error Handling

Most functions return `nil` for not-found rather than throwing. Invariant violations are returned as data:

```clojure
{:valid? false
 :violations [{:invariant :deps-exist
               :entity :fn/broken
               :message "Dependency :component/missing not found"
               :severity :error}]}
```
