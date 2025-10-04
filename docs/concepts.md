# Core Concepts

Atlas rethinks how we describe software architecture. This guide explains the key ideas.

## Semantic Identity vs. Spatial Identity

### The Traditional Approach

In most systems, identity is **spatial** - bound to where things live:

```
src/
  auth/
    handlers.clj        <- identity: src.auth.handlers
    middleware.clj      <- identity: src.auth.middleware
  users/
    handlers.clj        <- identity: src.users.handlers
```

This creates problems:
- Where does cross-cutting code go?
- How do you find "all things that validate"?
- How do you see domain boundaries?

### The Atlas Approach

Atlas uses **semantic identity** - what things mean:

```clojure
;; This function's identity IS its semantic meaning
#{:atlas/execution-function
  :domain/auth
  :operation/validate
  :tier/service}
```

Now you can query by any dimension:
- All auth things: `:domain/auth`
- All validators: `:operation/validate`
- All services: `:tier/service`
- Auth validators: `#{:domain/auth :operation/validate}`

## Compound Identities

A compound identity is a **set of qualified keywords**:

```clojure
#{:atlas/execution-function :domain/cart :operation/create :tier/service :effect/write}
```

### Rules

1. **One entity type** - Exactly one keyword like `:atlas/execution-function`
2. **Zero or more aspects** - Keywords describing meaning: `:domain/cart`, `:tier/service`
3. **All qualified** - Every keyword must have a namespace: `:foo/bar` not `:bar`

### Why Sets?

Sets capture that identity is **unordered** and **compositional**:

```clojure
;; These are the same entity
#{:atlas/execution-function :domain/auth :tier/service}
#{:tier/service :atlas/execution-function :domain/auth}

;; You can match on any subset
(query/find-by-aspect reg :domain/auth)     ; finds it
(query/find-by-aspect reg :tier/service)    ; finds it
(query/find-by-aspect reg #{:domain/auth :tier/service}) ; finds it
```

## Entity Types

Entity types classify what kind of thing you're describing.

### Built-in Types

| Type | Purpose |
|------|---------|
| `:atlas/execution-function` | Business logic functions |
| `:atlas/interface-endpoint` | API endpoints (HTTP, GraphQL, etc.) |
| `:atlas/structure-component` | Infrastructure (DB, cache, external APIs) |
| `:atlas/data-schema` | Data structure definitions |
| `:atlas/interface-protocol` | Interfaces/contracts |
| `:atlas/business-pattern` | Business patterns (saga, workflow) |
| `:atlas/governance-constraint` | Compliance rules |

### Custom Types

Define your own:

```clojure
(registry/register!
  :myapp/event
  :atlas/type
  #{}
  {:registry-definition/keys [:event/topic :event/schema]})

;; Now use it
(registry/register!
  :event/user-created
  :myapp/event
  #{:domain/users}
  {:event/topic "users"
   :event/schema [:map [:user-id :uuid]]})
```

## Aspects

Aspects are the semantic dimensions beyond entity type.

### Common Aspect Namespaces

**Domain** - Business domains:
```clojure
:domain/auth
:domain/users
:domain/billing
:domain/inventory
```

**Tier** - Architectural layers:
```clojure
:tier/foundation  ; infrastructure components
:tier/service     ; business logic
:tier/api         ; external interfaces
```

**Operation** - What action is performed:
```clojure
:operation/create
:operation/read
:operation/update
:operation/delete
:operation/validate
```

**Effect** - Side effect type:
```clojure
:effect/read
:effect/write
:effect/async
:effect/external
```

### Your Own Aspects

Use any qualified keyword. Common patterns:

```clojure
;; HTTP methods
:http/get :http/post :http/delete

;; Feature flags
:feature/beta :feature/experimental

;; Compliance
:compliance/gdpr :compliance/hipaa

;; Team ownership
:team/platform :team/growth
```

## Data Flow

Functions in Atlas declare their data flow explicitly.

### Context (Inputs)

What data the function needs to execute:

```clojure
{:execution-function/context [:user/id :auth/token]}
```

### Response (Outputs)

What data the function produces:

```clojure
{:execution-function/response [:user/data :user/roles]}
```

### Dependencies

What components or functions it needs:

```clojure
{:execution-function/deps #{:component/db :fn/validate-token}}
```

### Why This Matters

Atlas can analyze your entire system's data flow:

```clojure
;; What produces :user/data?
(entity/trace-data-flow :user/data)

;; What consumes :auth/token?
(query/where @registry/registry
  #(contains? (set (:execution-function/context %)) :auth/token))
```

## Tiers and Layers

Atlas enforces architectural boundaries through tiers.

### Standard Tiers

```
┌─────────────────────────────────────┐
│         :tier/api                   │  HTTP endpoints, GraphQL
├─────────────────────────────────────┤
│         :tier/service               │  Business logic functions
├─────────────────────────────────────┤
│         :tier/foundation            │  DB, cache, external APIs
└─────────────────────────────────────┘
```

### Tier Rules (Enforced by Invariants)

- Endpoints must be `:tier/api`
- Components must be `:tier/foundation`
- Services can depend on foundation, not vice versa
- API depends on services

## Invariants

Invariants are architectural rules that Atlas validates.

### Built-in Invariants

**Structural:**
- Dependencies must exist
- No circular dependencies
- All functions reachable from endpoints

**Data Flow:**
- Context keys must be producible
- Response keys must be consumed or terminal

**Semantic:**
- External integrations marked async
- Pure functions have no component deps
- Protocol implementations conform to contracts

### Checking Invariants

```clojure
(require '[atlas.invariant :as inv])

;; Check everything
(inv/check-all)
;; => {:valid? true :violations []}

;; Or specific checks
(inv/check [inv/invariant-deps-exist
            inv/invariant-no-circular-deps])

;; Human readable
(println (inv/report))
```

### Custom Invariants

Two styles:

**DSL-based (declarative):**
```clojure
(require '[atlas.invariant.dsl :as dsl])

(dsl/dsl-axiom
  :my/write-ops-have-audit
  :error
  "Write operations must have audit aspect"
  {:op :dsl.op/entity-has-aspect :args :effect/write}      ; when
  {:op :dsl.op/entity-has-aspect :args :audit/enabled})    ; then
```

**Function-based (imperative):**
```clojure
(dsl/fn-axiom
  :my/no-orphan-schemas
  :warning
  "Data schemas should be referenced somewhere"
  (fn [db]
    ;; Return seq of violations
    [{:entity :schema/orphan :message "Not referenced"}]))
```

## The Registry

The registry is a global atom holding all entities:

```clojure
;; The registry atom
atlas.registry/registry

;; Current state
@atlas.registry/registry
;; => {#{:atlas/... :domain/...} {:atlas/dev-id ... :prop ...}
;;     #{:atlas/... :domain/...} {...}
;;     ...}
```

### Registry Operations

```clojure
;; Register
(registry/register! dev-id type aspects props)

;; Query
(query/find-by-aspect @registry/registry :domain/auth)
(query/find-by-dev-id @registry/registry :fn/login)

;; Lookup
(registry/fetch @registry/registry #{:atlas/... :domain/...})
(registry/identity-for :fn/login)

;; Introspect
(registry/summary)            ; stats
(registry/clusters)           ; group by namespace
(registry/registered-types)   ; all entity types
```

## Putting It Together

Here's how these concepts combine in practice:

```clojure
;; 1. A component (foundation tier)
(registry/register!
  :component/stripe
  :atlas/structure-component
  #{:tier/foundation :domain/payments :integration/external}
  {:component/provides [:stripe/charge :stripe/refund]})

;; 2. A function using it (service tier)
(registry/register!
  :fn/process-payment
  :atlas/execution-function
  #{:tier/service :domain/payments :operation/create :effect/write}
  {:execution-function/context [:payment/amount :payment/method :user/id]
   :execution-function/response [:payment/id :payment/status]
   :execution-function/deps #{:component/stripe}})

;; 3. An endpoint exposing it (api tier)
(registry/register!
  :endpoint/create-payment
  :atlas/interface-endpoint
  #{:tier/api :domain/payments :http/post}
  {:endpoint/context [:http/body :auth/user]
   :endpoint/response [:http/json-response]
   :endpoint/deps #{:fn/process-payment}})

;; Now query across dimensions
(query/find-by-aspect @registry/registry :domain/payments)        ; all 3
(query/find-by-aspect @registry/registry :effect/write)           ; just fn
(query/find-by-aspect @registry/registry :integration/external)   ; just component

;; And validate
(inv/check-all)  ; verifies deps exist, tiers respected, etc.
```

## Summary

| Concept | What It Is |
|---------|-----------|
| **Compound Identity** | Set of keywords = semantic meaning |
| **Entity Type** | Category (function, endpoint, component) |
| **Aspects** | Semantic dimensions (domain, tier, operation) |
| **Data Flow** | Context (in) + Response (out) + Deps |
| **Tiers** | Architectural layers with rules |
| **Invariants** | Validated architectural constraints |
| **Registry** | Global store of all entities |

The key insight: **your architecture becomes data** - queryable, validatable, and explorable.
