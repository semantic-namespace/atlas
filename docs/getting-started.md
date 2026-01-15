# Getting Started with Atlas

> **5-minute tutorial** to understand Atlas basics.

## What You'll Build

A simple user management system with:
- A database component
- A function to fetch users
- An API endpoint

## Prerequisites

- Clojure CLI tools installed
- Basic Clojure knowledge

## Step 1: Setup

Add Atlas to your `deps.edn`:


```clojure
{:deps {com.github.semantic-namespace/atlas {:mvn/version "0.0.1-SNAPSHOT"}}
```

Start a REPL:

```bash
clojure -M:repl
```

## Step 2: Initialize the Registry

```clojure
(require '[atlas.registry :as registry])
(require '[atlas.ontology :as ont])

;; Register built-in entity types
(ont/register-entity-types!)
```

The registry is a global atom that holds all your semantic entities.

## Step 3: Register a Component

Components are foundational building blocks (databases, caches, external services):

```clojure
(registry/register!
  :component/user-db                    ; dev-id: human-readable identifier
  :atlas/structure-component            ; type: what kind of entity
  #{:tier/foundation                    ; aspects: semantic tags
    :domain/users}
  {:component/provides [:db/find-user   ; what this component offers
                        :db/save-user]})
```

**What happened?**
- Created an entity with compound identity `#{:atlas/structure-component :tier/foundation :domain/users}`
- Stored properties describing what it provides
- The `:domain/users` aspect links it semantically to user-related entities

## Step 4: Register a Function

Functions contain business logic and declare their data flow:

```clojure
(registry/register!
  :fn/get-user-by-id
  :atlas/execution-function
  #{:tier/service
    :domain/users
    :operation/read}
  {:execution-function/context [:user/id]           ; data this function needs
   :execution-function/response [:user/data]        ; data this function produces
   :execution-function/deps #{:component/user-db}}) ; dependencies
```

**Key insight:** The function declares:
- **Context**: What data flows in (`:user/id`)
- **Response**: What data flows out (`:user/data`)
- **Dependencies**: What components it needs

## Step 5: Register an Endpoint

Endpoints expose functions via HTTP:

```clojure
(registry/register!
  :endpoint/get-user
  :atlas/interface-endpoint
  #{:tier/api
    :domain/users
    :http/get}
  {:endpoint/context [:http/path-params]
   :endpoint/response [:http/json-response]
   :endpoint/deps #{:fn/get-user-by-id}})
```

## Step 6: Query the Registry

Now the powerful part - querying your architecture:

```clojure
(require '[atlas.query :as query])

;; Find all entities in the users domain
(query/find-by-aspect @registry/registry :domain/users)
;; => Returns all three entities we registered

;; Find only service-tier entities
(query/find-by-aspect @registry/registry #{:tier/service :domain/users})
;; => Returns just :fn/get-user-by-id

;; Get an entity's full semantic identity
(registry/identity-for :fn/get-user-by-id)
;; => #{:atlas/execution-function :tier/service :domain/users :operation/read}
```

## Step 7: Check Invariants

Atlas can validate your architecture:

```clojure
(require '[atlas.invariant :as inv])

;; Check all invariants
(inv/check-all)
;; => {:valid? true, :violations []}

;; Get a readable report
(println (inv/report))
```

What gets checked:
- All dependencies reference existing entities
- No circular dependencies
- Data flows are satisfiable (inputs are produced somewhere)
- Tier boundaries respected
- And more...

## Step 8: Explore (Optional)

For a visual view of your registry:

```clojure
(require '[atlas.atlas-ui.server :as ui])
(ui/start!)
;; Opens browser at http://localhost:8082
```

## What You Learned

1. **Compound identities** - Entities are sets of semantic aspects
2. **Entity types** - Different kinds of entities (functions, components, endpoints)
3. **Data flow** - Functions declare context (in) and response (out)
4. **Querying** - Find entities by any aspect combination
5. **Invariants** - Validate architectural rules automatically

## Next Steps

- Read [Core Concepts](concepts.md) for deeper understanding
- See [Examples](examples.md) for real-world patterns
- Check [API Reference](api-reference.md) for all functions

## Common Questions

**Q: Why not just use namespaces?**

Namespaces give you one dimension (where code lives). Compound identities give you N dimensions (what code means). You can query by domain, tier, operation type, etc. - all at once.

**Q: Do I need to use the UI?**

No. The core value is in the registry and query APIs. The UI is a bonus for exploration.

**Q: Can I define custom entity types?**

Yes! Register them with `:atlas/type`:

```clojure
(registry/register!
  :my-project/workflow
  :atlas/type
  #{}
  {:registry-definition/keys [:workflow/steps :workflow/transitions]})
```

**Q: What about actual function implementations?**

Atlas describes architecture, not implementations. Your actual functions live in regular Clojure namespaces. Atlas tells you what they mean and how they relate.
