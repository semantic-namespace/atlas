# Declared Dispatch — Atlas Execution Pattern

## The Problem

When code calls other code through a registry lookup instead of a direct function call, you get the **Service Locator** pattern:

```clojure
;; Direct call — dependency is visible in the require
(ns app.orders
  (:require [app.validation :as v]))

(defn process-order [order]
  (v/validate-order order))  ;; explicit, traceable, compile-time checked

;; Service Locator — dependency is hidden in the body
(defn process-order [order]
  (lookup/handle-tool {:tool/name :fn/validate-order
                       :tool/args {:order/data order}}))  ;; implicit, runtime-only
```

Service Locator has known costs:

| Cost | Description |
|------|-------------|
| **Implicit dependencies** | Can't see what a function depends on from its signature or requires |
| **No compile-time safety** | Typo in `:fn/validate-ordr` returns nil at runtime, not an error at load time |
| **Harder testing** | Must populate the registry before testing; can't just `with-redefs` a var |
| **Opaque call graph** | `grep` can't trace dynamic keyword lookups the way it traces function calls |
| **Runtime-only failure** | Removing a registration breaks nothing until someone calls it |

## Atlas Approach: Declared Dispatch

Atlas mitigates Service Locator costs by requiring every entity to **declare its dependencies** as part of its registration:

```clojure
(registry/register!
 :fn/process-order
 :atlas/execution-function
 #{:domain/orders :tier/service :effect/write}
 {:execution-function/deps #{:fn/validate-order :fn/calculate-price}
  :execution-function/context [:order/data]
  :execution-function/response [:order/confirmed?]
  :atlas/impl (fn [{:keys [order/data]}]
                ...)})
```

The dependency on `:fn/validate-order` is not hidden — it's declared in `:execution-function/deps` and lives in the registry as queryable data.

### What this gives you

**Visible dependency graph.** Every entity's deps are data, not buried in source:

```clojure
(lookup/props-for :fn/process-order)
;; => {:execution-function/deps #{:fn/validate-order :fn/calculate-price} ...}
```

**Invariant checking.** Validate that declared deps actually resolve, that architectural rules hold:

```clojure
(invariant/check-all)
;; catches: :fn/process-order declares dep on :fn/validate-order, which doesn't exist
```

**Blast radius.** Trace what breaks if you change something:

```clojure
(handle-tool {:tool/name :atlas.llm-ide/blast-radius
              :tool/args {:entity/dev-id-or-set :fn/validate-order}})
;; => :fn/process-order is downstream
```

**Architectural reasoning.** Query by any dimension:

```clojure
;; "What writes depend on what validations?"
;; "Which service-tier functions have no declared deps?"
;; "What's the longest dependency chain?"
```

## Service Locator vs Declared Dispatch

| | Service Locator | Declared Dispatch (Atlas) |
|---|---|---|
| Dependencies | Hidden in function body | Declared in `:execution-function/deps` |
| Discoverability | Grep for string literals | Query the registry |
| Compile-time safety | No | No (but `invariant/check-all` catches missing deps) |
| Call graph | Opaque | Queryable as data |
| Blast radius | Manual trace | `blast-radius` tool |
| Architectural rules | None | Invariants enforce them |
| Testing | Populate registry first | Same, but deps are declared — you know what to mock |

The remaining gap is **compile-time safety**: a misspelled keyword in `:deps` or `:tool/name` fails at runtime, not load time. `invariant/check-all` closes this gap at dev time — run it at REPL startup, in tests, or in CI.

## When to use which

**Direct function calls** — internal Clojure code where the caller and callee are in the same process, authored by the same team, and the dependency is straightforward:

```clojure
(defn process-order [order]
  (validate-order order)   ;; just call it
  (calculate-price order))
```

**Declared dispatch via handle-tool** — when any of these apply:

- **Cross-process boundary**: Emacs, MCP, or other external clients that can't call Clojure functions directly
- **Architectural visibility matters**: the dependency graph needs to be queryable, not just executable
- **Invariant enforcement**: you want to validate rules like "every `:effect/write` entity must depend on an `:operation/validate` entity"
- **Multi-dimensional discovery**: an LLM or tool needs to ask "what tools exist for `:intent/history`?"

**The line to hold**: if you're writing Clojure calling Clojure in the same process and don't need architectural reasoning about the call, use a direct function call. Declared dispatch earns its keep when the dependency graph itself is a first-class concern.

## handle-tool

The shared dispatcher lives in `atlas.registry.lookup`:

```clojure
(lookup/handle-tool {:tool/name :atlas.source-tracker/recent-entities
                     :tool/args {:query/count 5}})
```

It resolves `:atlas/impl` from the registry for the given dev-id and calls it with the args map. Returns the result or nil if no impl is found. Domain-agnostic — works for `:domain/ide`, `:domain/llm-ide`, or any other domain.

## Self-describing tools

Every tool registered with `:atlas/impl` also carries:

- `:atlas/docs` — human/LLM-readable description of what the tool does
- `:execution-function/context` — what keys the tool expects as input
- `:execution-function/response` — what shape the tool returns
- Aspects — `:intent/history`, `:tier/tooling`, etc. for discovery

This means the system can answer "what tools exist?", "what does this tool expect?", and "what will it return?" — all from the registry, before calling anything.
