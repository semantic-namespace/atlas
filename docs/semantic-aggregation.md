# Semantic Aggregation

Derive implicit aspects from the dependency graph — built into `compile!`.

## The problem

When `:fn/process-payment` depends on `:component/stripe`, and Stripe is `:integration/external :temporal/async :protocol/http`, the function is implicitly external and async. Without aggregation, the developer must manually repeat these aspects.

## How it works

`compile!` walks the dependency graph via type-refs and expands compound-ids with all aspects from deps. All aspects propagate bottom-to-top — no namespace filter.

```clojure
(require '[atlas.registry :as registry])

(registry/compile!)
;; => {:compile/entities 80, :compile/aggregated 17, :compile/type-refs [...]}

;; Declared by developer:
(registry/declared-aspects :fn/create-order)
;; => #{:domain/orders :effect/write :tier/service}

;; Added by aggregation:
(registry/derived-aspects :fn/create-order)
;; => #{:domain/persistence :domain/inventory :integration/internal
;;      :tier/foundation :component.type/database :effect/write :operation/reserve}
```

The compound-id in the compiled registry includes both declared and derived aspects. Queries work directly — `find-by-aspect :tier/foundation` finds everything that transitively touches foundation.

## Design: propagate everything, filter at query time

All aspects propagate. There is no namespace allow-list or deny-list.

**Rationale**: filtering at propagation time permanently loses information. Filtering at query time is strictly more powerful.

### Why this matters for each namespace

| Namespace | Signal when propagated |
|---|---|
| `effect/*` | A read fn calling a write fn transitively causes writes |
| `tier/*` | Entity keeps its own tier; derived tier shows dependency reach |
| `operation/*` | An endpoint shows the operations it orchestrates |
| `component.type/*` | Infrastructure deps visible without reading the full dep tree |
| `integration/*` | Callers of external services are externally integrated |
| `domain/*` | Touching a persistence component means touching that domain |
| `protocol/*` | HTTP/OAuth/SMTP propagates to callers |
| `temporal/*` | Async nature propagates |

## Type-ref driven discovery

The aggregator reads all registered `:atlas/type-ref` entities to discover which properties to follow:

```
type-ref/execution-function-deps  →  :execution-function/deps
type-ref/structure-component-deps →  :structure-component/deps
type-ref/interface-endpoint-deps  →  :interface-endpoint/deps
```

Any new ontology that registers a type-ref automatically participates in aggregation.

## Log preserves declared identity

The registration log always has the developer's original declaration:

```clojure
(registry/declared-aspects :fn/create-order)
;; => #{:domain/orders :effect/write :tier/service}
```

Provenance (which dep contributed which aspect) is computable on-demand by diffing declared vs compiled and walking deps. It is not stored in the registry — cleaner serialization.

## Redundancy invariant

`atlas.aggregation` auto-registers `:invariant/redundant-explicit-aspects` (severity: `:info`). It flags declared aspects that are already derivable from deps.

```clojure
(require '[atlas.aggregation])
(atlas.aggregation/redundant-aspects)
;; => [{:dev-id :fn/process-payment :redundant [:integration/external :temporal/async]} ...]
```

## Properties

- **Built into compile!** — no separate step
- **Propagate everything** — all aspects flow bottom-to-top
- **Type-ref driven** — discovers reference properties from the registry
- **Log preserves declared** — original aspects always recoverable
- **Cycle-safe** — tracks visited entities
- **Idempotent** — compile! always rebuilds from scratch
