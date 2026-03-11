# Semantic Aggregation

Derive implicit aspects from the dependency graph instead of tagging them by hand.

## The problem

When `:fn/process-payment` depends on `:component/stripe`, and Stripe is `:integration/external :temporal/async :protocol/http`, the function is implicitly external and async. Without aggregation, the developer must manually repeat these aspects on every function that uses Stripe.

## How it works

After all `register!` calls, run `(aggregate-semantics!)`. It discovers reference properties from the type-ref registry, walks each entity's references transitively, and propagates selected aspects into `:atlas/semantic` on the entity's props map.

```clojure
(require '[atlas.aggregation :as agg])

(agg/aggregate-semantics!)
;; => {:aggregation/enriched 17,
;;     :aggregation/total 80,
;;     :aggregation/type-refs [:structure-component/deps :execution-function/deps]}

(agg/semantic-of :fn/create-order)
;; => [{:aspect :domain/persistence,  :source :component/db,   :depth 1, :via :execution-function/deps}
;;     {:aspect :integration/internal, :source :component/db,   :depth 1, :via :execution-function/deps}
;;     {:aspect :domain/inventory,     :source :fn/reserve-pet, :depth 1, :via :execution-function/deps}]
```

The compound-id is never modified — `:atlas/semantic` lives on the props map alongside `:atlas/dev-id`, `:atlas/type`, etc.

## Type-ref driven discovery

The aggregator does not hardcode which properties to follow. It reads all registered `:atlas/type-ref` entities to discover reference properties:

```
type-ref/execution-function-deps  →  :execution-function/deps
type-ref/structure-component-deps →  :structure-component/deps
type-ref/interface-endpoint-deps  →  :interface-endpoint/deps
```

Any new ontology that registers a type-ref automatically participates in aggregation — no code changes needed.

## What propagates

Not all aspects flow upward. A default namespace allow-list controls propagation:

| Namespace | Propagates? | Rationale |
|---|---|---|
| `integration/*` | yes | A function using an external service IS externally integrated |
| `domain/*` | yes | Touching a persistence component means touching persistence |
| `protocol/*` | yes | HTTP/SMTP propagates to callers |
| `temporal/*` | yes | Async nature propagates |
| `tier/*` | **no** | A service calling a foundation component doesn't become foundation |
| `effect/*` | **no** | A read function calling a write function isn't necessarily a write |
| `operation/*` | **no** | Operation semantics are specific to the declaring entity |
| `component.type/*` | **no** | Implementation detail of the component |

The same namespace set applies to all type-refs. Pass a custom set to narrow or widen:

```clojure
(agg/aggregate-semantics! #{"integration" "protocol"})  ; narrower
```

## Transitive propagation

If A depends on B depends on C, A inherits from both. Depth tracking shows how far the aspect traveled:

```
:fn/create-order → :fn/reserve-pet → :component/db
                    depth 1            depth 2 (or depth 1 if also a direct dep)
```

When multiple deps contribute the same aspect, the shallowest depth wins.

## API

```clojure
;; Run after all register! calls (including ontology modules)
(agg/aggregate-semantics!)

;; Full derived vector with provenance
(agg/semantic-of :fn/create-order)
;; => [{:aspect :domain/persistence :source :component/db :depth 1 :via :execution-function/deps} ...]

;; Just the derived aspect keywords
(agg/semantic-aspects :fn/create-order)
;; => #{:domain/persistence :integration/internal :domain/inventory}

;; Explicit + derived combined
(agg/full-aspects :fn/create-order)
;; => #{:tier/service :domain/orders :effect/write :domain/persistence ...}

;; Provenance: who contributed a specific aspect?
(agg/semantic-sources :fn/cancel-order :domain/inventory)
;; => [{:source :fn/release-reservation :depth 1 :via :execution-function/deps}]

;; Find redundant explicit aspects
(agg/redundant-aspects)
;; => [{:dev-id :fn/process-payment
;;      :redundant [:domain/payments :integration/external]
;;      :sources {:domain/payments [:component/stripe]
;;               :integration/external [:component/stripe]}} ...]
```

## Redundancy invariant

The module auto-registers `:invariant/redundant-explicit-aspects` (severity: `:info`). It flags explicit aspects on a compound-id that are already derivable from the entity's deps via type-refs.

This is informational — redundant aspects aren't wrong, but they add noise. The developer can choose to remove them and let aggregation derive them, or keep them for readability.

```clojure
(require '[atlas.aggregation]) ; auto-registers the invariant
```

### Results across test apps

**pet-shop** (7 redundancies):

```
:fn/process-payment
   :domain/payments        <- [:component/stripe]
   :integration/external   <- [:component/stripe]
:fn/send-order-confirmation
   :domain/notifications   <- [:component/email-service]
   :integration/external   <- [:component/email-service]
   :temporal/async         <- [:component/email-service]
:fn/send-appointment-reminder
   :domain/notifications   <- [:component/twilio]
   :integration/external   <- [:component/twilio]
```

Pattern: functions that use external components manually declare `:integration/external`, `:temporal/async`, and the component's domain. All derivable from the dep.

**calendar-availability** (25 redundancies):

```
:fn/verify-google-signin-token
   :integration/external   <- [:component/google-oauth]
:fn/exchange-oauth-code-for-tokens
   :domain/google          <- [:component/google-oauth]
   :protocol/oauth         <- [:component/google-oauth]
:fn/generate-oauth-authorization-url
   :domain/google          <- [:component/google-oauth]
   :protocol/oauth         <- [:component/google-oauth]
:fn/refresh-oauth-token
   :domain/google          <- [:component/google-oauth]
   :protocol/oauth         <- [:component/google-oauth]
:fn/check-user-availability
   :integration/external   <- [:component/gcal-client]
:fn/list-user-calendar-events
   :integration/external   <- [:component/gcal-client]
:fn/get-calendar-events-in-range
   :integration/external   <- [:component/gcal-client]
:fn/create-authorized-user, :fn/delete-authorized-user,
:fn/find-users-by-language, :fn/get-authorized-user,
:fn/get-or-create-user, :fn/get-user-credential,
:fn/check-user-has-refresh-token, :fn/list-all-authorized-users,
:fn/update-authorized-user
   :domain/users           <- [:component/db]
:endpoint/google-signin-verify
   :integration/external   <- [:fn/verify-google-signin-token]
:endpoint/oauth-initiate
   :protocol/oauth         <- [:fn/generate-oauth-authorization-url]
:endpoint/oauth-callback
   :protocol/oauth         <- [:fn/exchange-oauth-code-for-tokens]
:endpoint/admin-create-user, :endpoint/admin-delete-user,
:endpoint/admin-list-users, :endpoint/admin-update-user
   :domain/users           <- [admin functions]
:endpoint/calendar-events-list, :endpoint/query-availability
   :domain/scheduling      <- [calendar functions]
:component/gcal-client
   :domain/google          <- [:component/google-oauth]
```

Three patterns:
- `:integration/external` on functions using Google components
- `:domain/google` and `:protocol/oauth` on functions using `:component/google-oauth`
- `:domain/users` on every function using `:component/db` (which has `:domain/users` because it IS the user repository)

**cart** (2 redundancies):

```
:component/session-store
   :integration/internal   <- [:component/db]
:endpoint/cart-graph-api
   :domain/cart            <- [:fn/add-to-cart]
```

Minimal — the cart app has very little overlap between explicit and derivable aspects.

## What the developer sees

Before (manual):
```clojure
#{:tier/service :domain/payments :effect/write :integration/external :temporal/async}
{:execution-function/deps #{:component/stripe}}
```

After (with aggregation):
```clojure
#{:tier/service :domain/payments :effect/write}
{:execution-function/deps #{:component/stripe}}
;; :integration/external, :temporal/async, :protocol/http derived from :component/stripe
```

## Properties

- **Type-ref driven** — discovers reference properties from the registry, not hardcoded
- **Props-only** — `:atlas/semantic` is data on the value map, never changes the compound-id
- **Idempotent** — safe to call multiple times; recomputes from scratch
- **Cycle-safe** — tracks visited entities to prevent infinite loops
- **Deduplicating** — each aspect appears at most once, from the shallowest source
