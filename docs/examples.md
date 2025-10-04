# Examples

Annotated examples showing Atlas patterns. See `test/app/` for complete implementations.

## Example 1: Simple CRUD Service

A minimal user management system demonstrating the three tiers.

```clojure
(require '[atlas.registry :as registry])
(require '[atlas.ontology :as ont])

;; Initialize
(ont/register-entity-types!)

;; ═══════════════════════════════════════════════════════════════
;; FOUNDATION TIER - Infrastructure components
;; ═══════════════════════════════════════════════════════════════

(registry/register!
  :component/user-db
  :atlas/structure-component
  #{:tier/foundation                    ; <- Required for components
    :domain/users}                      ; <- Business domain
  {:component/provides [:db/find-user
                        :db/save-user
                        :db/delete-user]})

;; ═══════════════════════════════════════════════════════════════
;; SERVICE TIER - Business logic functions
;; ═══════════════════════════════════════════════════════════════

(registry/register!
  :fn/get-user
  :atlas/execution-function
  #{:tier/service                       ; <- Business logic layer
    :domain/users
    :operation/read}                    ; <- What kind of operation
  {:execution-function/context [:user/id]           ; Data IN
   :execution-function/response [:user/data]        ; Data OUT
   :execution-function/deps #{:component/user-db}}) ; Dependencies

(registry/register!
  :fn/create-user
  :atlas/execution-function
  #{:tier/service
    :domain/users
    :operation/create
    :effect/write}                      ; <- Has side effects
  {:execution-function/context [:user/email :user/name]
   :execution-function/response [:user/id :user/data]
   :execution-function/deps #{:component/user-db}})

(registry/register!
  :fn/delete-user
  :atlas/execution-function
  #{:tier/service
    :domain/users
    :operation/delete
    :effect/write}
  {:execution-function/context [:user/id]
   :execution-function/response [:operation/success?]
   :execution-function/deps #{:component/user-db}})

;; ═══════════════════════════════════════════════════════════════
;; API TIER - HTTP endpoints
;; ═══════════════════════════════════════════════════════════════

(registry/register!
  :endpoint/get-user
  :atlas/interface-endpoint
  #{:tier/api                           ; <- External interface
    :domain/users
    :http/get}                          ; <- HTTP method
  {:endpoint/context [:http/path-params]
   :endpoint/response [:http/json-response]
   :endpoint/deps #{:fn/get-user}})

(registry/register!
  :endpoint/create-user
  :atlas/interface-endpoint
  #{:tier/api
    :domain/users
    :http/post}
  {:endpoint/context [:http/body]
   :endpoint/response [:http/json-response]
   :endpoint/deps #{:fn/create-user}})

(registry/register!
  :endpoint/delete-user
  :atlas/interface-endpoint
  #{:tier/api
    :domain/users
    :http/delete}
  {:endpoint/context [:http/path-params]
   :endpoint/response [:http/status]
   :endpoint/deps #{:fn/delete-user}})
```

**Query this registry:**

```clojure
(require '[atlas.query :as query])

;; All user domain entities
(query/find-by-aspect @registry/registry :domain/users)
;; => 6 entities

;; Only write operations
(query/find-by-aspect @registry/registry :effect/write)
;; => [:fn/create-user :fn/delete-user]

;; Group by tier
(query/by-tier @registry/registry)
;; => {:tier/api [...], :tier/service [...], :tier/foundation [...]}
```

---

## Example 2: Multi-Domain System

A system with multiple domains and cross-domain dependencies.

```clojure
;; ═══════════════════════════════════════════════════════════════
;; AUTH DOMAIN
;; ═══════════════════════════════════════════════════════════════

(registry/register!
  :component/oauth-provider
  :atlas/structure-component
  #{:tier/foundation
    :domain/auth
    :integration/external}              ; <- External service
  {:component/provides [:oauth/validate-token
                        :oauth/refresh-token]})

(registry/register!
  :fn/validate-session
  :atlas/execution-function
  #{:tier/service
    :domain/auth
    :operation/validate}
  {:execution-function/context [:auth/token]
   :execution-function/response [:auth/valid? :auth/user-id]
   :execution-function/deps #{:component/oauth-provider}})

;; ═══════════════════════════════════════════════════════════════
;; ORDERS DOMAIN (depends on auth)
;; ═══════════════════════════════════════════════════════════════

(registry/register!
  :component/order-db
  :atlas/structure-component
  #{:tier/foundation :domain/orders}
  {:component/provides [:db/create-order :db/get-order]})

(registry/register!
  :fn/create-order
  :atlas/execution-function
  #{:tier/service
    :domain/orders
    :operation/create
    :effect/write}
  {:execution-function/context [:auth/user-id    ; <- From auth domain!
                                :order/items
                                :order/shipping-address]
   :execution-function/response [:order/id :order/total]
   :execution-function/deps #{:component/order-db
                              :fn/validate-session}})  ; <- Cross-domain dep

(registry/register!
  :endpoint/create-order
  :atlas/interface-endpoint
  #{:tier/api
    :domain/orders
    :http/post}
  {:endpoint/context [:http/body :http/headers]  ; auth token in headers
   :endpoint/response [:http/json-response]
   :endpoint/deps #{:fn/create-order}})
```

**Analyze cross-domain coupling:**

```clojure
;; What domains does orders depend on?
(->> (query/find-by-aspect @registry/registry :domain/orders)
     (mapcat #(-> % :props :execution-function/deps))
     (map registry/identity-for)
     (mapcat #(filter (fn [k] (= "domain" (namespace k))) %))
     (into #{}))
;; => #{:domain/orders :domain/auth}
```

---

## Example 3: Data Schemas

Defining data structures with semantic annotations.

```clojure
;; ═══════════════════════════════════════════════════════════════
;; DATA SCHEMAS
;; ═══════════════════════════════════════════════════════════════

(registry/register!
  :schema/user
  :atlas/data-schema
  #{:domain/users
    :data/entity}                       ; <- It's an entity (has ID)
  {:schema/fields [:user/id
                   :user/email
                   :user/name
                   :user/created-at]})

(registry/register!
  :schema/user-credentials
  :atlas/data-schema
  #{:domain/auth
    :data/sensitive                     ; <- Contains PII
    :compliance/gdpr}                   ; <- GDPR relevant
  {:schema/fields [:auth/email
                   :auth/password-hash
                   :auth/mfa-secret]})

(registry/register!
  :schema/order
  :atlas/data-schema
  #{:domain/orders
    :data/entity
    :data/aggregate}                    ; <- Aggregate root
  {:schema/fields [:order/id
                   :order/user-id
                   :order/items         ; <- nested
                   :order/total
                   :order/status]})

(registry/register!
  :schema/order-item
  :atlas/data-schema
  #{:domain/orders
    :data/value-object}                 ; <- No independent ID
  {:schema/fields [:item/product-id
                   :item/quantity
                   :item/price]})
```

**Query schemas:**

```clojure
;; Find all sensitive data
(query/find-by-aspect @registry/registry :data/sensitive)
;; => [:schema/user-credentials]

;; Find GDPR-relevant schemas
(query/find-by-aspect @registry/registry :compliance/gdpr)
```

---

## Example 4: Protocols and Implementations

Defining interfaces and validating conformance.

```clojure
;; ═══════════════════════════════════════════════════════════════
;; PROTOCOL DEFINITION
;; ═══════════════════════════════════════════════════════════════

(registry/register!
  :protocol/payment-provider
  :atlas/interface-protocol
  #{:domain/payments}
  {:protocol/functions [:payment/charge
                        :payment/refund
                        :payment/get-status]})

;; ═══════════════════════════════════════════════════════════════
;; IMPLEMENTATION (Stripe)
;; ═══════════════════════════════════════════════════════════════

(registry/register!
  :component/stripe
  :atlas/structure-component
  #{:tier/foundation
    :domain/payments
    :integration/external}
  {:component/implements :protocol/payment-provider  ; <- Declares conformance
   :component/provides [:payment/charge              ; <- Must match protocol
                        :payment/refund
                        :payment/get-status]})

;; ═══════════════════════════════════════════════════════════════
;; ALTERNATIVE IMPLEMENTATION (PayPal)
;; ═══════════════════════════════════════════════════════════════

(registry/register!
  :component/paypal
  :atlas/structure-component
  #{:tier/foundation
    :domain/payments
    :integration/external}
  {:component/implements :protocol/payment-provider
   :component/provides [:payment/charge
                        :payment/refund
                        :payment/get-status]})
```

**Invariants will verify** that implementations provide all protocol functions.

---

## Example 5: Custom Entity Types

Extending Atlas with your own entity types.

```clojure
;; ═══════════════════════════════════════════════════════════════
;; DEFINE CUSTOM TYPE
;; ═══════════════════════════════════════════════════════════════

(registry/register!
  :myapp/event
  :atlas/type                           ; <- Register as a type
  #{}
  {:registry-definition/keys [:event/topic
                              :event/schema
                              :event/producers
                              :event/consumers]})

;; ═══════════════════════════════════════════════════════════════
;; USE CUSTOM TYPE
;; ═══════════════════════════════════════════════════════════════

(registry/register!
  :event/user-created
  :myapp/event                          ; <- Use your custom type
  #{:domain/users}
  {:event/topic "users.created"
   :event/schema :schema/user
   :event/producers #{:fn/create-user}
   :event/consumers #{:fn/send-welcome-email
                      :fn/update-analytics}})

(registry/register!
  :event/order-placed
  :myapp/event
  #{:domain/orders}
  {:event/topic "orders.placed"
   :event/schema :schema/order
   :event/producers #{:fn/create-order}
   :event/consumers #{:fn/process-payment
                      :fn/notify-warehouse}})
```

**Query your custom types:**

```clojure
;; Find all events
(query/where @registry/registry
  #(= :myapp/event (registry/entity-type (:identity %))))

;; Find events with specific consumer
(query/where @registry/registry
  #(contains? (-> % :props :event/consumers) :fn/process-payment))
```

---

## Example 6: Testing Pattern

Separating test implementations from production.

```clojure
;; ═══════════════════════════════════════════════════════════════
;; PRODUCTION COMPONENT
;; ═══════════════════════════════════════════════════════════════

(registry/register!
  :component/email-service
  :atlas/structure-component
  #{:tier/foundation
    :domain/notifications
    :integration/external}
  {:component/provides [:email/send]})

;; ═══════════════════════════════════════════════════════════════
;; TEST IMPLEMENTATION (separate entity!)
;; ═══════════════════════════════════════════════════════════════

(registry/register!
  :component/email-service-mock
  :atlas/structure-component
  #{:tier/foundation
    :domain/notifications
    :test/mock}                         ; <- Marked as test mock
  {:component/provides [:email/send]
   :component/replaces :component/email-service})
```

**Filter in production vs test:**

```clojure
;; Production: exclude test mocks
(query/where @registry/registry
  #(not (contains? (:identity %) :test/mock)))

;; Test: find all mocks
(query/find-by-aspect @registry/registry :test/mock)
```

---

## Patterns Summary

| Pattern | Key Aspects |
|---------|-------------|
| **CRUD Service** | `:operation/create`, `:operation/read`, etc. |
| **Write Effects** | `:effect/write` |
| **External Integration** | `:integration/external` |
| **Sensitive Data** | `:data/sensitive`, `:compliance/gdpr` |
| **Test Mocks** | `:test/mock` |
| **Custom Types** | Register with `:atlas/type` |

---

## Full Examples

For complete, runnable examples see:

- `test/app/calendar_availability.clj` - OAuth, users, calendar events
- `test/app/cart.clj` - Shopping cart with sessions
- `test/app/pet_shop.clj` - Multi-domain with external integrations
