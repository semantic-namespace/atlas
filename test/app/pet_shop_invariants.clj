(ns app.pet-shop-invariants
  "DSL invariant definitions for the pet shop application.

   These invariants encode architectural constraints for:
   - Payment security requirements
   - Authorization boundaries
   - Notification channel routing
   - Domain integrity rules
   - Architectural patterns"
  (:require
   [atlas.invariant.unified :as ax]))

;; =============================================================================
;; PAYMENT SECURITY AXIOMS
;; =============================================================================

(def payment-functions-use-stripe
  "Payment domain functions must depend on Stripe component."
  (ax/dsl-invariant
   :pet-shop/payment-uses-stripe
   :error
   "Payment processing must use Stripe component"
   {:op :dsl.op/logic-and
    :args [{:op :dsl.op/entity-has-aspect :args :domain/payments}
           {:op :dsl.op/entity-has-aspect :args :atlas/execution-function}]}
   {:op :dsl.op/entity-depends-on :args :component/stripe}))

(def payment-is-async
  "Payment operations should be marked as async (external integration)."
  (ax/dsl-invariant
   :pet-shop/payment-is-async
   :warning
   "Payment operations should be async"
   {:op :dsl.op/logic-and
    :args [{:op :dsl.op/entity-has-aspect :args :domain/payments}
           {:op :dsl.op/entity-has-aspect :args :atlas/execution-function}]}
   {:op :dsl.op/entity-has-aspect :args :temporal/async}))

(def payment-is-external
  "Payment operations are external integrations."
  (ax/dsl-invariant
   :pet-shop/payment-is-external
   :warning
   "Payment operations should be marked as external integration"
   {:op :dsl.op/logic-and
    :args [{:op :dsl.op/entity-has-aspect :args :domain/payments}
           {:op :dsl.op/entity-has-aspect :args :atlas/execution-function}]}
   {:op :dsl.op/entity-has-aspect :args :integration/external}))

;; =============================================================================
;; AUTHORIZATION AXIOMS
;; =============================================================================

(def order-endpoints-require-auth
  "Order-related endpoints must require authorization."
  (ax/dsl-invariant
   :pet-shop/orders-require-auth
   :error
   "Order endpoints must require authorization"
   {:op :dsl.op/logic-and
    :args [{:op :dsl.op/entity-has-aspect :args :atlas/interface-endpoint}
           {:op :dsl.op/entity-has-aspect :args :domain/orders}
           {:op :dsl.op/entity-lacks-aspect :args :ux.visibility/public}]}
   {:op :dsl.op/entity-has-aspect :args :authorization/required}))

(def grooming-endpoints-require-auth
  "Grooming booking endpoints must require authorization."
  (ax/dsl-invariant
   :pet-shop/grooming-booking-requires-auth
   :error
   "Grooming booking endpoints must require authorization"
   {:op :dsl.op/logic-and
    :args [{:op :dsl.op/entity-has-aspect :args :atlas/interface-endpoint}
           {:op :dsl.op/entity-has-aspect :args :domain/grooming}
           {:op :dsl.op/entity-has-aspect :args :operation/create}]}
   {:op :dsl.op/entity-has-aspect :args :authorization/required}))

;; =============================================================================
;; NOTIFICATION ROUTING AXIOMS
;; =============================================================================

(def email-notifications-use-email-service
  "Email channel notification functions must use email service component."
  (ax/dsl-invariant
   :pet-shop/email-uses-service
   :error
   "Email notification functions must use email service component"
   {:op :dsl.op/logic-and
    :args [{:op :dsl.op/entity-has-aspect :args :notification.channel/email}
           {:op :dsl.op/entity-has-aspect :args :atlas/execution-function}]}
   {:op :dsl.op/entity-depends-on :args :component/email-service}))

(def sms-notifications-use-twilio
  "SMS channel notification functions must use Twilio component."
  (ax/dsl-invariant
   :pet-shop/sms-uses-twilio
   :error
   "SMS notification functions must use Twilio component"
   {:op :dsl.op/logic-and
    :args [{:op :dsl.op/entity-has-aspect :args :notification.channel/sms}
           {:op :dsl.op/entity-has-aspect :args :atlas/execution-function}]}
   {:op :dsl.op/entity-depends-on :args :component/twilio}))

(def notifications-are-async
  "Notification functions should be async."
  (ax/dsl-invariant
   :pet-shop/notifications-async
   :warning
   "Notification operations should be async"
   {:op :dsl.op/entity-has-aspect :args :domain/notifications}
   {:op :dsl.op/entity-has-aspect :args :temporal/async}))

;; =============================================================================
;; DOMAIN INTEGRITY AXIOMS
;; =============================================================================

(def inventory-writes-use-db
  "Inventory write operations must use database."
  (ax/dsl-invariant
   :pet-shop/inventory-writes-use-db
   :error
   "Inventory write operations must use database"
   {:op :dsl.op/logic-and
    :args [{:op :dsl.op/entity-has-aspect :args :domain/inventory}
           {:op :dsl.op/entity-has-aspect :args :effect/write}]}
   {:op :dsl.op/entity-depends-on :args :component/db}))

(def customer-ops-use-db
  "Customer service functions must use database."
  (ax/dsl-invariant
   :pet-shop/customer-ops-use-db
   :error
   "Customer service functions must use database"
   {:op :dsl.op/logic-and
    :args [{:op :dsl.op/entity-has-aspect :args :domain/customers}
           {:op :dsl.op/entity-has-aspect :args :atlas/execution-function}
           {:op :dsl.op/entity-lacks-aspect :args :atlas/interface-endpoint}]}
   {:op :dsl.op/entity-depends-on :args :component/db}))

(def grooming-ops-use-db
  "Grooming service functions must use database."
  (ax/dsl-invariant
   :pet-shop/grooming-ops-use-db
   :error
   "Grooming service functions must use database"
   {:op :dsl.op/logic-and
    :args [{:op :dsl.op/entity-has-aspect :args :domain/grooming}
           {:op :dsl.op/entity-has-aspect :args :atlas/execution-function}
           {:op :dsl.op/entity-lacks-aspect :args :atlas/interface-endpoint}]}
   {:op :dsl.op/entity-depends-on :args :component/db}))

;; =============================================================================
;; PURE FUNCTION AXIOMS
;; =============================================================================

(def pure-functions-no-component-deps
  "Pure functions must not depend on components."
  (ax/dsl-invariant
   :pet-shop/pure-no-components
   :error
   "Pure functions must not depend on components"
   {:op :dsl.op/entity-has-aspect :args :effect/pure}
   {:op :dsl.op/logic-not
    :args [{:op :dsl.op/logic-or
            :args [{:op :dsl.op/entity-depends-on :args :component/db}
                   {:op :dsl.op/entity-depends-on :args :component/cache}
                   {:op :dsl.op/entity-depends-on :args :component/stripe}
                   {:op :dsl.op/entity-depends-on :args :component/twilio}
                   {:op :dsl.op/entity-depends-on :args :component/email-service}]}]}))

;; =============================================================================
;; TIER AXIOMS
;; =============================================================================

(def components-are-foundation
  "All components must be in foundation tier."
  (ax/dsl-invariant
   :pet-shop/components-foundation
   :error
   "Components must be foundation tier"
   {:op :dsl.op/entity-has-aspect :args :atlas/structure-component}
   {:op :dsl.op/entity-has-aspect :args :tier/foundation}))

(def endpoints-are-api
  "All endpoints must be in API tier."
  (ax/dsl-invariant
   :pet-shop/endpoints-api
   :error
   "Endpoints must be API tier"
   {:op :dsl.op/entity-has-aspect :args :atlas/interface-endpoint}
   {:op :dsl.op/entity-has-aspect :args :tier/api}))

;; =============================================================================
;; EXTERNAL INTEGRATION AXIOMS
;; =============================================================================

(def external-components-are-async
  "External integration components should be async."
  (ax/dsl-invariant
   :pet-shop/external-is-async
   :warning
   "External integrations should be async"
   {:op :dsl.op/logic-and
    :args [{:op :dsl.op/entity-has-aspect :args :integration/external}
           {:op :dsl.op/entity-has-aspect :args :atlas/structure-component}]}
   {:op :dsl.op/entity-has-aspect :args :temporal/async}))

;; =============================================================================
;; ARCHITECTURAL AXIOMS
;; =============================================================================

(def no-dependency-cycles
  "Dependency graph must be acyclic."
  (ax/dsl-invariant
   :pet-shop/no-cycles
   :error
   "Dependency graph must be acyclic"
   {:op :dsl.op/entity-has-aspect :args :atlas/execution-function}
   {:op :dsl.op/graph-acyclic :args {:edge :depends}}))

(def all-functions-reachable
  "All functions should be reachable from endpoints."
  (ax/dsl-invariant
   :pet-shop/all-reachable
   :warning
   "All functions should be reachable from endpoints"
   {:op :dsl.op/entity-has-aspect :args :atlas/execution-function}
   {:op :dsl.op/graph-reachable :args {:edge :depends}}))

;; =============================================================================
;; AXIOM COLLECTIONS
;; =============================================================================

(def payment-invariants
  "Payment-related invariants."
  [payment-functions-use-stripe
   payment-is-async
   payment-is-external])

(def authorization-invariants
  "Authorization boundary invariants."
  [order-endpoints-require-auth
   grooming-endpoints-require-auth])

(def notification-invariants
  "Notification routing invariants."
  [email-notifications-use-email-service
   sms-notifications-use-twilio
   notifications-are-async])

(def domain-integrity-invariants
  "Domain data integrity invariants."
  [inventory-writes-use-db
   customer-ops-use-db
   grooming-ops-use-db])

(def tier-invariants
  "Tier validation invariants."
  [components-are-foundation
   endpoints-are-api])

(def architectural-invariants
  "Architecture validation invariants."
  [no-dependency-cycles
   all-functions-reachable])

(def all-invariants
  "All pet shop invariants."
  (concat payment-invariants
          authorization-invariants
          notification-invariants
          domain-integrity-invariants
          [pure-functions-no-component-deps
           external-components-are-async]
          tier-invariants
          architectural-invariants))

;; =============================================================================
;; PUBLIC API
;; =============================================================================

(defn check-all
  "Check all pet shop invariants."
  []
  (ax/check-all all-invariants))

(defn check-payment
  "Check payment-related invariants only."
  []
  (ax/check-all payment-invariants))

(defn check-authorization
  "Check authorization invariants only."
  []
  (ax/check-all authorization-invariants))

(defn check-notifications
  "Check notification invariants only."
  []
  (ax/check-all notification-invariants))

(defn check-domain
  "Check domain integrity invariants only."
  []
  (ax/check-all domain-integrity-invariants))

(defn check-tiers
  "Check tier invariants only."
  []
  (ax/check-all tier-invariants))

(defn check-architecture
  "Check architectural invariants only."
  []
  (ax/check-all architectural-invariants))

(defn report
  "Print human-readable invariant report."
  []
  (ax/report all-invariants))
