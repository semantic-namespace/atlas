(ns app.cart-invariants
  "DSL invariant definitions for the shopping cart application.

   These invariants encode architectural constraints for:
   - Session management requirements
   - Cart calculation purity
   - PII compliance
   - Data flow integrity"
  (:require
   [atlas.invariant.unified :as ax]))

;; =============================================================================
;; SESSION MANAGEMENT AXIOMS
;; =============================================================================

(def session-read-uses-store
  "Session read operations must use session store."
  (ax/dsl-invariant
   :cart/session-read-uses-store
   :error
   "Session read operations must use session store"
   {:op :dsl.op/logic-and
    :args [{:op :dsl.op/entity-has-aspect :args :domain/session}
           {:op :dsl.op/entity-has-aspect :args :operation/read}]}
   {:op :dsl.op/entity-depends-on :args :component/session-store}))

(def session-update-uses-store
  "Session update operations must use session store."
  (ax/dsl-invariant
   :cart/session-update-uses-store
   :error
   "Session update operations must use session store"
   {:op :dsl.op/logic-and
    :args [{:op :dsl.op/entity-has-aspect :args :domain/session}
           {:op :dsl.op/entity-has-aspect :args :operation/update}]}
   {:op :dsl.op/entity-depends-on :args :component/session-store}))

(def session-id-consumers-use-store
  "Service functions consuming session/id must depend on session store."
  (ax/dsl-invariant
   :cart/session-id-consumers
   :error
   "Service functions consuming session/id must depend on session store"
   {:op :dsl.op/logic-and
    :args [{:op :dsl.op/entity-consumes :args :session/id}
           {:op :dsl.op/entity-lacks-aspect :args :atlas/interface-endpoint}]}
   {:op :dsl.op/entity-depends-on :args :component/session-store}))

;; =============================================================================
;; CART CALCULATION AXIOMS
;; =============================================================================

(def calculate-subtotal-is-pure
  "Cart subtotal calculation must be pure."
  (ax/dsl-invariant
   :cart/subtotal-is-pure
   :error
   "Subtotal calculation must be a pure function"
   {:op :dsl.op/entity-has-aspect :args :operation/calculate-subtotal}
   {:op :dsl.op/entity-has-aspect :args :effect/pure}))

(def calculate-tax-is-pure
  "Cart tax calculation must be pure."
  (ax/dsl-invariant
   :cart/tax-is-pure
   :error
   "Tax calculation must be a pure function"
   {:op :dsl.op/entity-has-aspect :args :operation/calculate-tax}
   {:op :dsl.op/entity-has-aspect :args :effect/pure}))

(def calculate-total-is-pure
  "Cart total calculation must be pure."
  (ax/dsl-invariant
   :cart/total-is-pure
   :error
   "Total calculation must be a pure function"
   {:op :dsl.op/entity-has-aspect :args :operation/calculate-total}
   {:op :dsl.op/entity-has-aspect :args :effect/pure}))

(def pure-functions-no-component-deps
  "Pure functions must not depend on components."
  (ax/dsl-invariant
   :cart/pure-no-components
   :error
   "Pure functions must not depend on components"
   {:op :dsl.op/entity-has-aspect :args :effect/pure}
   {:op :dsl.op/logic-not
    :args [{:op :dsl.op/logic-or
            :args [{:op :dsl.op/entity-depends-on :args :component/db}
                   {:op :dsl.op/entity-depends-on :args :component/session-store}]}]}))

;; =============================================================================
;; PII COMPLIANCE AXIOMS
;; =============================================================================

(def pii-functions-are-audited
  "PII-handling service functions must be audited."
  (ax/dsl-invariant
   :cart/pii-is-audited
   :error
   "PII-handling service functions must be audited"
   {:op :dsl.op/logic-and
    :args [{:op :dsl.op/entity-has-aspect :args :compliance/pii}
           {:op :dsl.op/entity-has-aspect :args :atlas/execution-function}
           {:op :dsl.op/entity-lacks-aspect :args :atlas/interface-endpoint}]}
   {:op :dsl.op/entity-has-aspect :args :compliance/audited}))

(def pii-endpoints-require-auth
  "Endpoints handling PII must require authorization."
  (ax/dsl-invariant
   :cart/pii-requires-auth
   :error
   "PII-handling endpoints must require authorization"
   {:op :dsl.op/logic-and
    :args [{:op :dsl.op/entity-has-aspect :args :compliance/pii}
           {:op :dsl.op/entity-has-aspect :args :atlas/interface-endpoint}]}
   {:op :dsl.op/entity-has-aspect :args :authorization/required}))

;; =============================================================================
;; CART OPERATION AXIOMS
;; =============================================================================

(def add-to-cart-produces-items
  "Add-to-cart must produce cart/items."
  (ax/dsl-invariant
   :cart/add-produces-items
   :error
   "Add-to-cart must produce cart/items"
   {:op :dsl.op/entity-has-aspect :args :operation/create}
   {:op :dsl.op/entity-produces :args :cart/items}))

(def remove-from-cart-produces-items
  "Remove-from-cart must produce cart/items."
  (ax/dsl-invariant
   :cart/remove-produces-items
   :error
   "Remove-from-cart must produce cart/items"
   {:op :dsl.op/entity-has-aspect :args :operation/remove-item}
   {:op :dsl.op/entity-produces :args :cart/items}))

(def update-quantity-produces-items
  "Update-quantity must produce cart/items."
  (ax/dsl-invariant
   :cart/update-qty-produces-items
   :error
   "Update-quantity must produce cart/items"
   {:op :dsl.op/entity-has-aspect :args :operation/modify-item}
   {:op :dsl.op/entity-produces :args :cart/items}))

(def clear-cart-produces-items
  "Clear-cart must produce cart/items."
  (ax/dsl-invariant
   :cart/clear-produces-items
   :error
   "Clear-cart must produce cart/items"
   {:op :dsl.op/entity-has-aspect :args :operation/clear-all}
   {:op :dsl.op/entity-produces :args :cart/items}))

;; =============================================================================
;; TIER AXIOMS
;; =============================================================================

(def components-are-foundation
  "All components must be in foundation tier."
  (ax/dsl-invariant
   :cart/components-foundation
   :error
   "Components must be foundation tier"
   {:op :dsl.op/entity-has-aspect :args :atlas/structure-component}
   {:op :dsl.op/entity-has-aspect :args :tier/foundation}))

(def endpoints-are-api
  "All endpoints must be in API tier."
  (ax/dsl-invariant
   :cart/endpoints-api
   :error
   "Endpoints must be API tier"
   {:op :dsl.op/entity-has-aspect :args :atlas/interface-endpoint}
   {:op :dsl.op/entity-has-aspect :args :tier/api}))

(def functions-are-service
  "Service functions must be in service tier."
  (ax/dsl-invariant
   :cart/functions-service
   :error
   "Service functions must be service tier"
   {:op :dsl.op/logic-and
    :args [{:op :dsl.op/entity-has-aspect :args :atlas/execution-function}
           {:op :dsl.op/entity-lacks-aspect :args :atlas/interface-endpoint}]}
   {:op :dsl.op/entity-has-aspect :args :tier/service}))

;; =============================================================================
;; ARCHITECTURAL AXIOMS
;; =============================================================================

(def no-dependency-cycles
  "Dependency graph must be acyclic."
  (ax/dsl-invariant
   :cart/no-cycles
   :error
   "Dependency graph must be acyclic"
   {:op :dsl.op/entity-has-aspect :args :atlas/execution-function}
   {:op :dsl.op/graph-acyclic :args {:edge :depends}}))

(def all-functions-reachable
  "All functions should be reachable from endpoints."
  (ax/dsl-invariant
   :cart/all-reachable
   :warning
   "All functions should be reachable from endpoints"
   {:op :dsl.op/entity-has-aspect :args :atlas/execution-function}
   {:op :dsl.op/graph-reachable :args {:edge :depends}}))

;; =============================================================================
;; AXIOM COLLECTIONS
;; =============================================================================

(def session-invariants
  "Session management invariants."
  [session-read-uses-store
   session-update-uses-store
   session-id-consumers-use-store])

(def calculation-invariants
  "Cart calculation purity invariants."
  [calculate-subtotal-is-pure
   calculate-tax-is-pure
   calculate-total-is-pure
   pure-functions-no-component-deps])

(def pii-invariants
  "PII compliance invariants."
  [pii-functions-are-audited
   pii-endpoints-require-auth])

(def cart-operation-invariants
  "Cart item operation invariants."
  [add-to-cart-produces-items
   remove-from-cart-produces-items
   update-quantity-produces-items
   clear-cart-produces-items])

(def tier-invariants
  "Tier validation invariants."
  [components-are-foundation
   endpoints-are-api
   functions-are-service])

(def architectural-invariants
  "Architecture validation invariants."
  [no-dependency-cycles
   all-functions-reachable])

(def all-invariants
  "All cart invariants."
  (concat session-invariants
          calculation-invariants
          pii-invariants
          cart-operation-invariants
          tier-invariants
          architectural-invariants))

;; =============================================================================
;; PUBLIC API
;; =============================================================================

(defn check-all
  "Check all cart invariants."
  []
  (ax/check-all all-invariants))

(defn check-session
  "Check session invariants only."
  []
  (ax/check-all session-invariants))

(defn check-calculations
  "Check calculation invariants only."
  []
  (ax/check-all calculation-invariants))

(defn check-pii
  "Check PII compliance invariants only."
  []
  (ax/check-all pii-invariants))

(defn check-cart-ops
  "Check cart operation invariants only."
  []
  (ax/check-all cart-operation-invariants))

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
