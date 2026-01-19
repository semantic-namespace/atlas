(ns app.calendar-availability-invariants
  "DSL invariant definitions for the calendar availability application.

   These invariants encode architectural constraints for:
   - OAuth integration requirements
   - External service async patterns
   - Pure function constraints
   - Data flow integrity"
  (:require
   [atlas.invariant.unified :as ax]))

;; =============================================================================
;; OAUTH INTEGRATION AXIOMS
;; =============================================================================

;; TODO review this def  ... this was handmade updated
(def oauth-functions-depend-on-oauth-component
  "Functions using OAuth protocol must depend on the OAuth component."
  (ax/dsl-invariant
   :calendar/oauth-requires-component
   :error
   "OAuth operations must depend on Google OAuth component"
   {:op :dsl.op/logic-and
    :args [{:op :dsl.op/entity-has-aspect :args :atlas/execution-function}
           {:op :dsl.op/entity-has-aspect :args :protocol/oauth}]}
   {:op :dsl.op/entity-depends-on :args :component/google-oauth}))

(def oauth-token-consumers-use-gcal
  "Functions consuming OAuth tokens must use the calendar client."
  (ax/dsl-invariant
   :calendar/token-consumers-use-gcal
   :error
   "Functions consuming OAuth tokens must depend on gcal-client"
   {:op :dsl.op/entity-consumes :args :oauth/access-token}
   {:op :dsl.op/entity-depends-on :args :component/gcal-client}))

;; =============================================================================
;; EXTERNAL INTEGRATION AXIOMS
;; =============================================================================

(def external-google-is-external
  "Google domain functions should be marked as external integration."
  (ax/dsl-invariant
   :calendar/google-is-external
   :warning
   "Google domain components should be marked as external"
   {:op :dsl.op/logic-and
    :args [{:op :dsl.op/entity-has-aspect :args :domain/google}
           {:op :dsl.op/entity-has-aspect :args :atlas/structure-component}]}
   {:op :dsl.op/entity-has-aspect :args :integration/external}))

;; =============================================================================
;; PURE FUNCTION AXIOMS
;; =============================================================================

(def pure-functions-no-component-deps
  "Pure functions must not depend on components (stateful resources)."
  (ax/dsl-invariant
   :calendar/pure-no-component-deps
   :error
   "Pure functions must not depend on components"
   {:op :dsl.op/entity-has-aspect :args :effect/pure}
   {:op :dsl.op/logic-not
    :args [{:op :dsl.op/logic-or
            :args [{:op :dsl.op/entity-depends-on :args :component/db}
                   {:op :dsl.op/entity-depends-on :args :component/google-oauth}
                   {:op :dsl.op/entity-depends-on :args :component/gcal-client}]}]}))

;; =============================================================================
;; TIER AXIOMS
;; =============================================================================

;; Helper: excludes meta-entities (ontology definitions, test implementations)
;; from business-logic invariants
(def ^:private not-meta-entity
  "Predicate that excludes ontology definitions and test implementations."
  {:op :dsl.op/logic-not
   :args [{:op :dsl.op/logic-or
           :args [{:op :dsl.op/entity-has-aspect :args :atlas/ontology}
                  {:op :dsl.op/entity-has-aspect :args :test/impl}]}]})

(def components-are-foundation
  "All components must be in foundation tier."
  (ax/dsl-invariant
   :calendar/components-foundation
   :error
   "Components must be foundation tier"
   {:op :dsl.op/entity-has-aspect :args :atlas/structure-component}
   {:op :dsl.op/entity-has-aspect :args :tier/foundation}))

(def endpoints-are-api
  "All endpoints must be in API tier."
  (ax/dsl-invariant
   :calendar/endpoints-api
   :error
   "Endpoints must be API tier"
   {:op :dsl.op/entity-has-aspect :args :atlas/interface-endpoint}
   {:op :dsl.op/entity-has-aspect :args :tier/api}))

(def functions-are-service
  "All functions must be in service tier (excluding ontology definitions and test impls)."
  (ax/dsl-invariant
   :calendar/functions-service
   :error
   "Functions must be service tier"
   {:op :dsl.op/logic-and
    :args [{:op :dsl.op/entity-has-aspect :args :atlas/execution-function}
           not-meta-entity]}
   {:op :dsl.op/entity-has-aspect :args :tier/service}))

;; =============================================================================
;; ARCHITECTURAL AXIOMS
;; =============================================================================

(def no-dependency-cycles
  "Dependency graph must be acyclic (excluding ontology definitions and test impls)."
  (ax/dsl-invariant
   :calendar/no-cycles
   :error
   "Dependency graph must be acyclic"
   {:op :dsl.op/logic-and
    :args [{:op :dsl.op/entity-has-aspect :args :atlas/execution-function}
           not-meta-entity]}
   {:op :dsl.op/graph-acyclic :args {:edge :depends}}))

(def all-functions-reachable
  "All functions should be reachable from endpoints (excluding ontology definitions and test impls)."
  (ax/dsl-invariant
   :calendar/all-reachable
   :warning
   "All functions should be reachable from endpoints"
   {:op :dsl.op/logic-and
    :args [{:op :dsl.op/entity-has-aspect :args :atlas/execution-function}
           not-meta-entity]}
   {:op :dsl.op/graph-reachable :args {:edge :depends}}))

;; =============================================================================
;; AXIOM COLLECTIONS
;; =============================================================================

(def oauth-invariants
  "OAuth-related invariants."
  [oauth-functions-depend-on-oauth-component
   oauth-token-consumers-use-gcal])

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
  "All calendar availability invariants."
  (concat oauth-invariants
          [external-google-is-external
           pure-functions-no-component-deps]
          tier-invariants
          architectural-invariants))

;; =============================================================================
;; PUBLIC API
;; =============================================================================

(defn check-all
  "Check all calendar invariants."
  []
  (ax/check-all all-invariants))

(defn check-oauth
  "Check OAuth-related invariants only."
  []
  (ax/check-all oauth-invariants))

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
