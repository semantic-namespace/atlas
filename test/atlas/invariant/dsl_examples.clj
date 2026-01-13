(ns atlas.invariant.dsl-examples
  "Examples showing how to define and use invariants with the DSL."
  (:require [atlas.invariant.dsl.operators :as dsl]))

;; =============================================================================
;; TIER AXIOMS
;; =============================================================================
(dsl/register-operators)
(def invariant-components-are-foundation
  {:invariant/id :invariant/components-are-foundation
   :invariant/level :error
   :invariant/when {:op :dsl.op/entity-has-aspect
                :args :atlas/structure-component}
   :invariant/assert {:op :dsl.op/entity-has-aspect
                  :args :tier/foundation}
   :invariant/doc "Components must be foundation-tier."})

(def invariant-functions-are-service
  {:invariant/id :invariant/functions-are-service
   :invariant/level :error
   :invariant/when {:op :dsl.op/entity-has-aspect
                :args :atlas/execution-function}
   :invariant/assert {:op :dsl.op/entity-has-aspect
                  :args :tier/service}
   :invariant/doc "Functions must be service-tier."})

(def invariant-endpoints-are-api
  {:invariant/id :invariant/endpoints-are-api
   :invariant/level :error
   :invariant/when {:op :dsl.op/entity-has-aspect
                :args :atlas/interface-endpoint}
   :invariant/assert {:op :dsl.op/entity-has-aspect
                  :args :tier/api}
   :invariant/doc "Endpoints must be API-tier."})

;; =============================================================================
;; DOMAIN & PROTOCOL AXIOMS
;; =============================================================================

(def invariant-external-is-async
  {:invariant/id :invariant/external-is-async
   :invariant/level :warning
   :invariant/when {:op :dsl.op/entity-has-aspect
                :args :integration/external}
   :invariant/assert {:op :dsl.op/entity-has-aspect
                  :args :temporal/async}
   :invariant/doc "External integrations must be async."})

(def invariant-oauth-requires-oauth-dep
  {:invariant/id :invariant/oauth-requires-oauth-dep
   :invariant/level :error
   :invariant/when {:op :dsl.op/entity-has-aspect
                :args :protocol/oauth}
   :invariant/assert {:op :dsl.op/entity-depends-on
                  :args :component/google-oauth}
   :invariant/doc "OAuth operations must depend on OAuth component."})

(def invariant-pii-must-be-audited
  {:invariant/id :invariant/pii-must-be-audited
   :invariant/level :warning
   :invariant/when {:op :dsl.op/entity-has-aspect
                :args :compliance/pii}
   :invariant/assert {:op :dsl.op/entity-has-aspect
                  :args :compliance/audited}
   :invariant/doc "PII handlers must be audited."})

;; =============================================================================
;; DEPENDENCY GRAPH AXIOMS
;; =============================================================================

(def invariant-deps-acyclic
  {:invariant/id :invariant/deps-acyclic
   :invariant/level :error
   :invariant/when {:op :dsl.op/entity-has-aspect
                :args :atlas/execution-function}
   :invariant/assert {:op :dsl.op/graph-acyclic
                  :args {:edge :depends}}
   :invariant/doc "Dependency graph must be acyclic."})

;; =============================================================================
;; DATAFLOW AXIOMS
;; =============================================================================

(def invariant-no-missing-producers
  {:invariant/id :invariant/no-missing-producers
   :invariant/level :error
   :invariant/when {:op :dsl.op/entity-consumes
                :args :key/x}
   :invariant/assert {:op :dsl.op/logic-or
                  :args [{:op :dsl.op/data-has-producer :args :key/x}
                         {:op :dsl.op/entity-has-aspect :args :atlas/interface-endpoint}]}
   :invariant/doc "Every consumed key must have a producer or be endpoint input."})

(def invariant-no-orphan-outputs
  {:invariant/id :invariant/no-orphan-outputs
   :invariant/level :warning
   :invariant/when {:op :dsl.op/entity-produces
                :args :key/x}
   :invariant/assert {:op :dsl.op/logic-or
                  :args [{:op :dsl.op/data-has-consumer :args :key/x}
                         {:op :dsl.op/entity-has-aspect :args :atlas/interface-endpoint}]}
   :invariant/doc "Every produced key must have a consumer or be an API output."})

;; =============================================================================
;; PURITY AXIOMS
;; =============================================================================

(def invariant-pure-no-external
  {:invariant/id :invariant/pure-no-external
   :invariant/level :warning
   :invariant/when {:op :dsl.op/entity-has-aspect
                :args :effect/pure}
   :invariant/assert {:op :dsl.op/entity-lacks-aspect
                  :args :integration/external}
   :invariant/doc "Pure functions must not use external integrations."})

;; =============================================================================
;; REACHABILITY AXIOMS
;; =============================================================================

(def invariant-all-fns-reachable
  {:invariant/id :invariant/all-fns-reachable
   :invariant/level :warning
   :invariant/when {:op :dsl.op/entity-has-aspect
                :args :atlas/execution-function}
   :invariant/assert {:op :dsl.op/graph-reachable
                  :args {:edge :depends}}
   :invariant/doc "Every function should be reachable from some endpoint."})

;; =============================================================================
;; ALL AXIOMS
;; =============================================================================

(def standard-invariants
  "Standard invariant suite for SOODF systems."
  [invariant-components-are-foundation
   invariant-functions-are-service
   invariant-endpoints-are-api
   invariant-external-is-async
   invariant-oauth-requires-oauth-dep
   invariant-pii-must-be-audited
   invariant-deps-acyclic
   invariant-no-missing-producers
   invariant-no-orphan-outputs
   invariant-pure-no-external
   invariant-all-fns-reachable])

;; =============================================================================
;; CONVENIENCE FUNCTIONS
;; =============================================================================

(defn check-all
  "Check all standard invariants."
  []
  (dsl/check-invariants standard-invariants))

(defn report
  "Print invariant validation report."
  []
  (let [{:keys [violations errors warnings valid?]} (check-all)]
    (println "\n=== AXIOM VALIDATION REPORT (DSL-DRIVEN) ===\n")
    (if valid?
      (println "âœ“ All error-level invariants pass")
      (do
        (println "âœ— ERRORS:")
        (doseq [e errors]
          (println "  -" (:invariant e) "on" (:entity e)))))
    (when (seq warnings)
      (println "\nâš  WARNINGS:")
      (doseq [w warnings]
        (println "  -" (:invariant w) "on" (:entity w))))
    (println "\nTotal:" (count violations) "issues (" (count errors) "errors," (count warnings) "warnings)")
    valid?))


;; =============================================================================
;; BASIC EXAMPLES
;; =============================================================================

(comment
  ;; Example 1: Simple aspect check
  (def my-invariant
    {:invariant/id :example/components-have-tier
     :invariant/level :error
     :invariant/when {:op :dsl.op/entity-has-aspect
                  :args :atlas/structure-component}
     :invariant/assert {:op :dsl.op/entity-has-aspect
                    :args :tier/foundation}})

  ;; Example 2: Dependency check
  (def oauth-dep-invariant
    {:invariant/id :example/oauth-needs-oauth-component
     :invariant/level :error
     :invariant/when {:op :dsl.op/entity-has-aspect
                  :args :protocol/oauth}
     :invariant/assert {:op :dsl.op/entity-depends-on
                    :args :component/google-oauth}})

  ;; Example 3: Dataflow check
  (def producer-exists-invariant
    {:invariant/id :example/data-has-producer
     :invariant/level :error
     :invariant/when {:op :dsl.op/entity-consumes
                  :args :user/email}
     :invariant/assert {:op :dsl.op/data-has-producer
                    :args :user/email}}))

;; =============================================================================
;; LOGICAL COMPOSITION EXAMPLES
;; =============================================================================

(comment
  ;; Example 4: AND - multiple conditions
  (def complex-and-invariant
    {:invariant/id :example/external-async-and-traced
     :invariant/level :warning
     :invariant/when {:op :dsl.op/entity-has-aspect
                  :args :integration/external}
     :invariant/assert {:op :dsl.op/logic-and
                    :args [{:op :dsl.op/entity-has-aspect :args :temporal/async}
                           {:op :dsl.op/entity-has-aspect :args :observability/traced}]}})

  ;; Example 5: OR - alternative conditions
  (def or-invariant
    {:invariant/id :example/consumed-from-endpoint-or-producer
     :invariant/level :error
     :invariant/when {:op :dsl.op/entity-consumes
                  :args :key/x}
     :invariant/assert {:op :dsl.op/logic-or
                    :args [{:op :dsl.op/data-has-producer :args :key/x}
                           {:op :dsl.op/entity-has-aspect :args :atlas/interface-endpoint}]}})

  ;; Example 6: NOT - negation
  (def not-invariant
    {:invariant/id :example/pure-no-external
     :invariant/level :warning
     :invariant/when {:op :dsl.op/entity-has-aspect
                  :args :effect/pure}
     :invariant/assert {:op :dsl.op/logic-not
                    :args {:op :dsl.op/entity-has-aspect
                           :args :integration/external}}})

  ;; Example 7: IMPLIES - conditional requirement
  (def implies-invariant
    {:invariant/id :example/pii-implies-audit
     :invariant/level :error
     :invariant/when {:op :dsl.op/entity-has-aspect
                  :args :atlas/execution-function}
     :invariant/assert {:op :dsl.op/logic-implies
                    :args [{:op :dsl.op/entity-has-aspect :args :compliance/pii}
                           {:op :dsl.op/entity-has-aspect :args :compliance/audited}]}}))

;; =============================================================================
;; GRAPH AXIOMS EXAMPLES
;; =============================================================================

(comment
  ;; Example 8: Acyclicity check
  (def acyclic-deps-invariant
    {:invariant/id :example/deps-acyclic
     :invariant/level :error
     :invariant/when {:op :dsl.op/entity-has-aspect
                  :args :atlas/execution-function}
     :invariant/assert {:op :dsl.op/graph-acyclic
                    :args {:edge :depends}}})

  ;; Example 9: Reachability check
  (def reachability-invariant
    {:invariant/id :example/all-reachable
     :invariant/level :warning
     :invariant/when {:op :dsl.op/entity-has-aspect
                  :args :atlas/execution-function}
     :invariant/assert {:op :dsl.op/graph-reachable
                    :args {:edge :depends}}}))

;; =============================================================================
;; DOMAIN-SPECIFIC AXIOMS (Calendar Example)
;; =============================================================================

(def calendar-invariants
  "Calendar availability system invariants."

  [;; A1: External integrations must be async
   {:invariant/id :calendar/external-is-async
    :invariant/level :warning
    :invariant/when {:op :dsl.op/entity-has-aspect
                 :args :integration/external}
    :invariant/assert {:op :dsl.op/entity-has-aspect
                   :args :temporal/async}}

   ;; A2: OAuth operations must depend on OAuth component
   {:invariant/id :calendar/oauth-requires-oauth-component
    :invariant/level :error
    :invariant/when {:op :dsl.op/entity-has-aspect
                 :args :protocol/oauth}
    :invariant/assert {:op :dsl.op/entity-depends-on
                   :args :component/google-oauth}}

   ;; A3: Service functions must depend only on foundation components
   {:invariant/id :calendar/service-uses-foundation
    :invariant/level :error
    :invariant/when {:op :dsl.op/entity-has-aspect
                 :args :tier/service}
    :invariant/assert {:op :dsl.op/entity-has-aspect
                   :args :tier/foundation}}

   ;; A4: No missing producers (except endpoint inputs)
   {:invariant/id :calendar/no-missing-producers
    :invariant/level :error
    :invariant/when {:op :dsl.op/entity-consumes
                 :args :key/x}
    :invariant/assert {:op :dsl.op/logic-or
                   :args [{:op :dsl.op/data-has-producer :args :key/x}
                          {:op :dsl.op/entity-has-aspect :args :atlas/interface-endpoint}]}}

   ;; A5: Pure functions must not use external integrations
   {:invariant/id :calendar/pure-no-external
    :invariant/level :warning
    :invariant/when {:op :dsl.op/entity-has-aspect
                 :args :effect/pure}
    :invariant/assert {:op :dsl.op/entity-lacks-aspect
                   :args :integration/external}}

   ;; A6: Scheduling availability checks must be external
   {:invariant/id :calendar/check-availability-is-external
    :invariant/level :warning
    :invariant/when {:op :dsl.op/entity-has-aspect
                 :args :domain/scheduling}
    :invariant/assert {:op :dsl.op/entity-has-aspect
                   :args :integration/external}}

   ;; A7: Dependency graph must be acyclic
   {:invariant/id :calendar/global-acyclic
    :invariant/level :error
    :invariant/when {:op :dsl.op/entity-has-aspect
                 :args :atlas/execution-function}
    :invariant/assert {:op :dsl.op/graph-acyclic
                   :args {:edge :depends}}}])

;; =============================================================================
;; USAGE EXAMPLES
;; =============================================================================

(comment
  ;; Check standard invariants
  (check-all)
  ;; => {:violations [...] :errors [...] :warnings [...] :valid? true/false}

  ;; Print report
  (report)
  ;; Prints formatted report to console

  ;; Check calendar-specific invariants
  (dsl/check-invariants calendar-invariants)

  ;; Compile a single invariant
  (dsl/compile-invariant (first standard-invariants))
  ;; => {:invariant/id ... :compiled/when ... :compiled/assert ...}

  ;; Manually check one invariant
  (dsl/check-invariant (dsl/compile-invariant my-invariant))
  ;; => [{:invariant :example/components-have-tier :entity :component/db :level :error}]

  ;; Inspect DSL operators
  (filter #(contains? % :semantic-namespace.invariant/operator)
          (keys @semantic-namespace.compound.identity/registry))
  ;; => (#{:dsl.op/entity-has-aspect :semantic-namespace.invariant/operator} ...)
  )

;; =============================================================================
;; TESTING AXIOMS
;; =============================================================================

(defn test-invariant
  "Test a single invariant and print results."
  [invariant]
  (let [compiled (dsl/compile-invariant invariant)
        violations (dsl/check-invariant compiled)]
    (if (empty? violations)
      (println "âœ“" (:invariant/id invariant) "passes")
      (do
        (println "âœ—" (:invariant/id invariant) "failed:")
        (doseq [v violations]
          (println "  -" (:entity v)))))))

(defn test-suite
  "Test a suite of invariants."
  [invariants]
  (doseq [invariant invariants]
    (test-invariant invariant)))

(comment
  ;; Test standard invariants one by one
  (test-suite standard-invariants)

  ;; Test calendar invariants
  (test-suite calendar-invariants))
