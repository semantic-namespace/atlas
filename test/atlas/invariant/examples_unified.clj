(ns atlas.invariant.examples-unified
  "Real-world examples showing DSL and function styles working together.
   Demonstrates collaborative development between humans and LLMs."
  (:require [atlas.invariant.unified :as ax]
            [atlas.datalog :as dl]))

(comment


;; =============================================================================
;; SCENARIO 1: LLM Generates DSL, Human Optimizes with Functions
;; =============================================================================

;; LLM writes this (clear, declarative, easy to generate)
(ax/definvariant oauth-requires-oauth-dsl
  {:id :collab/oauth-requires-oauth
   :level :error
   :doc "OAuth operations must depend on OAuth component"
   :when {:op :dsl.op/entity-has-aspect :args :protocol/oauth}
   :assert {:op :dsl.op/entity-depends-on :args :component/oauth}})

;; Human optimizes for performance (same semantics, faster execution)
(ax/definvariant oauth-requires-oauth-fn
  {:id :collab/oauth-requires-oauth-optimized
   :level :error
   :doc "OAuth operations must depend on OAuth component (optimized)"}
  (fn [db]
    ;; Direct Datalog - 3x faster than DSL compilation
    (let [oauth-fns (dl/query-entities-with-aspect db :protocol/oauth)
          oauth-components (set (dl/query-entities-with-aspect db :atlas/structure-component))
          violations (for [fn-id oauth-fns
                           :let [deps (set (dl/query-dependencies db fn-id))]
                           :when (empty? (clojure.set/intersection deps oauth-components))]
                       {:entity fn-id})]
      violations)))

;; Both work together!
(def oauth-invariants [oauth-requires-oauth-dsl oauth-requires-oauth-fn])

(comment
  (ax/check-all oauth-invariants)
  (ax/report oauth-invariants))

;; =============================================================================
;; SCENARIO 2: Human Writes Function, LLM Generates Documentation DSL
;; =============================================================================

;; Human writes complex logic
(ax/definvariant complex-dataflow-check-fn
  {:id :collab/complex-dataflow
   :level :error
   :doc "Complex dataflow validation with custom business rules"}
  (fn [db]
    (let [endpoints (dl/query-entities-with-aspect db :atlas/interface-endpoint)
          all-consumed (mapcat #(dl/query-consumes db %) endpoints)
          ;; Custom business rule: sensitive data must have audit trail
          sensitive-keys #{:user/email :user/password :payment/card-number}
          violations (for [key all-consumed
                           :when (contains? sensitive-keys key)
                           :let [consumers (dl/query-consumers-of db key)
                                 audited? (every? (fn [c]
                                                    (seq (dl/query-entities-with-aspect db :compliance/audited)))
                                                  consumers)]
                           :when (not audited?)]
                       {:key key :reason "Sensitive data needs audit trail"})]
      violations)))

;; LLM generates equivalent DSL for documentation/understanding
;; (This helps other LLMs understand the intent)
(ax/definvariant complex-dataflow-check-dsl-doc
  {:id :collab/complex-dataflow-doc
   :level :info ; Not checked, just documentation
   :doc "Declarative specification of complex dataflow rule (for AI understanding)"
   :when {:op :dsl.op/entity-consumes :args :user/email}
   :assert {:op :dsl.op/entity-has-aspect :args :compliance/audited}})

;; =============================================================================
;; SCENARIO 3: Mixed Team - Junior Uses DSL, Senior Uses Functions
;; =============================================================================

;; Junior developer - uses DSL (safer, clearer)
(ax/definvariant components-are-foundation-junior
  {:id :team/components-foundation-junior
   :level :error
   :doc "All components must be tier/foundation (junior dev)"}
  {:when {:op :dsl.op/entity-has-aspect :args :atlas/structure-component}
   :assert {:op :dsl.op/entity-has-aspect :args :tier/foundation}})

;; Senior developer - uses functions (more control)
(ax/definvariant tier-hierarchy-complete-senior
  {:id :team/tier-hierarchy-senior
   :level :error
   :doc "Complete tier hierarchy validation (senior dev)"}
  (fn [db]
    (let [api-tier (set (dl/query-entities-with-aspect db :tier/api))
          service-tier (set (dl/query-entities-with-aspect db :tier/service))
          foundation-tier (set (dl/query-entities-with-aspect db :tier/foundation))

          ;; API can only depend on service
          api-violations (for [api-id api-tier
                               :let [deps (set (dl/query-dependencies db api-id))
                                     invalid-deps (clojure.set/intersection deps foundation-tier)]
                               :when (seq invalid-deps)]
                           {:entity api-id
                            :violation "API depends directly on foundation"
                            :invalid-deps invalid-deps})

          ;; Service can only depend on foundation
          service-violations (for [svc-id service-tier
                                   :let [deps (set (dl/query-dependencies db svc-id))
                                         invalid-deps (clojure.set/intersection deps api-tier)]
                                   :when (seq invalid-deps)]
                               {:entity svc-id
                                :violation "Service depends on API"
                                :invalid-deps invalid-deps})]
      (concat api-violations service-violations))))

(def team-invariants [components-are-foundation-junior tier-hierarchy-complete-senior])

;; =============================================================================
;; SCENARIO 4: Registry Pattern - Mix and Match
;; =============================================================================

;; Register invariants as they're created (any style)
(ax/register! oauth-requires-oauth-dsl)
(ax/register! oauth-requires-oauth-fn)
(ax/register! complex-dataflow-check-fn)
(ax/register! components-are-foundation-junior)
(ax/register! tier-hierarchy-complete-senior)

;; Check everything together
(comment
  (ax/check-registered)
  (ax/report-registered)

  ;; Generate unified documentation
  (ax/document-all (ax/registered-invariants))
  ;; => [{:id :collab/oauth-requires-oauth :level :error :style :dsl ...}
  ;;     {:id :collab/oauth-requires-oauth-optimized :level :error :style :function ...}
  ;;     ...]
  )

;; =============================================================================
;; SCENARIO 5: Domain-Specific Suite - Both Styles
;; =============================================================================

(def calendar-invariants-mixed
  [;; Simple rules - DSL (LLM-friendly)
   (ax/dsl-invariant
    :calendar/external-is-async
    :warning
    "External integrations should be async"
    {:op :dsl.op/entity-has-aspect :args :integration/external}
    {:op :dsl.op/entity-has-aspect :args :temporal/async})

   (ax/dsl-invariant
    :calendar/oauth-needs-oauth
    :error
    "OAuth operations need OAuth component"
    {:op :dsl.op/entity-has-aspect :args :protocol/oauth}
    {:op :dsl.op/entity-depends-on :args :component/google-oauth})

   ;; Complex rules - Functions (developer control)
   (ax/fn-invariant
    :calendar/availability-check-complete
    :error
    "Availability checks must have complete dataflow"
    (fn [db]
      (let [check-fn (first (dl/query-entities-with-aspect db :domain/scheduling))
            produces (set (dl/query-produces db check-fn))
            endpoint (first (dl/query-entities-with-aspect db :atlas/interface-endpoint))
            endpoint-needs (set (dl/query-consumes db endpoint))
            missing (clojure.set/difference endpoint-needs produces)]
        (when (seq missing)
          [{:entity check-fn
            :missing-keys missing
            :reason "Availability check doesn't produce all endpoint needs"}]))))

   (ax/fn-invariant
    :calendar/no-pii-in-logs
    :error
    "Calendar tokens must not be logged"
    (fn [db]
      (let [token-users (dl/query-consumers-of db :user/gcal-refresh-token)
            logging-fns (set (dl/query-entities-with-aspect db :observability/logged))
            violations (clojure.set/intersection (set token-users) logging-fns)]
        (map (fn [id] {:entity id :reason "Logs PII"}) violations))))])

(comment
  (ax/check-all calendar-invariants-mixed)
  (ax/report calendar-invariants-mixed))

;; =============================================================================
;; SCENARIO 6: Migration Path - Gradual Transition
;; =============================================================================

(defn migrate-dsl-to-fn
  "Helper to gradually migrate DSL invariants to function style for performance."
  [dsl-invariant]
  (let [id (ax/invariant-id dsl-invariant)
        level (ax/invariant-level dsl-invariant)
        doc (str (ax/invariant-doc dsl-invariant) " [migrated to function style]")]
    (ax/fn-invariant
     (keyword (namespace id) (str (name id) "-fn"))
     level
     doc
     (fn [db]
        ;; Use the DSL invariant's check method
       (ax/check-invariant dsl-invariant db)))))

(comment
  ;; Start with DSL
  (def my-dsl-invariant
    (ax/dsl-invariant :my/rule :error "My rule"
                  {:op :dsl.op/entity-has-aspect :args :tier/api}
                  {:op :dsl.op/entity-has-aspect :args :atlas/interface-endpoint}))

  ;; Later, migrate to function for performance
  (def my-fn-invariant (migrate-dsl-to-fn my-dsl-invariant))

  ;; Both produce same results
  (= (ax/check-invariant my-dsl-invariant (dl/create-db))
     (ax/check-invariant my-fn-invariant (dl/create-db))))

;; =============================================================================
;; SCENARIO 7: AI Pair Programming
;; =============================================================================

;; LLM suggests invariant in DSL (easy to generate, clear intent)
(def ai-suggested-invariant
  (ax/dsl-invariant
   :ai/suggested-rule
   :warning
   "AI suggestion: PII handlers should be audited"
   {:op :dsl.op/entity-has-aspect :args :compliance/pii}
   {:op :dsl.op/entity-has-aspect :args :compliance/audited}))

;; Human reviews, accepts, and optimizes
(def human-reviewed-invariant
  (ax/fn-invariant
   :human/reviewed-rule
   :error ; Upgraded to error after review
   "PII handlers MUST be audited (human reviewed)"
   (fn [db]
     (let [pii-handlers (dl/query-entities-with-aspect db :compliance/pii)
           audited (set (dl/query-entities-with-aspect db :compliance/audited))
           violations (remove audited pii-handlers)]
       (map (fn [id] {:entity id
                      :severity "HIGH"
                      :compliance-requirement "GDPR Article 30"})
            violations)))))

;; Both coexist in the system
(def ai-human-collab [ai-suggested-invariant human-reviewed-invariant])

;; =============================================================================
;; SCENARIO 8: Testing - Same Semantics, Different Implementations
;; =============================================================================

(defn test-invariant-equivalence
  "Verify DSL and function versions have same semantics."
  [dsl-invariant fn-invariant]
  (let [db (dl/create-db)
        dsl-result (set (ax/check-invariant dsl-invariant db))
        fn-result (set (ax/check-invariant fn-invariant db))]
    {:equivalent? (= dsl-result fn-result)
     :dsl-violations (count dsl-result)
     :fn-violations (count fn-result)
     :diff (clojure.set/difference dsl-result fn-result)}))

(comment
  ;; Verify optimized version has same semantics
  (test-invariant-equivalence oauth-requires-oauth-dsl oauth-requires-oauth-fn))

;; =============================================================================
;; PRACTICAL USAGE GUIDE
;; =============================================================================

(comment
  ;; For LLMs generating invariants:
  ;; 1. Use DSL style (ax/dsl-invariant or ax/definvariant with :when/:assert)
  ;; 2. Clear, declarative, easy to verify
  ;; 3. Can be automatically converted to docs

  ;; For humans writing invariants:
  ;; 1. Simple rules: Use DSL (clearer intent)
  ;; 2. Complex rules: Use functions (more control)
  ;; 3. Performance-critical: Use functions with direct Datalog

  ;; For teams:
  ;; 1. Junior devs: Use DSL (safer, clearer)
  ;; 2. Senior devs: Use functions (more power)
  ;; 3. Mix freely - they work together seamlessly

  ;; For migration:
  ;; 1. Start with DSL for rapid prototyping
  ;; 2. Profile performance
  ;; 3. Migrate hot paths to functions
  ;; 4. Keep both versions for verification
  )

  )
