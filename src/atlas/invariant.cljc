(ns atlas.invariant
  "Fixed invariants that understand dataflow markers and terminal outputs."
  (:require [atlas.registry :as cid]
            [atlas.entity :as rt]
            [atlas.graph :as graph]
            [atlas.invariant.component :as component]
            [clojure.set :as set]))

;; =============================================================================
;; HELPERS
;; =============================================================================

(defn all-dev-ids []
  (map #(:atlas/dev-id (second %)) @cid/registry))

(defn all-context-keys []
  (->> @cid/registry vals (mapcat :interface-endpoint/context) (remove nil?) set))

(defn all-response-keys []
  (->> @cid/registry vals (mapcat :interface-endpoint/response) (remove nil?) set))

(defn endpoint-context-keys
  "Keys that come from outside (endpoint inputs)."
  []
  (->> @cid/registry
       (filter (fn [[id _]] (contains? id :atlas/interface-endpoint)))
       (mapcat (fn [[_ v]] (:interface-endpoint/context v)))
       set))

(defn endpoint-response-keys
  "Keys that go to the client (endpoint outputs) - terminal by design."
  []
  (->> @cid/registry
       (filter (fn [[id _]] (contains? id :atlas/interface-endpoint)))
       (mapcat (fn [[_ v]] (:interface-endpoint/response v)))
       set))

(defn display-output-keys
  "Keys marked as :dataflow/display-output - terminal by design."
  []
  (->> @cid/registry
       (filter (fn [[id _]] (contains? id :dataflow/display-output)))
       (map (fn [[id _]]
              ;; The key IS the dev-id for data-keys
              (:atlas/dev-id (cid/fetch id))))
       (remove nil?)
       set))

(defn external-input-keys
  "Keys marked as :dataflow/external-input - no internal producer expected."
  []
  (->> @cid/registry
       (filter (fn [[id _]] (contains? id :dataflow/external-input)))
       (map (fn [[id _]] (:atlas/dev-id (cid/fetch id))))
       (remove nil?)
       set))

;; =============================================================================
;; FIXED DATA FLOW AXIOMS
;; =============================================================================

(defn invariant-context-satisfiable
  "Every context key must be either:
   - An endpoint input (comes from client)
   - Marked as :dataflow/external-input
   - Produced by some function"
  []
  (let [endpoint-inputs (endpoint-context-keys)
        external-inputs (external-input-keys)
        produced (all-response-keys)
        available (set/union endpoint-inputs external-inputs produced)
        all-needed (all-context-keys)
        unsatisfied (set/difference all-needed available)]
    (when (seq unsatisfied)
      {:invariant :context-satisfiable
       :violation :unsatisfied-context
       :missing unsatisfied
       :severity :error
       :message (str "These context keys are needed but never produced: " unsatisfied)})))

(defn invariant-no-orphan-responses
  "Response keys should be consumed OR be terminal outputs.
   
   A key is NOT orphan if any of:
   - Consumed by another function's context
   - Appears in an endpoint response (goes to client)
   - Marked as :dataflow/display-output (UI-bound)
   
   This catches truly dead code while allowing legitimate terminal outputs."
  []
  (let [produced (all-response-keys)
        consumed (all-context-keys)
        endpoint-outputs (endpoint-response-keys)
        display-outputs (display-output-keys)
        ;; Terminal = goes somewhere outside the internal dataflow
        terminal (set/union endpoint-outputs display-outputs)
        ;; Orphan = produced but neither consumed internally nor terminal
        orphans (-> produced
                    (set/difference consumed)
                    (set/difference terminal))]
    (when (seq orphans)
      {:invariant :no-orphan-responses
       :violation :orphan-outputs
       :orphans orphans
       :severity :warning
       :message (str "These response keys are produced but never consumed or displayed: " orphans)})))

(defn invariant-no-orphan-responses-strict
  "Strict version: only allows consumed keys, not terminal.
   Use for internal service layers where everything should chain."
  []
  (let [;; Only look at non-endpoint functions
        internal-fns (->> @cid/registry
                          (filter (fn [[id _]]
                                    (and (contains? id :atlas/execution-function)
                                         (not (contains? id :atlas/interface-endpoint)))))
                          (map second))
        produced (->> internal-fns (mapcat :interface-endpoint/response) (remove nil?) set)
        consumed (all-context-keys)
        orphans (set/difference produced consumed)]
    (when (seq orphans)
      {:invariant :no-orphan-responses-strict
       :violation :orphan-internal-outputs
       :orphans orphans
       :severity :warning
       :message (str "Internal function outputs not consumed: " orphans)})))

;; =============================================================================
;; GRAPH + COMPONENT INVARIANTS (RE-EXPORTS)
;; =============================================================================

(def invariant-deps-exist graph/invariant-deps-exist)
(def invariant-no-circular-deps graph/invariant-no-circular-deps)
(def invariant-all-fns-reachable graph/invariant-all-fns-reachable)
(def graph-invariants graph/graph-invariants)

(def invariant-components-are-foundation component/invariant-components-are-foundation)
(def invariant-protocol-exists component/invariant-protocol-exists)
(def invariant-protocol-conformance component/invariant-protocol-conformance)
(def component-invariants component/component-invariants)

;; =============================================================================
;; TIER AXIOMS (unchanged)
;; =============================================================================

(defn invariant-endpoints-are-api-tier
  "Endpoints should be :tier/api."
  []
  (let [endpoints (rt/all-with-aspect :atlas/interface-endpoint)
        violations (remove #(rt/has-aspect? % :tier/api) endpoints)]
    (when (seq violations)
      {:invariant :endpoints-are-api-tier
       :violation :wrong-tier
       :endpoints violations
       :severity :error
       :message (str "Endpoints should be :tier/api: " violations)})))

;; =============================================================================
;; SEMANTIC CONSISTENCY AXIOMS
;; =============================================================================

(defn invariant-external-is-async
  "Functions marked :integration/external should also be :temporal/async."
  []
  (let [external-fns (filter #(rt/has-aspect? % :atlas/execution-function)
                             (rt/all-with-aspect :integration/external))
        violations (remove #(rt/has-aspect? % :temporal/async) external-fns)]
    (when (seq violations)
      {:invariant :external-is-async
       :violation :external-not-async
       :functions violations
       :severity :warning
       :message (str "External integrations should be async: " violations)})))

(defn invariant-pure-has-no-deps
  "Functions marked :effect/pure should have no component dependencies."
  []
  (let [pure-fns (filter #(rt/has-aspect? % :effect/pure)
                         (rt/all-with-aspect :atlas/execution-function))
        violations (for [fn-id pure-fns
                         :let [deps (rt/deps-for fn-id)
                               component-deps (filter #(rt/has-aspect? % :atlas/structure-component) deps)]
                         :when (seq component-deps)]
                     {:fn fn-id :component-deps (vec component-deps)})]
    (when (seq violations)
      {:invariant :pure-has-no-deps
       :violation :pure-fn-with-deps
       :details violations
       :severity :error
       :message "Pure functions should not depend on components"})))

;; =============================================================================
;; CHECK ALL
;; =============================================================================

(def all-invariants
  "All invariants in check order."
  [;; Structural
   invariant-deps-exist
   invariant-no-circular-deps
   ;; Tier
   invariant-components-are-foundation
   invariant-endpoints-are-api-tier
   ;; Dataflow
   invariant-context-satisfiable
   invariant-no-orphan-responses        ; ← uses fixed version
   ;; Reachability
   invariant-all-fns-reachable
   ;; Semantic consistency
   invariant-external-is-async
   invariant-pure-has-no-deps
   ;; Protocol conformance
   invariant-protocol-exists
   invariant-protocol-conformance])

(defn- result-level [result]
  (or (:level result) (:severity result)))

(defn- normalize-result
  "Ensure invariant results carry both :level and :severity."
  [result]
  (cond-> result
    (and (:severity result) (not (:level result)))
    (assoc :level (:severity result))
    (and (:level result) (not (:severity result)))
    (assoc :severity (:level result))))

(defn check
  "Run provided invariants and return {:valid? :errors :warnings :violations}."
  [invariants]
  (let [results (keep #(%) invariants)
        normalized (map normalize-result results)
        errors (filter #(= :error (result-level %)) normalized)
        warnings (filter #(= :warning (result-level %)) normalized)]
    {:violations normalized
     :errors errors
     :warnings warnings
     :violations-flat normalized
     :errors-flat errors
     :warnings-flat warnings
     :valid? (empty? errors)}))

(defn check-all
  "Run all invariants, return {:valid? :errors :warnings :violations}."
  []
  (check all-invariants))

(defn report
  "Print human-readable invariant report."
  []
  (let [{:keys [errors warnings valid?]} (check-all)]
    (println "\n=== AXIOM VALIDATION REPORT ===\n")
    (if valid?
      (println "✓ All error-level invariants pass")
      (do
        (println "✗ ERRORS:")
        (doseq [e errors]
          (println "  -" (:invariant e) ":" (:message e)))))
    (when (seq warnings)
      (println "\n⚠ WARNINGS:")
      (doseq [w warnings]
        (println "  -" (:invariant w) ":" (:message w))))
    (println "\nTotal:" (+ (count errors) (count warnings))
             "issues (" (count errors) "errors," (count warnings) "warnings)")
    valid?))
