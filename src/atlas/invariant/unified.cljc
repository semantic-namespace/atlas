(ns atlas.invariant.unified
  "Unified interface supporting both DSL and function-based invariants.
   Lets developers choose their style while maintaining full interoperability."
  (:require [atlas.datalog :as datalog]
            [atlas.invariant.dsl.datalog :as dsl-datalog]))

;; =============================================================================
;; INVARIANT PROTOCOLS - Both Styles Implement This
;; =============================================================================

(defprotocol Axiom
  "Common interface for all invariants regardless of implementation style."
  (invariant-id [this] "Unique identifier for this invariant")
  (invariant-level [this] "Severity level: :error or :warning")
  (invariant-doc [this] "Human-readable documentation")
  (check-invariant [this db] "Check invariant and return violations"))

;; =============================================================================
;; DSL-STYLE INVARIANT (Data-Driven)
;; =============================================================================

(defrecord DslAxiom [id level doc when assert]
  Axiom
  (invariant-id [_] id)
  (invariant-level [_] level)
  (invariant-doc [_] doc)
  (check-invariant [this db]
    (let [invariant-map {:invariant/id id
                         :invariant/level level
                         :invariant/when when
                         :invariant/assert assert}
          compiled (dsl-datalog/compile-invariant-datalog invariant-map db)]
      (dsl-datalog/check-invariant-datalog compiled db))))

(defn dsl-invariant
  "Create a DSL-style invariant (data-driven).

   Example:
   (dsl-invariant
     :my/oauth-needs-component
     :error
     \"OAuth operations must depend on OAuth component\"
     {:op :dsl.op/entity-has-aspect :args :protocol/oauth}
     {:op :dsl.op/entity-depends-on :args :component/oauth})"
  [id level doc when assert]
  (->DslAxiom id level doc when assert))

;; =============================================================================
;; FUNCTION-STYLE INVARIANT (Code-Driven)
;; =============================================================================

(defrecord FunctionAxiom [id level doc check-fn]
  Axiom
  (invariant-id [_] id)
  (invariant-level [_] level)
  (invariant-doc [_] doc)
  (check-invariant [_ db]
    (let [violations (check-fn db)]
      (map (fn [v] (assoc v :invariant id :level level))
           violations))))

(defn fn-invariant
  "Create a function-style invariant (code-driven).

   The check-fn receives a Datascript DB and returns a sequence of violation maps.

   Example:
   (fn-invariant
     :my/oauth-needs-component
     :error
     \"OAuth operations must depend on OAuth component\"
     (fn [db]
       (let [oauth-fns (datalog/query-entities-with-aspect db :protocol/oauth)
             violations (remove #(datalog/query-depends-on db % :component/oauth)
                               oauth-fns)]
         (map (fn [entity] {:entity entity}) violations))))"
  [id level doc check-fn]
  (->FunctionAxiom id level doc check-fn))

;; =============================================================================
;; HYBRID SUGAR - Best of Both Worlds
;; =============================================================================

(defmacro definvariant
  "Define an invariant with metadata for documentation and optional DSL or function body.

   DSL Style:
   (definvariant oauth-needs-component
     {:id :my/oauth-deps
      :level :error
      :doc \"OAuth operations must depend on OAuth component\"}
     {:when {:op :dsl.op/entity-has-aspect :args :protocol/oauth}
      :assert {:op :dsl.op/entity-depends-on :args :component/oauth}})

   Function Style:
   (definvariant oauth-needs-component
     {:id :my/oauth-deps
      :level :error
      :doc \"OAuth operations must depend on OAuth component\"}
     (fn [db]
       (let [oauth-fns (datalog/query-entities-with-aspect db :protocol/oauth)]
         (remove #(datalog/query-depends-on db % :component/oauth) oauth-fns))))

   Auto-detect Style:
   (definvariant oauth-needs-component
     {:id :my/oauth-deps
      :level :error
      :doc \"OAuth operations must depend on OAuth component\"
      :when {:op :dsl.op/entity-has-aspect :args :protocol/oauth}
      :assert {:op :dsl.op/entity-depends-on :args :component/oauth}})"
  [name metadata & body]
  (let [{:keys [id level doc when assert]} metadata
        body-form (first body)]
    (cond
      ;; Metadata contains :when/:assert - DSL style
      (and when assert)
      `(def ~name
         (->DslAxiom ~id ~level ~doc ~when ~assert))

      ;; Body is explicit {:when ... :assert ...} - DSL style
      (and (map? body-form) (:when body-form))
      `(def ~name
         (->DslAxiom ~id ~level ~doc ~(:when body-form) ~(:assert body-form)))

      ;; Body is a function - Function style
      (or (list? body-form) (= 'fn (first body-form)))
      `(def ~name
         (->FunctionAxiom ~id ~level ~doc ~body-form))

      ;; Error
      :else
      (throw (ex-info "definvariant requires either :when/:assert in metadata or a function body"
                      {:metadata metadata :body body})))))

;; =============================================================================
;; UNIFIED CHECKING
;; =============================================================================

(defn- normalize-violation
  "Ensure violation maps carry :level, :severity, and an optional :message."
  [violation level message]
  (let [level' (or (:level violation) level)
        severity' (or (:severity violation) level')]
    (cond-> violation
      level' (assoc :level level')
      severity' (assoc :severity severity')
      (and message (not (:message violation))) (assoc :message message))))

(defn check-all
  "Check all invariants regardless of style.

   Accepts:
   - Individual invariant (DslAxiom or FunctionAxiom)
   - Collection of invariants
   - Mix of both styles

   Returns unified violation format:
   {:violations [...] :errors [...] :warnings [...] :valid? boolean}"
  [invariants]
  (let [invariant-list (if (sequential? invariants) invariants [invariants])
        db (datalog/create-db)
        violations (vec (mapcat (fn [invariant]
                                  (let [level (invariant-level invariant)
                                        message (invariant-doc invariant)]
                                    (map #(normalize-violation % level message)
                                         (check-invariant invariant db))))
                                invariant-list))
        errors (filterv #(= :error (:level %)) violations)
        warnings (filterv #(= :warning (:level %)) violations)]
    {:violations violations
     :errors errors
     :warnings warnings
     :violations-flat violations
     :errors-flat errors
     :warnings-flat warnings
     :valid? (empty? errors)}))

(defn report
  "Print unified report for all invariant styles."
  [invariants]
  (let [{:keys [violations errors warnings valid?]} (check-all invariants)]
    (println "\n=== UNIFIED INVARIANT VALIDATION REPORT ===\n")
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
;; DOCUMENTATION GENERATION
;; =============================================================================

(defn document-invariant
  "Generate documentation for any invariant style."
  [invariant]
  {:id (invariant-id invariant)
   :level (invariant-level invariant)
   :doc (invariant-doc invariant)
   :style (cond
            (instance? DslAxiom invariant) :dsl
            (instance? FunctionAxiom invariant) :function
            :else :unknown)})

(defn document-all
  "Generate documentation for all invariants."
  [invariants]
  (let [invariant-list (if (sequential? invariants) invariants [invariants])]
    (mapv document-invariant invariant-list)))

;; =============================================================================
;; CONVERSION HELPERS
;; =============================================================================

(defn dsl->function
  "Convert a DSL invariant to function style.
   Useful for debugging or optimization."
  [dsl-invariant]
  (let [{:keys [id level doc when assert]} dsl-invariant]
    (fn-invariant
     id level doc
     (fn [db]
       (let [invariant-map {:invariant/id id
                            :invariant/level level
                            :invariant/when when
                            :invariant/assert assert}
             compiled (dsl-datalog/compile-invariant-datalog invariant-map db)]
         (dsl-datalog/check-invariant-datalog compiled db))))))

(defn function->dsl
  "Convert simple function invariants to DSL (when possible).
   Returns nil if conversion is not straightforward."
  [fn-invariant]
  ;; This is complex - only works for simple cases
  ;; Left as placeholder for future enhancement
  nil)

;; =============================================================================
;; INVARIANT REGISTRY
;; =============================================================================

(defonce invariant-registry (atom {}))

(defn register!
  "Register an invariant in the global registry."
  [invariant]
  (swap! invariant-registry assoc (invariant-id invariant) invariant))

(defn unregister!
  "Remove an invariant from the registry."
  [invariant-or-id]
  (let [id (if (keyword? invariant-or-id)
             invariant-or-id
             (invariant-id invariant-or-id))]
    (swap! invariant-registry dissoc id)))

(defn registered-invariants
  "Get all registered invariants."
  []
  (vals @invariant-registry))

(defn check-registered
  "Check all registered invariants."
  []
  (check-all (registered-invariants)))

(defn report-registered
  "Report on all registered invariants."
  []
  (report (registered-invariants)))

;; =============================================================================
;; USAGE EXAMPLES
;; =============================================================================

(comment
  ;; DSL Style - Great for LLMs and declarative thinking
  (def oauth-dsl
    (dsl-invariant
     :example/oauth-needs-component
     :error
     "OAuth operations must depend on OAuth component"
     {:op :dsl.op/entity-has-aspect :args :protocol/oauth}
     {:op :dsl.op/entity-depends-on :args :component/oauth}))

  ;; Function Style - Great for developers and complex logic
  (def oauth-fn
    (fn-invariant
     :example/oauth-needs-component-fn
     :error
     "OAuth operations must depend on OAuth component"
     (fn [db]
       (let [oauth-fns (datalog/query-entities-with-aspect db :protocol/oauth)
             violations (remove #(datalog/query-depends-on db % :component/oauth)
                                oauth-fns)]
         (map (fn [entity] {:entity entity}) violations)))))

  ;; Macro Style - Best of both (metadata + choice)
  (definvariant oauth-macro-dsl
    {:id :example/oauth-macro-dsl
     :level :error
     :doc "OAuth operations must depend on OAuth component"}
    {:when {:op :dsl.op/entity-has-aspect :args :protocol/oauth}
     :assert {:op :dsl.op/entity-depends-on :args :component/oauth}})

  (definvariant oauth-macro-fn
    {:id :example/oauth-macro-fn
     :level :error
     :doc "OAuth operations must depend on OAuth component"}
    (fn [db]
      (let [oauth-fns (datalog/query-entities-with-aspect db :protocol/oauth)]
        (remove #(datalog/query-depends-on db % :component/oauth) oauth-fns))))

  ;; Check any style
  (check-all oauth-dsl)
  (check-all oauth-fn)
  (check-all [oauth-dsl oauth-fn oauth-macro-dsl oauth-macro-fn])

  ;; Report any style
  (report [oauth-dsl oauth-fn])

  ;; Register and check
  (register! oauth-dsl)
  (register! oauth-fn)
  (check-registered)
  (report-registered)

  ;; Document
  (document-invariant oauth-dsl)
  (document-all [oauth-dsl oauth-fn]))
