(ns atlas.invariant.dsl.operators
  "DSL operators for semantic invariants. Each operator is itself a semantic entity."
  (:require [atlas.registry :as cid]
            [atlas.ontology :as ot]
            [atlas.registry.lookup :as rt]))

;; =============================================================================
;; DSL OPERATOR REGISTRY
;; =============================================================================
;; Each operator is a semantic entity with compile-time and runtime behavior

(defn register-operator!
  "Register a DSL operator as a semantic entity."
  [id operator-def]
  (swap! cid/registry assoc
         (conj id :semantic-namespace.invariant/operator)
         operator-def))


(defn register-operators []
  ;; =============================================================================
;; ENTITY PREDICATES
;; =============================================================================

  (register-operator! #{:dsl.op/entity-has-aspect}
                      {:operator/arity 1
                       :operator/args-type :keyword
                       :operator/doc "Checks if entity has the specified aspect."
                       :operator/compile-fn
                       (fn [aspect]
                         {:type :entity-predicate
                          :check-fn (fn [entity-id] (rt/has-aspect? entity-id aspect))})})

  (register-operator! #{:dsl.op/entity-lacks-aspect}
                      {:operator/arity 1
                       :operator/args-type :keyword
                       :operator/doc "Checks if entity lacks the specified aspect."
                       :operator/compile-fn
                       (fn [aspect]
                         {:type :entity-predicate
                          :check-fn (fn [entity-id] (not (rt/has-aspect? entity-id aspect)))})})

  (register-operator! #{:dsl.op/entity-depends-on}
                      {:operator/arity 1
                       :operator/args-type :keyword
                       :operator/doc "Checks if entity depends on specified entity."
                       :operator/compile-fn
                       (fn [dep-id]
                         {:type :entity-predicate
                          :check-fn (fn [entity-id]
                                      (contains? (set (ot/deps-for entity-id)) dep-id))})})

  (register-operator! #{:dsl.op/entity-produces}
                      {:operator/arity 1
                       :operator/args-type :keyword
                       :operator/doc "Checks if entity produces specified data key."
                       :operator/compile-fn
                       (fn [data-key]
                         {:type :entity-predicate
                          :check-fn (fn [entity-id]
                                      (let [props (rt/props-for entity-id)
                                            response-keys (or (:interface-endpoint/response props)
                                                              (:execution-function/response props))]
                                        (contains? (set response-keys) data-key)))})})

  (register-operator! #{:dsl.op/entity-consumes}
                      {:operator/arity 1
                       :operator/args-type :keyword
                       :operator/doc "Checks if entity consumes specified data key."
                       :operator/compile-fn
                       (fn [data-key]
                         {:type :entity-predicate
                          :check-fn (fn [entity-id]
                                      (let [props (rt/props-for entity-id)
                                            context-keys (or (:interface-endpoint/context props)
                                                             (:execution-function/context props))]
                                        (contains? (set context-keys) data-key)))})})

;; =============================================================================
;; DATAFLOW PREDICATES
;; =============================================================================

  (register-operator! #{:dsl.op/data-has-producer}
                      {:operator/arity 1
                       :operator/args-type :keyword
                       :operator/doc "Checks if data key has at least one producer."
                       :operator/compile-fn
                       (fn [data-key]
                         {:type :global-predicate
                          :check-fn (fn [_entity-id]
                                      (some (fn [[_id props]]
                                              (contains? (set (:interface-endpoint/response props)) data-key))
                                            @cid/registry))})})

  (register-operator! #{:dsl.op/data-has-consumer}
                      {:operator/arity 1
                       :operator/args-type :keyword
                       :operator/doc "Checks if data key has at least one consumer."
                       :operator/compile-fn
                       (fn [data-key]
                         {:type :global-predicate
                          :check-fn (fn [_entity-id]
                                      (some (fn [[_id props]]
                                              (contains? (set (:interface-endpoint/context props)) data-key))
                                            @cid/registry))})})

;; =============================================================================
;; LOGICAL OPERATORS
;; =============================================================================

  (register-operator! #{:dsl.op/logic-and}
                      {:operator/arity :variadic
                       :operator/args-type :node
                       :operator/doc "Logical AND of child predicates."
                       :operator/compile-fn
                       (fn [children]
                         {:type :combinator
                          :check-fn (fn [entity-id compiled-children]
                                      (every? (fn [child]
                                                ((:check-fn child) entity-id))
                                              compiled-children))})})

  (register-operator! #{:dsl.op/logic-or}
                      {:operator/arity :variadic
                       :operator/args-type :node
                       :operator/doc "Logical OR of child predicates."
                       :operator/compile-fn
                       (fn [children]
                         {:type :combinator
                          :check-fn (fn [entity-id compiled-children]
                                      (some (fn [child]
                                              ((:check-fn child) entity-id))
                                            compiled-children))})})

  (register-operator! #{:dsl.op/logic-not}
                      {:operator/arity 1
                       :operator/args-type :node
                       :operator/doc "Logical NOT of child predicate."
                       :operator/compile-fn
                       (fn [child]
                         {:type :combinator
                          :check-fn (fn [entity-id compiled-child]
                                      (not ((:check-fn compiled-child) entity-id)))})})

  (register-operator! #{:dsl.op/logic-implies}
                      {:operator/arity 2
                       :operator/args-type :node
                       :operator/doc "Logical implication: A implies B = (not A) or B"
                       :operator/compile-fn
                       (fn [[antecedent consequent]]
                         {:type :combinator
                          :check-fn (fn [entity-id [compiled-a compiled-b]]
                                      (or (not ((:check-fn compiled-a) entity-id))
                                          ((:check-fn compiled-b) entity-id)))})})

;; =============================================================================
;; GRAPH OPERATORS
;; =============================================================================

  (register-operator! #{:dsl.op/graph-acyclic}
                      {:operator/arity 1
                       :operator/args-type :map
                       :operator/doc "Checks if dependency graph is acyclic."
                       :operator/compile-fn
                       (fn [{:keys [edge]}]
                         {:type :graph-predicate
                          :check-fn (fn [_entity-id]
                                      (let [all-ids (map #(:atlas/dev-id (second %)) @cid/registry)
                                            deps-map (case edge
                                                       :depends (into {} (map (fn [id] [id (ot/deps-for id)]) all-ids))
                                                       :data-flow (into {} (map (fn [id] [id (ot/compute-data-deps id)]) all-ids)))]
                                        (letfn [(has-cycle? [id visited path]
                                                  (cond
                                                    (contains? path id) false
                                                    (contains? visited id) true
                                                    :else (every? #(has-cycle? % (conj visited id) (conj path id))
                                                                  (get deps-map id #{}))))]
                                          (every? #(has-cycle? % #{} #{}) all-ids))))})})

  (register-operator! #{:dsl.op/graph-reachable}
                      {:operator/arity 1
                       :operator/args-type :map
                       :operator/doc "Checks if entity is reachable from endpoints."
                       :operator/compile-fn
                       (fn [{:keys [edge]}]
                         {:type :graph-predicate
                          :check-fn (fn [entity-id]
                                      (let [endpoints (rt/all-with-aspect :atlas/interface-endpoint)
                                            reachable (atom #{})
                                            collect-reachable (fn collect [id]
                                                                (when-not (@reachable id)
                                                                  (swap! reachable conj id)
                                                                  (doseq [dep (ot/deps-for id)]
                                                                    (collect dep))))]
                                        (doseq [ep endpoints]
                                          (collect-reachable ep))
                                        (contains? @reachable entity-id)))})}))

;; =============================================================================
;; COMPILER
;; =============================================================================

(defn compile-node
  "Compile a DSL node into executable form."
  [node]
  (cond
    ;; Simple predicate node: {:op :dsl.op/entity-has-aspect :args :tier/service}
    (and (map? node) (:op node))
    (let [op-id (conj #{(:op node)} :semantic-namespace.invariant/operator)
          op-def (get @cid/registry op-id)]
      (when-not op-def
        (throw (ex-info "Unknown DSL operator" {:op (:op node)})))
      (let [compile-fn (:operator/compile-fn op-def)
            compiled (compile-fn (:args node))]
        (if (= :combinator (:type compiled))
          ;; Combinators need their children compiled
          (assoc compiled :compiled-children
                 (if (vector? (:args node))
                   (mapv compile-node (:args node))
                   [(compile-node (:args node))]))
          compiled)))

    ;; Already compiled
    (fn? node) node

    ;; Unknown
    :else (throw (ex-info "Invalid DSL node" {:node node}))))

(defn compile-invariant
  "Compile an invariant definition into executable form."
  [{:keys [invariant/when invariant/assert] :as invariant}]
  (assoc invariant
         :compiled/when (compile-node when)
         :compiled/assert (compile-node assert)))

;; =============================================================================
;; EVALUATOR
;; =============================================================================

(defn eval-predicate
  "Evaluate a compiled predicate against an entity."
  [compiled entity-id]
  (case (:type compiled)
    :entity-predicate ((:check-fn compiled) entity-id)
    :global-predicate ((:check-fn compiled) entity-id)
    :graph-predicate ((:check-fn compiled) entity-id)
    :combinator ((:check-fn compiled) entity-id (:compiled-children compiled))))

(defn check-invariant
  "Check a single invariant against all entities."
  [{:keys [invariant/id invariant/level invariant/doc compiled/when compiled/assert] :as compiled-invariant}]
  (let [all-entities (map :atlas/dev-id (vals @cid/registry))
        message doc
        violations (for [entity-id all-entities
                         :when (eval-predicate when entity-id)
                         :when (not (eval-predicate assert entity-id))]
                     (cond-> {:invariant id
                              :entity entity-id
                              :level level
                              :severity level}
                       message (assoc :message message)))]
    violations))

(defn check-invariants
  "Check all invariants and return violations."
  [invariants]
  (let [compiled (map compile-invariant invariants)
        violations (vec (mapcat check-invariant compiled))
        errors (filterv #(= :error (:level %)) violations)
        warnings (filterv #(= :warning (:level %)) violations)]
    {:violations violations
     :errors errors
     :warnings warnings
     :violations-flat violations
     :errors-flat errors
     :warnings-flat warnings
     :valid? (empty? errors)}))
