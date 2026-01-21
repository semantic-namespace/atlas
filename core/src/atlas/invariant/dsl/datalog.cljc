(ns atlas.invariant.dsl.datalog
  "Bridge between DSL invariants and Datalog backend."
  (:require [atlas.invariant.dsl.operators :as dsl.operators]
            [atlas.invariant.datalog :as invariant.datalog]
            [atlas.datalog :as datalog]
            [atlas.cljc.platform :as platform]
            [atlas.registry.lookup :as entity]
            [datascript.core :as d]))

;; =============================================================================
;; DSL OPERATOR COMPILATION TO DATALOG
;; =============================================================================

(defmulti compile-to-datalog
  "Compile DSL operator to Datalog query."
  (fn [node _db] (:op node)))

(defmethod compile-to-datalog :dsl.op/entity-has-aspect
  [{:keys [args]} db]
  (fn [entity-id]
    (boolean (some #(= entity-id %) (datalog/query-entities-with-aspect db args)))))

(defmethod compile-to-datalog :dsl.op/entity-lacks-aspect
  [{:keys [args]} db]
  (fn [entity-id]
    (boolean (some #(= entity-id %) (datalog/query-entities-lacking-aspect db args)))))

(defmethod compile-to-datalog :dsl.op/entity-depends-on
  [{:keys [args]} db]
  (fn [entity-id]
    (boolean (datalog/query-depends-on db entity-id args))))

(defmethod compile-to-datalog :dsl.op/entity-produces
  [{:keys [args]} db]
  (fn [entity-id]
    (let [produced (set (datalog/query-produces db entity-id))]
      (contains? produced args))))

(defmethod compile-to-datalog :dsl.op/entity-consumes
  [{:keys [args]} db]
  (fn [entity-id]
    (let [consumed (set (datalog/query-consumes db entity-id))]
      (contains? consumed args))))

(defmethod compile-to-datalog :dsl.op/data-has-producer
  [{:keys [args]} db]
  (fn [_entity-id]
    (seq (datalog/query-producers-of db args))))

(defmethod compile-to-datalog :dsl.op/data-has-consumer
  [{:keys [args]} db]
  (fn [_entity-id]
    (seq (datalog/query-consumers-of db args))))

(defmethod compile-to-datalog :dsl.op/logic-and
  [{:keys [args]} db]
  (let [compiled-children (mapv #(compile-to-datalog % db) args)]
    (fn [entity-id]
      (every? #(% entity-id) compiled-children))))

(defmethod compile-to-datalog :dsl.op/logic-or
  [{:keys [args]} db]
  (let [compiled-children (mapv #(compile-to-datalog % db) args)]
    (fn [entity-id]
      (some #(% entity-id) compiled-children))))

(defmethod compile-to-datalog :dsl.op/logic-not
  [{:keys [args]} db]
  (let [compiled-child (compile-to-datalog (first args) db)]
    (fn [entity-id]
      (not (compiled-child entity-id)))))

(defmethod compile-to-datalog :dsl.op/logic-implies
  [{:keys [args]} db]
  (let [[antecedent consequent] args
        compiled-a (compile-to-datalog antecedent db)
        compiled-b (compile-to-datalog consequent db)]
    (fn [entity-id]
      (or (not (compiled-a entity-id))
          (compiled-b entity-id)))))

(defmethod compile-to-datalog :dsl.op/graph-acyclic
  [node db]
  (fn [_entity-id]
    ;; Global check - no cycles in dependency graph
    (nil? (datalog/query-acyclic-deps db))))

(defmethod compile-to-datalog :dsl.op/graph-reachable
  [node db]
  (fn [entity-id]
    ;; Check if entity is reachable from endpoints
    (let [endpoints (datalog/query-entities-with-aspect db :atlas/interface-endpoint)
          reachable (atom #{})
          collect-reachable (fn collect [id]
                              (when-not (@reachable id)
                                (swap! reachable conj id)
                                (doseq [dep (datalog/query-dependencies db id)]
                                  (collect dep))))]
      (doseq [ep endpoints]
        (collect-reachable ep))
      (contains? @reachable entity-id))))

;; =============================================================================
;; DATALOG-BACKED AXIOM CHECKING
;; =============================================================================

(defn compile-invariant-datalog
  "Compile DSL invariant to use Datalog backend."
  [{:keys [invariant/when invariant/assert] :as invariant} db]
  (assoc invariant
         :compiled/when (compile-to-datalog when db)
         :compiled/assert (compile-to-datalog assert db)))

(defn eval-predicate-datalog
  "Evaluate compiled Datalog predicate."
  [compiled-fn entity-id]
  (compiled-fn entity-id))

(defn check-invariant-datalog
  "Check single invariant using Datalog backend.

   Filters out ontology meta-entities (marked with :atlas/ontology) as they
   are not business logic."
  [{:keys [invariant/id invariant/level invariant/doc compiled/when compiled/assert]} db]
  (let [;; Get all entities but exclude ontology meta-entities
        all-entities (->> (d/q '[:find [?dev-id ...]
                                 :where [?e :atlas/dev-id ?dev-id]]
                               db)
                          (remove #(entity/has-aspect? % :atlas/ontology)))
        message doc
        violations (for [entity-id all-entities
                         :when (eval-predicate-datalog when entity-id)
                         :when (not (eval-predicate-datalog assert entity-id))]
                     (cond-> {:invariant id
                              :entity entity-id
                              :level level
                              :severity level}
                       message (assoc :message message)))]
    violations))

(defn check-invariants-datalog
  "Check all invariants using Datalog backend."
  [invariants]
  (let [db (datalog/create-db)
        compiled (map #(compile-invariant-datalog % db) invariants)
        violations (vec (mapcat #(check-invariant-datalog % db) compiled))
        errors (filterv #(= :error (:level %)) violations)
        warnings (filterv #(= :warning (:level %)) violations)]
    {:violations violations
     :errors errors
     :warnings warnings
     :violations-flat violations
     :errors-flat errors
     :warnings-flat warnings
     :valid? (empty? errors)}))

;; =============================================================================
;; HYBRID APPROACH: BEST OF BOTH WORLDS
;; =============================================================================

(defn contains-graph-op?
  "Check if node contains graph operations."
  [node]
  (boolean
   (or (= (:op node) :dsl.op/graph-acyclic)
       (= (:op node) :dsl.op/graph-reachable)
       (and (map? node)
            (:args node)
            (let [args (:args node)]
              (cond
                ;; Logic operators have vector of child nodes
                (vector? args) (some contains-graph-op? args)
                ;; Single child node (like logic-not)
                (map? args) (contains-graph-op? args)
                ;; Simple args (keywords, maps for graph ops) - not recursive
                :else false))))))

(defn simple-invariant?
  "Check if invariant can be efficiently evaluated in pure Datalog."
  [{:keys [invariant/when invariant/assert]}]
  ;; Simple if no complex graph operations
  (and (not (contains-graph-op? when))
       (not (contains-graph-op? assert))))

(defn check-simple-invariants-datalog
  "Check simple invariants efficiently with pure Datalog."
  [db invariants]
  (let [compiled (map #(compile-invariant-datalog % db) invariants)
        violations (vec (mapcat #(check-invariant-datalog % db) compiled))
        errors (filterv #(= :error (:level %)) violations)
        warnings (filterv #(= :warning (:level %)) violations)]
    {:violations violations
     :errors errors
     :warnings warnings
     :violations-flat violations
     :errors-flat errors
     :warnings-flat warnings
     :valid? (empty? errors)}))

(defn check-invariants-hybrid
  "Check invariants using both DSL and Datalog.
   Uses Datalog for performance, DSL for flexibility."
  [invariants]
  (let [db (datalog/create-db)
        ;; Partition invariants by complexity
        simple-invariants (filter #(simple-invariant? %) invariants)
        complex-invariants (remove #(simple-invariant? %) invariants)

        ;; Check simple invariants with optimized Datalog
        simple-results (check-simple-invariants-datalog db simple-invariants)

        ;; Check complex invariants with flexible DSL
        complex-results (dsl.operators/check-invariants complex-invariants)

        violations (vec (concat (:violations simple-results)
                                (:violations complex-results)))
        errors (vec (concat (:errors simple-results)
                            (:errors complex-results)))
        warnings (vec (concat (:warnings simple-results)
                              (:warnings complex-results)))
        violations-flat (vec (concat (:violations-flat simple-results)
                                     (:violations-flat complex-results)))
        errors-flat (vec (concat (:errors-flat simple-results)
                                 (:errors-flat complex-results)))
        warnings-flat (vec (concat (:warnings-flat simple-results)
                                   (:warnings-flat complex-results)))]

    {:violations violations
     :errors errors
     :warnings warnings
     :violations-flat violations-flat
     :errors-flat errors-flat
     :warnings-flat warnings-flat
     :valid? (empty? errors-flat)}))

;; =============================================================================
;; DIRECT DATALOG QUERY INTERFACE
;; =============================================================================

(defn get-invariant-by-id
  "Get invariant definition by ID."
  [invariants invariant-id]
  (require '[semantic-namespace.invariant.definitions :as defs])
  (first (filter #(= invariant-id (:invariant/id %))
                 invariants)))

(defn query-invariant-violations
  "Query invariant violations using raw Datalog.
   More efficient for specific queries."
  [invariants invariant-id db]
  (case invariant-id
    :invariant/no-missing-producers
    (map (fn [[ctx consumer]]
           {:invariant :invariant/no-missing-producers
            :entity consumer
            :missing-key ctx
            :level :error})
         (datalog/query-missing-producers db))

    :invariant/no-orphan-outputs
    (map (fn [[resp producer]]
           {:invariant :invariant/no-orphan-outputs
            :entity producer
            :orphan-key resp
            :level :warning})
         (datalog/query-orphan-outputs db))

    :invariant/deps-exist
    (map (fn [[dev missing]]
           {:invariant :invariant/deps-exist
            :entity dev
            :missing-dep missing
            :level :error})
         (datalog/query-missing-dependencies db))

    ;; Default: use DSL compilation
    (let [invariant (get-invariant-by-id invariants invariant-id)]
      (check-invariant-datalog (compile-invariant-datalog invariant db) db))))

;; =============================================================================
;; PERFORMANCE COMPARISON
;; =============================================================================

(defn benchmark-backends
  "Compare performance of different backends."
  [invariants]
  (let [dsl-start (platform/now-ms)
        dsl-result (dsl.operators/check-invariants invariants)
        dsl-time (- (platform/now-ms) dsl-start)

        datalog-start (platform/now-ms)
        datalog-result (check-invariants-datalog invariants)
        datalog-time (- (platform/now-ms) datalog-start)

        native-start (platform/now-ms)
        native-result (invariant.datalog/check-all-datalog)
        native-time (- (platform/now-ms) native-start)]

    {:dsl {:time-ms dsl-time
           :violations (count (:violations dsl-result))}
     :datalog-dsl {:time-ms datalog-time
                   :violations (count (:violations datalog-result))}
     :native-datalog {:time-ms native-time
                      :violations (count (:violations native-result))}}))
