(ns atlas.invariant.datalog-test
  (:require
   [clojure.test :refer [deftest is use-fixtures testing]]
   [atlas.datalog :as graph.datalog]
   [atlas.invariant.datalog :as invariant.datalog]
   [atlas.invariant.dsl.datalog :as invariant.dsl.datalog]
   [atlas.invariant.dsl.operators :as dsl.operators]
   [atlas.invariant.unified :as invariant.unified]
   [atlas.registry :as registry]))

(use-fixtures :each
  (fn [f]
    (reset! registry/registry {})
    (f)
    (reset! registry/registry {})))

;; =============================================================================
;; TEST DATA FIXTURES
;; =============================================================================

(defn- seed-basic-registry!
  "Basic registry for dataflow tests."
  []
  (dsl.operators/register-operators)
  ;; Endpoint depends on downstream functions
  (registry/register!

   :endpoint/orders

   :atlas/interface-endpoint

   #{ :tier/api}
   {:atlas/dev-id :endpoint/orders
    :interface-endpoint/context [:ctx/order]
    :execution-function/deps #{:fn/consumer}})

  ;; Consumer pulls input from the endpoint and depends on producer
  (registry/register!

   :fn/consumer

   :atlas/execution-function

   #{ :domain/order :operation/consume}
   {:atlas/dev-id :fn/consumer
    :interface-endpoint/context [:ctx/order :out/shipping]
    :execution-function/deps #{:fn/producer}})

  ;; Producer generates a key used by the consumer
  (registry/register!

   :fn/producer

   :atlas/execution-function

   #{ :domain/order :operation/produce}
   {:atlas/dev-id :fn/producer
    :interface-endpoint/response [:out/shipping]})

  ;; Function consumes a key with no producer
  (registry/register!

   :fn/missing-producer

   :atlas/execution-function

   #{ :domain/data}
   {:atlas/dev-id :fn/missing-producer
    :interface-endpoint/context [:data/unproduced]})

  ;; Function produces a key nobody consumes
  (registry/register!

   :fn/orphan-producer

   :atlas/execution-function

   #{ :domain/orphan}
   {:atlas/dev-id :fn/orphan-producer
   :interface-endpoint/response [:out/orphan]}))

(defn- seed-dataflow-markers-registry!
  "Registry for dataflow marker tests."
  []
  ;; External input and display output markers (data keys registered as entities)
  (registry/register!
   :data/external
   :dataflow/external-input
   #{:data/key-external}
   {})

  (registry/register!
   :data/display
   :dataflow/display-output
   #{:data/key-display}
   {})

  ;; Endpoint declares terminal response key
  (registry/register!

   :endpoint/terminal

   :atlas/interface-endpoint

   #{ :tier/api :domain/dataflow}
   {:atlas/dev-id :endpoint/terminal
    :interface-endpoint/context [:data/external]
    :interface-endpoint/response [:data/terminal]})

  ;; Function consumes external input without producer
  (registry/register!

   :fn/consumes-external

   :atlas/execution-function

   #{ :domain/dataflow :operation/consume-external}
   {:atlas/dev-id :fn/consumes-external
    :interface-endpoint/context [:data/external]})

  ;; Functions produce terminal/display outputs without consumers
  (registry/register!

   :fn/produces-terminal

   :atlas/execution-function

   #{ :domain/dataflow :operation/produce-terminal}
   {:atlas/dev-id :fn/produces-terminal
    :interface-endpoint/response [:data/terminal]})

  (registry/register!
   :fn/produces-display
   :atlas/execution-function
   #{ :domain/dataflow :operation/produce-display}
   {:atlas/dev-id :fn/produces-display
    :interface-endpoint/response [:data/display]}))

(defn- seed-oauth-registry!
  "Registry with OAuth entities for protocol/dependency tests."
  []
  (registry/register!

   :endpoint/auth

   :atlas/interface-endpoint

   #{ :tier/api}
   {:atlas/dev-id :endpoint/auth
    :interface-endpoint/context [:user/credentials]
    :execution-function/deps #{:fn/oauth-handler}})

  (registry/register!
   :fn/oauth-handler
   :atlas/execution-function
   #{ :tier/service :protocol/oauth}
   {:atlas/dev-id :fn/oauth-handler
    :interface-endpoint/context [:user/credentials]
    :interface-endpoint/response [:auth/token]
    :execution-function/deps #{:component/oauth}})

  (registry/register!
   :fn/non-oauth
   :atlas/execution-function
   #{ :tier/service}
   {:atlas/dev-id :fn/non-oauth
    :interface-endpoint/context [:some/input]
    :interface-endpoint/response [:some/output]
    :execution-function/deps #{:component/db}})

  (registry/register!
   :component/oauth
   :atlas/structure-component
   #{ :tier/foundation :protocol/oauth}
   {:atlas/dev-id :component/oauth})

  (registry/register!
   :component/db
   :atlas/structure-component
   #{ :tier/foundation}
   {:atlas/dev-id :component/db}))

;; =============================================================================
;; DATALOG BACKEND TESTS
;; =============================================================================

(deftest datalog-surface-basic-violations
  (seed-basic-registry!)
  (let [db (graph.datalog/create-db)]
    (testing "missing producers are reported"
      (is (= #{[:data/unproduced :fn/missing-producer]}
             (set (graph.datalog/query-missing-producers db)))))
    (testing "orphan outputs are reported"
      (is (= #{[:out/orphan :fn/orphan-producer]}
             (set (graph.datalog/query-orphan-outputs db)))))))

(deftest datalog-respects-dataflow-markers
  (seed-dataflow-markers-registry!)
  (let [db (graph.datalog/create-db)
        missing (set (graph.datalog/query-missing-producers db))
        orphans (set (graph.datalog/query-orphan-outputs db))]
    (testing "external input keys are not flagged as missing"
      (is (not (contains? missing [:data/external :fn/consumes-external]))))
    (testing "endpoint response keys are not flagged as orphans"
      (is (not (contains? orphans [:data/terminal :fn/produces-terminal]))))
    (testing "display output keys are not flagged as orphans"
      (is (not (contains? orphans [:data/display :fn/produces-display]))))))

(deftest check-all-datalog-aggregates-summary
  (seed-basic-registry!)
  (let [{:keys [errors warnings valid?]} (invariant.datalog/check-all-datalog)
        total-warning-violations (reduce + (map #(count (:violations %)) warnings))]
    (is (= 1 (count errors)) "only missing producers invariant should be an error")
    (is (= 2 (count warnings)) "orphan outputs and unreachable functions invariants should be warnings")
    (is (= 3 total-warning-violations) "should have 3 total warning violations (1 orphan + 2 unreachable)")
    (is (false? valid?))))

;; =============================================================================
;; DSL OPERATOR COMPILATION TESTS
;; =============================================================================

(deftest compile-to-datalog-entity-has-aspect
  (seed-oauth-registry!)
  (let [db (graph.datalog/create-db)
        check-fn (invariant.dsl.datalog/compile-to-datalog
                  {:op :dsl.op/entity-has-aspect :args :protocol/oauth}
                  db)]
    (testing "entity with aspect returns true"
      (is (true? (check-fn :fn/oauth-handler)))
      (is (true? (check-fn :component/oauth))))
    (testing "entity without aspect returns false"
      (is (false? (check-fn :fn/non-oauth)))
      (is (false? (check-fn :component/db)))
      (is (false? (check-fn :endpoint/auth))))))

(deftest compile-to-datalog-entity-lacks-aspect
  (seed-oauth-registry!)
  (let [db (graph.datalog/create-db)
        check-fn (invariant.dsl.datalog/compile-to-datalog
                  {:op :dsl.op/entity-lacks-aspect :args :protocol/oauth}
                  db)]
    (testing "entity without aspect returns true"
      (is (true? (check-fn :fn/non-oauth)))
      (is (true? (check-fn :component/db))))
    (testing "entity with aspect returns false"
      (is (false? (check-fn :fn/oauth-handler)))
      (is (false? (check-fn :component/oauth))))))

(deftest compile-to-datalog-entity-depends-on
  (seed-oauth-registry!)
  (let [db (graph.datalog/create-db)
        check-fn (invariant.dsl.datalog/compile-to-datalog
                  {:op :dsl.op/entity-depends-on :args :component/oauth}
                  db)]
    (testing "entity with dependency returns true"
      (is (true? (check-fn :fn/oauth-handler))))
    (testing "entity without dependency returns false"
      (is (false? (check-fn :fn/non-oauth)))
      (is (false? (check-fn :component/oauth)))
      (is (false? (check-fn :endpoint/auth))))))

(deftest compile-to-datalog-entity-produces
  (seed-oauth-registry!)
  (let [db (graph.datalog/create-db)
        check-fn (invariant.dsl.datalog/compile-to-datalog
                  {:op :dsl.op/entity-produces :args :auth/token}
                  db)]
    (testing "entity that produces key returns true"
      (is (true? (check-fn :fn/oauth-handler))))
    (testing "entity that doesn't produce key returns false"
      (is (false? (check-fn :fn/non-oauth))))))

(deftest compile-to-datalog-entity-consumes
  (seed-oauth-registry!)
  (let [db (graph.datalog/create-db)
        check-fn (invariant.dsl.datalog/compile-to-datalog
                  {:op :dsl.op/entity-consumes :args :user/credentials}
                  db)]
    (testing "entity that consumes key returns true"
      (is (true? (check-fn :fn/oauth-handler))))
    (testing "entity that doesn't consume key returns false"
      (is (false? (check-fn :fn/non-oauth))))))

;; =============================================================================
;; LOGIC OPERATOR TESTS
;; =============================================================================

(deftest compile-to-datalog-logic-and
  (seed-oauth-registry!)
  (let [db (graph.datalog/create-db)
        ;; Entity must have both oauth AND service tier
        check-fn (invariant.dsl.datalog/compile-to-datalog
                  {:op :dsl.op/logic-and
                   :args [{:op :dsl.op/entity-has-aspect :args :protocol/oauth}
                          {:op :dsl.op/entity-has-aspect :args :tier/service}]}
                  db)]
    (testing "entity with both aspects returns true"
      (is (true? (check-fn :fn/oauth-handler))))
    (testing "entity with only one aspect returns false"
      (is (false? (check-fn :component/oauth))))  ; oauth but not service
    (testing "entity with neither aspect returns false"
      (is (false? (check-fn :component/db))))))

(deftest compile-to-datalog-logic-or
  (seed-oauth-registry!)
  (let [db (graph.datalog/create-db)
        ;; Entity has oauth OR api tier
        check-fn (invariant.dsl.datalog/compile-to-datalog
                  {:op :dsl.op/logic-or
                   :args [{:op :dsl.op/entity-has-aspect :args :protocol/oauth}
                          {:op :dsl.op/entity-has-aspect :args :tier/api}]}
                  db)]
    (testing "entity with first aspect returns true"
      (is (check-fn :fn/oauth-handler)))
    (testing "entity with second aspect returns true"
      (is (check-fn :endpoint/auth)))
    (testing "entity with neither returns falsy"
      (is (not (check-fn :component/db))))))

(deftest compile-to-datalog-logic-not
  (seed-oauth-registry!)
  (let [db (graph.datalog/create-db)
        ;; Entity does NOT have oauth
        check-fn (invariant.dsl.datalog/compile-to-datalog
                  {:op :dsl.op/logic-not
                   :args [{:op :dsl.op/entity-has-aspect :args :protocol/oauth}]}
                  db)]
    (testing "entity without aspect returns true"
      (is (true? (check-fn :fn/non-oauth)))
      (is (true? (check-fn :component/db))))
    (testing "entity with aspect returns false"
      (is (false? (check-fn :fn/oauth-handler)))
      (is (false? (check-fn :component/oauth))))))

(deftest compile-to-datalog-logic-implies
  (seed-oauth-registry!)
  (let [db (graph.datalog/create-db)
        ;; If entity has oauth, it must have service tier
        ;; oauth -> service = NOT(oauth) OR service
        check-fn (invariant.dsl.datalog/compile-to-datalog
                  {:op :dsl.op/logic-implies
                   :args [{:op :dsl.op/entity-has-aspect :args :protocol/oauth}
                          {:op :dsl.op/entity-has-aspect :args :tier/service}]}
                  db)]
    (testing "oauth=true, service=true -> true"
      (is (true? (check-fn :fn/oauth-handler))))
    (testing "oauth=true, service=false -> false (violation)"
      (is (false? (check-fn :component/oauth))))
    (testing "oauth=false -> true (vacuously true)"
      (is (true? (check-fn :fn/non-oauth)))
      (is (true? (check-fn :endpoint/auth))))))

;; =============================================================================
;; GRAPH OPERATOR TESTS
;; =============================================================================

(deftest compile-to-datalog-graph-acyclic
  (seed-oauth-registry!)
  (let [db (graph.datalog/create-db)
        check-fn (invariant.dsl.datalog/compile-to-datalog
                  {:op :dsl.op/graph-acyclic :args {:edge :depends}}
                  db)]
    (testing "acyclic graph returns true"
      (is (true? (check-fn :fn/oauth-handler))))))

(deftest compile-to-datalog-graph-reachable
  (seed-oauth-registry!)
  (let [db (graph.datalog/create-db)
        check-fn (invariant.dsl.datalog/compile-to-datalog
                  {:op :dsl.op/graph-reachable :args {:edge :depends}}
                  db)]
    (testing "reachable entities return true"
      (is (true? (check-fn :endpoint/auth)))
      (is (true? (check-fn :fn/oauth-handler)))
      (is (true? (check-fn :component/oauth))))))

;; =============================================================================
;; AXIOM COMPILATION AND CHECKING TESTS
;; =============================================================================

(deftest compile-and-check-invariant-datalog
  (seed-oauth-registry!)
  (let [db (graph.datalog/create-db)
        invariant {:invariant/id :test/oauth-needs-service
               :invariant/level :error
               :invariant/when {:op :dsl.op/entity-has-aspect :args :protocol/oauth}
               :invariant/assert {:op :dsl.op/entity-has-aspect :args :tier/service}}
        compiled (invariant.dsl.datalog/compile-invariant-datalog invariant db)
        violations (invariant.dsl.datalog/check-invariant-datalog compiled db)]
    (testing "invariant is compiled correctly"
      (is (fn? (:compiled/when compiled)))
      (is (fn? (:compiled/assert compiled))))
    (testing "violations are detected"
      ;; component/oauth has protocol/oauth but not tier/service
      (is (= 1 (count violations)))
      (is (= :component/oauth (:entity (first violations)))))))

(deftest check-invariants-datalog-multiple
  (seed-oauth-registry!)
  (let [invariants [{:invariant/id :test/oauth-has-service
                 :invariant/level :error
                 :invariant/when {:op :dsl.op/entity-has-aspect :args :protocol/oauth}
                 :invariant/assert {:op :dsl.op/entity-has-aspect :args :tier/service}}
                {:invariant/id :test/functions-have-tier
                 :invariant/level :warning
                 :invariant/when {:op :dsl.op/entity-has-aspect :args :atlas/execution-function}
                 :invariant/assert {:op :dsl.op/entity-has-aspect :args :tier/service}}]
        result (invariant.dsl.datalog/check-invariants-datalog invariants)]
    (testing "multiple invariants are checked"
      (is (= 1 (count (:errors result))))
      (is (= 0 (count (:warnings result))))
      (is (false? (:valid? result))))))

;; =============================================================================
;; HYBRID APPROACH TESTS
;; =============================================================================

(deftest simple-invariant-detection
  (testing "simple invariants are detected correctly"
    (let [simple {:invariant/when {:op :dsl.op/entity-has-aspect :args :protocol/oauth}
                  :invariant/assert {:op :dsl.op/entity-depends-on :args :component/oauth}}]
      (is (true? (invariant.dsl.datalog/simple-invariant? simple)))))

  (testing "complex invariants with graph ops are detected"
    (let [complex {:invariant/when {:op :dsl.op/entity-has-aspect :args :atlas/execution-function}
                   :invariant/assert {:op :dsl.op/graph-acyclic :args {:edge :depends}}}]
      (is (false? (invariant.dsl.datalog/simple-invariant? complex)))))

  (testing "nested graph ops are detected"
    (let [nested {:invariant/when {:op :dsl.op/entity-has-aspect :args :atlas/execution-function}
                  :invariant/assert {:op :dsl.op/logic-or
                                 :args [{:op :dsl.op/entity-has-aspect :args :tier/service}
                                        {:op :dsl.op/graph-reachable :args {:edge :depends}}]}}]
      (is (false? (invariant.dsl.datalog/simple-invariant? nested))))))

(deftest contains-graph-op-edge-cases
  (testing "nil node"
    (is (false? (invariant.dsl.datalog/contains-graph-op? nil))))
  (testing "empty map"
    (is (false? (invariant.dsl.datalog/contains-graph-op? {}))))
  (testing "keyword args"
    (is (false? (invariant.dsl.datalog/contains-graph-op?
                 {:op :dsl.op/entity-has-aspect :args :some/aspect}))))
  (testing "map args"
    (is (false? (invariant.dsl.datalog/contains-graph-op?
                 {:op :dsl.op/entity-depends-on :args :some/dep})))))

;; =============================================================================
;; UNIFIED INTERFACE TESTS
;; =============================================================================

(deftest unified-dsl-invariant
  (seed-oauth-registry!)
  (let [invariant (invariant.unified/dsl-invariant
               :test/oauth-must-depend
               :error
               "OAuth entities must depend on OAuth component"
               {:op :dsl.op/entity-has-aspect :args :protocol/oauth}
               {:op :dsl.op/entity-depends-on :args :component/oauth})
        result (invariant.unified/check-all invariant)]
    (testing "DSL invariant detects violations"
      (is (= 1 (count (:violations result))))
      (is (= :component/oauth (:entity (first (:violations result))))))))

(deftest unified-fn-invariant
  (seed-oauth-registry!)
  (let [invariant (invariant.unified/fn-invariant
               :test/oauth-fn-style
               :error
               "OAuth entities must depend on OAuth component (function style)"
               (fn [db]
                 (let [oauth-entities (graph.datalog/query-entities-with-aspect db :protocol/oauth)
                       violations (remove #(graph.datalog/query-depends-on db % :component/oauth) oauth-entities)]
                   (map (fn [e] {:entity e}) violations))))
        result (invariant.unified/check-all invariant)]
    (testing "Function invariant detects same violations"
      (is (= 1 (count (:violations result))))
      (is (= :component/oauth (:entity (first (:violations result))))))))

(deftest unified-mixed-invariants
  (seed-oauth-registry!)
  (let [dsl-ax (invariant.unified/dsl-invariant
                :test/dsl-check
                :error
                "DSL check"
                {:op :dsl.op/entity-has-aspect :args :protocol/oauth}
                {:op :dsl.op/entity-has-aspect :args :tier/service})
        fn-ax (invariant.unified/fn-invariant
               :test/fn-check
               :warning
               "Function check"
               (fn [db]
                 (let [components (graph.datalog/query-entities-with-aspect db :atlas/structure-component)
                       foundation-entities (set (graph.datalog/query-entities-with-aspect db :tier/foundation))
                       without-foundation (remove foundation-entities components)]
                   (map (fn [e] {:entity e}) without-foundation))))
        result (invariant.unified/check-all [dsl-ax fn-ax])]
    (testing "mixed invariant types work together"
      (is (map? result))
      (is (contains? result :violations))
      (is (contains? result :errors))
      (is (contains? result :warnings)))))

(deftest unified-invariant-protocol
  (seed-oauth-registry!)
  (let [invariant (invariant.unified/dsl-invariant
               :test/protocol-check
               :warning
               "Test invariant for protocol implementation"
               {:op :dsl.op/entity-has-aspect :args :atlas/execution-function}
               {:op :dsl.op/entity-has-aspect :args :tier/service})]
    (testing "Axiom protocol methods work"
      (is (= :test/protocol-check (invariant.unified/invariant-id invariant)))
      (is (= :warning (invariant.unified/invariant-level invariant)))
      (is (= "Test invariant for protocol implementation" (invariant.unified/invariant-doc invariant))))))

(deftest unified-document-invariant
  (seed-oauth-registry!)
  (let [dsl-ax (invariant.unified/dsl-invariant :test/dsl :error "DSL doc"
                                  {:op :dsl.op/entity-has-aspect :args :a/b}
                                  {:op :dsl.op/entity-has-aspect :args :c/d})
        fn-ax (invariant.unified/fn-invariant :test/fn :warning "FN doc" (fn [_] []))
        dsl-doc (invariant.unified/document-invariant dsl-ax)
        fn-doc (invariant.unified/document-invariant fn-ax)]
    (testing "DSL invariant documentation"
      (is (= :test/dsl (:id dsl-doc)))
      (is (= :error (:level dsl-doc)))
      (is (= :dsl (:style dsl-doc))))
    (testing "Function invariant documentation"
      (is (= :test/fn (:id fn-doc)))
      (is (= :warning (:level fn-doc)))
      (is (= :function (:style fn-doc))))))

;; =============================================================================
;; INVARIANT REGISTRY TESTS
;; =============================================================================

(deftest invariant-registry-operations
  (reset! invariant.unified/invariant-registry {})
  (let [invariant (invariant.unified/dsl-invariant :test/registry :error "Test"
                                 {:op :dsl.op/entity-has-aspect :args :a/b}
                                 {:op :dsl.op/entity-has-aspect :args :c/d})]
    (testing "register invariant"
      (invariant.unified/register! invariant)
      (is (= 1 (count (invariant.unified/registered-invariants))))
      (is (= invariant (first (invariant.unified/registered-invariants)))))

    (testing "unregister by invariant"
      (invariant.unified/unregister! invariant)
      (is (= 0 (count (invariant.unified/registered-invariants)))))

    (testing "unregister by id"
      (invariant.unified/register! invariant)
      (invariant.unified/unregister! :test/registry)
      (is (= 0 (count (invariant.unified/registered-invariants))))))
  (reset! invariant.unified/invariant-registry {}))

;; =============================================================================
;; DSL->FUNCTION CONVERSION TEST
;; =============================================================================

(deftest dsl-to-function-conversion
  (seed-oauth-registry!)
  (let [dsl-ax (invariant.unified/dsl-invariant
                :test/converted
                :error
                "Converted invariant"
                {:op :dsl.op/entity-has-aspect :args :protocol/oauth}
                {:op :dsl.op/entity-depends-on :args :component/oauth})
        fn-ax (invariant.unified/dsl->function dsl-ax)
        dsl-result (invariant.unified/check-all dsl-ax)
        fn-result (invariant.unified/check-all fn-ax)]
    (testing "converted function produces same results"
      (is (= (count (:violations dsl-result))
             (count (:violations fn-result))))
      (is (= (set (map :entity (:violations dsl-result)))
             (set (map :entity (:violations fn-result))))))))
