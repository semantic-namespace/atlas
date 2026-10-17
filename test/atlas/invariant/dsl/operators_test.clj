(ns atlas.invariant.dsl.operators-test
  "atlas.invariant.dsl.operators evaluates DSL invariants without Datascript,
   so they also run in babashka. It must agree with the Datalog backend
   (atlas.invariant.unified) wherever that backend can compile the invariant;
   the graph operators only the operators path implements must run."
  (:require [clojure.test :refer [deftest is testing]]
            [atlas.registry :as registry]
            [atlas.datalog :as datalog]
            [atlas.invariant.dsl.operators :as ops]
            [atlas.invariant.unified :as unified]
            [app.cart]
            [app.cart-invariants]
            [app.pet-shop]
            [app.pet-shop-invariants]
            [app.calendar-availability]
            [app.calendar-availability-invariants]))

(defn- fresh! [init]
  (registry/reset-all!)
  (doseq [n '[atlas.ontology atlas.ontology.execution-function atlas.ontology.structure-component
              atlas.ontology.interface-endpoint atlas.ontology.data-schema atlas.ontology.interface-protocol]]
    (require n :reload))
  (init)
  (datalog/reset-db-cache!))

(defn- dsl-invariants
  "Every DSL invariant var in `ns`, as the map the operators path takes."
  [ns]
  (->> (vals (ns-publics ns))
       (map deref)
       (filter #(instance? atlas.invariant.unified.DslAxiom %))
       (map (fn [{:keys [id level doc when assert]}]
              {:invariant/id id :invariant/level level :invariant/doc doc
               :invariant/when when :invariant/assert assert}))
       (sort-by (comp str :invariant/id))))

(defn- violations [result]
  (set (map (juxt :invariant :entity :level) (:violations result))))

(def apps
  [[app.cart/init-registry! 'app.cart-invariants]
   [app.pet-shop/init-registry! 'app.pet-shop-invariants]
   [app.calendar-availability/init-registry! 'app.calendar-availability-invariants]])

(deftest operators-agree-with-the-datalog-backend
  (doseq [[init ns] apps]
    (fresh! init)
    (let [invs (dsl-invariants ns)]
      (is (seq invs) (str ns " has DSL invariants"))
      (doseq [inv invs
              :let [datalog (try (violations (unified/check-all [(unified/dsl-invariant (:invariant/id inv) (:invariant/level inv) (:invariant/doc inv)
                                                                                         (:invariant/when inv) (:invariant/assert inv))]))
                                 (catch Exception _ ::not-compiled))
                    by-ops (violations (ops/check-invariants [inv]))]]
        (if (= ::not-compiled datalog)
          (testing (str (:invariant/id inv) " runs where the Datalog backend can't compile it")
            (is (set? by-ops)))
          (is (= datalog by-ops) (str (:invariant/id inv) " in " ns)))))))

(deftest combinators-nest
  (fresh! app.cart/init-registry!)
  (let [check #(violations (ops/check-invariants [{:invariant/id :t/x :invariant/level :error
                                                   :invariant/when %1 :invariant/assert %2}]))
        is-fn {:op :dsl.op/entity-has-aspect :args :atlas/execution-function}
        pure {:op :dsl.op/entity-has-aspect :args :effect/pure}]
    (testing "not, and, or, implies over each other"
      (is (= (check is-fn pure)
             (check is-fn {:op :dsl.op/logic-not :args {:op :dsl.op/logic-not :args pure}})))
      (is (= (check is-fn pure)
             (check {:op :dsl.op/logic-and :args [is-fn {:op :dsl.op/logic-or :args [is-fn pure]}]} pure)))
      (is (empty? (check is-fn {:op :dsl.op/logic-implies :args [pure pure]}))))))

(deftest ontology-entities-are-not-judged
  (fresh! app.cart/init-registry!)
  (let [hits (ops/check-invariants [{:invariant/id :t/fns-are-pure :invariant/level :warning
                                     :invariant/when {:op :dsl.op/entity-has-aspect :args :atlas/execution-function}
                                     :invariant/assert {:op :dsl.op/entity-has-aspect :args :effect/pure}}])]
    (is (not-any? #(= :atlas/execution-function (:entity %)) (:violations hits))
        "the execution-function ontology isn't an execution function")))
