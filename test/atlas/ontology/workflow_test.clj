(ns atlas.ontology.workflow-test
  (:require
   [clojure.test :refer [deftest is use-fixtures testing]]
   [clojure.spec.alpha :as s]
   [atlas.registry :as registry]
   [atlas.registry.lookup :as entity]
   [atlas.datalog :as datalog]
   [atlas.ontology :as ontology]
   [atlas.invariant :as inv]))

(use-fixtures :each
  (fn [f]
    (registry/reset-all!)
    (datalog/reset-all!)
    (require 'atlas.ontology.execution-function :reload)
    (require 'atlas.ontology.workflow-producer :reload)
    (require 'atlas.ontology.workflow :reload)
    (f)
    (registry/reset-all!)
    (datalog/reset-all!)))

;; =============================================================================
;; TEST DATA
;; =============================================================================

(defn- seed-workflow-registry!
  "Register a complete order-fulfillment workflow for testing."
  []
  ;; Execution-functions (producer business logic)
  (registry/register!
   :fn/fetch-inventory
   :atlas/execution-function
   #{:domain/warehouse :operation/fetch :tier/service}
   {:execution-function/context [:warehouse/sku]
    :execution-function/response [:workflow/signal :async/seconds-to-delay :warehouse/inventory]})

  (registry/register!
   :fn/reserve-stock
   :atlas/execution-function
   #{:domain/warehouse :operation/reserve :tier/service}
   {:execution-function/context [:warehouse/sku :warehouse/inventory]
    :execution-function/response [:workflow/signal :async/seconds-to-delay :warehouse/reservation-id]})

  (registry/register!
   :fn/notify-shipping
   :atlas/execution-function
   #{:domain/shipping :operation/notify :tier/service}
   {:execution-function/context [:warehouse/reservation-id]
    :execution-function/response [:workflow/signal :async/seconds-to-delay :shipping/tracking-id]})

  ;; Workflow-producers (FSM state nodes)
  (registry/register!
   :wp/fetch-inventory
   :atlas/workflow-producer
   #{:domain/warehouse :workflow/inventory-check}
   {:workflow-producer/execution-function :fn/fetch-inventory
    :workflow-producer/signals #{:signal/in-stock :signal/out-of-stock}
    :workflow-producer/output [:warehouse/inventory]})

  (registry/register!
   :wp/reserve-stock
   :atlas/workflow-producer
   #{:domain/warehouse :workflow/reservation}
   {:workflow-producer/execution-function :fn/reserve-stock
    :workflow-producer/signals #{:signal/reserved :signal/reserve-failed}
    :workflow-producer/output [:warehouse/reservation-id]})

  (registry/register!
   :wp/notify-shipping
   :atlas/workflow-producer
   #{:domain/shipping :workflow/shipping-notification}
   {:workflow-producer/execution-function :fn/notify-shipping
    :workflow-producer/signals #{:signal/shipped :signal/ship-failed}
    :workflow-producer/output [:shipping/tracking-id]})

  ;; Workflow (FSM graph) — per-producer transitions
  (registry/register!
   :wf/order-fulfillment
   :atlas/workflow
   #{:domain/warehouse :workflow/fulfillment}
   {:workflow/producers #{:wp/fetch-inventory :wp/reserve-stock :wp/notify-shipping}
    :workflow/initial-producer :wp/fetch-inventory
    :workflow/transitions {:wp/fetch-inventory {:signal/in-stock :wp/reserve-stock
                                                :signal/out-of-stock :wp/notify-shipping}
                           :wp/reserve-stock   {:signal/reserved :wp/notify-shipping}}
    :workflow/context #{:warehouse/sku}
    :workflow/async-effect #{:fn/notify-shipping}}))

;; =============================================================================
;; SPEC TESTS
;; =============================================================================

(deftest workflow-producer-specs
  (testing "valid workflow-producer props"
    (is (s/valid? :atlas/workflow-producer-props
                  {:workflow-producer/execution-function :fn/fetch-inventory
                   :workflow-producer/signals #{:signal/in-stock :signal/out-of-stock}
                   :workflow-producer/output [:warehouse/inventory]})))

  (testing "empty signals rejected"
    (is (not (s/valid? :atlas/workflow-producer-props
                       {:workflow-producer/execution-function :fn/fetch-inventory
                        :workflow-producer/signals #{}
                        :workflow-producer/output []}))))

  (testing "missing execution-function rejected"
    (is (not (s/valid? :atlas/workflow-producer-props
                       {:workflow-producer/signals #{:signal/ok}
                        :workflow-producer/output []}))))

  (testing "missing output rejected"
    (is (not (s/valid? :atlas/workflow-producer-props
                       {:workflow-producer/execution-function :fn/fetch-inventory
                        :workflow-producer/signals #{:signal/ok}})))))

(deftest workflow-specs
  (testing "valid workflow props with nested transitions"
    (is (s/valid? :atlas/workflow-props
                  {:workflow/producers #{:wp/a :wp/b}
                   :workflow/initial-producer :wp/a
                   :workflow/transitions {:wp/a {:signal/x :wp/b}}
                   :workflow/context #{:data/in}
                   :workflow/async-effect #{:fx/notify}})))

  (testing "missing transitions rejected"
    (is (not (s/valid? :atlas/workflow-props
                       {:workflow/producers #{:wp/a}
                        :workflow/initial-producer :wp/a}))))

  (testing "empty producers rejected"
    (is (not (s/valid? :atlas/workflow-props
                       {:workflow/producers #{}
                        :workflow/initial-producer :wp/a
                        :workflow/transitions {:wp/a {:signal/x :wp/a}}}))))

  (testing "same signal routes differently per producer"
    (is (s/valid? :atlas/workflow-props
                  {:workflow/producers #{:wp/a :wp/b :wp/c}
                   :workflow/initial-producer :wp/a
                   :workflow/transitions {:wp/a {:signal/success :wp/b}
                                          :wp/b {:signal/success :wp/c}}
                   :workflow/context #{:data/in}
                   :workflow/async-effect #{:fx/notify}}))))

;; =============================================================================
;; ONTOLOGY REGISTRATION TESTS
;; =============================================================================

(deftest ontology-registration
  (testing "workflow-producer ontology is registered"
    (is (some? (ontology/ontology-for :atlas/workflow-producer)))
    (is (= [:workflow-producer/execution-function :workflow-producer/signals
            :workflow-producer/output]
           (:ontology/keys (ontology/ontology-for :atlas/workflow-producer)))))

  (testing "workflow ontology is registered"
    (is (some? (ontology/ontology-for :atlas/workflow)))
    (is (= [:workflow/producers :workflow/initial-producer :workflow/transitions
            :workflow/async-effect :workflow/context]
           (:ontology/keys (ontology/ontology-for :atlas/workflow))))))

;; =============================================================================
;; DATALOG TESTS
;; =============================================================================

(deftest datalog-dependency-graph
  (seed-workflow-registry!)
  (let [db (datalog/create-db)]

    (testing "workflow depends on all its producers"
      (let [deps (set (datalog/query-dependencies db :wf/order-fulfillment))]
        (is (contains? deps :wp/fetch-inventory))
        (is (contains? deps :wp/reserve-stock))
        (is (contains? deps :wp/notify-shipping))))

    (testing "workflow-producer depends on its execution-function"
      (is (= [:fn/fetch-inventory]
             (datalog/query-dependencies db :wp/fetch-inventory)))
      (is (= [:fn/reserve-stock]
             (datalog/query-dependencies db :wp/reserve-stock)))
      (is (= [:fn/notify-shipping]
             (datalog/query-dependencies db :wp/notify-shipping))))

    (testing "transitive dependency chain: workflow → producer → execution-function"
      (let [chain (datalog/query-dependency-chain db :wf/order-fulfillment)]
        (is (contains? chain :wp/fetch-inventory))
        (is (contains? chain :fn/fetch-inventory))
        (is (contains? chain :fn/reserve-stock))
        (is (contains? chain :fn/notify-shipping))))))

(deftest datalog-workflow-aggregates-producer-dataflow
  (seed-workflow-registry!)
  (let [db (datalog/create-db)]

    (testing "producer emits :entity/produces for its output keys"
      (is (contains? (set (datalog/query-producers-of db :warehouse/inventory))
                     :wp/fetch-inventory))
      (is (contains? (set (datalog/query-producers-of db :warehouse/reservation-id))
                     :wp/reserve-stock)))

    (testing "workflow aggregates producer outputs uniformly (same verb as ef)"
      (let [producers (set (datalog/query-producers-of db :shipping/tracking-id))]
        (is (contains? producers :wp/notify-shipping))
        (is (contains? producers :wf/order-fulfillment))))

    (testing "workflow :entity/produces = union of producer outputs"
      (is (= #{:warehouse/inventory :warehouse/reservation-id :shipping/tracking-id}
             (set (datalog/query-produces db :wf/order-fulfillment)))))

    (testing "workflow :entity/consumes = external keys only (internal flow filtered out)"
      ;; :warehouse/inventory is consumed by :wp/reserve-stock but produced by :wp/fetch-inventory → internal
      ;; :warehouse/reservation-id is consumed by :wp/notify-shipping but produced by :wp/reserve-stock → internal
      ;; :warehouse/sku is consumed by :wp/fetch-inventory but produced by no producer → external
      (is (contains? (set (datalog/query-consumers-of db :warehouse/sku))
                     :wf/order-fulfillment))
      (is (not (contains? (set (datalog/query-consumers-of db :warehouse/inventory))
                          :wf/order-fulfillment)))
      (is (not (contains? (set (datalog/query-consumers-of db :warehouse/reservation-id))
                          :wf/order-fulfillment))))))

(deftest datalog-aspect-queries
  (seed-workflow-registry!)
  (let [db (datalog/create-db)]

    (testing "workflow entities discoverable by aspect"
      (let [workflows (set (datalog/query-entities-with-aspect db :atlas/workflow))
            producers (set (datalog/query-entities-with-aspect db :atlas/workflow-producer))]
        (is (contains? workflows :wf/order-fulfillment))
        (is (contains? producers :wp/fetch-inventory))
        (is (contains? producers :wp/reserve-stock))
        (is (contains? producers :wp/notify-shipping))))))

;; =============================================================================
;; INVARIANT TESTS
;; =============================================================================

(deftest invariants-pass-on-valid-workflow
  (seed-workflow-registry!)
  (let [result (inv/check-all)
        errors (->> (:violations result)
                    (filter #(= :error (:severity %)))
                    (filter #(re-find #"workflow" (str (:invariant %)))))]
    (testing "no workflow-related errors on valid data"
      (is (empty? errors)))))

(deftest invariant-signals-covered-warns-on-unhandled
  (seed-workflow-registry!)
  (let [result (inv/check-all)
        warnings (->> (:violations result)
                      (filter #(= :workflow-signals-covered (:invariant %))))]
    (testing "unhandled signals produce a warning"
      ;; :wp/reserve-stock declares :signal/reserve-failed but no transition for it
      ;; :wp/notify-shipping declares :signal/shipped and :signal/ship-failed but has no transitions entry
      (is (= 1 (count warnings)))
      (is (= :warning (:severity (first warnings)))))))

(deftest invariant-initial-not-in-producers
  (seed-workflow-registry!)
  (registry/register!
   :wf/bad-initial
   :atlas/workflow
   #{:domain/test :workflow/bad-initial}
   {:workflow/producers #{:wp/fetch-inventory}
    :workflow/initial-producer :wp/reserve-stock
    :workflow/transitions {:wp/fetch-inventory {:signal/in-stock :wp/fetch-inventory}}})

  (let [result (inv/check-all)
        violations (->> (:violations result)
                        (filter #(= :workflow-initial-in-producers (:invariant %))))]
    (testing "initial-producer outside producers set is an error"
      (is (= 1 (count violations)))
      (is (= :error (:severity (first violations)))))))

(deftest invariant-transition-target-not-in-producers
  (seed-workflow-registry!)
  (registry/register!
   :wf/bad-transition
   :atlas/workflow
   #{:domain/test :workflow/bad-transition}
   {:workflow/producers #{:wp/fetch-inventory}
    :workflow/initial-producer :wp/fetch-inventory
    :workflow/transitions {:wp/fetch-inventory {:signal/in-stock :wp/reserve-stock}}})

  (let [result (inv/check-all)
        violations (->> (:violations result)
                        (filter #(= :workflow-transition-targets-in-producers (:invariant %))))]
    (testing "transition target outside producers set is an error"
      (is (= 1 (count violations)))
      (is (= :error (:severity (first violations)))))))

(deftest invariant-transition-source-not-in-producers
  (seed-workflow-registry!)
  (registry/register!
   :wf/bad-source
   :atlas/workflow
   #{:domain/test :workflow/bad-source}
   {:workflow/producers #{:wp/fetch-inventory}
    :workflow/initial-producer :wp/fetch-inventory
    :workflow/transitions {:wp/fetch-inventory {:signal/in-stock :wp/fetch-inventory}
                           :wp/reserve-stock   {:signal/reserved :wp/fetch-inventory}}})

  (let [result (inv/check-all)
        violations (->> (:violations result)
                        (filter #(= :workflow-transition-sources-in-producers (:invariant %))))]
    (testing "transition source outside producers set is an error"
      (is (= 1 (count violations)))
      (is (= :error (:severity (first violations)))))))

(deftest invariant-producer-ef-must-exist
  (registry/register!
   :wp/orphan
   :atlas/workflow-producer
   #{:domain/test :workflow/orphan}
   {:workflow-producer/execution-function :fn/does-not-exist
    :workflow-producer/signals #{:signal/ok}
    :workflow-producer/output []})

  (let [result (inv/check-all)
        violations (->> (:violations result)
                        (filter #(= :workflow-producer-ef-exists (:invariant %))))]
    (testing "referencing non-existent execution-function is an error"
      (is (= 1 (count violations)))
      (is (= :error (:severity (first violations)))))))

(deftest invariant-producer-ef-must-return-signal
  (registry/register!
   :fn/no-signal
   :atlas/execution-function
   #{:domain/test :operation/test :tier/service}
   {:execution-function/context [:test/input]
    :execution-function/response [:test/output]})

  (registry/register!
   :wp/missing-signal-response
   :atlas/workflow-producer
   #{:domain/test :workflow/missing-signal}
   {:workflow-producer/execution-function :fn/no-signal
    :workflow-producer/signals #{:signal/ok}
    :workflow-producer/output []})

  (let [result (inv/check-all)
        violations (->> (:violations result)
                        (filter #(= :workflow-producer-ef-has-signal-response (:invariant %))))]
    (testing "execution-function missing :workflow/signal response is an error"
      (is (= 1 (count violations)))
      (is (= :error (:severity (first violations))))
      (let [detail (first (:details (first violations)))]
        (is (contains? (:missing-response-keys detail) :workflow/signal))
        (is (contains? (:missing-response-keys detail) :async/seconds-to-delay))))))

(deftest same-producer-different-workflows
  (seed-workflow-registry!)
  ;; Reuse :wp/fetch-inventory in a different workflow with different routing
  (registry/register!
   :wf/quick-check
   :atlas/workflow
   #{:domain/warehouse :workflow/quick-check}
   {:workflow/producers #{:wp/fetch-inventory :wp/notify-shipping}
    :workflow/initial-producer :wp/fetch-inventory
    :workflow/transitions {:wp/fetch-inventory {:signal/in-stock :wp/notify-shipping
                                                :signal/out-of-stock :wp/notify-shipping}}})

  (let [db (datalog/create-db)]
    (testing "both workflows exist and share a producer"
      (let [workflows (set (datalog/query-entities-with-aspect db :atlas/workflow))]
        (is (contains? workflows :wf/order-fulfillment))
        (is (contains? workflows :wf/quick-check))))

    (testing "both workflows depend on the shared producer"
      (let [deps-fulfillment (set (datalog/query-dependencies db :wf/order-fulfillment))
            deps-quick (set (datalog/query-dependencies db :wf/quick-check))]
        (is (contains? deps-fulfillment :wp/fetch-inventory))
        (is (contains? deps-quick :wp/fetch-inventory))))))
