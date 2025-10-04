(ns app.pet-shop-test
  "Comprehensive test suite and utilities for the pet-shop application.

   This file consolidates:
   - Mock data and implementations (formerly pet_shop_core.clj)
   - Query demonstrations (formerly pet_shop_queries.clj)
   - Test cases for the pet-shop application

   Structure:
   1. Mock Data
   2. Mock Implementations
   3. Runtime (wiring)
   4. Endpoint Handlers
   5. Query Demonstrations
   6. Tests"
  (:require
   [clojure.test :refer [deftest testing is use-fixtures run-tests]]
   [clojure.pprint :as pp]
   [atlas.registry :as cid]
   [atlas.entity :as rt]
   [atlas.datalog :as ax.dl]
   [atlas.invariant.unified :as ax]
   [app.pet-shop :as sut]
   [app.pet-shop-invariants :as dsl-invariants]
   [app.test-utils :as test-utils]
   [clojure.set :as set]))


;; =============================================================================
;; MOCK DATA
;; =============================================================================

(def mock-db
  "In-memory mock database."
  (atom {:pets {"pet-1" {:pet/id "pet-1"
                         :pet/name "Max"
                         :pet/species :dog
                         :pet/breed "Golden Retriever"
                         :pet/age 2
                         :pet/price 450.00
                         :pet/status :available}
                "pet-2" {:pet/id "pet-2"
                         :pet/name "Whiskers"
                         :pet/species :cat
                         :pet/breed "Persian"
                         :pet/age 3
                         :pet/price 300.00
                         :pet/status :available}}

         :customers {"cust-1" {:customer/id "cust-1"
                               :customer/email "john@example.com"
                               :customer/name "John Doe"
                               :customer/phone "555-1234"
                               :customer/loyalty-points 50}}

         :orders {}

         :appointments {}}))

(def mock-grooming-slots
  [{:slot/datetime #inst "2024-01-20T09:00" :slot/available true}
   {:slot/datetime #inst "2024-01-20T10:00" :slot/available true}
   {:slot/datetime #inst "2024-01-20T11:00" :slot/available false}
   {:slot/datetime #inst "2024-01-20T14:00" :slot/available true}])


;; =============================================================================
;; MOCK IMPLEMENTATIONS
;; =============================================================================

(defn- gen-id [prefix]
  (str prefix "-" (java.util.UUID/randomUUID)))

;; Component mocks
(def component-impls
  {:component/db
   {:query   (fn [table id] (get-in @mock-db [table id]))
    :insert! (fn [table id data] (swap! mock-db assoc-in [table id] data) data)
    :update! (fn [table id data] (swap! mock-db update-in [table id] merge data) data)
    :list    (fn [table pred] (vec (filter pred (vals (get @mock-db table)))))}

   :component/cache
   {:get  (fn [k] nil)
    :set! (fn [k v] nil)}

   :component/stripe
   {:charge! (fn [token amount]
               (if (= token "tok_fail")
                 {:success false :error "declined"}
                 {:success true :charge-id (gen-id "ch")}))}

   :component/twilio
   {:send-sms! (fn [phone msg]
                 {:success true :sid (gen-id "sms")})}

   :component/email-service
   {:send-email! (fn [to subject body]
                   {:success true :id (gen-id "email")})}})

;; Function implementations
(def function-impls
  {;; -------------------------
   ;; Inventory
   ;; -------------------------
   :fn/list-available-pets
   (fn [{:keys [pet/species pet/breed]} {:keys [db]}]
     (let [pets ((:list db) :pets
                            (fn [p]
                              (and (= :available (:pet/status p))
                                   (or (nil? species) (= species (:pet/species p)))
                                   (or (nil? breed) (= breed (:pet/breed p))))))]
       {:inventory/available-pets pets}))

   :fn/get-pet-details
   (fn [{:keys [pet/id]} {:keys [db]}]
     (when-let [pet ((:query db) :pets id)]
       (select-keys pet [:pet/name :pet/species :pet/breed :pet/age :pet/price :pet/status])))

   :fn/reserve-pet
   (fn [ctx {:keys [db]}]
     (let [pet-id (:pet/id ctx)
           customer-id (:customer/id ctx)]
       ((:update! db) :pets pet-id {:pet/status :reserved :reserved-by customer-id})
       {:pet/status :reserved}))

   :fn/release-reservation
   (fn [{:keys [pet/id]} {:keys [db]}]
     ((:update! db) :pets id {:pet/status :available :reserved-by nil})
     {:pet/status :available})

   :fn/validate-pet-availability
   (fn [{:keys [pet/status]} _]
     {:pet/status status
      :valid? (= status :available)})

   ;; -------------------------
   ;; Customers
   ;; -------------------------
   :fn/get-customer
   (fn [{:keys [customer/id]} {:keys [db]}]
     (when-let [cust ((:query db) :customers id)]
       (select-keys cust [:customer/email :customer/name :customer/phone :customer/loyalty-points])))

   :fn/create-customer
   (fn [{:keys [customer/email customer/name customer/phone]} {:keys [db]}]
     (let [id (gen-id "cust")]
       ((:insert! db) :customers id
        {:customer/id id
         :customer/email email
         :customer/name name
         :customer/phone phone
         :customer/loyalty-points 0})
       {:customer/id id}))

   :fn/update-loyalty-points
   (fn [{:keys [customer/id order/total]} {:keys [db]}]
     (let [points-earned (quot (long total) 10)
           current (or (:customer/loyalty-points ((:query db) :customers id)) 0)
           new-points (+ current points-earned)]
       ((:update! db) :customers id {:customer/loyalty-points new-points})
       {:customer/loyalty-points new-points}))

   ;; -------------------------
   ;; Orders
   ;; -------------------------
   :fn/calculate-order-total
   (fn [{:keys [order/items customer/loyalty-points]} _]
     (let [base-total (reduce + (map #(:pet/price % 0) items))
           discount (min (* (or loyalty-points 0) 0.1) (* base-total 0.1))
           final (- base-total discount)]
       {:order/total final}))

   :fn/create-order
   (fn [{:keys [customer/id order/items]} {:keys [db]}]
     (let [order-id (gen-id "order")
           total (reduce + (map (fn [item]
                                  (or (:pet/price item)
                                      (:pet/price ((:query db) :pets (:pet/id item)))
                                      0))
                                items))]
       ((:insert! db) :orders order-id
        {:order/id order-id
         :order/customer-id id
         :order/items items
         :order/total total
         :order/status :pending
         :order/created-at (java.util.Date.)})
       {:order/id order-id :order/total total :order/status :pending}))

   :fn/process-payment
   (fn [{:keys [order/id order/total payment/token]} {:keys [stripe]}]
     (let [result ((:charge! stripe) token total)]
       (if (:success result)
         {:payment/result :success :order/status :paid}
         {:payment/result :declined :order/status :pending})))

   :fn/complete-order
   (fn [{:keys [order/id payment/result]} {:keys [db]}]
     (if (= result :success)
       (do
         ((:update! db) :orders id {:order/status :fulfilled})
         {:order/status :fulfilled})
       {:order/status :failed}))

   :fn/cancel-order
   (fn [{:keys [order/id]} {:keys [db]}]
     ((:update! db) :orders id {:order/status :cancelled})
     {:order/status :cancelled})

   :fn/get-order-history
   (fn [{:keys [customer/id]} {:keys [db]}]
     (let [orders ((:list db) :orders #(= id (:order/customer-id %)))]
       (vec (map #(select-keys % [:order/id :order/items :order/total :order/status :order/created-at])
                 orders))))

   ;; -------------------------
   ;; Grooming
   ;; -------------------------
   :fn/get-grooming-slots
   (fn [_ _]
     {:grooming/slots (vec (filter :slot/available mock-grooming-slots))})

   :fn/book-appointment
   (fn [{:keys [customer/id appointment/pet-id appointment/datetime appointment/service]} {:keys [db]}]
     (let [apt-id (gen-id "apt")]
       ((:insert! db) :appointments apt-id
        {:appointment/id apt-id
         :appointment/customer-id id
         :appointment/pet-id pet-id
         :appointment/datetime datetime
         :appointment/service service
         :appointment/status :scheduled})
       {:appointment/id apt-id :appointment/status :scheduled}))

   :fn/cancel-appointment
   (fn [{:keys [appointment/id]} {:keys [db]}]
     ((:update! db) :appointments id {:appointment/status :cancelled})
     {:appointment/status :cancelled})

   ;; -------------------------
   ;; Notifications
   ;; -------------------------
   :fn/send-order-confirmation
   (fn [{:keys [customer/email order/id order/items order/total]} {:keys [email-service]}]
     (let [result ((:send-email! email-service)
                   email
                   (str "Order Confirmation #" id)
                   (str "Total: $" total))]
       {:notification/sent? (:success result)}))

   :fn/send-appointment-reminder
   (fn [{:keys [customer/phone appointment/datetime appointment/service]} {:keys [twilio]}]
     (let [result ((:send-sms! twilio)
                   phone
                   (str "Reminder: " (name service) " on " datetime))]
       {:notification/sent? (:success result)}))})


;; =============================================================================
;; RUNTIME
;; =============================================================================

(def invoke (test-utils/make-invoke function-impls component-impls))

(defn requires-auth?
  "Check if endpoint requires authorization using framework."
  [endpoint-id]
  (rt/has-aspect? endpoint-id :authorization/required))


;; =============================================================================
;; ENDPOINT HANDLERS
;; =============================================================================

(defmulti call-endpoint
  "Call an endpoint with request context."
  (fn [endpoint-id _ctx] endpoint-id))

(defmethod call-endpoint :default [endpoint-id ctx]
  (when (and (requires-auth? endpoint-id)
             (not (:auth/user-id ctx)))
    (throw (ex-info "Unauthorized" {:endpoint endpoint-id})))

  ;; Simple endpoints: invoke first dep
  (let [deps (rt/deps-for endpoint-id)
        fn-dep (first (filter #(rt/has-aspect? % :atlas/execution-function) deps))]
    (if fn-dep
      (invoke fn-dep ctx)
      (throw (ex-info "No function dep" {:endpoint endpoint-id})))))

(defmethod call-endpoint :endpoint/list-pets [_ ctx]
  (invoke :fn/list-available-pets ctx))

(defmethod call-endpoint :endpoint/get-pet [_ ctx]
  (invoke :fn/get-pet-details ctx))

(defmethod call-endpoint :endpoint/signup [_ ctx]
  (invoke :fn/create-customer ctx))

(defmethod call-endpoint :endpoint/order-history [_ ctx]
  (when-not (:auth/user-id ctx)
    (throw (ex-info "Unauthorized" {:endpoint :endpoint/order-history})))
  (invoke :fn/get-order-history {:customer/id (:auth/user-id ctx)}))

(defmethod call-endpoint :endpoint/grooming-availability [_ ctx]
  (invoke :fn/get-grooming-slots ctx))

(defmethod call-endpoint :endpoint/purchase-pet [_ ctx]
  (when-not (:auth/user-id ctx)
    (throw (ex-info "Unauthorized" {:endpoint :endpoint/purchase-pet})))

  (let [user-id (:auth/user-id ctx)
        pet-id (:pet/id ctx)
        payment-token (:payment/token ctx)

        ;; 1. Get customer
        customer (invoke :fn/get-customer {:customer/id user-id})
        _ (when-not customer
            (throw (ex-info "Customer not found" {:customer/id user-id})))

        ;; 2. Get pet
        pet (invoke :fn/get-pet-details {:pet/id pet-id})
        _ (when-not pet
            (throw (ex-info "Pet not found" {:pet/id pet-id})))

        ;; 3. Validate availability
        validation (invoke :fn/validate-pet-availability {:pet/status (:pet/status pet)})
        _ (when-not (:valid? validation)
            (throw (ex-info "Pet not available" {:pet/status (:pet/status pet)})))

        ;; 4. Calculate total
        total-result (invoke :fn/calculate-order-total
                             {:order/items [{:pet/id pet-id :pet/price (:pet/price pet)}]
                              :customer/loyalty-points (:customer/loyalty-points customer)})

        ;; 5. Create order
        order (invoke :fn/create-order
                      {:customer/id user-id
                       :order/items [{:pet/id pet-id}]})

        ;; 6. Reserve pet
        _ (invoke :fn/reserve-pet {:pet/id pet-id :customer/id user-id})

        ;; 7. Process payment
        payment (invoke :fn/process-payment
                        {:order/id (:order/id order)
                         :order/total (:order/total order)
                         :payment/token payment-token})

        ;; 8. Complete order
        completion (invoke :fn/complete-order
                           {:order/id (:order/id order)
                            :payment/result (:payment/result payment)})

        ;; 9. Send confirmation
        _ (invoke :fn/send-order-confirmation
                  {:customer/email (:customer/email customer)
                   :order/id (:order/id order)
                   :order/items (:order/items order)
                   :order/total (:order/total order)})]

    {:order/id (:order/id order)
     :order/status (:order/status completion)}))

(defmethod call-endpoint :endpoint/cancel-order [_ ctx]
  (when-not (:auth/user-id ctx)
    (throw (ex-info "Unauthorized" {:endpoint :endpoint/cancel-order})))
  (invoke :fn/cancel-order {:order/id (:order/id ctx)}))

(defmethod call-endpoint :endpoint/book-grooming [_ ctx]
  (when-not (:auth/user-id ctx)
    (throw (ex-info "Unauthorized" {:endpoint :endpoint/book-grooming})))

  (let [customer (invoke :fn/get-customer {:customer/id (:auth/user-id ctx)})
        booking (invoke :fn/book-appointment
                        {:customer/id (:auth/user-id ctx)
                         :appointment/pet-id (:appointment/pet-id ctx)
                         :appointment/datetime (:appointment/datetime ctx)
                         :appointment/service (:appointment/service ctx)})]
    ;; Send reminder
    (invoke :fn/send-appointment-reminder
            {:customer/phone (:customer/phone customer)
             :appointment/datetime (:appointment/datetime ctx)
             :appointment/service (:appointment/service ctx)})
    booking))

(defmethod call-endpoint :endpoint/cancel-grooming [_ ctx]
  (when-not (:auth/user-id ctx)
    (throw (ex-info "Unauthorized" {:endpoint :endpoint/cancel-grooming})))
  (invoke :fn/cancel-appointment {:appointment/id (:appointment/id ctx)}))


;; =============================================================================
;; QUERY DEMONSTRATIONS
;; =============================================================================

(defn demo-data-flow
  "Demonstrate data flow tracing for critical keys.
   Shows which entities produce and consume specific data."
  []
  (println "\n=== DATA FLOW ANALYSIS ===\n")

  (let [db (ax.dl/create-db)
        critical-keys [:payment/token :payment/result :pet/status
                       :order/status :customer/loyalty-points]]

    (doseq [k critical-keys]
      (let [{:keys [producers consumers]} (ax.dl/query-data-flow db k)]
        (println (str k ":"))
        (println "  Producers:" (or (seq producers) "EXTERNAL INPUT"))
        (println "  Consumers:" (or (seq consumers) "TERMINAL OUTPUT"))
        (println)))))

(defn demo-dependency-chains
  "Demonstrate transitive dependency analysis.
   Shows the complete dependency tree for complex endpoints."
  []
  (println "\n=== DEPENDENCY CHAIN ANALYSIS ===\n")

  (let [db (ax.dl/create-db)
        endpoints [:endpoint/purchase-pet :endpoint/book-grooming]]

    (doseq [ep endpoints]
      (let [deps (ax.dl/query-dependency-chain db ep)]
        (println (str ep ":"))
        (println "  Direct + Transitive deps (" (count deps) " total):")
        (doseq [d (sort deps)]
          (println "    -" d))
        (println)))))

(defn demo-reverse-dependencies
  "Demonstrate reverse dependency lookup.
   Shows which higher-level entities depend on foundational components."
  []
  (println "\n=== REVERSE DEPENDENCY ANALYSIS ===\n")

  (let [db (ax.dl/create-db)
        components [:component/db :component/stripe :component/twilio]]

    (doseq [comp components]
      (let [dependents (ax.dl/query-reverse-dependencies db comp)]
        (println (str comp " is used by:"))
        (if (seq dependents)
          (doseq [d (sort dependents)]
            (println "  -" d))
          (println "  (no direct dependents)"))
        (println)))))

(defn demo-aspect-frequency
  "Demonstrate aspect usage analysis.
   Reveals the semantic distribution across the system."
  []
  (println "\n=== ASPECT FREQUENCY ANALYSIS ===\n")

  (let [db (ax.dl/create-db)
        freqs (sort-by second > (ax.dl/query-aspect-frequency db))]

    (println "Top aspects by usage:\n")
    (doseq [[aspect count] (take 20 freqs)]
      (println (format "  %-45s %3d" (str aspect) count)))))

(defn demo-multi-aspect-queries
  "Demonstrate complex multi-aspect queries.
   Shows how to find entities by semantic intersection."
  []
  (println "\n=== MULTI-ASPECT QUERY ANALYSIS ===\n")

  (let [db (ax.dl/create-db)
        queries [
                 ;; External integrations that write data
                 {:aspects [:integration/external :effect/write]
                  :description "External write operations"}

                 ;; Authenticated endpoints in orders domain
                 {:aspects [:atlas/interface-endpoint :authorization/required :domain/orders]
                  :description "Authenticated order endpoints"}

                 ;; Pure functions (no side effects)
                 {:aspects [:atlas/execution-function :effect/pure]
                  :description "Pure functions"}

                 ;; Email notifications
                 {:aspects [:semantic-namespace/notification :notification.channel/email]
                  :description "Email notifications"}

                 ;; SMS notifications
                 {:aspects [:semantic-namespace/notification :notification.channel/sms]
                  :description "SMS notifications"}]]

    (doseq [{:keys [aspects description]} queries]
      (let [results (ax.dl/query-entities-with-aspect db (last aspects))]
        (println description "(" (count results) "found):")
        (doseq [r results]
          (println "  -" r))
        (println)))))

(defn demo-architectural-insights
  "Demonstrate higher-level architectural queries.
   Combines multiple queries to reveal system patterns."
  []
  (println "\n=== ARCHITECTURAL INSIGHTS ===\n")

  (let [db (ax.dl/create-db)]

    ;; 1. External dependencies risk analysis
    (println "External Integration Risk:")
    (let [external-fns (ax.dl/query-entities-with-aspect db :integration/external)
          external-deps (map #(hash-map
                                :entity %
                                :reverse-deps (count (ax.dl/query-reverse-dependencies db %)))
                             external-fns)
          sorted (sort-by :reverse-deps > external-deps)]
      (doseq [{:keys [entity reverse-deps]} sorted]
        (println (format "  %-40s used by %2d entities" (str entity) reverse-deps))))

    (println)

    ;; 2. Endpoint coverage analysis
    (println "Endpoint to Function Ratio:")
    (let [endpoints (count (ax.dl/query-entities-with-aspect db :atlas/interface-endpoint))
          functions (count (ax.dl/query-entities-with-aspect db :atlas/execution-function))]
      (println (format "  %d endpoints exposing %d functions (%.2f:1 ratio)"
                       endpoints functions (double (/ functions endpoints)))))

    (println)

    ;; 3. Domain complexity analysis
    (println "Domain Complexity (by function count):")
    (let [domains [:domain/inventory :domain/customers :domain/orders
                   :domain/grooming :domain/payments :domain/notifications]
          domain-fns (map #(hash-map
                             :domain %
                             :count (count (ax.dl/query-entities-with-aspect db %)))
                          domains)
          sorted (sort-by :count > domain-fns)]
      (doseq [{:keys [domain count]} sorted]
        (println (format "  %-30s %2d entities" (str domain) count))))

    (println)

    ;; 4. Unreachable functions warning
    (println "Unreachable Functions:")
    (let [unreachable (ax.dl/query-unreachable-functions db)]
      (if (seq unreachable)
        (doseq [fn unreachable]
          (println "  ⚠ " fn))
        (println "  ✓ All functions reachable from endpoints")))))

(defn demo-security-audit
  "Demonstrate security-focused queries.
   Identifies authorization boundaries and data protection."
  []
  (println "\n=== SECURITY AUDIT ===\n")

  (let [db (ax.dl/create-db)]

    ;; 1. Public endpoints
    (println "Public Endpoints (no auth required):")
    (let [all-endpoints (ax.dl/query-entities-with-aspect db :atlas/interface-endpoint)
          authed (set (ax.dl/query-entities-with-aspect db :authorization/required))
          public (remove authed all-endpoints)]
      (doseq [ep (sort public)]
        (println "  -" ep)))

    (println)

    ;; 2. Payment handling
    (println "Payment Processing Chain:")
    (let [payment-fns (ax.dl/query-entities-with-aspect db :domain/payments)]
      (doseq [pf payment-fns]
        (let [deps (ax.dl/query-dependency-chain db pf)
              rev-deps (ax.dl/query-reverse-dependencies db pf)]
          (println (str "  " pf ":"))
          (println "    Dependencies:" (count deps))
          (println "    Used by:" (vec rev-deps)))))

    (println)

    ;; 3. External communication channels
    (println "External Communication Channels:")
    (let [external (ax.dl/query-entities-with-aspect db :integration/external)]
      (doseq [ext (sort external)]
        (let [users (ax.dl/query-reverse-dependencies db ext)]
          (println (format "  %-35s → used by %d entities" (str ext) (count users))))))))

(defn demo-impact-analysis
  "Demonstrate impact analysis for changes.
   Shows what would be affected if a component changes."
  [entity-id]
  (println (format "\n=== IMPACT ANALYSIS: %s ===\n" entity-id))

  (let [db (ax.dl/create-db)]

    ;; Direct dependents
    (println "Direct Impact (entities that depend on this):")
    (let [direct (ax.dl/query-reverse-dependencies db entity-id)]
      (if (seq direct)
        (doseq [d (sort direct)]
          (println "  -" d))
        (println "  (none)")))

    (println)

    ;; Transitive impact (find all entities that transitively depend)
    (println "Transitive Impact (all affected entities):")
    (let [find-all-dependents
          (fn find-deps [id visited]
            (let [direct (ax.dl/query-reverse-dependencies db id)
                  new-deps (remove visited direct)
                  all-deps (reduce (fn [acc dep]
                                     (into acc (find-deps dep (conj visited dep))))
                                   (set new-deps)
                                   new-deps)]
              all-deps))
          all-affected (find-all-dependents entity-id #{})]
      (if (seq all-affected)
        (doseq [e (sort all-affected)]
          (println "  -" e))
        (println "  (none)")))

    (println)

    ;; What this entity depends on
    (println "This entity depends on:")
    (let [deps (ax.dl/query-dependencies db entity-id)]
      (if (seq deps)
        (doseq [d (sort deps)]
          (println "  -" d))
        (println "  (no dependencies)")))))

(defn run-all-demos
  "Execute all query demonstrations."
  []
  (println "\n")
  (println "╔════════════════════════════════════════════════════════════════╗")
  (println "║   ADVANCED DATALOG QUERIES - PET SHOP DEMONSTRATION           ║")
  (println "╚════════════════════════════════════════════════════════════════╝")

  ;; Initialize the pet shop registry
  (sut/init-registry!)

  ;; Run demos
  (demo-data-flow)
  (demo-dependency-chains)
  (demo-reverse-dependencies)
  (demo-aspect-frequency)
  (demo-multi-aspect-queries)
  (demo-architectural-insights)
  (demo-security-audit)

  ;; Interactive examples
  (println "\n=== INTERACTIVE IMPACT ANALYSIS EXAMPLES ===\n")
  (demo-impact-analysis :component/db)
  (demo-impact-analysis :fn/process-payment)

  (println "\n")
  (println "╔════════════════════════════════════════════════════════════════╗")
  (println "║   TRY IT YOURSELF                                              ║")
  (println "╚════════════════════════════════════════════════════════════════╝")
  (println)
  (println "Example queries to try:")
  (println "  (demo-impact-analysis :component/stripe)")
  (println "  (demo-impact-analysis :fn/reserve-pet)")
  (println "  (demo-impact-analysis :endpoint/purchase-pet)")
  (println))


;; =============================================================================
;; TEST UTILITIES
;; =============================================================================

(defn reset-mock-db! []
  (reset! mock-db
          {:pets {"pet-1" {:pet/id "pet-1"
                           :pet/name "Max"
                           :pet/species :dog
                           :pet/breed "Golden Retriever"
                           :pet/age 2
                           :pet/price 450.00
                           :pet/status :available}
                  "pet-2" {:pet/id "pet-2"
                           :pet/name "Whiskers"
                           :pet/species :cat
                           :pet/breed "Persian"
                           :pet/age 3
                           :pet/price 300.00
                           :pet/status :available}}
           :customers {"cust-1" {:customer/id "cust-1"
                                 :customer/email "john@example.com"
                                 :customer/name "John Doe"
                                 :customer/phone "555-1234"
                                 :customer/loyalty-points 50}}
           :orders {}
           :appointments {}}))

;; --- Test Fixture ---

(use-fixtures :each
  (test-utils/make-fixture-with-reset
    sut/init-registry!
    reset-mock-db!))




;; =============================================================================
;; DATAFLOW TESTS
;; =============================================================================

(deftest test-dataflow-customer-to-order
  (testing "Customer data flows correctly to order total calculation"
    (reset-mock-db!)
    (let [customer (invoke :fn/get-customer {:customer/id "cust-1"})
          total (invoke :fn/calculate-order-total
                        {:order/items [{:pet/price 100}]
                         :customer/loyalty-points (:customer/loyalty-points customer)})]
      (is (number? (:order/total total)))
      ;; With 50 points, discount = min(5, 10) = 5
      (is (= 95.0 (:order/total total))))))

(deftest test-dataflow-payment-to-completion
  (testing "Payment result flows to order completion"
    (reset-mock-db!)
    (let [payment-success (invoke :fn/process-payment
                                  {:order/id "test" :order/total 100 :payment/token "tok_valid"})
          completion (invoke :fn/complete-order
                             {:order/id "test" :payment/result (:payment/result payment-success)})]
      (is (= :success (:payment/result payment-success)))
      (is (= :fulfilled (:order/status completion))))

    (let [payment-fail (invoke :fn/process-payment
                               {:order/id "test" :order/total 100 :payment/token "tok_fail"})
          completion (invoke :fn/complete-order
                             {:order/id "test" :payment/result (:payment/result payment-fail)})]
      (is (= :declined (:payment/result payment-fail)))
      (is (= :failed (:order/status completion))))))

(deftest test-dataflow-reservation-lifecycle
  (testing "Pet reservation and release flow"
    (reset-mock-db!)
    (let [;; Reserve
          reserve-result (invoke :fn/reserve-pet {:pet/id "pet-1" :customer/id "cust-1"})
          pet-after-reserve (invoke :fn/get-pet-details {:pet/id "pet-1"})

          ;; Release
          release-result (invoke :fn/release-reservation {:pet/id "pet-1"})
          pet-after-release (invoke :fn/get-pet-details {:pet/id "pet-1"})]

      (is (= :reserved (:pet/status reserve-result)))
      (is (= :reserved (:pet/status pet-after-reserve)))
      (is (= :available (:pet/status release-result)))
      (is (= :available (:pet/status pet-after-release))))))


;; =============================================================================
;; AUTH BOUNDARY TESTS
;; =============================================================================

(deftest test-auth-boundaries
  (testing "Auth endpoints reject unauthenticated requests"
    (doseq [endpoint [:endpoint/purchase-pet
                      :endpoint/order-history
                      :endpoint/cancel-order
                      :endpoint/book-grooming
                      :endpoint/cancel-grooming]]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Unauthorized"
            (call-endpoint endpoint {}))
          (str endpoint " should require auth"))))

  (testing "Public endpoints allow unauthenticated requests"
    (reset-mock-db!)
    (is (map? (call-endpoint :endpoint/list-pets {})))
    (is (map? (call-endpoint :endpoint/get-pet {:pet/id "pet-1"})))
    (is (map? (call-endpoint :endpoint/grooming-availability {})))))


;; =============================================================================
;; ENDPOINT INTEGRATION TESTS
;; =============================================================================

(deftest test-endpoint-list-pets
  (testing "List pets returns only available pets"
    (reset-mock-db!)
    (let [result (call-endpoint :endpoint/list-pets {})]
      (is (contains? result :inventory/available-pets))
      (is (= 2 (count (:inventory/available-pets result))))
      (is (every? #(= :available (:pet/status %))
                  (:inventory/available-pets result))))))

(deftest test-endpoint-get-pet
  (testing "Get pet returns pet details"
    (reset-mock-db!)
    (let [result (call-endpoint :endpoint/get-pet {:pet/id "pet-1"})]
      (is (= "Max" (:pet/name result)))
      (is (= :dog (:pet/species result))))))

(deftest test-endpoint-signup
  (testing "Signup creates new customer"
    (reset-mock-db!)
    (let [result (call-endpoint :endpoint/signup
                                {:customer/email "new@test.com"
                                 :customer/name "New User"
                                 :customer/phone "555-0000"})]
      (is (string? (:customer/id result)))
      (is (get-in @mock-db [:customers (:customer/id result)])))))

(deftest test-endpoint-purchase-flow
  (testing "Full purchase flow succeeds"
    (reset-mock-db!)
    (let [result (call-endpoint :endpoint/purchase-pet
                                {:auth/user-id "cust-1"
                                 :pet/id "pet-1"
                                 :payment/token "tok_valid"})]
      (is (string? (:order/id result)))
      (is (= :fulfilled (:order/status result)))
      ;; Pet should be reserved
      (is (= :reserved (:pet/status (get-in @mock-db [:pets "pet-1"]))))))

  (testing "Purchase fails for unavailable pet"
    (reset-mock-db!)
    ;; Reserve pet first
    (invoke :fn/reserve-pet {:pet/id "pet-1" :customer/id "other"})
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not available"
          (call-endpoint :endpoint/purchase-pet
                         {:auth/user-id "cust-1"
                          :pet/id "pet-1"
                          :payment/token "tok_valid"})))))

(deftest test-endpoint-book-grooming
  (testing "Book grooming creates appointment"
    (reset-mock-db!)
    (let [result (call-endpoint :endpoint/book-grooming
                                {:auth/user-id "cust-1"
                                 :appointment/pet-id "pet-1"
                                 :appointment/datetime #inst "2024-02-01T10:00"
                                 :appointment/service :bath})]
      (is (string? (:appointment/id result)))
      (is (= :scheduled (:appointment/status result))))))


;; =============================================================================
;; DSL AXIOM VALIDATION TESTS
;; =============================================================================

(deftest test-dsl-invariants-payment
  (testing "Payment functions use Stripe component"
    (let [result (ax/check-all dsl-invariants/payment-functions-use-stripe)]
      (is (true? (:valid? result))
          (str "Payment invariant violations: " (:violations result)))))

  (testing "Payment operations are async"
    (let [result (ax/check-all dsl-invariants/payment-is-async)]
      ;; Warning-level, so valid? should be true but may have warnings
      (is (empty? (:errors result))
          "Should have no payment async errors")))

  (testing "All payment invariants pass"
    (let [result (dsl-invariants/check-payment)]
      (is (empty? (:errors result))
          (str "Payment invariant errors: " (:errors result))))))

(deftest test-dsl-invariants-authorization
  (testing "Order endpoints require authorization"
    (let [result (ax/check-all dsl-invariants/order-endpoints-require-auth)]
      (is (true? (:valid? result))
          "Order endpoints should require authorization")))

  (testing "Grooming booking requires authorization"
    (let [result (ax/check-all dsl-invariants/grooming-endpoints-require-auth)]
      (is (true? (:valid? result))
          "Grooming booking should require authorization")))

  (testing "All authorization invariants pass"
    (let [result (dsl-invariants/check-authorization)]
      (is (true? (:valid? result))
          (str "Authorization invariant violations: " (:errors result))))))

(deftest test-dsl-invariants-notifications
  (testing "Email notifications use email service"
    (let [result (ax/check-all dsl-invariants/email-notifications-use-email-service)]
      (is (true? (:valid? result))
          "Email notifications should use email service")))

  (testing "SMS notifications use Twilio"
    (let [result (ax/check-all dsl-invariants/sms-notifications-use-twilio)]
      (is (true? (:valid? result))
          "SMS notifications should use Twilio")))

  (testing "All notification invariants pass"
    (let [result (dsl-invariants/check-notifications)]
      (is (empty? (:errors result))
          (str "Notification invariant errors: " (:errors result))))))

(deftest test-dsl-invariants-domain
  (testing "Inventory writes use database"
    (let [result (ax/check-all dsl-invariants/inventory-writes-use-db)]
      (is (true? (:valid? result))
          "Inventory writes should use database")))

  (testing "Customer operations use database"
    (let [result (ax/check-all dsl-invariants/customer-ops-use-db)]
      (is (true? (:valid? result))
          "Customer operations should use database")))

  (testing "Grooming operations use database"
    (let [result (ax/check-all dsl-invariants/grooming-ops-use-db)]
      (is (true? (:valid? result))
          "Grooming operations should use database")))

  (testing "All domain invariants pass"
    (let [result (dsl-invariants/check-domain)]
      (is (true? (:valid? result))
          (str "Domain invariant violations: " (:errors result))))))

(deftest test-dsl-invariants-tiers
  (testing "Components are foundation tier"
    (let [result (ax/check-all dsl-invariants/components-are-foundation)]
      (is (true? (:valid? result))
          "All components should be foundation tier")))

  (testing "Endpoints are API tier"
    (let [result (ax/check-all dsl-invariants/endpoints-are-api)]
      (is (true? (:valid? result))
          "All endpoints should be API tier")))

  (testing "All tier invariants pass"
    (let [result (dsl-invariants/check-tiers)]
      (is (true? (:valid? result))
          (str "Tier violations: " (:errors result))))))

(deftest test-dsl-invariants-pure-functions
  (testing "Pure functions have no component dependencies"
    (let [result (ax/check-all dsl-invariants/pure-functions-no-component-deps)]
      (is (true? (:valid? result))
          "Pure functions should not depend on components"))))

(deftest test-dsl-invariants-architecture
  (testing "No dependency cycles"
    (let [result (ax/check-all dsl-invariants/no-dependency-cycles)]
      (is (true? (:valid? result))
          "Dependency graph should be acyclic")))

  (testing "All functions reachable from endpoints"
    (let [result (ax/check-all dsl-invariants/all-functions-reachable)]
      (is (empty? (:errors result))
          "Should have no error-level reachability issues"))))

(deftest test-dsl-invariants-all
  (testing "All DSL invariants pass"
    (let [result (dsl-invariants/check-all)]
      (is (empty? (:errors result))
          (str "DSL invariant errors: " (:errors result)))))

  (testing "DSL invariants can be documented"
    (let [docs (ax/document-all dsl-invariants/all-invariants)]
      (is (pos? (count docs)))
      (is (every? #(contains? % :id) docs))
      (is (every? #(= :dsl (:style %)) docs)))))


;; =============================================================================
;; DEMO / INTERACTIVE SECTION
;; =============================================================================

(comment
  ;; Run all demonstrations
  (run-all-demos)

  ;; Run individual demos
  (do (sut/init-registry!)
      (demo-data-flow))

  (do (sut/init-registry!)
      (demo-dependency-chains))

  (do (sut/init-registry!)
      (demo-impact-analysis :component/db))

  ;; Direct query examples
  (let [db (ax.dl/create-db)]
    ;; Find all payment-related entities
    (ax.dl/query-entities-with-aspect db :domain/payments))

  (let [db (ax.dl/create-db)]
    ;; Trace how customer email flows through the system
    (ax.dl/query-data-flow db :customer/email))

  (let [db (ax.dl/create-db)]
    ;; Find what depends on the database
    (ax.dl/query-reverse-dependencies db :component/db))

  (let [db (ax.dl/create-db)]
    ;; Get complete dependency tree for purchase endpoint
    (ax.dl/query-dependency-chain db :endpoint/purchase-pet))
  )

