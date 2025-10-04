(ns app.cart-test
  "Test suite for the shopping cart application."
  (:require
   [clojure.test :refer [deftest testing is use-fixtures]]
   [atlas.registry :as cid]
   [atlas.entity :as rt]
   [atlas.invariant.unified :as ax]
   [app.cart :as sut]
   [app.cart-invariants :as dsl-invariants]
   [app.test-utils :as test-utils]))


;; =============================================================================
;; MOCK DATA
;; =============================================================================

(def mock-products
  {":product/1" {:product/id ":product/1"
                 :product/name "Wireless Headphones"
                 :product/price 79.99M
                 :product/sizes []
                 :product/colors ["black" "silver" "blue"]}
   ":product/2" {:product/id ":product/2"
                 :product/name "Cotton T-Shirt"
                 :product/price 24.99M
                 :product/sizes ["XS" "S" "M" "L" "XL" "2XL"]
                 :product/colors ["white" "black" "navy" "red"]}})

(def mock-sessions
  (atom {":session/1" {:session/id ":session/1"
                       :session/data {:cart/id "cart-001"
                                      :cart/items []
                                      :cart/user-id nil
                                      :cart/shipping-address nil
                                      :cart/guest? true}}
         ":session/2" {:session/id ":session/2"
                       :session/data {:cart/id "cart-002"
                                      :cart/items []
                                      :cart/user-id "user-123"
                                      :cart/shipping-address nil
                                      :cart/guest? false}}
         ":session/3" {:session/id ":session/3"
                       :session/data {:cart/id "cart-003"
                                      :cart/items [{:item/product-id ":product/2"
                                                    :item/quantity 2
                                                    :item/price 24.99M
                                                    :item/size "M"
                                                    :item/color "black"}]
                                      :cart/user-id "user-456"
                                      :cart/shipping-address {:address/line1 "123 Main St"
                                                              :address/city "Portland"
                                                              :address/postal-code "97201"
                                                              :address/country "US"}
                                      :cart/guest? false}}}))


;; =============================================================================
;; MOCK IMPLEMENTATIONS
;; =============================================================================

(declare invoke)

(def component-impls
  {:component/db
   {:query (fn [type id] (get mock-products id))}

   :component/session-store
   {:get-session (fn [session-id]
                   (:session/data (get @mock-sessions session-id)))
    :save-session (fn [session-id session-data]
                    (swap! mock-sessions assoc-in [session-id :session/data] session-data)
                    {:session/id session-id})}})

(def function-impls
  {:fn/get-session
   (fn [{sid :session/id} {:keys [session-store]}]
     (let [session-data ((:get-session session-store) sid)]
       {:session/data session-data}))

   :fn/save-session
   (fn [{sid :session/id sdata :session/data} {:keys [session-store]}]
     ((:save-session session-store) sid sdata))

   :fn/get-product
   (fn [{pid :product/id} {:keys [db]}]
     (when-let [product ((:query db) :product pid)]
       (select-keys product [:product/id :product/name :product/price :product/sizes :product/colors])))

   :fn/get-cart
   (fn [{data :session/data} _]
     (if data
       (select-keys data [:cart/id :cart/items :cart/user-id :cart/shipping-address :cart/guest?])
       {:cart/id nil :cart/items [] :cart/user-id nil :cart/shipping-address nil :cart/guest? true}))

   :fn/add-to-cart
   (fn [{sdata :session/data pid :item/product-id qty :item/quantity sz :item/size clr :item/color} deps]
     ;; Pattern B: Function looks up product price
     (let [product-ctx {:product/id pid}
           product-result (invoke :fn/get-product product-ctx)
           price (:product/price product-result)
           new-item {:item/product-id pid :item/quantity qty :item/price price :item/size sz :item/color clr}
           existing-items (:cart/items sdata)
           filtered (remove #(and (= (:item/product-id %) pid) (= (:item/size %) sz) (= (:item/color %) clr)) existing-items)
           new-items (conj (vec filtered) new-item)]
       {:cart/items new-items :item/price price}))

   :fn/remove-from-cart
   (fn [{sdata :session/data pid :item/product-id sz :item/size clr :item/color} _]
     (let [existing-items (:cart/items sdata)
           new-items (vec (remove #(and (= (:item/product-id %) pid) (= (:item/size %) sz) (= (:item/color %) clr)) existing-items))]
       {:cart/items new-items}))

   :fn/update-quantity
   (fn [{qty :item/quantity pid :item/product-id sz :item/size clr :item/color sdata :session/data} _]
     (if (<= qty 0)
       (let [new-items (vec (remove #(and (= (:item/product-id %) pid) (= (:item/size %) sz) (= (:item/color %) clr)) (:cart/items sdata)))]
         {:cart/items new-items})
       (let [new-items (vec (map #(if (and (= (:item/product-id %) pid) (= (:item/size %) sz) (= (:item/color %) clr)) (assoc % :item/quantity qty) %) (:cart/items sdata)))]
         {:cart/items new-items})))

   :fn/clear-cart
   (fn [{data :session/data} _]
     {:cart/items []})

   :fn/merge-carts
   (fn [{src-sid :merge/source-session-id tgt-uid :merge/target-user-id} {:keys [session-store]}]
     ;; Sum-quantities merge strategy: duplicates have quantities summed
     (let [source-session ((:get-session session-store) src-sid)
           source-items (:cart/items source-session)
           ;; Group items by product/size/color and sum quantities
           merged-items (reduce (fn [acc item]
                                  (let [key [(:item/product-id item) (:item/size item) (:item/color item)]
                                        existing (first (filter #(= [(:item/product-id %) (:item/size %) (:item/color %)] key) acc))]
                                    (if existing
                                      (vec (map #(if (= [(:item/product-id %) (:item/size %) (:item/color %)] key)
                                                   (assoc % :item/quantity (+ (:item/quantity %) (:item/quantity item)))
                                                   %)
                                               acc))
                                      (conj acc item))))
                                []
                                source-items)]
       {:cart/items merged-items :cart/user-id tgt-uid}))

   :fn/update-shipping-address
   (fn [{sdata :session/data l1 :address/line1 l2 :address/line2 city :address/city pc :address/postal-code ctry :address/country} _]
     (let [address (cond-> {:address/line1 l1 :address/city city :address/postal-code pc :address/country ctry}
                     l2 (assoc :address/line2 l2))]
       {:cart/shipping-address address}))

   ;; Server-side cart calculation functions
   :fn/calculate-cart-subtotal
   (fn [{items :cart/items} _]
     (let [subtotal (reduce + (map #(:item/price % 0M) items))]
       {:cart/subtotal subtotal}))

   :fn/calculate-cart-tax
   (fn [{subtotal :cart/subtotal country :address/country} _]
     ;; Simple tax calculation: 10% for US, 20% for others
     (let [tax-rate (if (= country "US") 0.10M 0.20M)
           tax (bigdec (* subtotal tax-rate))]
       {:cart/tax tax}))

   :fn/calculate-cart-total
   (fn [{subtotal :cart/subtotal tax :cart/tax} _]
     (let [total (+ subtotal tax)]
       {:cart/total total}))})


;; =============================================================================
;; RUNTIME HELPERS
;; =============================================================================

(def invoke (test-utils/make-invoke function-impls component-impls))


;; =============================================================================
;; TEST UTILITIES
;; =============================================================================

(defn reset-mock-sessions! []
  (reset! mock-sessions
          {":session/1" {:session/id ":session/1"
                         :session/data {:cart/id "cart-001"
                                        :cart/items []
                                        :cart/user-id nil
                                        :cart/shipping-address nil
                                        :cart/guest? true}}
           ":session/2" {:session/id ":session/2"
                         :session/data {:cart/id "cart-002"
                                        :cart/items []
                                        :cart/user-id "user-123"
                                        :cart/shipping-address nil
                                        :cart/guest? false}}
           ":session/3" {:session/id ":session/3"
                         :session/data {:cart/id "cart-003"
                                        :cart/items [{:item/product-id ":product/2"
                                                      :item/quantity 2
                                                      :item/price 24.99M
                                                      :item/size "M"
                                                      :item/color "black"}]
                                        :cart/user-id "user-456"
                                        :cart/shipping-address {:address/line1 "123 Main St"
                                                                :address/city "Portland"
                                                                :address/postal-code "97201"
                                                                :address/country "US"}
                                        :cart/guest? false}}}))

(use-fixtures :each
  (test-utils/make-fixture-with-reset
    sut/init-registry!
    reset-mock-sessions!))




;; =============================================================================
;; SESSION MANAGEMENT TESTS
;; =============================================================================

(deftest test-get-session
  (testing "Can retrieve session data"
    (reset-mock-sessions!)
    (let [result (invoke :fn/get-session {:session/id ":session/1"})]
      (is (contains? result :session/data))
      (is (map? (:session/data result)))
      (is (= "cart-001" (:cart/id (:session/data result)))))))

(deftest test-save-session
  (testing "Can save session data"
    (reset-mock-sessions!)
    (let [new-data {:cart/id "cart-new" :cart/items [] :cart/user-id "new-user" :cart/shipping-address nil :cart/guest? false}
          result (invoke :fn/save-session {:session/id ":session/1" :session/data new-data})
          saved (invoke :fn/get-session {:session/id ":session/1"})]
      (is (= ":session/1" (:session/id result)))
      (is (= "cart-new" (:cart/id (:session/data saved)))))))


;; =============================================================================
;; PRODUCT LOOKUP TESTS
;; =============================================================================

(deftest test-get-product
  (testing "Can retrieve product details"
    (let [result (invoke :fn/get-product {:product/id ":product/1"})]
      (is (= ":product/1" (:product/id result)))
      (is (= "Wireless Headphones" (:product/name result)))
      (is (= 79.99M (:product/price result)))
      (is (= ["black" "silver" "blue"] (:product/colors result)))))

  (testing "Returns nil for non-existent product"
    (let [result (invoke :fn/get-product {:product/id ":product/999"})]
      (is (nil? result)))))


;; =============================================================================
;; CART OPERATION TESTS
;; =============================================================================

(deftest test-get-cart
  (testing "Can retrieve cart from session"
    (reset-mock-sessions!)
    (let [session-data ((:get-session (get component-impls :component/session-store)) ":session/2")
          result (invoke :fn/get-cart {:session/data session-data})]
      (is (= "cart-002" (:cart/id result)))
      (is (= "user-123" (:cart/user-id result)))
      (is (false? (:cart/guest? result)))))

  (testing "Returns default empty cart for nil session"
    (let [result (invoke :fn/get-cart {:session/data nil})]
      (is (nil? (:cart/id result)))
      (is (empty? (:cart/items result)))
      (is (true? (:cart/guest? result))))))

(deftest test-add-to-cart
  (testing "Can add item to cart"
    (reset-mock-sessions!)
    (let [session-data ((:get-session (get component-impls :component/session-store)) ":session/1")
          result (invoke :fn/add-to-cart {:session/data session-data :item/product-id ":product/1" :item/quantity 1 :item/size nil :item/color "black" :product/price 79.99M})]
      (is (= 1 (count (:cart/items result))))
      (is (= ":product/1" (:item/product-id (first (:cart/items result))))))))

(deftest test-remove-from-cart
  (testing "Can remove item from cart"
    (reset-mock-sessions!)
    (let [session-data ((:get-session (get component-impls :component/session-store)) ":session/3")
          result (invoke :fn/remove-from-cart {:session/data session-data :item/product-id ":product/2" :item/size "M" :item/color "black"})]
      (is (empty? (:cart/items result))))))

(deftest test-update-quantity
  (testing "Can update item quantity"
    (reset-mock-sessions!)
    (let [session-data ((:get-session (get component-impls :component/session-store)) ":session/3")
          result (invoke :fn/update-quantity {:session/data session-data :item/product-id ":product/2" :item/size "M" :item/color "black" :item/quantity 5})]
      (is (= 1 (count (:cart/items result))))
      (is (= 5 (:item/quantity (first (:cart/items result))))))))

(deftest test-clear-cart
  (testing "Can clear all items from cart"
    (reset-mock-sessions!)
    (let [session-data ((:get-session (get component-impls :component/session-store)) ":session/3")
          result (invoke :fn/clear-cart {:session/data session-data})]
      (is (empty? (:cart/items result))))))

(deftest test-merge-carts
  (testing "Can merge carts from source to target user"
    (reset-mock-sessions!)
    (let [result (invoke :fn/merge-carts {:merge/source-session-id ":session/3" :merge/target-user-id "user-789"})]
      (is (= 1 (count (:cart/items result))))
      (is (= "user-789" (:cart/user-id result))))))


;; =============================================================================
;; SHIPPING ADDRESS TESTS
;; =============================================================================

(deftest test-update-shipping-address
  (testing "Can set complete shipping address"
    (reset-mock-sessions!)
    (let [session-data ((:get-session (get component-impls :component/session-store)) ":session/1")
          result (invoke :fn/update-shipping-address {:session/data session-data :address/line1 "456 Oak Ave" :address/line2 "Apt 3B" :address/city "Seattle" :address/postal-code "98101" :address/country "US"})]
      (is (= "456 Oak Ave" (:address/line1 (:cart/shipping-address result))))
      (is (= "Apt 3B" (:address/line2 (:cart/shipping-address result))))
      (is (= "Seattle" (:address/city (:cart/shipping-address result)))))))




;; =============================================================================
;; SEMANTIC PATTERN TESTS
;; =============================================================================

(deftest test-add-to-cart-fetches-product-price
  (testing "Add-to-cart successfully fetches and includes product price"
    (reset-mock-sessions!)
    (let [session-data ((:get-session (get component-impls :component/session-store)) ":session/1")
          result (invoke :fn/add-to-cart {:session/data session-data :item/product-id ":product/2" :item/quantity 1 :item/size "M" :item/color "black"})]
      (is (= 24.99M (:item/price result)))
      (is (= 24.99M (:item/price (first (:cart/items result))))))))

(deftest test-cart-calculations
  (testing "Subtotal calculation sums item prices"
    (reset-mock-sessions!)
    (let [items [{:item/price 100M} {:item/price 50M}]
          result (invoke :fn/calculate-cart-subtotal {:cart/items items})]
      (is (= 150M (:cart/subtotal result)))))

  (testing "Tax calculation applies correct rate"
    (testing "US tax rate (10%)"
      (let [result (invoke :fn/calculate-cart-tax {:cart/subtotal 100M :address/country "US"})]
        (is (= 10M (:cart/tax result)))))

    (testing "Non-US tax rate (20%)"
      (let [result (invoke :fn/calculate-cart-tax {:cart/subtotal 100M :address/country "CA"})]
        (is (= 20M (:cart/tax result))))))

  (testing "Total calculation adds subtotal and tax"
    (let [result (invoke :fn/calculate-cart-total {:cart/subtotal 100M :cart/tax 10M})]
      (is (= 110M (:cart/total result))))))

(deftest test-merge-carts-sums-quantities
  (testing "Merge-carts sums quantities for duplicate items"
    (reset-mock-sessions!)
    ;; Session 3 has one item with qty 2
    ;; Add another with same product/size/color to session 3
    (swap! mock-sessions assoc-in [":session/3" :session/data :cart/items]
           [{:item/product-id ":product/2" :item/quantity 2 :item/price 24.99M :item/size "M" :item/color "black"}
            {:item/product-id ":product/2" :item/quantity 3 :item/price 24.99M :item/size "M" :item/color "black"}])

    (let [result (invoke :fn/merge-carts {:merge/source-session-id ":session/3" :merge/target-user-id "user-new"})]
      ;; Should have 1 item with summed quantity of 5
      (is (= 1 (count (:cart/items result))))
      (is (= 5 (:item/quantity (first (:cart/items result)))))
      (is (= "user-new" (:cart/user-id result))))))


;; =============================================================================
;; DSL AXIOM VALIDATION TESTS
;; =============================================================================

(deftest test-dsl-invariants-session
  (testing "Session read operations use session store"
    (let [result (ax/check-all dsl-invariants/session-read-uses-store)]
      (is (true? (:valid? result))
          "Session read should use session store")))

  (testing "Session update operations use session store"
    (let [result (ax/check-all dsl-invariants/session-update-uses-store)]
      (is (true? (:valid? result))
          "Session update should use session store")))

  (testing "All session invariants pass"
    (let [result (dsl-invariants/check-session)]
      (is (true? (:valid? result))
          (str "Session invariant violations: " (:errors result))))))

(deftest test-dsl-invariants-calculations
  (testing "Subtotal calculation is pure"
    (let [result (ax/check-all dsl-invariants/calculate-subtotal-is-pure)]
      (is (true? (:valid? result))
          "Subtotal calculation should be pure")))

  (testing "Tax calculation is pure"
    (let [result (ax/check-all dsl-invariants/calculate-tax-is-pure)]
      (is (true? (:valid? result))
          "Tax calculation should be pure")))

  (testing "Total calculation is pure"
    (let [result (ax/check-all dsl-invariants/calculate-total-is-pure)]
      (is (true? (:valid? result))
          "Total calculation should be pure")))

  (testing "Pure functions have no component dependencies"
    (let [result (ax/check-all dsl-invariants/pure-functions-no-component-deps)]
      (is (true? (:valid? result))
          "Pure functions should not depend on components")))

  (testing "All calculation invariants pass"
    (let [result (dsl-invariants/check-calculations)]
      (is (true? (:valid? result))
          (str "Calculation invariant violations: " (:errors result))))))

(deftest test-dsl-invariants-pii
  (testing "PII-handling functions are audited"
    (let [result (ax/check-all dsl-invariants/pii-functions-are-audited)]
      (is (true? (:valid? result))
          "PII functions should be audited")))

  (testing "PII endpoints require authorization"
    (let [result (ax/check-all dsl-invariants/pii-endpoints-require-auth)]
      (is (true? (:valid? result))
          "PII endpoints should require authorization")))

  (testing "All PII invariants pass"
    (let [result (dsl-invariants/check-pii)]
      (is (true? (:valid? result))
          (str "PII invariant violations: " (:errors result))))))

(deftest test-dsl-invariants-cart-operations
  (testing "Add-to-cart produces cart/items"
    (let [result (ax/check-all dsl-invariants/add-to-cart-produces-items)]
      (is (true? (:valid? result))
          "Add-to-cart should produce cart/items")))

  (testing "Remove-from-cart produces cart/items"
    (let [result (ax/check-all dsl-invariants/remove-from-cart-produces-items)]
      (is (true? (:valid? result))
          "Remove-from-cart should produce cart/items")))

  (testing "Update-quantity produces cart/items"
    (let [result (ax/check-all dsl-invariants/update-quantity-produces-items)]
      (is (true? (:valid? result))
          "Update-quantity should produce cart/items")))

  (testing "Clear-cart produces cart/items"
    (let [result (ax/check-all dsl-invariants/clear-cart-produces-items)]
      (is (true? (:valid? result))
          "Clear-cart should produce cart/items")))

  (testing "All cart operation invariants pass"
    (let [result (dsl-invariants/check-cart-ops)]
      (is (true? (:valid? result))
          (str "Cart operation invariant violations: " (:errors result))))))

(deftest test-dsl-invariants-tiers
  (testing "Components are foundation tier"
    (let [result (ax/check-all dsl-invariants/components-are-foundation)]
      (is (true? (:valid? result))
          "All components should be foundation tier")))

  (testing "Endpoints are API tier"
    (let [result (ax/check-all dsl-invariants/endpoints-are-api)]
      (is (true? (:valid? result))
          "All endpoints should be API tier")))

  (testing "Functions are service tier"
    (let [result (ax/check-all dsl-invariants/functions-are-service)]
      (is (true? (:valid? result))
          "All functions should be service tier")))

  (testing "All tier invariants pass"
    (let [result (dsl-invariants/check-tiers)]
      (is (true? (:valid? result))
          (str "Tier violations: " (:errors result))))))

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
      (is (true? (:valid? result))
          (str "DSL invariant violations: " (:errors result)))))

  (testing "DSL invariants can be documented"
    (let [docs (ax/document-all dsl-invariants/all-invariants)]
      (is (pos? (count docs)))
      (is (every? #(contains? % :id) docs))
      (is (every? #(= :dsl (:style %)) docs)))))
