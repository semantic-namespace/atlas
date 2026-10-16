(ns app.cart-sidecar-test
  "The cart registry authored as EDN files plus annotated vars
   (app.cart-impl), checked against the same registry authored with
   register! calls (app.cart)."
  (:require
   [clojure.test :refer [deftest testing is use-fixtures]]
   [clojure.java.io :as io]
   [atlas.registry :as registry]
   [atlas.registry.lookup :as rt]
   [atlas.registry.serialise :as serialise]
   [atlas.datalog :as datalog]
   [atlas.invariant :as inv]
   [atlas.ontology :as o]
   [atlas.sidecar :as sidecar]
   [app.cart :as cart]
   [app.cart-impl]))

(def files
  (mapv #(io/resource (str "app/cart_registry/" % ".edn")) ["schemas" "components" "functions" "endpoints"]))

(defn- fresh! []
  (registry/reset-all!)
  (datalog/reset-db-cache!)
  (doseq [n '[atlas.ontology atlas.ontology.execution-function atlas.ontology.structure-component
              atlas.ontology.interface-endpoint atlas.ontology.data-schema]]
    (require n :reload)))

(defn- load-sidecar! [] (sidecar/load! files '[app.cart-impl]))

(use-fixtures :each (fn [f] (fresh!) (load-sidecar!) (f) (registry/reset-all!)))

;; =============================================================================
;; Same registry either way
;; =============================================================================

(deftest same-registry-as-register-calls
  (let [from-files (serialise/sanitize @registry/registry)
        violations-files (set (map :rule (:violations (inv/check-all))))
        _ (do (fresh!) (cart/init-registry!))
        from-calls (serialise/sanitize @registry/registry)
        violations-calls (set (map :rule (:violations (inv/check-all))))]
    (is (= from-calls from-files)
        "every compound identity and prop matches, once fn values are left out")
    (is (= violations-calls violations-files))
    (is (registry/validate-ontology-specs))))

(deftest every-function-is-bound-to-its-var
  (doseq [[id v] (sidecar/annotated-vars '[app.cart-impl])]
    (is (identical? v (:atlas/impl (rt/props-for id))) (str id))
    (is (= id (:atlas/dev-id (meta v)))))
  (is (= 13 (count (sidecar/annotated-vars '[app.cart-impl])))))

(deftest exported-snapshot-drops-the-vars
  (is (not-any? :atlas/impl (vals (serialise/sanitize @registry/registry)))))

;; =============================================================================
;; Running the cart through the registry
;; =============================================================================

(def products
  {":product/2" {:product/id ":product/2" :product/name "Cotton T-Shirt" :product/price 24.99M
                 :product/sizes ["M"] :product/colors ["black"]}})

(defn- components [sessions]
  {:component/db {:query (fn [_ id] (get products id))}
   :component/session-store {:get-session (fn [sid] (get @sessions sid))
                             :save-session (fn [sid data] (swap! sessions assoc sid data) {:session/id sid})}})

(defn- invoker
  "Calls a function entity through its registry :atlas/impl, with its deps
   resolved from the registry: components from `comps`, functions as a
   recursive call."
  [comps]
  (fn invoke [id ctx]
    ((:atlas/impl (rt/props-for id))
     ctx
     (into {} (for [d (o/deps-for id)]
                [(keyword (name d)) (or (get comps d) (fn [c] (invoke d c)))])))))

(deftest cart-runs-through-registry-impls
  (let [sessions (atom {"s1" {:cart/items [{:item/product-id ":product/2" :item/quantity 1 :item/price 24.99M
                                            :item/size "M" :item/color "black"}]}})
        invoke (invoker (components sessions))]
    (testing "add-to-cart gets the price through its :fn/get-product dep"
      (let [{:item/keys [price]} (invoke :fn/add-to-cart {:session/data {:cart/items []}
                                                          :item/product-id ":product/2" :item/quantity 2
                                                          :item/size "M" :item/color "black"})]
        (is (= 24.99M price))))
    (testing "merge sums duplicate lines"
      (swap! sessions update-in ["s1" :cart/items] conj
             {:item/product-id ":product/2" :item/quantity 3 :item/price 24.99M :item/size "M" :item/color "black"})
      (is (= 4 (-> (invoke :fn/merge-carts {:merge/source-session-id "s1" :merge/target-user-id "u1"})
                   :cart/items first :item/quantity))))
    (testing "totals"
      (let [{:cart/keys [subtotal]} (invoke :fn/calculate-cart-subtotal {:cart/items [{:item/price 100M}]})
            {:cart/keys [tax]} (invoke :fn/calculate-cart-tax {:cart/subtotal subtotal :address/country "US"})]
        (is (= 110.00M (:cart/total (invoke :fn/calculate-cart-total {:cart/subtotal subtotal :cart/tax tax}))))))))

(deftest redefining-a-var-is-seen-without-reloading
  (let [v #'app.cart-impl/clear-cart
        original @v]
    (try
      (alter-var-root v (constantly (fn [_ _] {:cart/items [:redefined]})))
      (is (= [:redefined] (:cart/items ((:atlas/impl (rt/props-for :fn/clear-cart)) {} {}))))
      (finally (alter-var-root v (constantly original))))))

;; =============================================================================
;; Code and files disagreeing
;; =============================================================================

(deftest linkage-problems
  (let [entities (sidecar/merge-entities (map (comp sidecar/read-entities slurp) files))
        vars (sidecar/annotated-vars '[app.cart-impl])]
    (is (empty? (sidecar/linkage-problems entities vars)))
    (is (= [{:problem :var-without-entity :dev-id :fn/clear-cart :var #'app.cart-impl/clear-cart}]
           (sidecar/linkage-problems (dissoc entities :fn/clear-cart) vars)))
    (is (= [{:problem :entity-without-impl :dev-id :fn/clear-cart :type :atlas/execution-function}]
           (sidecar/linkage-problems entities (dissoc vars :fn/clear-cart))))
    (testing "non-function entities need no var"
      (is (not-any? #(= :component/db (:dev-id %)) (sidecar/linkage-problems entities {}))))))

(deftest a-dev-id-in-two-files-is-an-error
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"more than one registry file"
                        (sidecar/merge-entities [{:fn/a {}} {:fn/b {}} {:fn/a {}}]))))

;; =============================================================================
;; Static view: form <-> entity without loading code (what review needs)
;; =============================================================================

(deftest static-links-find-each-annotated-form
  (if-not (try (requiring-resolve 'clj-kondo.core/run!) (catch Exception _ nil))
    (is true "clj-kondo not on classpath; static-links not checked")
    (let [links (sidecar/static-links ["test/app/cart_impl.clj"])]
      (is (= (set (keys (sidecar/annotated-vars '[app.cart-impl]))) (set (keys links))))
      (is (= {:ns 'app.cart-impl :name 'merge-carts}
             (select-keys (links :fn/merge-carts) [:ns :name])))
      (is (pos-int? (:row (links :fn/merge-carts)))))))
