(ns atlas.source.index-test
  "atlas.source.index over the cart example's EDN files and annotated vars.
   The edit-by-edit behaviour (cache, undeclared calls, renames, merging,
   conflicts) is checked under bb by `bb source-smoke`."
  (:require [clojure.test :refer [deftest is testing]]
            [atlas.source.index :as index]
            [app.cart-impl]
            [atlas.source :as source]))

(def kondo? (some? (try (requiring-resolve 'clj-kondo.core/run!) (catch Exception _ nil))))

(def idx (delay (index/build {:root "." :registry ["test/app/cart_registry"] :src ["test/app/cart_impl.clj"]})))

(deftest cart-index
  (if-not kondo?
    (is true "clj-kondo not on classpath; atlas.source.index not checked")
    (do
      (testing "where: EDN entry line and implementing var"
        (let [w (index/where @idx :fn/add-to-cart)]
          (is (= {:file "test/app/cart_registry/functions.edn" :line 34} (:definition w)))
          (is (= 'app.cart-impl/add-to-cart (get-in w [:impl :var])))
          (is (= #{:fn/get-product} (get-in w [:entry :execution-function/deps])))))
      (testing "every annotated var is found without loading code, at its defn"
        (let [vars (source/annotated-vars '[app.cart-impl])]
          (is (= (set (keys vars)) (set (for [id (keys (:entities @idx)) :when (:impl (index/where @idx id))] id))))
          (doseq [[id v] vars]
            (is (= (:line (meta v)) (get-in (index/where @idx id) [:impl :line])) (str id)))))
      (testing "uses: entries naming it"
        (is (= #{:fn/add-to-cart :endpoint/cart-graph-api}
               (set (map :dev-id (:referenced-by (index/uses @idx :fn/get-product)))))))
      (testing "data: producers and consumers"
        (let [d (index/data-key @idx :cart/subtotal)]
          (is (= #{:fn/calculate-cart-subtotal :endpoint/cart-graph-api} (set (map :dev-id (:producers d)))))
          (is (= #{:fn/calculate-cart-tax :fn/calculate-cart-total} (set (map :dev-id (:consumers d)))))))
      (testing "check: the cart's code and files agree; only contract notes remain"
        (let [problems (index/check @idx)]
          (is (every? #(= :info (:severity %)) problems))
          (is (= #{:fn/merge-carts :fn/calculate-cart-subtotal} (set (map :dev-id problems)))
              "both read item fields nested inside :cart/items"))))))
