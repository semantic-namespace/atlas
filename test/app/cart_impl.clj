(ns app.cart-impl
  "The shopping cart as ordinary Clojure. Nothing here requires atlas: each
   function names the entity it implements in its metadata, and the entity's
   definition (aspects, context, response, deps) lives in
   test/app/cart_registry/*.edn.

   Every function takes the context map and a map of its resolved deps, keyed
   by the dep's name: a component's operations, or a function dep as a fn of
   one context.")

(defn- same-line? [pid sz clr item]
  (and (= (:item/product-id item) pid) (= (:item/size item) sz) (= (:item/color item) clr)))

;; Session management

(defn ^{:atlas.source/id :fn/get-session} get-session
  [{sid :session/id} {:keys [session-store]}]
  {:session/data ((:get-session session-store) sid)})

(defn ^{:atlas.source/id :fn/save-session} save-session
  [{sid :session/id sdata :session/data} {:keys [session-store]}]
  ((:save-session session-store) sid sdata))

;; Product lookup

(defn ^{:atlas.source/id :fn/get-product} get-product
  [{pid :product/id} {:keys [db]}]
  (when-let [product ((:query db) :product pid)]
    (select-keys product [:product/id :product/name :product/price :product/sizes :product/colors])))

;; Cart operations

(defn ^{:atlas.source/id :fn/get-cart} get-cart
  [{data :session/data} _]
  (if data
    (select-keys data [:cart/id :cart/items :cart/user-id :cart/shipping-address :cart/guest?])
    {:cart/id nil :cart/items [] :cart/user-id nil :cart/shipping-address nil :cart/guest? true}))

(defn ^{:atlas.source/id :fn/add-to-cart} add-to-cart
  [{sdata :session/data pid :item/product-id qty :item/quantity sz :item/size clr :item/color} {:keys [get-product]}]
  (let [price (:product/price (get-product {:product/id pid}))
        new-item {:item/product-id pid :item/quantity qty :item/price price :item/size sz :item/color clr}]
    {:cart/items (conj (vec (remove (partial same-line? pid sz clr) (:cart/items sdata))) new-item)
     :item/price price}))

(defn ^{:atlas.source/id :fn/remove-from-cart} remove-from-cart
  [{sdata :session/data pid :item/product-id sz :item/size clr :item/color} _]
  {:cart/items (vec (remove (partial same-line? pid sz clr) (:cart/items sdata)))})

(defn ^{:atlas.source/id :fn/update-quantity} update-quantity
  [{qty :item/quantity pid :item/product-id sz :item/size clr :item/color sdata :session/data} _]
  (let [items (:cart/items sdata)]
    {:cart/items (if (<= qty 0)
                   (vec (remove (partial same-line? pid sz clr) items))
                   (mapv #(if (same-line? pid sz clr %) (assoc % :item/quantity qty) %) items))}))

(defn ^{:atlas.source/id :fn/clear-cart} clear-cart
  [_ _]
  {:cart/items []})

(defn ^{:atlas.source/id :fn/merge-carts} merge-carts
  "Sum-quantities merge: items with the same product, size and colour have
   their quantities added."
  [{src-sid :merge/source-session-id tgt-uid :merge/target-user-id} {:keys [session-store]}]
  (let [line (juxt :item/product-id :item/size :item/color)
        items (:cart/items ((:get-session session-store) src-sid))
        merged (reduce (fn [acc item]
                         (if-let [i (first (keep-indexed #(when (= (line %2) (line item)) %1) acc))]
                           (update-in acc [i :item/quantity] + (:item/quantity item))
                           (conj acc item)))
                       []
                       items)]
    {:cart/items merged :cart/user-id tgt-uid}))

(defn ^{:atlas.source/id :fn/update-shipping-address} update-shipping-address
  [{l1 :address/line1 l2 :address/line2 city :address/city pc :address/postal-code ctry :address/country} _]
  {:cart/shipping-address (cond-> {:address/line1 l1 :address/city city :address/postal-code pc :address/country ctry}
                            l2 (assoc :address/line2 l2))})

;; Totals (server-side)

(defn ^{:atlas.source/id :fn/calculate-cart-subtotal} calculate-cart-subtotal
  [{items :cart/items} _]
  {:cart/subtotal (reduce + (map #(:item/price % 0M) items))})

(defn ^{:atlas.source/id :fn/calculate-cart-tax} calculate-cart-tax
  "10% for the US, 20% elsewhere."
  [{subtotal :cart/subtotal country :address/country} _]
  {:cart/tax (bigdec (* subtotal (if (= country "US") 0.10M 0.20M)))})

(defn ^{:atlas.source/id :fn/calculate-cart-total} calculate-cart-total
  [{subtotal :cart/subtotal tax :cart/tax} _]
  {:cart/total (+ subtotal tax)})
