(ns app.cart-source-test
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
   [atlas.source :as source]
   [clojure.edn]
   [clojure.string]
   [app.cart :as cart]
   [app.cart-impl]))

(def files
  "Classpath resources; the string is each source's label in :atlas/source."
  (mapv #(str "app/cart_registry/" % ".edn") ["schemas" "components" "functions" "endpoints"]))

(defn- fresh! []
  (registry/reset-all!)
  (source/forget-loads!)
  (datalog/reset-db-cache!)
  (doseq [n '[atlas.ontology atlas.ontology.execution-function atlas.ontology.structure-component
              atlas.ontology.interface-endpoint atlas.ontology.data-schema]]
    (require n :reload)))

(defn- load-source! [] (source/load! files '[app.cart-impl]))

(use-fixtures :each (fn [f] (fresh!) (load-source!) (f) (registry/reset-all!) (source/forget-loads!)))

;; =============================================================================
;; Same registry either way
;; =============================================================================

(deftest same-registry-as-register-calls
  (let [from-files (update-vals (serialise/sanitize @registry/registry) #(dissoc % :atlas/source))
        violations-files (set (map :rule (:violations (inv/check-all))))
        _ (do (fresh!) (cart/init-registry!))
        from-calls (serialise/sanitize @registry/registry)
        violations-calls (set (map :rule (:violations (inv/check-all))))]
    (is (= from-calls from-files)
        "every compound identity and prop matches, once fn values and :atlas/source are left out")
    (is (= violations-calls violations-files))
    (is (registry/validate-ontology-specs))))

(deftest every-function-is-bound-to-its-var
  (doseq [[id v] (source/annotated-vars '[app.cart-impl])]
    (is (identical? v (:atlas/impl (rt/props-for id))) (str id))
    (is (= id (:atlas.source/id (meta v)))))
  (is (= 13 (count (source/annotated-vars '[app.cart-impl])))))

(deftest exported-snapshot-drops-the-vars-keeps-the-sources
  (let [exported (serialise/sanitize @registry/registry)]
    (is (not-any? :atlas/impl (vals exported)))
    (is (= ["app/cart_registry/functions.edn"]
           (:atlas/source (rt/props-for :fn/get-cart))))))

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

(defn- merged [] (source/merge-sources (for [f files] {:source f :entities (source/read-entities (slurp (io/resource f)))})))

(deftest linkage-problems
  (let [entities (:entities (merged))
        vars (source/annotated-vars '[app.cart-impl])]
    (is (empty? (source/linkage-problems entities vars)))
    (is (= [{:problem :var-without-entity :dev-id :fn/clear-cart :var #'app.cart-impl/clear-cart}]
           (source/linkage-problems (dissoc entities :fn/clear-cart) vars)))
    (is (= [{:problem :entity-without-impl :dev-id :fn/clear-cart :type :atlas/execution-function}]
           (source/linkage-problems entities (dissoc vars :fn/clear-cart))))
    (testing "non-function entities need no var"
      (is (not-any? #(= :component/db (:dev-id %)) (source/linkage-problems entities {}))))
    (testing "an entity no source gives a type"
      (is (= [{:problem :entity-without-type :dev-id :fn/orphan}]
             (source/linkage-problems {:fn/orphan {:atlas/aspects #{}}} {}))))))

;; =============================================================================
;; Merging sources
;; =============================================================================

(def authored {:source "registry/functions.edn"
               :entities {:fn/get-cart {:atlas/type :atlas/execution-function
                                        :atlas/aspects #{:tier/service :domain/cart}
                                        :execution-function/context [:session/data]}}})

(def business {:source "registry/business.edn"
               :entities {:fn/get-cart {:atlas/aspects #{:value/retention}
                                        :atlas/business-pattern :cart-recovery}}})

(def generated {:source 'app.tools/generate!
                :entities {:fn/get-cart {:atlas/type :atlas/execution-function
                                         :mcp-tool/name "get_cart"}}})

(deftest sources-each-give-their-own-keys
  (let [{:keys [entities provenance conflicts]} (source/merge-sources [authored business generated])
        e (:fn/get-cart entities)]
    (is (empty? conflicts))
    (is (= #{:tier/service :domain/cart :value/retention} (:atlas/aspects e)) "aspects are unioned")
    (is (= [:session/data :cart-recovery "get_cart"]
           [(first (:execution-function/context e)) (:atlas/business-pattern e) (:mcp-tool/name e)]))
    (is (= #{"registry/functions.edn" 'app.tools/generate!} (get-in provenance [:fn/get-cart :atlas/type]))
        "a value given twice is recorded with both sources")
    (is (= #{"registry/business.edn"} (get-in provenance [:fn/get-cart :atlas/business-pattern])))))

(deftest merge-does-not-depend-on-order
  (let [sources [authored business generated]
        results (set (for [a sources b sources c sources :when (= 3 (count (distinct [a b c])))]
                       (source/merge-sources [a b c])))]
    (is (= 1 (count results)))))

(deftest a-key-given-two-values-is-a-conflict
  (let [other {:source "registry/other.edn"
               :entities {:fn/get-cart {:atlas/type :atlas/interface-endpoint
                                        :execution-function/context [:session/id]}}}
        {:keys [entities conflicts]} (source/merge-sources [authored other])]
    (is (= [{:problem :conflicting-values :dev-id :fn/get-cart :key :atlas/type
             :values [{:value :atlas/execution-function :sources #{"registry/functions.edn"}}
                      {:value :atlas/interface-endpoint :sources #{"registry/other.edn"}}]}
            {:problem :conflicting-values :dev-id :fn/get-cart :key :execution-function/context
             :values [{:value [:session/data] :sources #{"registry/functions.edn"}}
                      {:value [:session/id] :sources #{"registry/other.edn"}}]}]
           conflicts))
    (is (= #{:tier/service :domain/cart} (get-in entities [:fn/get-cart :atlas/aspects]))
        "the entity keeps the keys that don't conflict")
    (is (not (contains? (:fn/get-cart entities) :atlas/type)))))

(deftest load-refuses-conflicting-sources
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"sources conflict"
                        (source/load! (conj files {:source "test" :entities {:fn/get-cart {:execution-function/deps #{:fn/x}}}})
                                      '[app.cart-impl]))))

(deftest a-generator-adds-keys-to-an-authored-entity
  (fresh!)
  (source/load! (conj files {:source 'app.tools/generate! :entities {:fn/get-cart {:mcp-tool/name "get_cart"}}})
                '[app.cart-impl])
  (is (= "get_cart" (:mcp-tool/name (rt/props-for :fn/get-cart))))
  (is (= ["app/cart_registry/functions.edn" 'app.tools/generate!] (:atlas/source (rt/props-for :fn/get-cart)))))

;; =============================================================================
;; Reloading after an edit
;; =============================================================================

(defn- temp-registry
  "The cart registry files copied to a temp dir, as file paths."
  []
  (let [dir (doto (java.io.File/createTempFile "atlas-source" "") .delete .mkdirs)]
    (vec (for [f files]
           (let [out (io/file dir (.getName (io/file f)))]
             (spit out (slurp (io/resource f)))
             (str out))))))

(defn- edit! [path from to]
  (let [s (slurp path)]
    (assert (clojure.string/includes? s from) (str "edit target not in " path))
    (spit path (clojure.string/replace-first s from to))))

(deftest reload-follows-edits-to-registry-files
  (fresh!)
  (let [[_ _ functions :as paths] (temp-registry)
        extra (str (io/file (.getParentFile (io/file functions)) "extra.edn"))]
    (spit extra "{}")
    (source/load! (conj paths extra) '[app.cart-impl])
    (edit! functions ":execution-function/context [:session/data]\n  :execution-function/response [:cart/id"
           ":execution-function/context [:session/data :cart/locale]\n  :execution-function/response [:cart/id")
    (spit extra "{:schema/locale {:atlas/type :atlas/data-schema :atlas/aspects #{:domain/cart} :data-schema/fields [:cart/locale]}}")
    (testing "an edited entry changes, a new file's entity appears"
      (let [r (source/reload!)]
        (is (= [:schema/locale] (:added r)))
        (is (= [:fn/get-cart] (:changed r)))
        (is (= [:session/data :cart/locale] (:execution-function/context (rt/props-for :fn/get-cart))))
        (is (identical? #'app.cart-impl/get-cart (:atlas/impl (rt/props-for :fn/get-cart))) "the var link survives")))
    (testing "an entry removed from a file goes on reload"
      (spit extra "{}")
      (is (= {:added [] :removed [:schema/locale] :changed []} (source/reload!)))
      (is (nil? (rt/props-for :schema/locale))))
    (testing "a reload that fails changes nothing"
      (let [before @registry/registry]
        (edit! functions " :fn/clear-cart\n" " :fn/clear-all\n")
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"disagree" (source/reload!)))
        (is (= before @registry/registry))))))

(deftest reload-calls-generators-again
  (fresh!)
  (let [n (atom 0)
        gen (fn [] {:source 'test/gen :entities {:fn/get-cart {:mcp-tool/name (str "get_cart_v" (swap! n inc))}}})]
    (source/load! (conj files gen) '[app.cart-impl])
    (is (= "get_cart_v1" (:mcp-tool/name (rt/props-for :fn/get-cart))))
    (is (= [:fn/get-cart] (:changed (source/reload!))))
    (is (= "get_cart_v2" (:mcp-tool/name (rt/props-for :fn/get-cart))))))

(deftest unregister-ns-still-removes-what-a-namespace-registered
  (fresh!)
  (binding [*ns* (create-ns 'app.cart-source-test.other)]
    (registry/register! :fn/elsewhere :atlas/execution-function #{:tier/service} {}))
  (load-source!)
  (is (= #{:fn/elsewhere} (registry/unregister-ns! 'app.cart-source-test.other)))
  (is (nil? (rt/props-for :fn/elsewhere)))
  (is (some? (rt/props-for :fn/get-cart))))

;; =============================================================================
;; REPL snapshot
;; =============================================================================

(defn- eventually [pred]
  (let [deadline (+ (System/currentTimeMillis) 3000)]
    (loop [] (or (pred) (when (< (System/currentTimeMillis) deadline) (Thread/sleep 50) (recur))))))

(deftest watch-keeps-the-snapshot-meta-source-reads
  (let [root (doto (java.io.File/createTempFile "atlas-snapshot" "") .delete .mkdirs)
        path (str (io/file root ".meta-source" "packs" "atlas.edn"))
        watch! (requiring-resolve 'atlas.meta-source/watch!)
        unwatch! (requiring-resolve 'atlas.meta-source/unwatch!)
        read-snap #(clojure.edn/read-string (slurp path))]
    (try
      (watch! path)
      (testing "written at once, in meta-source's snapshot format, with the registry as this process has it"
        (is (= :atlas (:pack (read-snap))))
        (is (number? (:written (read-snap))))
        (is (= ["app/cart_registry/functions.edn"] (get-in (read-snap) [:entities :fn/get-cart :atlas/source])))
        (is (not-any? :atlas/impl (vals (:entities (read-snap))))))
      (testing "rewritten after a runtime registration"
        (registry/register! :tool/generated :atlas/execution-function #{:tier/service} {:execution-function/context [:x/y]})
        (is (eventually #(= [:x/y] (get-in (read-snap) [:entities :tool/generated :execution-function/context])))))
      (finally (unwatch!)))))
