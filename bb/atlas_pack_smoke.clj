(ns atlas-pack-smoke
  "meta-source (https://github.com/semantic-namespace/meta-source) with
   atlas.meta-source/pack: the cart example's EDN files and annotated vars,
   register! calls read as sources, their merge with EDN, atlas's checks,
   and the runtime snapshot. Nothing is loaded from the projects read."
  (:require [atlas.meta-source :as atlas-pack]
            [atlas.registry :as registry]
            [babashka.fs :as fs]
            [clojure.string :as str]
            [meta-source.hover :as hover]
            [meta-source.index :as index]))

(def failures (atom 0))

(defn- check [what ok?]
  (println (if ok? "ok  " "FAIL") what)
  (when-not ok? (swap! failures inc)))

(defn- build [opts]
  ;; no :id-key: the pack declares :atlas.source/id
  (index/build (merge {:registry [] :src []
                       :packs {'atlas.meta-source/pack {}}}
                      opts)))

(defn- project
  "A temp project root holding `files` ({relative-path content})."
  [files]
  (let [root (str (fs/create-temp-dir {:prefix "atlas-pack"}))]
    (doseq [[path content] files]
      (let [f (fs/file root path)] (fs/create-dirs (fs/parent f)) (spit f content)))
    root))

(defn- problems [idx p] (filter #(= p (:problem %)) (index/check idx)))

(def calls
  "(ns app.things
  (:require [atlas.registry :as registry]
            [app.spec :as spec]))

(def computed-deps #{:component/db})

(defn init! []
  (registry/register!
   :fn/literal
   :atlas/execution-function
   #{:tier/service :domain/things}
   {:execution-function/context [::spec/id :thing/name]
    :execution-function/response [::result]
    :execution-function/deps computed-deps
    :atlas/impl (fn [ctx] ctx)})

  (registry/register! :atlas/data-schema #{:domain/things :entity/aggregate}
                      {:data-schema/fields [:thing/id]})

  (registry/register! (keyword \"fn\" \"computed\") :atlas/execution-function #{:tier/service} {}))

(registry/register! :component/db :atlas/structure-component #{:tier/foundation} {:structure-component/deps #{}})
")

(defn -main [& _]
  ;; ---- the cart: EDN registry files and the vars carrying :atlas.source/id
  (let [idx (build {:root "." :registry ["test/app/cart_registry"] :src ["test/app/cart_impl.clj"]})
        w (index/where idx :fn/add-to-cart)]
    (check "where: the EDN entry's line, and the var implementing it"
           (and (= {:file "test/app/cart_registry/functions.edn" :line 34} (:definition w))
                (= 'app.cart-impl/add-to-cart (get-in w [:impl :var]))
                (= #{:fn/get-product} (get-in w [:entry :execution-function/deps]))))
    (check "uses: the entries naming it"
           (= #{:fn/add-to-cart :endpoint/cart-graph-api}
              (set (map :dev-id (:referenced-by (index/uses idx :fn/get-product))))))
    (check "data: atlas's context and response keys are its dataflow"
           (let [d (index/data-key idx :cart/subtotal)]
             (and (= #{:fn/calculate-cart-subtotal :endpoint/cart-graph-api} (set (map :dev-id (:producers d))))
                  (= #{:fn/calculate-cart-tax :fn/calculate-cart-total} (set (map :dev-id (:consumers d)))))))
    (check "check: code and files agree; only notes on keys read outside a contract remain"
           (let [ps (index/check idx)]
             (and (every? #(= :info (:severity %)) ps)
                  (= #{:fn/merge-carts :fn/calculate-cart-subtotal} (set (map :dev-id ps))))))
    (check "the hover gives the type and aspects"
           (str/includes? (:eldoc (hover/lookup idx :fn/add-to-cart)) "execution-function")))

  ;; ---- register! calls, read as sources
  (let [root (project {"src/app/things.clj" calls})
        idx (build {:root root :src ["src"]})
        w (index/where idx :fn/literal)]
    (check "a register! call written as data is an entity, defined at the call"
           (and (= {:file "src/app/things.clj" :line 8 :pack :atlas} (:definition w))
                (= #{:tier/service :domain/things} (get-in w [:entry :atlas/aspects]))))
    (check "its ::keywords are resolved with the file's namespace and aliases"
           (and (= [:app.spec/id :thing/name] (get-in w [:entry :execution-function/context]))
                (= [:app.things/result] (get-in w [:entry :execution-function/response]))))
    (check "props written as code are left out, and named"
           (and (= [:atlas/impl :execution-function/deps] (:dropped (first (:sources w))))
                (not (contains? (:entry w) :execution-function/deps))))
    (check "an execution function from a register! call needs no var: its impl is inline"
           (empty? (problems idx :entity-without-impl)))
    (check "a 3-argument call gets the dev-id register! would generate; a top-level call is read too"
           (and (index/where idx (registry/generate-dev-id #{:atlas/data-schema :domain/things :entity/aggregate}))
                (index/where idx :component/db)))
    (check "a call whose dev-id is computed is reported as not read, not guessed"
           (and (= [{:pack :atlas :reason "dev-id, type or aspects not written as data" :file "src/app/things.clj" :line 20}]
                   (map #(select-keys % [:pack :reason :file :line]) (problems idx :pack-form-not-read)))
                (= {:read 3 :not-read 1} (:atlas (:packs (index/summary idx)))))))

  ;; ---- register! calls and EDN entries merge
  (let [same (project {"src/app/things.clj" calls
                       "registry/more.edn" "{:fn/literal {:atlas/aspects #{:value/retention} :atlas/business-pattern :recovery}}"})
        other (project {"src/app/things.clj" calls
                        "registry/other.edn" "{:fn/literal {:atlas/type :atlas/interface-endpoint}}"})]
    (check "an EDN entry adding to a register! entity merges; aspects are a union"
           (let [w (index/where (build {:root same :registry ["registry"] :src ["src"]}) :fn/literal)]
             (and (= #{:tier/service :domain/things :value/retention} (get-in w [:entry :atlas/aspects]))
                  (= :recovery (get-in w [:entry :atlas/business-pattern]))
                  (= #{"src/app/things.clj" "registry/more.edn"} (set (map :file (:sources w)))))))
    (check "an EDN entry contradicting a register! call is a conflict naming both lines"
           (let [c (first (problems (build {:root other :registry ["registry"] :src ["src"]}) :conflicting-values))]
             (and (= :atlas/type (:key c))
                  (= #{{:file "src/app/things.clj" :line 8} {:file "registry/other.edn" :line 1}}
                     (set (mapcat :sources (:values c))))))))

  ;; ---- the cart says the same in its register! calls and its EDN files
  (check "app.cart's 20 register! calls say what app/cart_registry/*.edn say"
         (let [entries #(update-vals (:entities %) :entry)]
           (= (entries (build {:root "." :src ["test/app/cart.clj"]}))
              (entries (build {:root "." :registry ["test/app/cart_registry"]})))))

  ;; ---- atlas's checks
  (let [root (project {"registry/r.edn" (str "{:fn/untyped {:execution-function/context [:x/a]}\n"
                                             " :fn/unimplemented {:atlas/type :atlas/execution-function :atlas/aspects #{:tier/service}}}")})
        idx (build {:root root :registry ["registry"]})]
    (check "an entity no source gives an :atlas/type is an error"
           (= [[:fn/untyped :error]] (map (juxt :dev-id :severity) (problems idx :entity-without-type))))
    (check "an execution function from EDN alone, with no var carrying the meta key, is an error"
           (= [[:fn/unimplemented :error]] (map (juxt :dev-id :severity) (problems idx :entity-without-impl)))))

  ;; ---- a var carrying :atlas.source/id for an entity no source defines
  (let [root (project {"registry/r.edn" "{:fn/known {:atlas/type :atlas/execution-function :atlas/aspects #{:tier/service}}}"
                       "src/app/core.clj" (str "(ns app.core)\n"
                                               "(defn ^{:atlas.source/id :fn/known} known [x] x)\n"
                                               "(defn ^{:atlas.source/id :fn/orphan} orphan [x] x)\n")})
        idx (build {:root root :registry ["registry"] :src ["src"]})]
    (check "the pack's meta key links a defn to its entity, with no :id-key configured"
           (= 'app.core/known (:var (:impl (index/where idx :fn/known)))))
    (check "a var carrying the meta key for an entity no source defines is an error, at the var"
           (and (= [[:fn/orphan :error 3]] (map (juxt :dev-id :severity (comp :line :impl)) (problems idx :var-without-entity)))
                (empty? (problems idx :binding-only)))))

  ;; ---- the runtime half: what this process registered, written where meta-source reads it
  (let [root (project {"registry/functions.edn" (slurp "test/app/cart_registry/functions.edn")})]
    (registry/register! :tool/generated :atlas/execution-function #{:tier/service} {:execution-function/context [:x/y]})
    (registry/register! :fn/get-cart :atlas/execution-function #{:tier/service :domain/cart} {:execution-function/context [:session/old]})
    (atlas-pack/snapshot! (str root "/.meta-source/packs/atlas.edn"))
    (let [idx (build {:root root :registry ["registry"]})
          g (index/where idx :tool/generated)]
      (check "an entity only the running registry has comes from the snapshot"
             (and (= :atlas (get-in g [:definition :pack])) (:runtime-as-of (:definition g))
                  (= [:x/y] (get-in g [:entry :execution-function/context]))))
      (check "an entity a file defines comes from the file, which wins over a stale snapshot"
             (= [:session/data] (get-in (index/where idx :fn/get-cart) [:entry :execution-function/context])))
      (check "where the code and the running registry disagree on aspects is a warning"
             (some #(and (= :fn/get-cart (:dev-id %)) (= :warning (:severity %)))
                   (problems idx :runtime-disagrees)))))

  (println (if (zero? @failures) "atlas-pack-smoke: all checks passed" (str "atlas-pack-smoke: " @failures " failed")))
  (System/exit (if (zero? @failures) 0 1)))
