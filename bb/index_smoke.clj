(ns index-smoke
  "Copies the cart example (test/app/cart_registry, test/app/cart_impl.clj)
   into a throwaway project, edits it the way an LLM would, and checks that
   atlas.index sees each edit, re-analysing only the files that changed."
  (:require [atlas.index :as index]
            [babashka.fs :as fs]
            [clojure.string :as str]))

(def failures (atom 0))

(defn- check [what ok?]
  (println (if ok? "ok  " "FAIL") what)
  (when-not ok? (swap! failures inc)))

(defn- edit! [f from to]
  (let [s (slurp f)]
    (when-not (str/includes? s from) (throw (ex-info "edit target not found" {:file f :from from})))
    (spit f (str/replace-first s from to))
    ;; move the mtime forward: some filesystems keep it to the second
    (.setLastModified (fs/file f) (+ 2000 (System/currentTimeMillis)))))

(defn -main [& _]
  (let [dir (fs/create-temp-dir {:prefix "atlas-index-smoke"})
        reg (fs/path dir "registry")
        src (fs/path dir "src" "app")
        opts {:root (str dir) :registry ["registry"] :src ["src"] :cache (str (fs/path dir ".atlas" "index-cache.transit"))}]
    (fs/copy-tree "test/app/cart_registry" reg)
    (fs/create-dirs src)
    (fs/copy "test/app/cart_impl.clj" (fs/path src "cart_impl.clj"))
    (spit (str (fs/path src "cart_extra.clj")) "(ns app.cart-extra)\n(defn helper [x] x)\n")
    (try
      (let [idx (index/build opts)]
        (check "cold build analyses both source files" (= 2 (:analysed idx)))
        (check "no problems beyond contract notes" (every? #(= :info (:severity %)) (index/check idx)))
        (check "where gives the EDN line and the var"
               (= {:file "registry/functions.edn" :line 34} (:definition (index/where idx :fn/add-to-cart))))
        (check "coverage counts the unlinked helper" (= {:linked-vars 13 :public-fns 14}
                                                        (select-keys (:coverage (index/uses idx :fn/get-cart)) [:linked-vars :public-fns]))))

      (let [idx (index/build opts)]
        (check "warm build analyses nothing" (zero? (:analysed idx))))

      (edit! (str (fs/path src "cart_impl.clj"))
             "(defn ^{:atlas/dev-id :fn/clear-cart} clear-cart\n  [_ _]\n  {:cart/items []})"
             "(defn ^{:atlas/dev-id :fn/clear-cart} clear-cart\n  [_ _]\n  (calculate-cart-subtotal {:cart/items []} {})\n  {:cart/items []})")
      (let [idx (index/build opts)
            problems (index/check idx)]
        (check "an edit re-analyses only that file" (= 1 (:analysed idx)))
        (check "calling another entity's var without declaring it is flagged"
               (some #(and (= :undeclared-call (:problem %)) (= :fn/clear-cart (:dev-id %))
                           (= :fn/calculate-cart-subtotal (:calls %))) problems))
        (check "uses shows the new caller"
               (some #(= :fn/clear-cart (:dev-id %)) (:called-from (index/uses idx :fn/calculate-cart-subtotal)))))

      (edit! (str (fs/path reg "functions.edn")) " :fn/clear-cart\n" " :fn/clear-all\n")
      (let [problems (index/check (index/build opts))]
        (check "renaming the entry but not the var is caught from both sides"
               (and (some #(and (= :var-without-entity (:problem %)) (= :fn/clear-cart (:dev-id %))) problems)
                    (some #(and (= :entity-without-impl (:problem %)) (= :fn/clear-all (:dev-id %))) problems))))

      (spit (str (fs/path reg "dup.edn")) "{:fn/get-cart {:atlas/type :atlas/execution-function :atlas/aspects #{}}}")
      (spit (str (fs/path src "cart_dup.clj")) "(ns app.cart-dup)\n(defn ^{:atlas/dev-id :fn/get-cart} get-cart-2 [_ _])\n")
      (let [idx (index/build opts)
            problems (index/check idx)]
        (check "a new file is analysed on its own" (= 1 (:analysed idx)))
        (check "a dev-id in two EDN files is an error"
               (some #(= :dev-id-in-several-files (:problem %)) problems))
        (check "a dev-id on two vars is an error"
               (some #(= :dev-id-on-several-vars (:problem %)) problems)))
      (finally (fs/delete-tree dir)))
    (if (pos? @failures)
      (do (println @failures "failed") (System/exit 1))
      (println "OK"))))
