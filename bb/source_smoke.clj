(ns source-smoke
  "Copies the cart example (test/app/cart_registry, test/app/cart_impl.clj)
   into a throwaway project, edits it the way an LLM would, and checks that
   atlas.source.index sees each edit, re-analysing only the files that changed."
  (:require [atlas.source.hover :as hover]
            [atlas.store.canonical :as canon]
            [babashka.process :as p]
            [atlas.source.index :as index]
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
  (let [dir (fs/create-temp-dir {:prefix "atlas-source-smoke"})
        reg (fs/path dir "registry")
        src (fs/path dir "src" "app")
        opts {:root (str dir) :registry ["registry"] :src ["src"] :cache (str (fs/path dir ".atlas" "source-index.transit"))}]
    (fs/create-dirs reg)
    (doseq [f (fs/glob "test/app/cart_registry" "[a-z]*.edn")] (fs/copy f (fs/path reg (fs/file-name f))))
    (spit (str (fs/path reg ".#lock.edn")) "an editor lock file: never a source")
    (fs/create-dirs src)
    (fs/copy "test/app/cart_impl.clj" (fs/path src "cart_impl.clj"))
    (spit (str (fs/path src "cart_extra.clj")) "(ns app.cart-extra)\n(defn helper [x] x)\n")
    (try
      (let [idx (index/build opts)]
        (check "cold build analyses both source files" (= 2 (:analysed idx)))
        (check "editor lock and auto-save files are not sources" (empty? (:read-problems idx)))
        (check "no problems beyond contract notes" (every? #(= :info (:severity %)) (index/check idx)))
        (check "where gives the EDN line and the var"
               (= {:file "registry/functions.edn" :line 34} (:definition (index/where idx :fn/add-to-cart))))
        (check "coverage counts the unlinked helper" (= {:linked-vars 13 :public-fns 14}
                                                        (select-keys (:coverage (index/uses idx :fn/get-cart)) [:linked-vars :public-fns]))))

      (let [idx (index/build opts)]
        (check "warm build analyses nothing" (zero? (:analysed idx))))

      (let [live (index/build (dissoc opts :cache))
            same (index/build (assoc (dissoc opts :cache) :previous live))]
        (check "in memory, nothing changed: the same index back" (= (dissoc live :analysed) (dissoc same :analysed)))
        (edit! (str (fs/path reg "functions.edn")) ":execution-function/context [:session/data]\n  :execution-function/response [:cart/id"
               ":execution-function/context [:session/data :cart/locale]\n  :execution-function/response [:cart/id")
        (let [after (index/build (assoc (dissoc opts :cache) :previous same))]
          (check "in memory, a saved EDN edit is in the next hover"
                 (re-find #":cart/locale" (:describe (hover/lookup after :fn/get-cart))))
          (check "in memory, an EDN edit re-analyses no code" (zero? (:analysed after)))))

      (edit! (str (fs/path src "cart_impl.clj"))
             "(defn ^{:atlas.source/id :fn/clear-cart} clear-cart\n  [_ _]\n  {:cart/items []})"
             "(defn ^{:atlas.source/id :fn/clear-cart} clear-cart\n  [_ _]\n  (calculate-cart-subtotal {:cart/items []} {})\n  {:cart/items []})")
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

      (spit (str (fs/path reg "business.edn"))
            "{:fn/get-cart {:atlas/aspects #{:value/retention}\n               :atlas/business-pattern :cart-recovery}}")
      (let [idx (index/build opts)
            w (index/where idx :fn/get-cart)]
        (check "a second file adding keys to an entity is merged, not an error"
               (not-any? #(= :error (:severity %)) (remove #(#{:var-without-entity :entity-without-impl} (:problem %)) (index/check idx))))
        (check "aspects from both files are unioned"
               (contains? (:aspects w) :value/retention))
        (check "where lists each file and the keys it gave"
               (= [{:file "registry/business.edn" :line 1 :keys [:atlas/aspects :atlas/business-pattern]}
                   {:file "registry/functions.edn" :line 27
                    :keys [:atlas/aspects :atlas/type :execution-function/context :execution-function/deps :execution-function/response]}]
                  (:sources w)))
        (check "the definition is the file that gives the type"
               (= "registry/functions.edn" (get-in w [:definition :file]))))

      (spit (str (fs/path reg "other.edn")) "{:fn/get-cart {:atlas/type :atlas/interface-endpoint}}")
      (spit (str (fs/path src "cart_dup.clj")) "(ns app.cart-dup)\n(defn ^{:atlas.source/id :fn/get-cart} get-cart-2 [_ _])\n")
      (let [idx (index/build opts)
            problems (index/check idx)
            conflict (first (filter #(= :conflicting-values (:problem %)) problems))]
        (check "a new file is analysed on its own" (= 1 (:analysed idx)))
        (check "a key with two values is a conflict naming both files and lines"
               (= {:dev-id :fn/get-cart :key :atlas/type
                   :values [{:value :atlas/execution-function :sources [{:file "registry/functions.edn" :line 27}]}
                            {:value :atlas/interface-endpoint :sources [{:file "registry/other.edn" :line 1}]}]}
                  (select-keys conflict [:dev-id :key :values])))
        (check "a dev-id on two vars is an error"
               (some #(= :dev-id-on-several-vars (:problem %)) problems)))
      ;; the store CI keeps: last in line, for what neither files nor the REPL say
      (let [store (fs/path dir "store")
            git (fn [& args] (apply p/shell {:dir (str store) :out :string :err :string} "git" "-c" "user.name=smoke" "-c" "user.email=smoke@example.com" args))
            record! (fn [reg msg]
                      (doseq [[path content] (canon/registry->files reg)]
                        (let [f (fs/path store "demo" path)] (fs/create-dirs (fs/parent f)) (spit (str f) content)))
                      (git "add" "-A") (git "commit" "-q" "-m" msg)
                      (str/trim (:out (git "rev-parse" "HEAD"))))
            entity (fn [id type aspects props] [(conj aspects type) (assoc props :atlas/dev-id id :atlas/type type)])
            sopts (assoc (dissoc opts :cache) :store {:dir (str store) :prefix "demo" :ref "main"})]
        (fs/create-dirs store)
        (git "init" "-q")
        (git "symbolic-ref" "HEAD" "refs/heads/main")
        (let [c1 (record! (into {} [(entity :fn/only-in-store :atlas/execution-function #{:tier/service :domain/generated}
                                            {:execution-function/context [:x/y] :atlas/source ["registry/functions.edn"]})
                                    (entity :fn/get-cart :atlas/execution-function #{:tier/service}
                                            {:execution-function/context [:stale/value]})])
                          "registry: abc1234")
              idx (index/build sopts)
              w (index/where idx :fn/only-in-store)]
          (check "the store fills an entity no file or REPL defines, at the recorded commit"
                 (= c1 (get-in w [:definition :store-commit])))
          (check "a stored :atlas/source points M-. at the file"
                 (= "registry/functions.edn" (get-in w [:definition :file])))
          (check "an entity a file defines comes from the file, not the store"
                 (and (= "registry/functions.edn" (get-in (index/where idx :fn/get-cart) [:definition :file]))
                      (not= [:stale/value] (get-in (index/where idx :fn/get-cart) [:entry :execution-function/context]))))
          (check "the hover says it's from the store"
                 (re-find #"store only" (:eldoc (hover/lookup idx :fn/only-in-store))))
          (let [c2 (record! (into {} [(entity :fn/only-in-store :atlas/execution-function #{:tier/service :domain/generated}
                                              {:execution-function/context [:x/z]})])
                            "registry: def5678")
                idx2 (index/build (assoc sopts :previous idx))]
            (check "a new store commit is read on the next build"
                   (and (= c2 (get-in (index/where idx2 :fn/only-in-store) [:definition :store-commit]))
                        (= [:x/z] (get-in (index/where idx2 :fn/only-in-store) [:entry :execution-function/context])))))))
      (finally (fs/delete-tree dir)))
    (if (pos? @failures)
      (do (println @failures "failed") (System/exit 1))
      (println "OK"))))
