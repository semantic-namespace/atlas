(ns atlas.source.register-test
  "register! calls read by atlas.source.index as sources, without loading
   the code that makes them."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [atlas.registry :as registry]
            [atlas.source.index :as index]))

(def kondo? (some? (try (requiring-resolve 'clj-kondo.core/run!) (catch Exception _ nil))))

(defn- project
  "A temp project root holding `files` ({relative-path content})."
  [files]
  (let [root (doto (java.io.File/createTempFile "atlas-register" "") .delete .mkdirs)]
    (doseq [[path content] files]
      (let [f (io/file root path)] (io/make-parents f) (spit f content)))
    (str root)))

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

(deftest register-calls-are-sources
  (if-not kondo?
    (is true "clj-kondo not on classpath; not checked")
    (let [root (project {"src/app/things.clj" calls})
          idx (index/build {:root root :registry ["registry"] :src ["src"]})
          w (index/where idx :fn/literal)]
      (testing "a call written as data is an entity, defined at the call"
        (is (= {:file "src/app/things.clj" :line 8 :register! true} (:definition w)))
        (is (= #{:tier/service :domain/things} (:aspects w))))
      (testing "auto-resolved keywords are resolved with the file's namespace and aliases"
        (is (= [:app.spec/id :thing/name] (get-in w [:entry :execution-function/context])))
        (is (= [:app.things/result] (get-in w [:entry :execution-function/response]))))
      (testing "props given as code are left out and named"
        (is (= [:atlas/impl :execution-function/deps] (get-in w [:sources 0 :dropped])))
        (is (not (contains? (:entry w) :execution-function/deps))))
      (testing "an inline :atlas/impl is the impl; no var is required"
        (is (:inline (:impl w)))
        (is (not-any? #(= :entity-without-impl (:problem %)) (index/check idx))))
      (testing "a 3-argument call gets the dev-id register! would generate"
        (is (some? (index/where idx (registry/generate-dev-id #{:atlas/data-schema :domain/things :entity/aggregate})))))
      (testing "a top-level call is read too"
        (is (some? (index/where idx :component/db))))
      (testing "a call whose dev-id is computed is counted, not guessed"
        (is (= [{:severity :info :problem :register-call-not-read
                 :reason "dev-id, type or aspects not written as data" :file "src/app/things.clj" :line 20}]
               (filter #(= :register-call-not-read (:problem %)) (index/check idx))))
        (is (= {:read 3 :not-read 1} (:register-calls (index/summary idx))))))))

(deftest register-and-edn-sources-merge
  (if-not kondo?
    (is true "clj-kondo not on classpath; not checked")
    (let [same (project {"src/app/things.clj" calls
                         "registry/more.edn" "{:fn/literal {:atlas/aspects #{:value/retention} :atlas/business-pattern :recovery}}"})
          other (project {"src/app/things.clj" calls
                          "registry/other.edn" "{:fn/literal {:atlas/type :atlas/interface-endpoint}}"})]
      (testing "a registry file adding keys to a register! entity merges"
        (let [w (index/where (index/build {:root same :registry ["registry"] :src ["src"]}) :fn/literal)]
          (is (= #{:tier/service :domain/things :value/retention} (:aspects w)))
          (is (= :recovery (get-in w [:entry :atlas/business-pattern])))
          (is (= #{"src/app/things.clj" "registry/more.edn"} (set (map :file (:sources w)))))))
      (testing "a registry file contradicting a register! call is a conflict naming the call's line"
        (let [c (first (filter #(= :conflicting-values (:problem %))
                               (index/check (index/build {:root other :registry ["registry"] :src ["src"]}))))]
          (is (= :atlas/type (:key c)))
          (is (= #{{:file "src/app/things.clj" :line 8} {:file "registry/other.edn" :line 1}}
                 (set (mapcat :sources (:values c))))))))))

(deftest the-cart-reads-the-same-either-way
  (if-not kondo?
    (is true "clj-kondo not on classpath; not checked")
    (let [entries #(update-vals (:entities %) :entry)]
      (is (= (entries (index/build {:root "." :registry ["no-such-dir"] :src ["test/app/cart.clj"]}))
             (entries (index/build {:root "." :registry ["test/app/cart_registry"] :src ["no-such-dir"]})))
          "the 20 register! calls in app.cart say what app/cart_registry/*.edn say"))))
