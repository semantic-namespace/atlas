(ns atlas.adapter.integrant-test
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [atlas.adapter.integrant :as ig-adapter]
            [atlas.registry :as registry]
            [atlas.query :as query]))

(use-fixtures :each
  (fn [f]
    (reset! registry/registry {})
    (f)
    (reset! registry/registry {})))

;; =============================================================================
;; KEY NORMALIZATION TESTS
;; =============================================================================

(deftest test-simple-key-conversion
  (testing "Simple key becomes dev-id with #{dev-id} as aspects"
    (let [result (ig-adapter/integrant-key->atlas-def :component/logger {})]
      (is (= :component/logger (:atlas/dev-id result)))
      (is (= #{:component/logger} (:atlas/aspects result)))
      (is (= :atlas/structure-component (:atlas/type result)))
      (is (= #{} (:structure-component/deps result))))))

(deftest test-composite-key-conversion
  (testing "Composite key: dev-id = base (first element), aspects = both"
    (let [result (ig-adapter/integrant-key->atlas-def
                  [:persistence/base :accounts/db]
                  {})]
      (is (= :persistence/base (:atlas/dev-id result)))
      (is (= #{:persistence/base :accounts/db} (:atlas/aspects result)))
      ;; Composite key vector is stored as metadata
      (is (= [:persistence/base :accounts/db] (:integrant/composite-key result)))))

  (testing "Three-element composite key uses first element"
    (let [result (ig-adapter/integrant-key->atlas-def
                  [:a/base :b/middle :c/specific]
                  {})]
      (is (= :a/base (:atlas/dev-id result)))
      (is (= #{:a/base :b/middle :c/specific} (:atlas/aspects result)))
      ;; Composite key vector is stored
      (is (= [:a/base :b/middle :c/specific] (:integrant/composite-key result)))))

  (testing "Simple key has no composite-key metadata"
    (let [result (ig-adapter/integrant-key->atlas-def
                  :component/logger
                  {})]
      (is (= :component/logger (:atlas/dev-id result)))
      (is (nil? (:integrant/composite-key result))))))

;; =============================================================================
;; DEPENDENCY EXTRACTION TESTS
;; =============================================================================

(deftest test-dependency-extraction
  (testing "Extracts {:key :x} references as dependencies"
    (let [result (ig-adapter/integrant-key->atlas-def
                  :service/users
                  {:db {:key :component/db}
                   :cache {:key :component/cache}})]
      (is (= #{:component/db :component/cache}
             (:structure-component/deps result)))))

  (testing "Extracts nested {:key :x} references"
    (let [result (ig-adapter/integrant-key->atlas-def
                  :service/orders
                  {:config {:persistence {:key :component/db}}
                   :logging {:key :component/logger}})]
      (is (= #{:component/db :component/logger}
             (:structure-component/deps result)))))

  (testing "Extracts {:key :x} from vectors"
    (let [result (ig-adapter/integrant-key->atlas-def
                  :service/multi
                  {:deps [{:key :component/a}
                          {:key :component/b}]})]
      (is (= #{:component/a :component/b}
             (:structure-component/deps result)))))

  (testing "Ignores non-reference maps"
    (let [result (ig-adapter/integrant-key->atlas-def
                  :service/config
                  {:port 8080
                   :host "localhost"
                   :key "not-a-ref"
                   :db {:key :component/db}})]
      (is (= #{:component/db}
             (:structure-component/deps result))))))

;; =============================================================================
;; IDENTITIES DICTIONARY TESTS
;; =============================================================================

(deftest test-identities-dictionary
  (testing "Custom identity overrides default for simple key"
    (let [result (ig-adapter/integrant-key->atlas-def
                  :component/logger
                  {}
                  {:identities {:component/logger #{:domain/logging :tier/foundation}}})]
      (is (= :component/logger (:atlas/dev-id result)))
      (is (= #{:domain/logging :tier/foundation} (:atlas/aspects result)))))

  (testing "Custom identity overrides composite key identity"
    (let [result (ig-adapter/integrant-key->atlas-def
                  [:persistence/base :accounts/db]
                  {}
                  {:identities {:persistence/base #{:domain/accounts :tier/foundation :storage/postgres}}})]
      (is (= :persistence/base (:atlas/dev-id result)))
      (is (= #{:domain/accounts :tier/foundation :storage/postgres}
             (:atlas/aspects result)))))

  (testing "Falls back to composite identity when not in dictionary"
    (let [result (ig-adapter/integrant-key->atlas-def
                  [:persistence/base :orders/db]
                  {}
                  {:identities {:cache/base #{:domain/cache}}})]
      (is (= :persistence/base (:atlas/dev-id result)))
      (is (= #{:persistence/base :orders/db} (:atlas/aspects result))))))

;; =============================================================================
;; CONFIG CONVERSION TESTS
;; =============================================================================

(deftest test-config->atlas-defs
  (testing "Converts full config to atlas definitions"
    (let [config {:component/logger {}
                  :component/db {:logger {:key :component/logger}}
                  :service/users {:db {:key :component/db}
                                  :logger {:key :component/logger}}}
          defs (ig-adapter/config->atlas-defs config)
          by-id (into {} (map (juxt :atlas/dev-id identity) defs))]
      (is (= 3 (count defs)))
      (is (= #{:component/logger} (:atlas/aspects (by-id :component/logger))))
      (is (= #{:component/logger} (:structure-component/deps (by-id :component/db))))
      (is (= #{:component/db :component/logger}
             (:structure-component/deps (by-id :service/users))))))

  (testing "Handles mixed simple and composite keys"
    (let [config {:component/logger {}
                  [:persistence/base :accounts/db] {:logger {:key :component/logger}}
                  [:cache/base :orders/cache] {:logger {:key :component/logger}}}
          defs (ig-adapter/config->atlas-defs config)
          by-id (into {} (map (juxt :atlas/dev-id identity) defs))]
      (is (= 3 (count defs)))
      (is (= #{:component/logger} (:atlas/aspects (by-id :component/logger))))
      ;; Composite keys use first element as dev-id (when not shared)
      (is (= #{:persistence/base :accounts/db} (:atlas/aspects (by-id :persistence/base))))
      (is (= #{:cache/base :orders/cache} (:atlas/aspects (by-id :cache/base)))))))

(deftest test-config->atlas-defs-options
  (testing ":filter-fn filters components"
    (let [config {:component/logger {}
                  :component/db {}
                  :service/users {}}
          defs (ig-adapter/config->atlas-defs
                config
                {:filter-fn (fn [k _] (= "component" (namespace k)))})]
      (is (= 2 (count defs)))
      (is (every? #(= "component" (namespace (:atlas/dev-id %))) defs))))

  (testing ":include-config? includes original config"
    (let [config {:component/db {:port 5432 :host "localhost"}}
          defs (ig-adapter/config->atlas-defs config {:include-config? true})
          def (first defs)]
      (is (= {:port 5432 :host "localhost"} (:integrant/config def)))))

  (testing ":identities applies to all matching keys"
    (let [config {:component/logger {}
                  :component/db {}
                  :service/users {}}
          defs (ig-adapter/config->atlas-defs
                config
                {:identities {:component/logger #{:domain/logging}
                              :service/users #{:domain/users :tier/service}}})
          by-id (into {} (map (juxt :atlas/dev-id identity) defs))]
      (is (= #{:domain/logging} (:atlas/aspects (by-id :component/logger))))
      (is (= #{:component/db} (:atlas/aspects (by-id :component/db))))
      (is (= #{:domain/users :tier/service} (:atlas/aspects (by-id :service/users)))))))

;; =============================================================================
;; REGISTRATION TESTS
;; =============================================================================

(deftest test-register-config!
  (testing "Registers all components to registry"
    (let [config {:component/logger {}
                  :component/db {:logger {:key :component/logger}}
                  :service/users {:db {:key :component/db}}}
          registered-count (ig-adapter/register-config! config)]
      (is (= 3 registered-count))
      (is (= 3 (count @registry/registry)))
      ;; Registry is keyed by compound-identity, use query to find by dev-id
      (is (some? (query/find-by-dev-id @registry/registry :component/logger)))
      (is (some? (query/find-by-dev-id @registry/registry :component/db)))
      (is (some? (query/find-by-dev-id @registry/registry :service/users)))))

  (testing "Registered entities have correct structure"
    (reset! registry/registry {})
    (ig-adapter/register-config!
     {:service/users {:db {:key :component/db}}}
     {:identities {:service/users #{:domain/users :tier/service}}})
    (let [[identity entity] (query/find-by-dev-id @registry/registry :service/users)]
      (is (some? entity))
      (is (contains? identity :domain/users))
      (is (contains? identity :tier/service))
      (is (= :atlas/structure-component (:atlas/type entity)))))

  (testing "Composite keys are registered with composite-key metadata"
    (reset! registry/registry {})
    (ig-adapter/register-config!
     {[:persistence/base :accounts/db] {:pool-size 5}})
    (let [[identity entity] (query/find-by-dev-id @registry/registry :persistence/base)]
      (is (some? entity))
      (is (contains? identity :persistence/base))
      (is (contains? identity :accounts/db))
      ;; The composite key vector is stored in the entity
      (is (= [:persistence/base :accounts/db] (:integrant/composite-key entity))))))

;; =============================================================================
;; ANALYSIS HELPER TESTS
;; =============================================================================

(deftest test-dependency-graph
  (testing "Builds correct dependency graph"
    (let [config {:component/logger {}
                  :component/db {:logger {:key :component/logger}}
                  :service/users {:db {:key :component/db}
                                  :logger {:key :component/logger}}}
          {:keys [nodes edges]} (ig-adapter/dependency-graph config)]
      (is (= #{:component/logger :component/db :service/users} nodes))
      (is (= 3 (count edges)))
      (is (some #{[:component/db :component/logger]} edges))
      (is (some #{[:service/users :component/db]} edges))
      (is (some #{[:service/users :component/logger]} edges)))))

(deftest test-find-roots
  (testing "Finds components with no dependents"
    (let [config {:component/logger {}
                  :component/db {:logger {:key :component/logger}}
                  :service/users {:db {:key :component/db}}}
          roots (set (ig-adapter/find-roots config))]
      (is (= #{:service/users} roots))))

  (testing "Multiple roots"
    (let [config {:component/logger {}
                  :service/a {:logger {:key :component/logger}}
                  :service/b {:logger {:key :component/logger}}}
          roots (set (ig-adapter/find-roots config))]
      (is (= #{:service/a :service/b} roots)))))

(deftest test-find-leaves
  (testing "Finds components with no dependencies"
    (let [config {:component/logger {}
                  :component/db {:logger {:key :component/logger}}
                  :service/users {:db {:key :component/db}}}
          leaves (set (ig-adapter/find-leaves config))]
      (is (= #{:component/logger} leaves))))

  (testing "Multiple leaves"
    (let [config {:component/logger {}
                  :component/cache {}
                  :service/users {:logger {:key :component/logger}
                                  :cache {:key :component/cache}}}
          leaves (set (ig-adapter/find-leaves config))]
      (is (= #{:component/logger :component/cache} leaves)))))

(deftest test-summarize-config
  (testing "Produces correct summary"
    (let [config {:component/logger {}
                  :component/cache {}
                  :component/db {:logger {:key :component/logger}}
                  :service/users {:db {:key :component/db}
                                  :cache {:key :component/cache}}}
          summary (ig-adapter/summarize-config config)]
      (is (= 4 (:component-count summary)))
      (is (= #{:service/users} (set (:roots summary))))
      (is (= #{:component/logger :component/cache} (set (:leaves summary)))))))

;; =============================================================================
;; RUNTIME RESOLUTION TESTS
;; =============================================================================

(deftest test-resolve-config
  (testing "Resolves {:key :x} references with component instances"
    (let [db-instance {:type :db :connected true}
          logger-instance {:type :logger}
          components {:component/db db-instance
                      :component/logger logger-instance}
          config {:db {:key :component/db}
                  :logger {:key :component/logger}
                  :timeout 5000}
          resolved (ig-adapter/resolve-config config components)]
      (is (= db-instance (:db resolved)))
      (is (= logger-instance (:logger resolved)))
      (is (= 5000 (:timeout resolved)))))

  (testing "Resolves nested {:key :x} references"
    (let [db-instance {:connected true}
          components {:component/db db-instance}
          config {:persistence {:primary {:key :component/db}}
                  :options {:retry true}}
          resolved (ig-adapter/resolve-config config components)]
      (is (= db-instance (get-in resolved [:persistence :primary])))
      (is (= true (get-in resolved [:options :retry])))))

  (testing "Resolves {:key :x} in vectors"
    (let [a-instance {:name :a}
          b-instance {:name :b}
          components {:component/a a-instance :component/b b-instance}
          config {:deps [{:key :component/a} {:key :component/b}]}
          resolved (ig-adapter/resolve-config config components)]
      (is (= [a-instance b-instance] (:deps resolved)))))

  (testing "Returns nil for missing components"
    (let [config {:db {:key :component/db}}
          resolved (ig-adapter/resolve-config config {})]
      (is (nil? (:db resolved)))))

  (testing "Preserves non-reference values"
    (let [config {:port 8080
                  :host "localhost"
                  :key "api-key"
                  :nested {:value 42}}
          resolved (ig-adapter/resolve-config config {})]
      (is (= config resolved)))))

(deftest test-build-init-config
  (testing "Builds init config by resolving refs from integrant config"
    (let [ig-config {:component/db {:pool-size 10}
                     :component/logger {}
                     :service/users {:db {:key :component/db}
                                     :logger {:key :component/logger}
                                     :timeout 5000}}
          db-instance {:type :db :pool-size 10}
          logger-instance {:type :logger}
          running {:component/db db-instance
                   :component/logger logger-instance}
          init-config (ig-adapter/build-init-config ig-config :service/users running)]
      (is (= db-instance (:db init-config)))
      (is (= logger-instance (:logger init-config)))
      (is (= 5000 (:timeout init-config)))))

  (testing "Works with composite keys"
    (let [ig-config {[:persistence/base :accounts/db] {:pool-size 5}
                     :service/accounts {:db {:key :persistence/base}}}
          db-instance {:type :accounts-db}
          running {:persistence/base db-instance}
          init-config (ig-adapter/build-init-config
                       ig-config :service/accounts running)]
      (is (= db-instance (:db init-config)))))

  (testing "Handles component with no deps"
    (let [ig-config {:component/logger {:level :info :format :json}}
          init-config (ig-adapter/build-init-config ig-config :component/logger {})]
      (is (= {:level :info :format :json} init-config))))

  (testing "Handles deeply nested refs"
    (let [ig-config {:service/complex
                     {:persistence {:primary {:key :component/db}}
                      :options {:cache {:key :component/cache}}
                      :timeout 1000}}
          running {:component/db {:type :db}
                   :component/cache {:type :cache}}
          init-config (ig-adapter/build-init-config
                       ig-config :service/complex running)]
      (is (= {:type :db} (get-in init-config [:persistence :primary])))
      (is (= {:type :cache} (get-in init-config [:options :cache])))
      (is (= 1000 (:timeout init-config)))))

  (testing "Returns nil for missing integrant key"
    (let [ig-config {:component/db {}}
          init-config (ig-adapter/build-init-config ig-config :missing/key {})]
      (is (nil? init-config)))))

(deftest test-deps->ns-map
  (testing "Builds namespaced map from deps"
    (let [db-instance {:type :db}
          logger-instance {:type :logger}
          components {:component/db db-instance
                      :component/logger logger-instance}
          atlas-def {:structure-component/deps #{:component/db :component/logger}}
          ns-map (ig-adapter/deps->ns-map atlas-def components)]
      (is (= db-instance (:component/db ns-map)))
      (is (= logger-instance (:component/logger ns-map)))
      (is (= 2 (count ns-map)))))

  (testing "Works with destructuring"
    (let [db-instance {:type :db}
          logger-instance {:type :logger}
          components {:component/db db-instance
                      :component/logger logger-instance}
          atlas-def {:structure-component/deps #{:component/db :component/logger}}
          {:component/keys [db logger]} (ig-adapter/deps->ns-map atlas-def components)]
      (is (= db-instance db))
      (is (= logger-instance logger))))

  (testing "Returns nil for missing components"
    (let [atlas-def {:structure-component/deps #{:component/db}}
          ns-map (ig-adapter/deps->ns-map atlas-def {})]
      (is (contains? ns-map :component/db))
      (is (nil? (:component/db ns-map)))))

  (testing "Handles empty deps"
    (let [atlas-def {:structure-component/deps #{}}
          ns-map (ig-adapter/deps->ns-map atlas-def {:component/db {}})]
      (is (= {} ns-map))))

  (testing "Only includes declared deps"
    (let [components {:component/db {} :component/logger {} :component/cache {}}
          atlas-def {:structure-component/deps #{:component/db}}
          ns-map (ig-adapter/deps->ns-map atlas-def components)]
      (is (= 1 (count ns-map)))
      (is (contains? ns-map :component/db))
      (is (not (contains? ns-map :component/logger))))))

;; Helper to access private fn for tests
(def ^:private normalize-integrant-key @#'ig-adapter/normalize-integrant-key)
(def ^:private find-shared-bases @#'ig-adapter/find-shared-bases)
(def ^:private adapted-dev-id @#'ig-adapter/adapted-dev-id)

;; =============================================================================
;; SHARED BASE DETECTION TESTS
;; =============================================================================

(deftest test-find-shared-bases
  (testing "Detects when multiple composite keys share the same base (first element)"
    (let [config {[:co.yorba.services.search.inbox/update-search-status
                   :accounts-by-email.spec.workers.logins-by-inbox/success] {}
                  [:co.yorba.services.search.inbox/update-search-status
                   :accounts-by-email.spec.workers.gmail-threads/success] {}
                  [:co.yorba.services.search.inbox/reset-failed-state
                   :accounts-by-email.spec.workers.logins-by-inbox/success] {}
                  [:co.yorba.services.search.inbox/reset-failed-state
                   :accounts-by-email.spec.workers.gmail-threads/success] {}
                  [:co.yorba.services.search.logins/schedule-periodical
                   :accounts-by-email.spec.workers.logins-by-inbox/success] {}}
          shared (find-shared-bases config)]
      ;; :co.yorba.services.search.inbox/update-search-status is shared by 2 keys
      (is (contains? shared :co.yorba.services.search.inbox/update-search-status))
      ;; :co.yorba.services.search.inbox/reset-failed-state is shared by 2 keys
      (is (contains? shared :co.yorba.services.search.inbox/reset-failed-state))
      ;; :co.yorba.services.search.logins/schedule-periodical is not shared
      (is (not (contains? shared :co.yorba.services.search.logins/schedule-periodical)))))

  (testing "Returns empty set when no bases are shared"
    (let [config {[:persistence/base :accounts/db] {}
                  [:cache/base :orders/cache] {}
                  :component/logger {}}
          shared (find-shared-bases config)]
      (is (empty? shared))))

  (testing "Detects shared first elements"
    (let [config {[:persistence/base :accounts/db] {}
                  [:persistence/base :orders/db] {}
                  :component/logger {}}
          shared (find-shared-bases config)]
      ;; :persistence/base is shared by 2 keys
      (is (contains? shared :persistence/base))))

  (testing "Handles config with only simple keys"
    (let [config {:component/logger {}
                  :component/db {}
                  :service/users {}}
          shared (find-shared-bases config)]
      (is (empty? shared)))))

(deftest test-adapted-dev-id
  (testing "Creates adapted dev-id with ___ separator"
    (is (= :foo.zz___bar/yy
           (adapted-dev-id [:foo/zz :bar/yy])))
    (is (= :co.yorba.services.search.logins.schedule-periodical___accounts-by-email.spec.workers.logins-by-inbox/success
           (adapted-dev-id [:co.yorba.services.search.logins/schedule-periodical
                            :accounts-by-email.spec.workers.logins-by-inbox/success])))))

(deftest test-shared-base-conversion
  (testing "Composite keys with shared bases (first element) get adapted dev-ids"
    (let [config {[:co.yorba.services.search.inbox/update-search-status
                   :accounts-by-email.spec.workers.logins-by-inbox/success] {}
                  [:co.yorba.services.search.inbox/update-search-status
                   :accounts-by-email.spec.workers.gmail-threads/success] {}
                  [:co.yorba.services.search.logins/schedule-periodical
                   :accounts-by-email.spec.workers.logins-by-inbox/success] {}}
          defs (ig-adapter/config->atlas-defs config)
          dev-ids (set (map :atlas/dev-id defs))]
      ;; update-search-status is shared, so both get adapted dev-ids
      (is (contains? dev-ids :co.yorba.services.search.inbox.update-search-status___accounts-by-email.spec.workers.logins-by-inbox/success))
      (is (contains? dev-ids :co.yorba.services.search.inbox.update-search-status___accounts-by-email.spec.workers.gmail-threads/success))
      ;; schedule-periodical is unique, so uses first element as dev-id
      (is (contains? dev-ids :co.yorba.services.search.logins/schedule-periodical))))

  (testing "Composite keys with unique bases use first element as dev-id"
    (let [config {[:persistence/base :accounts/db] {}
                  [:cache/base :orders/cache] {}}
          defs (ig-adapter/config->atlas-defs config)
          dev-ids (set (map :atlas/dev-id defs))]
      ;; Each has unique first element, so use first element as dev-id
      (is (contains? dev-ids :persistence/base))
      (is (contains? dev-ids :cache/base))))

  (testing "Mixed: some shared, some unique"
    (let [config {[:shared/base :variant1/a] {}
                  [:shared/base :variant2/b] {}
                  [:unique/base :variant3/c] {}}
          defs (ig-adapter/config->atlas-defs config)
          dev-ids (set (map :atlas/dev-id defs))]
      ;; Shared first element gets adapted dev-ids
      (is (contains? dev-ids :shared.base___variant1/a))
      (is (contains? dev-ids :shared.base___variant2/b))
      ;; Unique first element uses it as dev-id
      (is (contains? dev-ids :unique/base)))))

;; =============================================================================
;; TOPOLOGICAL SORT TESTS
;; =============================================================================

(deftest test-topo-sort
  (testing "Sorts nodes with dependencies first"
    (let [nodes #{:a :b :c}
          edges [[:a :b] [:b :c]]  ;; a->b->c (a depends on b, b depends on c)
          sorted (ig-adapter/topo-sort nodes edges)]
      ;; c has no deps, then b, then a
      (is (= [:c :b :a] sorted))))

  (testing "Handles diamond dependency"
    (let [nodes #{:a :b :c :d}
          edges [[:a :b] [:a :c] [:b :d] [:c :d]]
          sorted (ig-adapter/topo-sort nodes edges)]
      ;; d first (no deps), then b and c (depend on d), then a (depends on b and c)
      (is (= :d (first sorted)))
      (is (= :a (last sorted)))))

  (testing "Handles independent nodes"
    (let [nodes #{:a :b :c}
          edges []
          sorted (ig-adapter/topo-sort nodes edges)]
      (is (= 3 (count sorted)))
      (is (= nodes (set sorted)))))

  (testing "Handles single node"
    (let [sorted (ig-adapter/topo-sort #{:a} [])]
      (is (= [:a] sorted)))))

;; =============================================================================
;; SYSTEM LIFECYCLE TESTS
;; =============================================================================

(deftest test-init-order
  (testing "Returns components in dependency order"
    (let [ig-config {:component/logger {}
                     :component/db {:logger {:key :component/logger}}
                     :service/users {:db {:key :component/db}
                                     :logger {:key :component/logger}}}
          order (ig-adapter/init-order ig-config)]
      ;; logger first (no deps), then db, then users
      (is (= :component/logger (first order)))
      (is (= :service/users (last order)))
      (is (some #{:component/db} (butlast order)))))

  (testing "Handles composite keys"
    (let [ig-config {:component/logger {}
                     [:persistence/base :accounts/db] {:logger {:key :component/logger}}}
          order (ig-adapter/init-order ig-config)]
      (is (= :component/logger (first order)))
      (is (= [:persistence/base :accounts/db] (last order))))))

(deftest test-halt-order
  (testing "Returns components in reverse dependency order"
    (let [ig-config {:component/logger {}
                     :component/db {:logger {:key :component/logger}}
                     :service/users {:db {:key :component/db}}}
          order (ig-adapter/halt-order ig-config)]
      ;; users first (most dependent), then db, then logger
      (is (= :service/users (first order)))
      (is (= :component/logger (last order))))))

(deftest test-start-system
  (testing "Starts all components in order with resolved deps"
    (let [ig-config {:component/logger {:level :info}
                     :component/db {:logger {:key :component/logger}
                                    :pool-size 5}
                     :service/users {:db {:key :component/db}
                                     :timeout 1000}}
          init-calls (atom [])
          mock-init (fn [ig-key config]
                      (swap! init-calls conj {:key ig-key :config config})
                      {:type (normalize-integrant-key ig-key)
                       :config config})
          running (ig-adapter/start-system ig-config mock-init)]
      ;; All components started
      (is (= 3 (count @init-calls)))
      (is (= 3 (count running)))

      ;; Started in correct order
      (is (= :component/logger (:key (first @init-calls))))
      (is (= :service/users (:key (last @init-calls))))

      ;; Dependencies were resolved
      (let [users-call (last @init-calls)
            users-config (:config users-call)]
        (is (= 1000 (:timeout users-config)))
        (is (map? (:db users-config)))
        (is (= :component/db (:type (:db users-config))))))))

(deftest test-stop-system
  (testing "Stops all components in reverse order"
    (let [ig-config {:component/logger {}
                     :component/db {:logger {:key :component/logger}}
                     :service/users {:db {:key :component/db}}}
          running {:component/logger {:type :logger}
                   :component/db {:type :db}
                   :service/users {:type :users}}
          halt-calls (atom [])
          mock-halt (fn [ig-key instance]
                      (swap! halt-calls conj {:key ig-key :instance instance}))]
      (ig-adapter/stop-system ig-config running mock-halt)

      ;; All components stopped
      (is (= 3 (count @halt-calls)))

      ;; Stopped in reverse order (dependents first)
      (is (= :service/users (:key (first @halt-calls))))
      (is (= :component/logger (:key (last @halt-calls))))))

  (testing "Skips missing components gracefully"
    (let [ig-config {:component/logger {}
                     :component/db {:logger {:key :component/logger}}}
          running {:component/logger {:type :logger}}  ;; db not running
          halt-calls (atom [])
          mock-halt (fn [ig-key instance]
                      (swap! halt-calls conj ig-key))]
      (ig-adapter/stop-system ig-config running mock-halt)
      ;; Only logger stopped
      (is (= [:component/logger] @halt-calls)))))

;; =============================================================================
;; EDGE CASES
;; =============================================================================

(deftest test-empty-config
  (testing "Handles empty config"
    (let [defs (ig-adapter/config->atlas-defs {})]
      (is (= [] defs)))
    (let [count (ig-adapter/register-config! {})]
      (is (= 0 count)))))

(deftest test-self-referential-not-extracted
  (testing "Does not extract non-keyword :key values"
    (let [result (ig-adapter/integrant-key->atlas-def
                  :component/config
                  {:key "string-value"
                   :another {:key 123}})]
      (is (= #{} (:structure-component/deps result))))))

(deftest test-deeply-nested-deps
  (testing "Extracts deeply nested dependencies"
    (let [result (ig-adapter/integrant-key->atlas-def
                  :service/complex
                  {:level1
                   {:level2
                    {:level3
                     {:level4 {:key :component/deep}}}}})]
      (is (= #{:component/deep} (:structure-component/deps result))))))
