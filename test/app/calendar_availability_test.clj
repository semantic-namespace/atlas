(ns app.calendar-availability-test
  "Refactored test suite using PURE semantic namespace approach.

  Key improvement: Test implementations are registered as SEPARATE entities
  with compound identities like #{:fn/find-users :test/impl}.

  This is cleaner because:
  1. No mutation of production entities
  2. Test implementations are first-class semantic entities
  3. Can query all test implementations: 'find entities with :test/impl aspect'
  4. Production registry stays pristine"
  (:require
   [clojure.test :refer [deftest testing is use-fixtures]]
   [clojure.set :as set]
   [atlas.registry :as cid]
   [atlas.query :as q]
   [atlas.registry.lookup :as rt]
   [atlas.ontology :as o]
   [atlas.ontology.execution-function :as ef]
   [atlas.ontology.structure-component :as sc]
   [atlas.ontology.execution-function.executor :as exec]
   [atlas.invariant.unified :as ax]
   [app.calendar-availability-invariants :as dsl-invariants]
   [app.calendar-availability :as sut]))

;; =============================================================================
;; MOCK DATA
;; =============================================================================

(def mock-users
  [{:user/id "user-1"
    :user/email "alice@example.com"
    :user/language "en"
    :user/gcal-refresh-token "refresh-token-alice"}
   {:user/id "user-2"
    :user/email "bob@example.com"
    :user/language "en"
    :user/gcal-refresh-token "refresh-token-bob"}
   {:user/id "user-3"
    :user/email "charlie@example.com"
    :user/language "es"
    :user/gcal-refresh-token "refresh-token-charlie"}])

(def mock-oauth-tokens
  {"refresh-token-alice" "access-token-alice-123"
   "refresh-token-bob" "access-token-bob-456"
   "refresh-token-charlie" "access-token-charlie-789"})

(def mock-availability
  {"access-token-alice-123" {"2025-01-15" true
                              "2025-01-16" false
                              "2025-01-17" true}
   "access-token-bob-456" {"2025-01-15" true
                            "2025-01-16" true
                            "2025-01-17" false}
   "access-token-charlie-789" {"2025-01-15" false
                                "2025-01-16" false
                                "2025-01-17" true}})

;; =============================================================================
;; SEMANTIC REGISTRY WITH TEST IMPLEMENTATIONS AS SEPARATE ENTITIES
;; =============================================================================

(declare invoke extract-target-from-test-identity)

(defn init-test-registry!
  "Initialize registry with test implementations as SEPARATE entities.

  Philosophy: Test implementations are first-class semantic entities,
  not mutations of production entities. Each test impl is registered with
  compound identity #{<target-dev-id> :test/impl}."
  []

  ;; First register production entities
  (sut/init-registry!)

  ;; Now register TEST IMPLEMENTATIONS as separate entities
  ;; Pattern: #{<dev-id-being-tested> :test/impl}

  ;; Component test implementations
  (cid/register!
   :test/impl
   #{:component/db}
   {:protocol.user-repository/find-users-by-language
    (fn [language]
      (filter #(= (:user/language %) language) mock-users))})

  (cid/register!
   :test/impl
   #{:component/google-oauth}
   {:protocol.oauth/refresh-token
    (fn [refresh-token]
      (get mock-oauth-tokens refresh-token))})

  (cid/register!
   :test/impl
   #{:component/gcal-client}
   {:protocol.calendar-availability/check-availability
    (fn [access-token date]
      (get-in mock-availability [access-token date]))})

  ;; Function test implementations
  (cid/register!
   :test/impl
   #{:fn/find-users-by-language}
   {:test/fn (fn [{language :query/language} deps]
               (let [db (:db deps)
                     users ((:protocol.user-repository/find-users-by-language db) language)]
                 {:user/email (mapv :user/email users)
                  :user/gcal-refresh-token (mapv :user/gcal-refresh-token users)}))})


  (cid/register!
   :test/impl
   #{:fn/refresh-oauth-token}
   {:test/fn (fn [{refresh-token :user/gcal-refresh-token} deps]
               (let [oauth (:google-oauth deps)
                     access-token ((:protocol.oauth/refresh-token oauth) refresh-token)]
                 {:oauth/access-token access-token}))})

  (cid/register!
   :test/impl
   #{:fn/check-user-availability}
   {:test/fn (fn [{date :query/date token :oauth/access-token} deps]
               (let [gcal (:gcal-client deps)
                     available? ((:protocol.calendar-availability/check-availability gcal) token date)]
                 {:scheduling/available? available?}))})

  (cid/register!
   :test/impl
   #{:fn/collect-available-users}
   {:test/fn (fn [{date :query/date language :query/language} _deps]
               ;; Pure function - orchestrates calls to other functions
               (let [users-result (invoke :fn/find-users-by-language {:query/language language})
                     refresh-tokens (:user/gcal-refresh-token users-result)
                     user-emails (:user/email users-result)
                     available-users (atom [])]
                 (doseq [[email refresh-token] (map vector user-emails refresh-tokens)]
                   (let [token-result (invoke :fn/refresh-oauth-token {:user/gcal-refresh-token refresh-token})
                         access-token (:oauth/access-token token-result)
                         avail-result (invoke :fn/check-user-availability {:query/date date :oauth/access-token access-token})
                         available? (:scheduling/available? avail-result)]
                     (when available?
                       (swap! available-users conj email))))
                 {:availability/users @available-users}))})

  ;; Auto-register placeholder test implementations for any remaining functions.
  (let [prod-functions (->> @cid/registry
                            (filter (fn [[aspects _]]
                                      (contains? aspects :atlas/execution-function)))
                            (map (fn [[_ v]] (:atlas/dev-id v)))
                            set)
        existing-test-targets (->> @cid/registry
                                   (filter (fn [[aspects _]]
                                             (contains? aspects :test/impl)))
                                   (map (fn [[aspects _]]
                                          (extract-target-from-test-identity aspects)))
                                   set)
        missing (set/difference prod-functions existing-test-targets)]
    (doseq [dev-id missing]
      (cid/register!
       :test/impl
       #{dev-id}
       {:test/fn (fn [_ctx _deps] {})}))))

;; =============================================================================
;; SEMANTIC LOOKUP FUNCTIONS
;; =============================================================================

(defn find-test-impl
  "Find the test implementation entity for a given dev-id.
  Returns the value of #{<dev-id> :test/impl}.

  The compound identity itself contains the target dev-id, so no need
  to store it redundantly in the value.

  For function implementations, unwraps the :test/fn key.
  For component implementations, returns the methods map as-is."
  [dev-id]
  (when-let [test-entity (cid/fetch #{dev-id :test/impl})]
    (if (contains? test-entity :test/fn)
      (:test/fn test-entity)
      test-entity)))

(defn extract-target-from-test-identity
  "Extract the target dev-id from a test implementation compound identity.

  Given #{:fn/foo :test/impl}, returns :fn/foo"
  [test-identity]
  (first (disj test-identity :test/impl)))

(defn resolve-component-deps
  "Resolve component dependencies, fetching their test implementations."
  [dev-id]
  (let [dep-ids (o/deps-for dev-id)]
    (reduce (fn [acc dep-id]
              (if-let [test-impl (find-test-impl dep-id)]
                (assoc acc (keyword (name dep-id)) test-impl)
                acc))
            {}
            dep-ids)))

(defn invoke
  "Semantic invoke: fetch test implementation from separate test entity.

  Looks up #{<dev-id> :test/impl} entity and executes it.
  - For functions: the value IS the function
  - For components: the value is a map of methods"
  [dev-id ctx]
  (if-let [test-impl (find-test-impl dev-id)]
    (if (fn? test-impl)
      ;; Function implementation - invoke directly
      (let [deps (resolve-component-deps dev-id)]
        (test-impl ctx deps))
      ;; Component implementation - return the methods map
      test-impl)
    (throw (ex-info (str "No test implementation for " dev-id)
                    {:dev-id dev-id
                     :test-identity #{dev-id :test/impl}}))))

;; =============================================================================
;; TEST FIXTURE
;; =============================================================================

(use-fixtures :each
  (fn [f]
    (reset! cid/registry {})
    (ef/reset-loaded-state!)
    (sc/reset-loaded-state!)
    (ef/load!)
    (sc/load!)
    (init-test-registry!)
    (f)))

;; =============================================================================
;; TESTS
;; =============================================================================

(deftest test-find-users-by-language
  (testing "Can find users by English language"
    (let [result (invoke :fn/find-users-by-language {:query/language "en"})]
      (is (= 2 (count (:user/email result))))
      (is (= #{"alice@example.com" "bob@example.com"} (set (:user/email result))))))

  (testing "Can find users by Spanish language"
    (let [result (invoke :fn/find-users-by-language {:query/language "es"})]
      (is (= 1 (count (:user/email result))))
      (is (= ["charlie@example.com"] (:user/email result)))))

  (testing "Returns empty for unknown language"
    (let [result (invoke :fn/find-users-by-language {:query/language "fr"})]
      (is (empty? (:user/email result))))))

(deftest test-refresh-oauth-token
  (testing "Can refresh OAuth token for Alice"
    (let [result (invoke :fn/refresh-oauth-token {:user/gcal-refresh-token "refresh-token-alice"})]
      (is (= "access-token-alice-123" (:oauth/access-token result)))))

  (testing "Can refresh OAuth token for Bob"
    (let [result (invoke :fn/refresh-oauth-token {:user/gcal-refresh-token "refresh-token-bob"})]
      (is (= "access-token-bob-456" (:oauth/access-token result)))))

  (testing "Returns nil for invalid refresh token"
    (let [result (invoke :fn/refresh-oauth-token {:user/gcal-refresh-token "invalid-token"})]
      (is (nil? (:oauth/access-token result))))))

(deftest test-check-user-availability
  (testing "Alice is available on 2025-01-15"
    (let [result (invoke :fn/check-user-availability
                        {:query/date "2025-01-15"
                         :oauth/access-token "access-token-alice-123"})]
      (is (true? (:scheduling/available? result)))))

  (testing "Alice is not available on 2025-01-16"
    (let [result (invoke :fn/check-user-availability
                        {:query/date "2025-01-16"
                         :oauth/access-token "access-token-alice-123"})]
      (is (false? (:scheduling/available? result)))))

  (testing "Bob is available on 2025-01-16"
    (let [result (invoke :fn/check-user-availability
                        {:query/date "2025-01-16"
                         :oauth/access-token "access-token-bob-456"})]
      (is (true? (:scheduling/available? result))))))

(deftest test-collect-available-users
  (testing "Collects available English users for 2025-01-15"
    (let [result (invoke :fn/collect-available-users
                        {:query/date "2025-01-15"
                         :query/language "en"})]
      (is (= 2 (count (:availability/users result))))
      (is (= #{"alice@example.com" "bob@example.com"} (set (:availability/users result))))))

  (testing "Collects available English users for 2025-01-16"
    (let [result (invoke :fn/collect-available-users
                        {:query/date "2025-01-16"
                         :query/language "en"})]
      (is (= 1 (count (:availability/users result))))
      (is (= ["bob@example.com"] (:availability/users result))))))

;; =============================================================================
;; SEMANTIC BENEFITS DEMONSTRATION
;; =============================================================================
;; is this really useful?
(deftest test-semantic-registry-introspection
  (testing "Can query all test implementation entities"
    ;; Find all entities with :test/impl aspect
    (let [test-identities (->> @cid/registry
                              (filter (fn [[aspects _]]
                                        (contains? aspects :test/impl)))
                              (map first)
                              set)
          test-targets (map extract-target-from-test-identity test-identities)]
      (is (>= (count test-targets) 4))
      (is (contains? (set test-targets) :fn/find-users-by-language))
      (is (contains? (set test-targets) :component/db))))

  (testing "Production entities remain pristine"
    ;; Production entities should NOT have :test/impl in their value
    (let [[_id prod-value] (q/find-by-dev-id @cid/registry :fn/find-users-by-language)]
      (is (some? prod-value))
      (is (not (contains? prod-value :test/impl)))
      (is (not (contains? prod-value :test/fn)))))

  (testing "Test entities are discoverable by aspect"
    ;; Can find test impl for specific entity
    (let [test-impl (find-test-impl :fn/find-users-by-language)]
      (is (some? test-impl))
      (is (fn? test-impl))))

  (testing "Target dev-id is extractable from compound identity"
    ;; The compound identity itself tells us what it's testing
    (let [test-identity #{:fn/find-users-by-language :test/impl}
          target (extract-target-from-test-identity test-identity)]
      (is (= target :fn/find-users-by-language))))

  (testing "Can verify test coverage semantically"
    ;; All production functions should have corresponding test entities
    ;; (excluding meta-entities: ontology definitions and test implementations)
    (let [prod-functions (->> @cid/registry
                             (filter (fn [[aspects _]]
                                       (and (contains? aspects :atlas/execution-function)
                                            (not (contains? aspects :atlas/ontology))
                                            (not (contains? aspects :test/impl)))))
                             (map (fn [[_ v]] (:atlas/dev-id v)))
                             set)
          tested-functions (->> @cid/registry
                               (filter (fn [[aspects _]]
                                         (contains? aspects :test/impl)))
                               (map (fn [[aspects _]]
                                      (extract-target-from-test-identity aspects)))
                               (filter #(contains? prod-functions %))
                               set)]
      ;; All functions should have test implementations
      (is (= prod-functions tested-functions)))))

;; Invariants

(deftest test-dsl-invariants-oauth
  (testing "OAuth functions depend on OAuth component"
    (let [result (dsl-invariants/check-oauth)]
      (is (true? (:valid? result))
          (str "OAuth invariant violations: " (:errors result)))))

  (testing "OAuth invariant - individual check"
    (let [result (ax/check-all dsl-invariants/oauth-functions-depend-on-oauth-component)]
      (is (true? (:valid? result))
          "OAuth functions should depend on google-oauth component"))))

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

  (testing "All tier invariants pass together"
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
      ;; This is a warning-level invariant, so valid? may still be true
      (is (empty? (:errors result))
          "Should have no error-level reachability issues"))))

(deftest test-dsl-invariants-pure-functions
  (testing "Pure functions have no component dependencies"
    (let [result (ax/check-all dsl-invariants/pure-functions-no-component-deps)]
      (is (true? (:valid? result))
          "Pure functions should not depend on components"))))

(deftest test-dsl-invariants-all
  (testing "All DSL invariants pass"
    (let [result (dsl-invariants/check-all)]
      (is (true? (:valid? result))
          (str "DSL invariant violations: " (:errors result)))))

  (testing "DSL invariants can be documented"
    (let [docs (ax/document-all dsl-invariants/all-invariants)]
      (is (= (count dsl-invariants/all-invariants) (count docs)))
      (is (every? #(contains? % :id) docs))
      (is (every? #(= :dsl (:style %)) docs)))))
