(ns atlas.llm-ide-test
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [atlas.llm-ide :as llm-ide]
            [atlas.registry :as registry]
            [atlas.ontology :as ontology]
            [atlas.ontology.execution-function :as ef]
            [atlas.ontology.interface-endpoint :as ie]
            [atlas.datalog :as datalog]))

(use-fixtures :each
  (fn [f]
    (reset! registry/registry {})
    (datalog/reset-db-cache!)  ;; Clear datalog cache when registry changes
    (require 'atlas.ontology.execution-function :reload)
    (require 'atlas.ontology.interface-endpoint :reload)
    (require 'atlas.llm-ide :reload)  ;; Re-execute top-level tool registrations
    (f)
    (reset! registry/registry {})
    (datalog/reset-db-cache!)))

;; =============================================================================
;; TEST DATA
;; =============================================================================

(defn setup-test-registry! []
  ;; Reset DB cache since we're adding new entities
  (datalog/reset-db-cache!)
  ;; Foundation - each needs unique identity
  (registry/register!
   :component/db
   :atlas/structure-component
   #{:tier/foundation :domain/users :storage/database}
   {})

  (registry/register!
   :component/cache
   :atlas/structure-component
   #{:tier/foundation :domain/users :storage/cache}
   {})

  ;; Service layer
  (registry/register!
   :fn/get-user
   :atlas/execution-function
   #{:tier/service :domain/users :operation/read}
   {:execution-function/context [:user/id]
    :execution-function/response [:user/data]
    :execution-function/deps #{:component/db}})

  (registry/register!
   :fn/update-user
   :atlas/execution-function
   #{:tier/service :domain/users :operation/update}
   {:execution-function/context [:user/id :user/data]
    :execution-function/response [:user/updated?]
    :execution-function/deps #{:component/db :component/cache}})

  (registry/register!
   :fn/validate-token
   :atlas/execution-function
   #{:tier/service :domain/auth :operation/validate}
   {:execution-function/context [:auth/token]
    :execution-function/response [:auth/valid? :user/id]
    :execution-function/deps #{}})

  ;; API layer
  (registry/register!
   :endpoint/get-user
   :atlas/interface-endpoint
   #{:tier/api :domain/users :http/get}
   {:interface-endpoint/context [:http/path-params :auth/token]
    :interface-endpoint/response [:http/json-response]
    :interface-endpoint/deps #{:fn/get-user :fn/validate-token}})

  (registry/register!
   :endpoint/update-user
   :atlas/interface-endpoint
   #{:tier/api :domain/users :http/put}
   {:interface-endpoint/context [:http/body :auth/token]
    :interface-endpoint/response [:http/json-response]
    :interface-endpoint/deps #{:fn/update-user :fn/validate-token}}))
;; =============================================================================
;; FRONTAL CONTROLLER TESTS
;; =============================================================================

(deftest test-handle-tool-unknown
  (testing "Unknown tool returns error"
    (let [result (llm-ide/handle-tool {:tool/name :unknown/tool :tool/args {}})]
      (is (false? (:success? result)))
      (is (= "Unknown tool" (get-in result [:error :message]))))))

(deftest test-available-tools
  (llm-ide/register-tools!)
  (testing "Lists all available tools from registry"
    (let [tools (llm-ide/available-tools)]
      (is (vector? tools))
      (is (>= (count tools) 13))
      (is (every? :tool/name tools))
      (is (every? :tool/args tools))
      (is (every? :tool/returns tools)))))

;; =============================================================================
;; INTENT TOOL TESTS
;; =============================================================================

(deftest test-blast-radius
  (setup-test-registry!)
  (testing "Calculates downstream impact"
    (let [result (llm-ide/handle-tool
                  {:tool/name :atlas.llm-ide/blast-radius
                   :tool/args {:entity/dev-id-or-set :component/db :query/max-hops 3}})]
      (is (true? (:success? result)))
      (let [data (:data result)]
        (is (vector? (:affected data)))
        (is (set? (:tiers-hit data)))
        (is (set? (:domains-hit data)))
        ;; db is used by fn/get-user and fn/update-user
        (is (some #(= :fn/get-user (:entity %)) (:affected data)))))))

(deftest test-trace-causes
  (setup-test-registry!)
  (testing "Traces upstream dependencies"
    (let [result (llm-ide/handle-tool
                  {:tool/name :atlas.llm-ide/trace-causes
                   :tool/args {:symptom/dev-id :endpoint/get-user :query/max-hops 3}})]
      (is (true? (:success? result)))
      (let [data (:data result)]
        (is (vector? (:upstream data)))
        ;; endpoint depends on fn/get-user which depends on db
        (is (some #(= :fn/get-user (:entity %)) (:upstream data)))))))

(deftest test-change-risk
  (setup-test-registry!)
  (testing "Assesses change risk with defaults"
    (let [result (llm-ide/handle-tool
                  {:tool/name :atlas.llm-ide/change-risk
                   :tool/args {:entity/dev-id-set #{:component/db}}})]
      (is (true? (:success? result)))
      (let [data (:data result)]
        (is (number? (:risk-score data)))
        (is (vector? (:reasons data)))
        ;; db is foundation tier
        (is (some #(= :high-risk-aspect (:type %)) (:reasons data))))))
  (testing "Assesses change risk with custom aspects"
    (let [result (llm-ide/handle-tool
                  {:tool/name :atlas.llm-ide/change-risk
                   :tool/args {:entity/dev-id-set #{:component/db}
                               :risk/centrality-threshold 2
                               :aspect/high-risk :tier/foundation
                               :aspect/domain-namespace "domain"}})]
      (is (true? (:success? result)))
      (let [data (:data result)]
        (is (number? (:risk-score data)))
        (is (vector? (:reasons data)))))))

(deftest test-explain-area
  (setup-test-registry!)
  (testing "Explains area around focus with defaults"
    (let [result (llm-ide/handle-tool
                  {:tool/name :atlas.llm-ide/explain-area
                   :tool/args {:query/focus :domain/users :query/depth 2}})]
      (is (true? (:success? result)))
      (let [data (:data result)]
        (is (number? (:entity-count data)))
        (is (vector? (:entry-points data)))
        (is (vector? (:components data))))))
  (testing "Explains area with custom aspects"
    (let [result (llm-ide/handle-tool
                  {:tool/name :atlas.llm-ide/explain-area
                   :tool/args {:query/focus :domain/users
                               :query/depth 2
                               :aspect/entry-point :tier/api
                               :aspect/key-component :tier/foundation}})]
      (is (true? (:success? result)))
      (let [data (:data result)]
        (is (number? (:entity-count data)))))))

(deftest test-suggest-placement
  (setup-test-registry!)
  (testing "Suggests placement for new code"
    (let [result (llm-ide/handle-tool
                  {:tool/name :atlas.llm-ide/suggest-placement
                   :tool/args {:entity/intended-aspects #{:tier/service :domain/users}}})]
      (is (true? (:success? result)))
      (let [data (:data result)]
        (is (vector? (:similar data)))
        (is (> (count (:similar data)) 0))))))

;; =============================================================================
;; DIAGNOSTIC TOOL TESTS
;; =============================================================================

(deftest test-orphans
  (setup-test-registry!)
  ;; Add an orphan
  (registry/register!
   :fn/orphan
   :atlas/execution-function
   #{:tier/service :domain/orphan}
   {})
  (testing "Finds orphan entities with defaults"
    (let [result (llm-ide/handle-tool
                  {:tool/name :atlas.llm-ide/orphans
                   :tool/args {}})]
      (is (true? (:success? result)))
      (let [data (:data result)]
        (is (vector? (:orphans data)))
        (is (some #(= :fn/orphan (:entity %)) (:orphans data))))))
  (testing "Finds orphan entities with custom exclusion aspect"
    (let [result (llm-ide/handle-tool
                  {:tool/name :atlas.llm-ide/orphans
                   :tool/args {:aspect/exclude-from-orphan-check :tier/api
                               :aspect/type-namespace "atlas"}})]
      (is (true? (:success? result)))
      (let [data (:data result)]
        (is (vector? (:orphans data)))))))

(deftest test-islands
  (setup-test-registry!)
  ;; Add disconnected island - each needs unique identity
  (registry/register!
   :fn/isolated-a
   :atlas/execution-function
   #{:tier/service :domain/isolated :operation/a}
   {:execution-function/deps #{:fn/isolated-b}})
  (registry/register!
   :fn/isolated-b
   :atlas/execution-function
   #{:tier/service :domain/isolated :operation/b}
   {})
  (testing "Finds disconnected clusters"
    (let [result (llm-ide/handle-tool
                  {:tool/name :atlas.llm-ide/islands
                   :tool/args {}})]
      (is (true? (:success? result)))
      (let [data (:data result)]
        (is (vector? (:islands data)))
        (is (> (:count data) 1))))))

(deftest test-bottlenecks
  (setup-test-registry!)
  (testing "Finds bottleneck entities"
    (let [result (llm-ide/handle-tool
                  {:tool/name :atlas.llm-ide/bottlenecks
                   :tool/args {}})]
      (is (true? (:success? result)))
      (let [data (:data result)]
        (is (vector? (:bottlenecks data)))
        ;; fn/get-user is a bottleneck (has both in-degree and out-degree > 0)
        (is (some #(= :fn/get-user (:entity %)) (:bottlenecks data)))))))

(deftest test-aspect-anomalies
  (setup-test-registry!)
  ;; Add entity with sparse aspect
  (registry/register!
   :fn/unique
   :atlas/execution-function
   #{:tier/service :domain/unique-only-used-once}
   {})
  (testing "Finds aspect anomalies"
    (let [result (llm-ide/handle-tool
                  {:tool/name :atlas.llm-ide/aspect-anomalies
                   :tool/args {}})]
      (is (true? (:success? result)))
      (let [data (:data result)]
        (is (vector? (:sparse data)))
        (is (some #(= :domain/unique-only-used-once %) (:sparse data)))))))

;; =============================================================================
;; QUERY TOOL TESTS
;; =============================================================================

(deftest test-by-aspect
  (setup-test-registry!)
  (testing "Queries by aspect"
    (let [result (llm-ide/handle-tool
                  {:tool/name :atlas.llm-ide/by-aspect
                   :tool/args {:query/aspect :domain/users}})]
      (is (true? (:success? result)))
      (let [entities (get-in result [:data :entities])]
        (is (vector? entities))
        (is (some #{:fn/get-user} entities))
        (is (some #{:endpoint/get-user} entities))))))

(deftest test-entity-detail
  (setup-test-registry!)
  (testing "Gets entity details"
    (let [result (llm-ide/handle-tool
                  {:tool/name :atlas.llm-ide/entity-detail
                   :tool/args {:entity/dev-id :fn/get-user}})]
      (is (true? (:success? result)))
      (let [data (:data result)]
        (is (= :fn/get-user (:entity/dev-id data)))))))

;; =============================================================================
;; SELF-REGISTRATION TESTS
;; =============================================================================

(deftest test-tool-self-registration
  (testing "Tools register themselves as execution-functions"
    ;; Tools should be queryable via handle-tool
    (let [result (llm-ide/handle-tool {:tool/name :atlas.llm-ide/by-aspect
                                       :tool/args {:query/aspect :domain/llm-ide}})
          llm-tools (:data result)]
      (is (true? (:success? result)))
      (is (> (count (:entities llm-tools)) 10)))

    ;; Can query by intent
    (let [result (llm-ide/handle-tool {:tool/name :atlas.llm-ide/by-aspect
                                       :tool/args {:query/aspect :intent/trace}})
          trace-tools (:data result)]
      (is (some #{:atlas.llm-ide/blast-radius} (:entities trace-tools))))

    (let [result (llm-ide/handle-tool {:tool/name :atlas.llm-ide/by-aspect
                                       :tool/args {:query/aspect :intent/diagnose}})
          diagnose-tools (:data result)]
      (is (some #{:atlas.llm-ide/orphans} (:entities diagnose-tools))))))

;; =============================================================================
;; LLM CONTEXT TESTS
;; =============================================================================

(deftest test-llm-context
  (setup-test-registry!)
  (llm-ide/register-tools!)
  (testing "Exports full LLM context"
    (let [ctx (llm-ide/llm-context)]
      (is (map? (:atlas/ontology ctx)))
      (is (vector? (:atlas/tools ctx)))
      (is (vector? (:atlas/types ctx)))
      (is (vector? (:atlas/registry ctx))))))

(deftest test-llm-context-compact
  (setup-test-registry!)
  (llm-ide/register-tools!)
  (testing "Exports compact LLM context"
    (let [ctx (llm-ide/llm-context-compact)]
      (is (vector? (:atlas/tools ctx)))
      (is (vector? (:atlas/types ctx)))
      (is (number? (:atlas/entity-count ctx))))))
