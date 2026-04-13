(ns atlas.test.llm-ide
  "LLM-IDE tool surface for atlas test-cases.

   Provides three MCP-discoverable tools:

     :atlas.llm-ide/test-run
       Run a single test-case by dev-id, returns the report.

     :atlas.llm-ide/test-run-all
       Run all test-cases (optionally filtered by aspect), returns summary.

     :atlas.llm-ide/test-case-from-trace
       Generate a candidate test-case from an exec-history trace-id.

   Tools self-register on namespace load — require this namespace from your
   dev REPL/system startup to make them discoverable via the registry."
  (:require [atlas.registry :as registry]
            [atlas.test.runner :as runner]))

;; ============================================================================
;; HELPERS
;; ============================================================================

(defn- ensure-keyword
  [x]
  (cond
    (keyword? x) x
    (string? x)  (keyword (cond-> x (= \: (first x)) (subs 1)))
    :else        x))

(defn- ensure-uuid
  [x]
  (cond
    (uuid? x)   x
    (string? x) #?(:clj (java.util.UUID/fromString x)
                   :cljs (uuid x))
    :else        x))

;; ============================================================================
;; TOOL REGISTRATIONS
;; ============================================================================

(registry/register!
 :atlas.llm-ide/test-run
 :atlas/execution-function
 #{:tier/tooling :domain/llm-ide :intent/testing :tool/test-run}
 {:execution-function/context  [:test/case-dev-id :test/ig-config
                                :test/init-key-fn :test/halt-key-fn]
  :execution-function/response [:test/case :test/target :test/pass?
                                :test/trace-id :test/duration-ms
                                :test/burst-size :test/checks-passed
                                :test/checks-failed :test/failed-checks]
  :execution-function/deps     #{}
  :atlas/docs "Run a single :atlas/test-case by dev-id. Pass :test/case-dev-id (required). Optionally pass :test/ig-config (integrant config map) to start real components — otherwise only mocked components are available. Returns a report with :test/pass?, :test/trace-id (for drilldown via exec-history tools), duration, and any failed checks."
  :atlas/impl (fn [{:test/keys [case-dev-id ig-config init-key-fn halt-key-fn]}]
                (runner/run-test
                 {:test/case-dev-id (ensure-keyword case-dev-id)
                  :test/ig-config   ig-config
                  :test/init-key-fn init-key-fn
                  :test/halt-key-fn halt-key-fn}))})

(registry/register!
 :atlas.llm-ide/test-run-all
 :atlas/execution-function
 #{:tier/tooling :domain/llm-ide :intent/testing :tool/test-run-all}
 {:execution-function/context  [:test/aspect :test/ig-config
                                :test/init-key-fn :test/halt-key-fn]
  :execution-function/response [:test/total :test/passed :test/failed
                                :test/reports]
  :execution-function/deps     #{}
  :atlas/docs "Run all :atlas/test-case entities (optionally filtered by :test/aspect). Returns {:test/total n :test/passed n :test/failed n :test/reports [...]}. Each report in :test/reports has the same shape as :atlas.llm-ide/test-run output."
  :atlas/impl (fn [{:test/keys [aspect ig-config init-key-fn halt-key-fn]}]
                (runner/run-all
                 {:test/aspect      (some-> aspect ensure-keyword)
                  :test/ig-config   ig-config
                  :test/init-key-fn init-key-fn
                  :test/halt-key-fn halt-key-fn}))})

(registry/register!
 :atlas.llm-ide/test-case-from-trace
 :atlas/execution-function
 #{:tier/tooling :domain/llm-ide :intent/testing :tool/test-case-from-trace}
 {:execution-function/context  [:query/trace-id :test/dev-id]
  :execution-function/response [:test-case/target :test-case/fixture
                                :test-case/expectations :test/registration-edn]
  :execution-function/deps     #{}
  :atlas/docs "Generate a candidate :atlas/test-case registration from an exec-history trace-id. Inspects the burst to find the entry-point, lifts args into a fixture, and suggests trace expectations. Returns :test/registration-edn — a printable EDN string to review and register. Use this to mechanically convert bug reports or observed executions into regression tests."
  :atlas/impl (fn [{:query/keys [trace-id] :test/keys [dev-id]}]
                (runner/test-case-from-trace
                 {:trace-id    (ensure-uuid trace-id)
                  :test-dev-id (some-> dev-id ensure-keyword)}))})
