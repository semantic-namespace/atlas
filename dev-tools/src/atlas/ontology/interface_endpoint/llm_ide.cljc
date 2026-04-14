(ns atlas.ontology.interface-endpoint.llm-ide
  "LLM-IDE tool surface for testing interface-endpoints in a local dev session.

   Provides two MCP-discoverable tools registered under :domain/llm-ide / :intent/diagnose:

     :atlas.llm-ide/endpoint-test-template
       Generates a test-case registration for a given endpoint.
       Components become mocks, exec-fn deps become trace expectations.
       The generated code registers an :atlas/test-case entity.

     :atlas.llm-ide/endpoint-execute
       Directly runs an endpoint's pipeline in-process using :atlas/impl
       values from the registry. No manual wiring needed.

   Tools self-register on namespace load — require this namespace from your
   dev REPL/system startup to make them discoverable via the registry."
  (:require [atlas.registry :as registry]
            [atlas.registry.lookup :as entity]
            [atlas.ontology :as o]
            [atlas.ontology.execution-function.executor :as executor]
            [clojure.string :as str]))

;; ============================================================================
;; HELPERS
;; ============================================================================

(defn- ensure-keyword
  [x]
  (cond
    (keyword? x) x
    (string? x)  (keyword (cond-> x (= \: (first x)) (subs 1)))
    :else        x))

(defn- entity-type-for [id]
  (when-let [identity (entity/identity-for id)]
    (some #(when (= "atlas" (namespace %)) %) identity)))

(defn- direct-deps
  "Return direct deps of endpoint-id partitioned by entity type.
   Returns {:components [dev-ids...] :exec-fns [dev-ids...]}."
  [endpoint-id]
  (let [deps (o/deps-for endpoint-id)]
    {:components (vec (sort (filter #(= :atlas/structure-component (entity-type-for %))
                                    deps)))
     :exec-fns   (vec (sort (filter #(= :atlas/execution-function (entity-type-for %))
                                    deps)))}))

(defn- impl-map-for
  "Build an impl-map from the atlas registry for an endpoint and its direct
   exec-fn deps. Reads :atlas/impl from each entity's props."
  [endpoint-id]
  (let [{:keys [exec-fns]} (direct-deps endpoint-id)
        all-ids (conj exec-fns endpoint-id)]
    (into {}
          (keep (fn [dev-id]
                  (when-let [impl (:atlas/impl (entity/props-for dev-id))]
                    [dev-id impl])))
          all-ids)))

(defn- context-keys-for-endpoint
  "Return the direct context keys declared by the endpoint."
  [endpoint-id]
  (vec (sort (o/context-for endpoint-id))))

(defn- sample-value-for
  "Return a placeholder sample value for a qualified keyword."
  [k]
  (let [nm (name k)]
    (cond
      (str/includes? nm "id")    (str "<" nm ">")
      (str/includes? nm "token") (str "<" nm ">")
      (str/includes? nm "?")     false
      (str/includes? nm "count") 0
      (str/ends-with? nm "s")    []
      :else                      (str "<" nm ">"))))

(defn- response-keys-for
  "Return the direct response keys declared by the entity."
  [dev-id]
  (vec (sort (o/response-for dev-id))))

(defn- generate-template
  "Generate a test-case registration for endpoint-id.
   Components become mocks, exec-fn deps become trace expectations."
  [endpoint-id]
  (let [{:keys [components exec-fns]} (direct-deps endpoint-id)
        ctx-keys     (context-keys-for-endpoint endpoint-id)
        resp-keys    (response-keys-for endpoint-id)
        test-dev-id  (keyword "test" (name endpoint-id))]
    (str/join
     "\n"
     (remove
      nil?
      [(str ";; Test-case for " endpoint-id)
       ";; Generated from atlas registry — edit fixture values and expectations"
       ""
       "(require '[atlas.registry :as registry])"
       "(require '[atlas.test.runner :as test-runner])"
       ""
       (str "(registry/register! " )
       (str " " test-dev-id)
       " :atlas/test-case"
       " #{:atlas/test-case}  ;; add domain/test aspects as needed"
       ""
       (str " {;; ---- target ----"
            "\n  :test-case/target " endpoint-id)
       ""
       "  ;; ---- fixture (initial context) ----"
       (str "  :test-case/fixture {"
            (str/join "\n                      "
                      (map (fn [k] (str k " " (pr-str (sample-value-for k))))
                           ctx-keys))
            "}")
       ""
       ;; Mocks section
       (if (seq components)
         (str "  ;; ---- mocks (omit to start real components from ig-config) ----\n"
              "  ;; Replace with mock values, or remove to use real subsystem\n"
              "  :test-case/mocks {"
              (str/join "\n                     "
                        (map (fn [c] (str c " nil ;; TODO: mock or remove"))
                             components))
              "}")
         "  ;; No structure-component deps to mock")
       ""
       "  ;; ---- expectations ----"
       "  :test-case/expectations"
       (str "  [{:expectation/kind :result")
       (if (first resp-keys)
         (str "    :expectation/pred (fn [r] (contains? r " (first resp-keys) "))")
         "    :expectation/pred (fn [r] (some? r))")
       "    :expectation/docs \"TODO: describe expected result\"}"
       ;; Trace expectations for exec-fn deps
       (when (seq exec-fns)
         (str/join
          "\n"
          (map (fn [dep]
                 (str "   {:expectation/kind :trace\n"
                      "    :expectation/must-contain " dep "\n"
                      "    :expectation/docs \"Pipeline calls " dep "\"}"))
               exec-fns)))
       "   ]})"
       ""
       ";; ---- run ----"
       (str "(test-runner/run-test {:test/case-dev-id " test-dev-id "})")
       ""
       ";; With integrant config (starts real components minus mocks):"
       (str ";; (test-runner/run-test {:test/case-dev-id " test-dev-id)
       ";;                        :test/ig-config   ig-config"
       ";;                        :test/init-key-fn ig/init-key"
       ";;                        :test/halt-key-fn ig/halt-key!})"])))

  )

;; ============================================================================
;; TOOL REGISTRATIONS
;; ============================================================================

(registry/register!
 :atlas.llm-ide/endpoint-test-template
 :atlas/execution-function
 #{:tier/tooling :domain/llm-ide :intent/diagnose :tool/endpoint-test-template}
 {:execution-function/context  [:entity/dev-id]
  :execution-function/response [:endpoint/template]
  :execution-function/deps     #{}
  :atlas/docs "Generate an :atlas/test-case registration for an interface-endpoint. Pass :entity/dev-id (the endpoint's dev-id keyword). Returns :endpoint/template — a string that registers a test-case entity with fixture, mocks, and expectations. The test can then be run via test-runner/run-test and Kaocha."
  :atlas/impl (fn [{:entity/keys [dev-id]}]
                (let [eid (ensure-keyword dev-id)]
                  {:endpoint/template (generate-template eid)}))})

(registry/register!
 :atlas.llm-ide/endpoint-execute
 :atlas/execution-function
 #{:tier/tooling :domain/llm-ide :intent/diagnose :tool/endpoint-execute}
 {:execution-function/context  [:entity/dev-id :endpoint/context]
  :execution-function/response [:endpoint/result]
  :execution-function/deps     #{}
  :atlas/docs "Execute an interface-endpoint's full pipeline in-process using :atlas/impl values from the registry. Pass :entity/dev-id (the endpoint's dev-id keyword) and :endpoint/context (a map of the required context keys). The impl-map is constructed automatically from all transitive deps. Returns :endpoint/result with the final merged context map, or an error map if a step fails."
  :atlas/impl (fn [{:entity/keys [dev-id] :endpoint/keys [context]}]
                (let [eid      (ensure-keyword dev-id)
                      imap     (impl-map-for eid)
                      pipeline (executor/build-pipeline eid imap)
                      result   (pipeline (or context {}))]
                  {:endpoint/result result}))})
