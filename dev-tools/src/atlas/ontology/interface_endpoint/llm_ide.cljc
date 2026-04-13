(ns atlas.ontology.interface-endpoint.llm-ide
  "LLM-IDE tool surface for testing interface-endpoints in a local dev session.

   Provides two MCP-discoverable tools registered under :domain/llm-ide / :intent/diagnose:

     :atlas.llm-ide/endpoint-test-template
       Generates a Clojure REPL snippet for testing a given endpoint.
       Components default to integrant dev/*system*, exec-fn impls default
       to :atlas/impl from registry. Both sections are editable.

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

(defn- generate-template
  "Generate a Clojure REPL code string for testing endpoint-id.
   Separates components (from integrant) and exec-fn impls (from registry)."
  [endpoint-id]
  (let [{:keys [components exec-fns]} (direct-deps endpoint-id)
        ctx-keys   (context-keys-for-endpoint endpoint-id)
        all-ef-ids (conj exec-fns endpoint-id)
        with-impl  (filter #(:atlas/impl (entity/props-for %)) all-ef-ids)
        no-impl    (remove #(:atlas/impl (entity/props-for %)) all-ef-ids)]
    (str/join
     "\n"
     (remove
      nil?
      [(str ";; Endpoint test template for " endpoint-id)
       ";; Generated from atlas registry — edit values as needed"
       ""
       "(require '[atlas.ontology.execution-function.executor :as executor])"
       "(require '[atlas.registry.lookup :as entity])"
       ""
       ;; Components section
       (when (seq components)
         (str ";; ---- components (default: from integrant dev/*system*) ----\n"
              ";; Replace with mocks if needed:\n"
              ";;   :component/db (reify YourProtocol ...)\n"
              "(def components\n"
              "  {"
              (str/join "\n   "
                        (map (fn [c] (str c " (" c " dev/system)"))
                             components))
              "})"))
       (when (empty? components)
         ";; No structure-components in dependency tree")
       ""
       ;; Exec-fn impls section
       ";; ---- exec-fn impls (default: from registry :atlas/impl) ----"
       ";; Replace individual fns to test with stubs:"
       ";;   :fn/example (fn [ctx] {:some/key \"mock\"})"
       (when (seq no-impl)
         (str ";; NOTE: no :atlas/impl found for: "
              (str/join ", " (map str no-impl))
              "\n;; Add :atlas/impl to their register! calls, or wire manually below."))
       (str "(def impl-map\n"
            "  {"
            (str/join "\n   "
                      (map (fn [id]
                             (str id " (:atlas/impl (entity/props-for " id "))"))
                           with-impl))
            "})")
       ""
       ;; Context section
       ";; ---- initial context ----"
       (str "(def ctx\n"
            (if (seq components)
              (str "  (merge components\n"
                   "         {"
                   (str/join "\n          "
                             (map (fn [k]
                                    (str k " " (pr-str (sample-value-for k))))
                                  ctx-keys))
                   "}))")
              (str "  {"
                   (str/join "\n   "
                             (map (fn [k]
                                    (str k " " (pr-str (sample-value-for k))))
                                  ctx-keys))
                   "})")))
       ""
       ";; ---- run pipeline ----"
       (str "(def pipeline (executor/build-pipeline " endpoint-id " impl-map))")
       "(pipeline ctx)"]))))

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
  :atlas/docs "Generate a Clojure REPL snippet for testing an interface-endpoint. Pass :entity/dev-id (the endpoint's dev-id keyword). Returns :endpoint/template — a string you can paste into a REPL. The snippet has three editable sections: (1) components from integrant dev/*system*, (2) exec-fn impls from registry :atlas/impl, (3) initial context with sample values. All transitive deps are included."
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
