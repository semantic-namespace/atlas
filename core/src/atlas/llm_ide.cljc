(ns atlas.llm-ide
  "LLM-IDE: Intent-based tools for LLM consumption via MCP.

   ## Prerequisites

   Load ontology modules before using traversal tools:
     (require '[atlas.ontology.execution-function :as ef])
     (ef/load!)

   ## Tool Registration

   All tools are registered as :atlas/execution-function with base identity:
   #{:atlas/execution-function :tier/tooling :domain/llm-ide :tool/<name>}

   Plus one of:
   - :intent/trace   - trace-causes, blast-radius, change-risk
   - :intent/explain - explain-area
   - :intent/suggest - suggest-placement
   - :intent/diagnose - orphans, islands, bottlenecks, anomalies, structural-gaps
   - :intent/query   - by-aspect, entity-detail, data-flow

   ## Ontology-Agnostic Design

   Tools accept aspect parameters to work with any registry/ontology:
   - :aspect/entry-point - Aspect marking entry points (default: :tier/api)
   - :aspect/key-component - Aspect marking key components (default: :tier/foundation)
   - :aspect/high-risk - Aspect marking high-risk code (default: :tier/foundation)
   - :aspect/external-integration - Aspect marking external integrations (default: :integration/external)
   - :aspect/exclude-from-orphan-check - Aspect to exclude from orphan detection (default: :tier/api)
   - :aspect/domain-namespace - Namespace for domain aspects (default: \"domain\")
   - :aspect/type-namespace - Namespace for type aspects (default: \"atlas\")
   - :risk/centrality-threshold - Threshold for high-centrality detection (default: 3)

   ## Usage

   Frontal controller pattern:
     (handle-tool {:tool/name :atlas.llm-ide/blast-radius
                   :tool/args {:entity/dev-id-or-set :fn/foo :query/max-hops 3}})
     => {:success? true :data {:affected [...] :tiers-hit #{...} :domains-hit #{...}}}

   Custom ontology aspects:
     (handle-tool {:tool/name :atlas.llm-ide/explain-area
                   :tool/args {:query/focus :domain/payments
                               :aspect/entry-point :layer/controller
                               :aspect/key-component :layer/repository}})

   Self-registration (tools become queryable via registry):
     (register-tools!)
     (ide/entities-with-aspect :domain/llm-ide)
     => [:atlas.llm-ide/blast-radius :atlas.llm-ide/orphans ...]

   Context export for LLM prompts:
     (llm-context) => {:atlas/ontology ... :atlas/registry ... :atlas/tools ...}"
  (:require [atlas.registry :as registry]
            [atlas.registry.lookup :as lookup]
            [atlas.query :as q]
            [atlas.ontology :as ontology]
            [atlas.ide :as ide]
            [atlas.invariant :as inv]
            [atlas.datalog :as datalog]
            [clojure.spec.alpha :as s]
            [clojure.set :as set]))


(s/def :entity/dev-id qualified-keyword?)
(s/def :query/depth int?)
(s/def :query/focus any?)

(s/def :symptom/dev-id qualified-keyword?)

(s/def :entity/dev-id-set (s/coll-of qualified-keyword?))

(s/def :entity/dev-id-or-set (s/or :one qualified-keyword? :multiple :entity/dev-id-set))

(s/def :query/max-hops int?)

(s/def :query/aspect qualified-keyword?)
(s/def :query/entities (s/coll-of map?))

(s/def :risk/score int?)
(s/def :risk/reasons string?)
(s/def  :risk/suggestions string?)

(s/def :entity/intended-aspects (s/coll-of qualified-keyword?))

(s/def :dataflow/consumes (s/coll-of qualified-keyword?))
(s/def :dataflow/produces (s/coll-of qualified-keyword?))

;; Ontology-agnostic aspect parameters
(s/def :aspect/entry-point qualified-keyword?)
(s/def :aspect/key-component qualified-keyword?)
(s/def :aspect/high-risk qualified-keyword?)
(s/def :aspect/external-integration qualified-keyword?)
(s/def :aspect/exclude-from-orphan-check qualified-keyword?)
(s/def :aspect/domain-namespace string?)
(s/def :aspect/type-namespace string?)
(s/def :risk/centrality-threshold int?)


;; =============================================================================
;; GRAPH HELPERS (transitive traversal via datalog)
;; =============================================================================
;;
;; These functions delegate to atlas.datalog for efficient cached queries.
;; The datalog DB is created once and cached for the app lifetime.

(defn- downstream-closure
  "Transitive closure of dependents up to max-hops.
   Uses datalog/query-downstream-closure with cached DB."
  [start-ids max-hops]
  (datalog/query-downstream-closure (datalog/get-db) start-ids max-hops))

(defn- upstream-closure
  "Transitive closure of dependencies up to max-hops.
   Uses datalog/query-upstream-closure with cached DB."
  [start-ids max-hops]
  (datalog/query-upstream-closure (datalog/get-db) start-ids max-hops))

(defn- in-degree
  "Count of inbound edges (how many entities depend on this one)"
  [dev-id]
  (count (ide/dependents-of dev-id)))

(defn- out-degree
  "Count of outbound edges (dependencies)"
  [dev-id]
  (count (ide/dependencies-of dev-id)))

(defn- extract-aspects
  "Extract aspects of given namespace from entity identity"
  [dev-id aspect-ns]
  (let [id (lookup/identity-for dev-id)]
    (filter #(= aspect-ns (namespace %)) id)))

;; =============================================================================
;; FRONTAL CONTROLLER
;; =============================================================================

(defn handle-tool
  "Frontal controller for LLM-IDE tools.

   Input: {:tool/name :qualified/keyword
           :tool/args {:qualified/keyword value ...}}

   Returns: {:success? boolean
             :data {...} or :error {...}}

   Looks up :atlas/impl from the registry for the given tool name."
  [{:tool/keys [name args]}]
  (if-let [handler (:atlas/impl (lookup/props-for name))]
    (try
      {:success? true
       :data (handler (or args {}))}
      (catch #?(:clj Exception :cljs js/Error) e
        {:success? false
         :error {:message #?(:clj (.getMessage e) :cljs (.-message e))
                 :tool name}}))
    {:success? false
     :error {:message "Unknown tool"
             :tool name
             :available (vec (ide/entities-with-aspect :domain/llm-ide))}}))

(defn available-tools
  "List all available tools by querying the registry for :domain/llm-ide entities."
  []
  (->> (ide/entities-with-aspect :domain/llm-ide)
       (map (fn [dev-id]
              (let [props (lookup/props-for dev-id)]
                {:tool/name dev-id
                 :tool/args (:execution-function/context props)
                 :tool/returns (:execution-function/response props)})))
       vec))

;; =============================================================================
;; LLM CONTEXT EXPORT
;; =============================================================================

(defn llm-context
  "Export full context for LLM prompt injection.

   Returns: {:atlas/ontology {...}
             :atlas/tools [...]
             :atlas/types [...]
             :atlas/registry {...}}"
  []
  {:atlas/ontology {:aspects (ide/list-aspects)
                    :aspect-namespaces (ide/list-aspect-namespaces)}
   :atlas/tools (available-tools)
   :atlas/types (ide/list-entity-types)
   :atlas/registry (ide/list-all-entities)
   :atlas/invariants (ide/check-invariants)})

(defn llm-context-compact
  "Compact context for smaller prompts - just structure, not full registry."
  []
  {:atlas/tools (available-tools)
   :atlas/types (ide/list-entity-types)
   :atlas/aspect-namespaces (ide/list-aspect-namespaces)
   :atlas/entity-count (count @registry/registry)})

;; =============================================================================
;; TOOL SELF-REGISTRATION (explicit registry/register! calls)
;; =============================================================================

(defn register-tools!
  "Register all LLM-IDE tools as :atlas/execution-function entities.
   Each tool includes :atlas/impl with its handler function inlined.
   Safe to call multiple times."
[])

;; Intent: trace
(registry/register!
 :atlas.llm-ide/trace-causes
 :atlas/execution-function
 #{:tier/tooling :domain/llm-ide :intent/trace :tool/trace-causes}
 {:execution-function/context [:symptom/dev-id :query/max-hops]
  :execution-function/response [:trace/upstream :trace/components :trace/failure-modes]
  :atlas/impl (fn [{:keys [:symptom/dev-id :query/max-hops]}]
                (let [max-hops (or max-hops 5)
                      upstream (upstream-closure #{dev-id} max-hops)
                      entities (map :entity upstream)
                      components (filter #(lookup/has-aspect? % :atlas/structure-component) entities)
                      failure-modes (filter #(lookup/has-aspect? % :atlas/risk-failure-mode) entities)]
                  {:upstream (vec upstream)
                   :components (vec components)
                   :failure-modes (vec failure-modes)}))})

(registry/register!
 :atlas.llm-ide/blast-radius
 :atlas/execution-function
 #{:tier/tooling :domain/llm-ide :intent/trace :tool/blast-radius}
 {:execution-function/context [:entity/dev-id-or-set :query/max-hops]
  :execution-function/response [:impact/affected :impact/tiers-hit :impact/domains-hit]
  :atlas/impl (fn [{:keys [:entity/dev-id-or-set :query/max-hops]}]
                (let [max-hops (or max-hops 3)
                      changing-set (if (set? dev-id-or-set) dev-id-or-set #{dev-id-or-set})
                      affected (downstream-closure changing-set max-hops)
                      entities (map :entity affected)
                      tiers (->> entities
                                 (mapcat #(extract-aspects % "tier"))
                                 set)
                      domains (->> entities
                                   (mapcat #(extract-aspects % "domain"))
                                   set)]
                  {:affected (vec affected)
                   :tiers-hit tiers
                   :domains-hit domains}))})

(registry/register!
 :atlas.llm-ide/change-risk
 :atlas/execution-function
 #{:tier/tooling :domain/llm-ide :intent/trace :tool/change-risk}
 {:execution-function/context [:entity/dev-id-set
                               :risk/centrality-threshold
                               :aspect/domain-namespace
                               :aspect/high-risk
                               :aspect/external-integration]
  :execution-function/response [:risk/score :risk/reasons :risk/suggestions]
  :atlas/impl (fn [{:keys [:entity/dev-id-set
                           :risk/centrality-threshold
                           :aspect/domain-namespace
                           :aspect/high-risk
                           :aspect/external-integration]}]
                (let [dev-id-set (if (set? dev-id-set) dev-id-set #{dev-id-set})
                      centrality-threshold (or centrality-threshold 3)
                      domain-namespace (or domain-namespace "domain")
                      high-risk (or high-risk :tier/foundation)
                      external-integration (or external-integration :integration/external)
                      high-in-degree (filter #(> (in-degree %) centrality-threshold) dev-id-set)
                      crosses-domain (let [domains (set (mapcat #(extract-aspects % domain-namespace) dev-id-set))]
                                       (> (count domains) 1))
                      touches-high-risk (some #(lookup/has-aspect? % high-risk) dev-id-set)
                      touches-external (some #(lookup/has-aspect? % external-integration) dev-id-set)
                      reasons (cond-> []
                                (seq high-in-degree)
                                (conj {:type :high-centrality
                                       :entities (vec high-in-degree)
                                       :detail (str "Many entities depend on these (>" centrality-threshold ")")})
                                crosses-domain
                                (conj {:type :crosses-domain
                                       :detail (str "Change spans multiple " domain-namespace " domains")})
                                touches-high-risk
                                (conj {:type :high-risk-aspect
                                       :aspect high-risk
                                       :detail (str "Modifying " high-risk " code")})
                                touches-external
                                (conj {:type :external-integration
                                       :aspect external-integration
                                       :detail (str "Touches " external-integration)}))
                      risk-score (/ (count reasons) 4.0)]
                  {:risk-score risk-score
                   :reasons reasons
                   :suggestions (when (> risk-score 0.5)
                                  ["Consider smaller incremental changes"
                                   "Add tests for affected downstream entities"])}))})

    ;; Intent: explain
(registry/register!
 :atlas.llm-ide/explain-area
 :atlas/execution-function
 #{:tier/tooling :domain/llm-ide :intent/explain :tool/explain-area}
 {:execution-function/context [:query/focus :query/depth :aspect/entry-point :aspect/key-component]
  :execution-function/response [:area/entry-points :area/key-entities :area/components]
  :atlas/impl (fn [{:keys [:query/focus :query/depth :aspect/entry-point :aspect/key-component]}]
                (let [depth (or depth 2)
                      entry-point (or entry-point :tier/api)
                      key-component (or key-component :tier/foundation)
                      base-entities (if (keyword? focus)
                                      (if (lookup/identity-for focus)
                                        (let [up (map :entity (upstream-closure #{focus} depth))
                                              down (map :entity (downstream-closure #{focus} depth))]
                                          (set (concat [focus] up down)))
                                        (set (ide/entities-with-aspect focus)))
                                      #{focus})
                      entry-points (->> base-entities
                                        (filter #(lookup/has-aspect? % entry-point))
                                        vec)
                      in-degrees (map (fn [e] {:entity e
                                               :in-degree (count (set/intersection
                                                                  (set (ide/dependents-of e))
                                                                  base-entities))})
                                      base-entities)
                      key-entities (->> in-degrees
                                        (sort-by :in-degree >)
                                        (take 5)
                                        (map :entity)
                                        vec)
                      components (->> base-entities
                                      (filter #(lookup/has-aspect? % key-component))
                                      vec)]
                  {:focus focus
                   :entity-count (count base-entities)
                   :entry-points entry-points
                   :key-entities key-entities
                   :components components}))})

    ;; Intent: suggest
(registry/register!
 :atlas.llm-ide/suggest-placement
 :atlas/execution-function
 #{:tier/tooling :domain/llm-ide :intent/suggest :tool/suggest-placement}
 {:execution-function/context [:entity/intended-aspects :dataflow/consumes :dataflow/produces]
  :execution-function/response [:suggestion/similar :suggestion/deps :suggestion/patterns]
  :atlas/impl (fn [{:keys [:entity/intended-aspects :dataflow/consumes :dataflow/produces]}]
                (let [aspect-set (if (set? intended-aspects) intended-aspects (set intended-aspects))
                      similar (->> @registry/registry
                                   (map (fn [[id props]]
                                          (let [entity-aspects (registry/aspects id)
                                                shared (set/intersection aspect-set entity-aspects)
                                                missing (set/difference aspect-set entity-aspects)]
                                            {:entity (:atlas/dev-id props)
                                             :overlap (if (seq aspect-set)
                                                        (/ (count shared) (count aspect-set))
                                                        0)
                                             :shared (vec (sort shared))
                                             :missing (vec (sort missing))})))
                                   (filter #(> (:overlap %) 0))
                                   (sort-by :overlap >)
                                   (take 10)
                                   vec)
                      suggested-deps (when (seq consumes)
                                       (->> consumes
                                            (mapcat ide/producers-of)
                                            distinct
                                            vec))]
                  {:similar similar
                   :suggested-deps (or suggested-deps [])
                   :patterns (->> similar
                                  (take 3)
                                  (map :entity)
                                  vec)}))})

    ;; Intent: diagnose
(registry/register!
 :atlas.llm-ide/orphans
 :atlas/execution-function
 #{:tier/tooling :domain/llm-ide :intent/diagnose :tool/orphans}
 {:execution-function/context [:aspect/exclude-from-orphan-check :aspect/type-namespace]
  :execution-function/response [:diagnose/orphans :diagnose/count]
  :atlas/impl (fn [{:keys [:aspect/exclude-from-orphan-check :aspect/type-namespace]}]
                (let [exclude-from-orphan-check (or exclude-from-orphan-check :tier/api)
                      type-namespace (or type-namespace "atlas")
                      all-ids (inv/all-dev-ids)
                      orphaned (->> all-ids
                                    (remove #(lookup/has-aspect? % exclude-from-orphan-check))
                                    (filter #(zero? (in-degree %)))
                                    (map (fn [e] {:entity e
                                                  :type (first (extract-aspects e type-namespace))}))
                                    vec)]
                  {:orphans orphaned
                   :count (count orphaned)}))})

(registry/register!
 :atlas.llm-ide/islands
 :atlas/execution-function
 #{:tier/tooling :domain/llm-ide :intent/diagnose :tool/islands}
 {:execution-function/context []
  :execution-function/response [:diagnose/islands :diagnose/count]
  :atlas/impl (fn [_]
                (let [all-ids (inv/all-dev-ids)
                      adjacency (reduce (fn [adj id]
                                          (let [deps (set (ide/dependencies-of id))
                                                dependents (set (ide/dependents-of id))]
                                            (assoc adj id (set/union deps dependents))))
                                        {}
                                        all-ids)
                      find-component (fn [start visited]
                                       (loop [queue [start]
                                              component #{}
                                              seen visited]
                                         (if (empty? queue)
                                           [component seen]
                                           (let [node (first queue)
                                                 neighbors (get adjacency node #{})
                                                 new-neighbors (set/difference neighbors seen)]
                                             (recur (into (rest queue) new-neighbors)
                                                    (conj component node)
                                                    (conj seen node))))))
                      components (loop [remaining all-ids
                                        visited #{}
                                        result []]
                                   (if (empty? remaining)
                                     result
                                     (let [start (first remaining)]
                                       (if (visited start)
                                         (recur (rest remaining) visited result)
                                         (let [[component new-visited] (find-component start visited)]
                                           (recur (rest remaining)
                                                  new-visited
                                                  (conj result (vec component))))))))]
                  {:islands (vec (sort-by (comp - count) components))
                   :count (count components)}))})

(registry/register!
 :atlas.llm-ide/bottlenecks
 :atlas/execution-function
 #{:tier/tooling :domain/llm-ide :intent/diagnose :tool/bottlenecks}
 {:execution-function/context []
  :execution-function/response [:diagnose/bottlenecks]
  :atlas/impl (fn [_]
                (let [all-ids (inv/all-dev-ids)
                      scores (->> all-ids
                                  (map (fn [e]
                                         (let [in (in-degree e)
                                               out (out-degree e)]
                                           {:entity e
                                            :in-degree in
                                            :out-degree out
                                            :score (* in out)})))
                                  (filter #(> (:score %) 0))
                                  (sort-by :score >)
                                  (take 20)
                                  vec)]
                  {:bottlenecks scores}))})

(registry/register!
 :atlas.llm-ide/aspect-anomalies
 :atlas/execution-function
 #{:tier/tooling :domain/llm-ide :intent/diagnose :tool/aspect-anomalies}
 {:execution-function/context []
  :execution-function/response [:diagnose/sparse :diagnose/similar-names]
  :atlas/impl (fn [_]
                (let [aspects (ide/list-aspects)
                      freq-map (into {} (map (fn [{:keys [aspect/aspect aspect/count]}]
                                               [aspect count])
                                             aspects))
                      sparse (->> freq-map
                                  (filter #(= 1 (val %)))
                                  (map key)
                                  vec)
                      aspect-names (keys freq-map)
                      similar-pairs (->> (for [a aspect-names
                                               b aspect-names
                                               :when (and (not= a b)
                                                          (= (namespace a) (namespace b))
                                                          (< (Math/abs (- (count (name a)) (count (name b)))) 2))]
                                           #{a b})
                                         distinct
                                         (map vec)
                                         vec)]
                  {:sparse sparse
                   :similar-names similar-pairs}))})

(registry/register!
 :atlas.llm-ide/structural-gaps
 :atlas/execution-function
 #{:tier/tooling :domain/llm-ide :intent/diagnose :tool/structural-gaps}
 {:execution-function/context []
  :execution-function/response [:diagnose/gaps]
  :atlas/impl (fn [_]
                  ;; Uses cached datalog DB with precomputed aspects/deps for efficiency
                {:gaps (datalog/query-structural-gaps (datalog/get-db) 0.5 20)})})

    ;; Intent: query
(registry/register!
 :atlas.llm-ide/by-aspect
 :atlas/execution-function
 #{:tier/tooling :domain/llm-ide :intent/query :tool/by-aspect}
 {:execution-function/context [:query/aspect]
  :execution-function/response [:query/entities]
  :atlas/impl (fn [{:keys [:query/aspect]}]
                {:entities (vec (ide/entities-with-aspect aspect))})})

(registry/register!
 :atlas.llm-ide/entity-detail
 :atlas/execution-function
 #{:tier/tooling :domain/llm-ide :intent/query :tool/entity-detail}
 {:execution-function/context [:entity/dev-id]
  :execution-function/response [:entity/info]
  :atlas/impl (fn [{:keys [:entity/dev-id]}]
                (ide/entity-info dev-id))})

(registry/register!
 :atlas.llm-ide/data-flow
 :atlas/execution-function
 #{:tier/tooling :domain/llm-ide :intent/query :tool/data-flow}
 {:execution-function/context [:entity/dev-id]
  :execution-function/response [:dataflow/trace]
  :atlas/impl (fn [{:keys [:entity/dev-id]}]
                {:flow (ide/data-flow dev-id)})})

:registered

(defn unregister-tools!
  "Unregister tools (for testing)."
  []
  (doseq [dev-id (ide/entities-with-aspect :domain/llm-ide)]
    (when-let [id (lookup/identity-for dev-id)]
      (registry/remove id)))
  :unregistered)
