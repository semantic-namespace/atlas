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
   - :intent/history  - snapshot-registry, diff-versions, version-summary, entity-timeline, snapshot-and-diff

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

   Batch operations (analyze multiple entities efficiently):
     (handle-tool {:tool/name :atlas.llm-ide/batch-entity-detail
                   :tool/args {:entity/dev-id-set #{:fn/foo :fn/bar :fn/baz}}})
     (handle-tool {:tool/name :atlas.llm-ide/compare-entities
                   :tool/args {:entity/dev-id-set #{:fn/foo :fn/bar}}})

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
            [atlas.history :as history]
            [clojure.spec.alpha :as s]
            [clojure.set :as set]))


(s/def :entity/dev-id qualified-keyword?)
(s/def :query/depth int?)
(s/def :query/focus any?)

(s/def :symptom/dev-id qualified-keyword?)

(s/def :entity/dev-id-set (s/coll-of qualified-keyword?))

(defn- ensure-keyword
  "Convert string to keyword, handling colon-prefixed strings from MCP/JSON."
  [x]
  (cond
    (keyword? x) x
    (string? x)  (keyword (cond-> x (= \: (first x)) (subs 1)))
    :else        x))

(defn- ensure-keyword-set
  "Normalize a dev-id-set argument into a set of keywords.
   Handles: keywords, strings, colon-prefixed strings, sets, vectors."
  [dev-id-set]
  (let [coll (cond
               (set? dev-id-set)        dev-id-set
               (sequential? dev-id-set) (set dev-id-set)
               :else                    #{dev-id-set})]
    (set (map ensure-keyword coll))))

(s/def :entity/dev-id-or-set (s/or :one qualified-keyword? :multiple :entity/dev-id-set))

(s/def :query/max-hops int?)

(s/def :query/aspect qualified-keyword?)
(s/def :query/entities (s/coll-of map?))

(s/def :risk/score int?)
(s/def :risk/reasons string?)
(s/def  :risk/suggestions string?)

(s/def :data/key qualified-keyword?)
(s/def :entity/type qualified-keyword?)
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

(defn tools-by-category
  "Categorize tools by intent and batch/single operation type."
  []
  (let [all-tools (available-tools)
        by-intent (group-by (fn [tool]
                             (let [identity (lookup/identity-for (:tool/name tool))
                                   intent (first (filter #(= "intent" (namespace %)) identity))]
                               intent))
                           all-tools)
        batch-tools (filter (fn [tool]
                             (or (re-find #"batch|compare|aggregate" (name (:tool/name tool)))
                                 (some #(re-find #"dev-id-set" (name %)) (:tool/args tool))))
                           all-tools)
        single-tools (remove (set batch-tools) all-tools)]
    {:by-intent by-intent
     :batch-tools batch-tools
     :single-tools single-tools
     :summary {:total (count all-tools)
              :batch (count batch-tools)
              :single (count single-tools)
              :intents (keys by-intent)}}))

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

;; Intent: trace
(registry/register!
 :atlas.llm-ide/trace-causes
 :atlas/execution-function
 #{:tier/tooling :domain/llm-ide :intent/trace :tool/trace-causes}
 {:execution-function/context [:symptom/dev-id :query/max-hops]
  :execution-function/response [:trace/upstream :trace/components :trace/failure-modes]
  :execution-function/deps #{}
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
  :execution-function/deps #{}
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
  :execution-function/deps #{}
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
  :execution-function/deps #{}
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
  :execution-function/deps #{}
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
  :execution-function/deps #{}
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
  :execution-function/deps #{}
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
  :execution-function/deps #{}
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
  :execution-function/deps #{}
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
  :execution-function/deps #{}
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
  :execution-function/deps #{}
  :atlas/impl (fn [{:keys [:query/aspect]}]
                {:entities (vec (ide/entities-with-aspect aspect))})})

(registry/register!
 :atlas.llm-ide/entity-detail
 :atlas/execution-function
 #{:tier/tooling :domain/llm-ide :intent/query :tool/entity-detail}
 {:execution-function/context [:entity/dev-id]
  :execution-function/response [:entity/info]
  :execution-function/deps #{}
  :atlas/impl (fn [{:keys [:entity/dev-id]}]
                (ide/entity-info dev-id))})

(registry/register!
 :atlas.llm-ide/data-flow
 :atlas/execution-function
 #{:tier/tooling :domain/llm-ide :intent/query :tool/data-flow}
 {:execution-function/context [:entity/dev-id]
  :execution-function/response [:dataflow/trace]
  :execution-function/deps #{}
  :atlas/impl (fn [{:keys [:entity/dev-id]}]
                {:flow (ide/data-flow dev-id)})})

;; Intent: query (data key queries)
(registry/register!
 :atlas.llm-ide/producers-of
 :atlas/execution-function
 #{:tier/tooling :domain/llm-ide :intent/query :tool/producers-of}
 {:execution-function/context [:data/key]
  :execution-function/response [:dataflow/producers]
  :execution-function/deps #{}
  :atlas/impl (fn [{:keys [:data/key]}]
                (let [data-key (if (keyword? key) key (keyword key))
                      producers (vec (ide/producers-of data-key))]
                  {:data-key data-key
                   :producers producers
                   :count (count producers)}))})

(registry/register!
 :atlas.llm-ide/consumers-of
 :atlas/execution-function
 #{:tier/tooling :domain/llm-ide :intent/query :tool/consumers-of}
 {:execution-function/context [:data/key]
  :execution-function/response [:dataflow/consumers]
  :execution-function/deps #{}
  :atlas/impl (fn [{:keys [:data/key]}]
                (let [data-key (if (keyword? key) key (keyword key))
                      consumers (vec (ide/consumers-of data-key))]
                  {:data-key data-key
                   :consumers consumers
                   :count (count consumers)}))})

(registry/register!
 :atlas.llm-ide/trace-data-key
 :atlas/execution-function
 #{:tier/tooling :domain/llm-ide :intent/trace :tool/trace-data-key}
 {:execution-function/context [:data/key]
  :execution-function/response [:dataflow/trace :dataflow/connected?]
  :execution-function/deps #{:atlas.llm-ide/producers-of :atlas.llm-ide/consumers-of}
  :atlas/impl (fn [{:keys [:data/key]}]
                (let [data-key (if (keyword? key) key (keyword key))
                      trace (ide/trace-data-flow data-key)]
                  {:data-key (:dataflow/data-key trace)
                   :produced-by (:dataflow/produced-by trace)
                   :consumed-by (:dataflow/consumed-by trace)
                   :connected? (:dataflow/connected? trace)
                   :producers-count (count (:dataflow/produced-by trace))
                   :consumers-count (count (:dataflow/consumed-by trace))}))})

;; Intent: query (ontology/type queries)
(registry/register!
 :atlas.llm-ide/list-entity-types
 :atlas/execution-function
 #{:tier/tooling :domain/llm-ide :intent/query :tool/list-entity-types}
 {:execution-function/context []
  :execution-function/response [:types/all]
  :execution-function/deps #{}
  :atlas/impl (fn [_]
                {:types (vec (ide/list-entity-types))
                 :count (count (ide/list-entity-types))})})

(registry/register!
 :atlas.llm-ide/entities-by-type
 :atlas/execution-function
 #{:tier/tooling :domain/llm-ide :intent/query :tool/entities-by-type}
 {:execution-function/context [:entity/type]
  :execution-function/response [:entities/list :entities/count]
  :execution-function/deps #{}
  :atlas/impl (fn [{:keys [:entity/type]}]
                (let [entity-type (if (keyword? type) type (keyword type))
                      entities (vec (ide/entities-with-aspect entity-type))]
                  {:entity-type entity-type
                   :entities entities
                   :count (count entities)}))})

(registry/register!
 :atlas.llm-ide/ontology-info
 :atlas/execution-function
 #{:tier/tooling :domain/llm-ide :intent/query :tool/ontology-info}
 {:execution-function/context [:entity/type]
  :execution-function/response [:ontology/keys :ontology/metadata]
  :execution-function/deps #{}
  :atlas/impl (fn [{:keys [:entity/type]}]
                (let [entity-type (if (keyword? type) type (keyword type))
                      ontology-def (ontology/ontology-for entity-type)
                      ontology-keys (ide/entity-type-ontology-keys entity-type)]
                  {:entity-type entity-type
                   :ontology-keys ontology-keys
                   :ontology-definition ontology-def
                   :available? (some? ontology-def)}))})

(registry/register!
 :atlas.llm-ide/entities-by-all-types
 :atlas/execution-function
 #{:tier/tooling :domain/llm-ide :intent/query :tool/entities-by-all-types}
 {:execution-function/context []
  :execution-function/response [:entities/by-type :types/summary]
  :execution-function/deps #{}
  :atlas/impl (fn [_]
                (let [all-types (ide/list-entity-types)
                      by-type (into {}
                                   (map (fn [entity-type]
                                          [entity-type
                                           {:entities (vec (ide/entities-with-aspect entity-type))
                                            :count (count (ide/entities-with-aspect entity-type))}])
                                        all-types))
                      total (reduce + (map (comp :count val) by-type))]
                  {:by-type by-type
                   :type-count (count all-types)
                   :total-entities total
                   :summary (into {} (map (fn [[k v]] [k (:count v)]) by-type))}))})

;; =============================================================================
;; BATCH OPERATIONS
;; =============================================================================

(registry/register!
 :atlas.llm-ide/batch-entity-detail
 :atlas/execution-function
 #{:tier/tooling :domain/llm-ide :intent/query :tool/batch-entity-detail}
 {:execution-function/context [:entity/dev-id-set]
  :execution-function/response [:batch/results :batch/summary]
  :execution-function/deps #{}
  :atlas/impl (fn [{:keys [:entity/dev-id-set]}]
                (let [entity-set (ensure-keyword-set dev-id-set)
                      results (mapv (fn [entity-id]
                                     {:entity entity-id
                                      :info (ide/entity-info entity-id)
                                      :exists? (some? (lookup/identity-for entity-id))})
                                   (vec entity-set))
                      existing (filter :exists? results)
                      missing (remove :exists? results)]
                  {:results (vec existing)
                   :missing (mapv :entity missing)
                   :summary {:total (count entity-set)
                            :found (count existing)
                            :missing (count missing)}}))})

(registry/register!
 :atlas.llm-ide/compare-entities
 :atlas/execution-function
 #{:tier/tooling :domain/llm-ide :intent/query :tool/compare-entities}
 {:execution-function/context [:entity/dev-id-set]
  :execution-function/response [:comparison/entities
                                :comparison/common-aspects
                                :comparison/unique-aspects
                                :comparison/metrics]
  :execution-function/deps #{}
  :atlas/impl (fn [{:keys [:entity/dev-id-set]}]
                (let [entity-set (ensure-keyword-set dev-id-set)

                      ;; Gather entity data
                      entities (mapv (fn [id]
                                      (when (some? (lookup/identity-for id))
                                        {:entity id
                                         :props (lookup/props-for id)
                                         :identity (lookup/identity-for id)
                                         :type (first (extract-aspects id "atlas"))
                                         :domains (vec (extract-aspects id "domain"))
                                         :tiers (vec (extract-aspects id "tier"))
                                         :in-degree (in-degree id)
                                         :out-degree (out-degree id)}))
                                    (vec entity-set))
                      valid-entities (filterv some? entities)

                      ;; Find common and unique aspects
                      all-identities (map (comp set :identity) valid-entities)
                      common-aspects (when (seq all-identities)
                                      (apply set/intersection all-identities))
                      unique-per-entity (mapv (fn [e]
                                               {:entity (:entity e)
                                                :unique-aspects (vec (set/difference
                                                                      (set (:identity e))
                                                                      (or common-aspects #{})))})
                                             valid-entities)

                      ;; Calculate metrics
                      metrics {:avg-in-degree (when (seq valid-entities)
                                               (/ (reduce + (map :in-degree valid-entities))
                                                  (count valid-entities)))
                              :avg-out-degree (when (seq valid-entities)
                                               (/ (reduce + (map :out-degree valid-entities))
                                                  (count valid-entities)))
                              :types (frequencies (map :type valid-entities))
                              :domains (frequencies (mapcat :domains valid-entities))
                              :tiers (frequencies (mapcat :tiers valid-entities))}]

                  {:entities valid-entities
                   :common-aspects (vec (or common-aspects #{}))
                   :unique-aspects unique-per-entity
                   :metrics metrics
                   :summary {:requested (count entity-set)
                            :found (count valid-entities)
                            :missing (- (count entity-set) (count valid-entities))}}))})

(registry/register!
 :atlas.llm-ide/batch-blast-radius
 :atlas/execution-function
 #{:tier/tooling :domain/llm-ide :intent/trace :tool/batch-blast-radius}
 {:execution-function/context [:entity/dev-id-set :query/max-hops]
  :execution-function/response [:batch/results :batch/aggregate]
  :execution-function/deps #{:atlas.llm-ide/blast-radius}
  :atlas/impl (fn [{:keys [:entity/dev-id-set :query/max-hops]}]
                (let [entity-set (ensure-keyword-set dev-id-set)
                      max-hops (or max-hops 3)

                      ;; Run blast-radius for each entity
                      results (mapv (fn [entity-id]
                                     (let [result (handle-tool
                                                   {:tool/name :atlas.llm-ide/blast-radius
                                                    :tool/args {:entity/dev-id-or-set entity-id
                                                               :query/max-hops max-hops}})]
                                       {:entity entity-id
                                        :blast-radius (:data result)
                                        :success? (:success? result)}))
                                   (vec entity-set))

                      successful (filter :success? results)

                      ;; Aggregate results
                      all-affected (set (mapcat #(map :entity (get-in % [:blast-radius :affected]))
                                               successful))
                      all-tiers (set (mapcat #(get-in % [:blast-radius :tiers-hit])
                                            successful))
                      all-domains (set (mapcat #(get-in % [:blast-radius :domains-hit])
                                              successful))

                      ;; Find overlaps
                      overlap-map (reduce (fn [acc entity-id]
                                           (let [affected (set (map :entity
                                                                   (get-in (first (filter #(= entity-id (:entity %)) successful))
                                                                          [:blast-radius :affected])))]
                                             (assoc acc entity-id affected)))
                                         {}
                                         entity-set)
                      overlapping-affected (filter (fn [affected-id]
                                                    (> (count (filter #(contains? (val %) affected-id)
                                                                     overlap-map))
                                                       1))
                                                  all-affected)]

                  {:results (vec successful)
                   :aggregate {:total-affected (count all-affected)
                              :tiers-hit all-tiers
                              :domains-hit all-domains
                              :overlapping-entities (vec overlapping-affected)
                              :overlap-count (count overlapping-affected)}
                   :summary {:entities-analyzed (count entity-set)
                            :successful (count successful)
                            :combined-blast-radius (count all-affected)}}))})

(registry/register!
 :atlas.llm-ide/aggregate-by-aspect
 :atlas/execution-function
 #{:tier/tooling :domain/llm-ide :intent/query :tool/aggregate-by-aspect}
 {:execution-function/context [:query/aspect]
  :execution-function/response [:aggregate/by-type
                                :aggregate/by-tier
                                :aggregate/by-domain
                                :aggregate/metrics]
  :execution-function/deps #{}
  :atlas/impl (fn [{:keys [:query/aspect]}]
                (let [entities (vec (ide/entities-with-aspect aspect))

                      ;; Group by type
                      by-type (group-by (fn [id]
                                         (first (extract-aspects id "atlas")))
                                       entities)

                      ;; Group by tier
                      by-tier (group-by (fn [id]
                                         (first (extract-aspects id "tier")))
                                       entities)

                      ;; Group by domain
                      by-domain (group-by (fn [id]
                                           (first (extract-aspects id "domain")))
                                         entities)

                      ;; Calculate metrics
                      in-degrees (map in-degree entities)
                      out-degrees (map out-degree entities)]

                  {:total (count entities)
                   :entities entities
                   :by-type (into {} (map (fn [[k v]] [k (count v)]) by-type))
                   :by-tier (into {} (map (fn [[k v]] [k (count v)]) by-tier))
                   :by-domain (into {} (map (fn [[k v]] [k (count v)]) by-domain))
                   :metrics {:avg-in-degree (when (seq in-degrees)
                                             (/ (reduce + in-degrees) (count in-degrees)))
                            :avg-out-degree (when (seq out-degrees)
                                             (/ (reduce + out-degrees) (count out-degrees)))
                            :max-in-degree (when (seq in-degrees) (apply max in-degrees))
                            :max-out-degree (when (seq out-degrees) (apply max out-degrees))}}))})

(registry/register!
 :atlas.llm-ide/batch-change-risk
 :atlas/execution-function
 #{:tier/tooling :domain/llm-ide :intent/trace :tool/batch-change-risk}
 {:execution-function/context [:entity/dev-id-set
                               :risk/centrality-threshold
                               :aspect/domain-namespace
                               :aspect/high-risk
                               :aspect/external-integration]
  :execution-function/response [:batch/results :batch/aggregate-risk]
  :execution-function/deps #{:atlas.llm-ide/change-risk}
  :atlas/impl (fn [{:keys [:entity/dev-id-set] :as args}]
                (let [entity-set (ensure-keyword-set dev-id-set)

                      ;; Individual risk assessments
                      individual-results (mapv (fn [entity-id]
                                                {:entity entity-id
                                                 :risk (handle-tool
                                                        {:tool/name :atlas.llm-ide/change-risk
                                                         :tool/args (assoc args :entity/dev-id-set #{entity-id})})})
                                              (vec entity-set))

                      ;; Combined risk assessment
                      combined-risk (handle-tool
                                     {:tool/name :atlas.llm-ide/change-risk
                                      :tool/args args})

                      ;; Sort by risk score
                      sorted-by-risk (vec (sort-by #(get-in % [:risk :data :risk-score] 0) >
                                                  individual-results))

                      high-risk (filter #(> (get-in % [:risk :data :risk-score] 0) 0.5)
                                       individual-results)]

                  {:individual-results sorted-by-risk
                   :combined-risk (:data combined-risk)
                   :high-risk-entities (mapv :entity high-risk)
                   :summary {:total-entities (count entity-set)
                            :high-risk-count (count high-risk)
                            :combined-risk-score (get-in combined-risk [:data :risk-score] 0)
                            :recommendation (if (> (count high-risk) (/ (count entity-set) 2))
                                             "High proportion of risky changes - consider smaller increments"
                                             "Risk is manageable - proceed with appropriate testing")}}))})

;; =============================================================================
;; HISTORY SPECS
;; =============================================================================

(s/def :history/version-label string?)
(s/def :history/version-old string?)
(s/def :history/version-new string?)
(s/def :history/renames (s/map-of qualified-keyword? qualified-keyword?))

;; =============================================================================
;; Intent: history — snapshot, diff, and timeline tools
;; =============================================================================
;;
;; Workflow: Developer works on their project, periodically snapshots the registry.
;; Later, asks the LLM "what changed?" — LLM uses these tools via MCP to report.
;;
;; Typical flow:
;;   1. snapshot-registry "start"         — baseline
;;   2. ... developer works for an hour ...
;;   3. snapshot-registry "after-refactor" — new state
;;   4. diff-versions "start" "after-refactor" — what changed
;;   5. version-summary "after-refactor"  — aggregate view

(defn history-reset!
  "Reset history — reinitializes conn and clears snapshot chain.
   Call this between tests or when starting a fresh history session."
  []
  (ide/history-init!))

(defn- ensure-history-conn!
  "Auto-initialize history conn if needed. Returns the conn (for snapshot-version!)."
  []
  (when-not (ide/history-get-conn)
    (history-reset!))
  (ide/history-get-conn))

(defn- history-db
  "Get the current history db value (for query functions)."
  []
  @(ensure-history-conn!))

(defn- take-snapshot!
  "Snapshot the current registry via ide/history-snapshot! (which maintains the chain).
   rename-map: optional {old-dev-id new-dev-id} for explicit rename tracking.
   Returns {:snapshot ... :versions [...]}."
  ([label] (take-snapshot! label nil))
  ([label rename-map]
   (let [result (ide/history-snapshot! label rename-map)
         conn   (ensure-history-conn!)]
     {:snapshot result
      :versions (history/versions @conn)})))

(registry/register!
 :atlas.llm-ide/snapshot-registry
 :atlas/execution-function
 #{:tier/tooling :domain/llm-ide :intent/history :tool/snapshot-registry}
 {:execution-function/context [:history/version-label :history/renames]
  :execution-function/response [:history/snapshot-result :history/versions]
  :execution-function/deps #{}
  :atlas/impl (fn [{:keys [:history/version-label :history/renames]}]
                (let [label (or version-label
                                (str "snap-" #?(:clj (System/currentTimeMillis)
                                                :cljs (.now js/Date))))]
                  (take-snapshot! label renames)))})

(registry/register!
 :atlas.llm-ide/history-versions
 :atlas/execution-function
 #{:tier/tooling :domain/llm-ide :intent/history :tool/history-versions}
 {:execution-function/context []
  :execution-function/response [:history/versions]
  :execution-function/deps #{}
  :atlas/impl (fn [_]
                {:versions (history/versions (history-db))})})

(registry/register!
 :atlas.llm-ide/diff-versions
 :atlas/execution-function
 #{:tier/tooling :domain/llm-ide :intent/history :tool/diff-versions}
 {:execution-function/context [:history/version-old :history/version-new]
  :execution-function/response [:history/snap-diff :history/edge-diff :history/vocabulary-diff :history/summary]
  :execution-function/deps #{}
  :atlas/impl (fn [{:keys [:history/version-old :history/version-new]}]
                (let [db (history-db)]
                  {:snap-changes {:old (history/version-diff db version-old)
                                  :new (history/version-diff db version-new)}
                   :deleted (history/version-deleted db version-new)
                   :renames (history/version-renames db version-new)
                   :edge-diff (history/edge-diff db version-old version-new)
                   :vocabulary-diff (history/vocabulary-diff db version-old version-new)
                   :summary {:old (history/version-summary db version-old)
                             :new (history/version-summary db version-new)}}))})

(registry/register!
 :atlas.llm-ide/version-summary
 :atlas/execution-function
 #{:tier/tooling :domain/llm-ide :intent/history :tool/version-summary}
 {:execution-function/context [:history/version-label]
  :execution-function/response [:history/summary :history/diff :history/renames]
  :execution-function/deps #{}
  :atlas/impl (fn [{:keys [:history/version-label]}]
                (let [db (history-db)]
                  {:summary (history/version-summary db version-label)
                   :diff (history/version-diff db version-label)
                   :renames (history/version-renames db version-label)}))})

(registry/register!
 :atlas.llm-ide/entity-timeline
 :atlas/execution-function
 #{:tier/tooling :domain/llm-ide :intent/history :tool/entity-timeline}
 {:execution-function/context [:entity/dev-id]
  :execution-function/response [:history/timeline]
  :execution-function/deps #{}
  :atlas/impl (fn [{:keys [:entity/dev-id]}]
                {:timeline (history/full-timeline (history-db) dev-id)})})

(registry/register!
 :atlas.llm-ide/snapshot-and-diff
 :atlas/execution-function
 #{:tier/tooling :domain/llm-ide :intent/history :tool/snapshot-and-diff}
 {:execution-function/context [:history/version-label]
  :execution-function/response [:history/snapshot-result :history/diff :history/vocabulary-diff
                                :history/summary :history/invariants]
  :execution-function/deps #{:atlas.llm-ide/snapshot-registry}
  :atlas/impl (fn [{:keys [:history/version-label]}]
                (let [label (or version-label
                                (str "snap-" #?(:clj (System/currentTimeMillis)
                                                :cljs (.now js/Date))))
                      prev-versions (history/versions (history-db))
                      prev-version (last prev-versions)
                      {:keys [snapshot versions]} (take-snapshot! label)
                      db-after (history-db)
                      diff-data (when prev-version
                                  {:snap-changes (history/version-diff db-after label)
                                   :deleted (history/version-deleted db-after label)
                                   :renames (history/version-renames db-after label)
                                   :edge-diff (history/edge-diff db-after prev-version label)
                                   :vocabulary-diff (history/vocabulary-diff db-after prev-version label)
                                   :summary {:old (history/version-summary db-after prev-version)
                                             :new (history/version-summary db-after label)}})
                      invariants (ide/check-invariants)]
                  (cond-> {:snapshot snapshot
                           :version label
                           :previous-version prev-version
                           :invariants invariants
                           :versions versions}
                    diff-data (assoc :diff diff-data))))})

