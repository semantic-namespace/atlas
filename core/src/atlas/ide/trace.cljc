(ns atlas.ide.trace
  "IDE trace API — dependency graphs, data-flow analysis, impact assessment.
   All functions are self-registered as :atlas/execution-function entities."
  (:require [atlas.registry :as cid]
            [atlas.registry.lookup :as rt]
            [atlas.registry.graph :as graph]
            [atlas.invariant :as ax]
            [atlas.ontology :as ot]
            [atlas.query :as query]
            [atlas.cljc.platform :as platform]
            [clojure.set :as set]))

;; =============================================================================
;; HELPERS
;; =============================================================================

(defn- ensure-keyword
  [x]
  (cond
    (keyword? x) x
    (string? x)  (keyword (cond-> x (= \: (first x)) (subs 1)))
    :else        x))

(defn- sorted-vec [coll]
  (->> (or coll []) sort vec))

;; =============================================================================
;; CACHES
;; =============================================================================

(def ^:private data-key-cache (atom {:time 0 :entity/produces {} :entity/consumes {}}))
(def ^:private reverse-deps-cache (atom {:time 0 :data {}}))
(def ^:private cache-ttl-ms 5000)

(defn- build-data-key-cache []
  (reduce (fn [acc [_ props]]
            (let [id (:atlas/dev-id props)
                  context (ot/context-for id)
                  response (ot/response-for id)
                  produces (:entity/produces acc)
                  consumes (:entity/consumes acc)
                  producer-updated (reduce (fn [m k] (update m k (fnil conj #{}) id)) produces response)
                  consumer-updated (reduce (fn [m k] (update m k (fnil conj #{}) id)) consumes context)]
              (assoc acc :entity/produces producer-updated :entity/consumes consumer-updated)))
          {:entity/produces {} :entity/consumes {}}
          @cid/registry))

(defn- get-data-key-cache []
  (let [now (platform/now-ms)
        cache @data-key-cache
        time (:time cache)
        produces (:entity/produces cache)
        consumes (:entity/consumes cache)]
    (if (and (pos? time) (< (- now time) cache-ttl-ms))
      {:entity/produces produces :entity/consumes consumes}
      (let [new-cache (build-data-key-cache)
            new-produces (:entity/produces new-cache)
            new-consumes (:entity/consumes new-cache)]
        (swap! data-key-cache assoc :time now :entity/produces new-produces :entity/consumes new-consumes)
        {:entity/produces new-produces :entity/consumes new-consumes}))))

(defn- dataflow-dependencies-for
  [dev-id]
  (let [context-keys (ot/context-for dev-id)
        cache (get-data-key-cache)
        produces-map (:entity/produces cache)]
    (set (mapcat (fn [ctx-key]
                   (get produces-map ctx-key #{}))
                 context-keys))))

(defn- effective-dependencies-for
  [dev-id]
  (let [explicit-deps (ot/deps-for dev-id)
        dataflow-deps (dataflow-dependencies-for dev-id)]
    (set/union explicit-deps dataflow-deps)))

(defn- build-reverse-deps []
  (let [all-ids (ax/all-dev-ids)]
    (reduce (fn [acc dev-id]
              (let [deps (effective-dependencies-for dev-id)]
                (reduce (fn [inner dep]
                          (update inner dep (fnil conj #{}) dev-id))
                        acc
                        deps)))
            {}
            all-ids)))

(defn- get-reverse-deps []
  (let [now (platform/now-ms)
        {:keys [time data]} @reverse-deps-cache]
    (if (and (pos? time) (< (- now time) cache-ttl-ms))
      data
      (let [new-data (build-reverse-deps)]
        (swap! reverse-deps-cache assoc :time now :data new-data)
        new-data))))

(defn- dep-entity?
  [kw]
  (when-let [identity (rt/identity-for kw)]
    (or (contains? identity :atlas/execution-function)
        (contains? identity :atlas/structure-component))))

(defn- classify-context
  [context-keys]
  (let [{deps true data false}
        (group-by (fn [k] (boolean (dep-entity? k))) context-keys)]
    {:input (vec (sort (or data [])))
     :context-deps (vec (sort (or deps [])))}))

;; =============================================================================
;; PUBLIC API
;; =============================================================================

(defn data-flow
  "Get data flow info for a function: what it needs, who produces it, satisfaction status."
  [dev-id]
  (->> (ot/trace-data-flow (ensure-keyword dev-id))
       (map (fn [{:keys [needs produced-by satisfied?]}]
              {:dataflow/needs needs
               :dataflow/produced-by (sorted-vec produced-by)
               :dataflow/satisfied? (boolean satisfied?)}))
       vec))

(defn execution-order
  "Get topologically sorted execution order of all execution functions."
  []
  (graph/topo-sort-by-data (rt/all-with-aspect :atlas/execution-function)))

(defn dependents-of
  "Find what depends on this entity (reverse dependency lookup)."
  [dev-id]
  (let [dev-id-kw (ensure-keyword dev-id)
        reverse-deps (get-reverse-deps)
        dependents (get reverse-deps dev-id-kw #{})]
    (vec (sort dependents))))

(defn dependencies-of
  "Find what this entity depends on (explicit deps + dataflow-derived deps)."
  [dev-id]
  (vec (sort (effective-dependencies-for (ensure-keyword dev-id)))))

(defn recursive-dependencies-of
  "Get all transitive dependencies for an entity, recursively.
   Traverses execution-function/deps, structure-component/deps, and endpoint/deps.
   Context/input keys that are execution-functions or structure-components are
   classified as :dep/context-deps; the rest are :dep/input.
   Returns flat vector in BFS order, excluding the root entity itself."
  [dev-id]
  (let [dev-id-kw (ensure-keyword dev-id)
        entity-type-for (fn [id]
                          (when-let [identity (rt/identity-for id)]
                            (some #(when (= "atlas" (namespace %)) %) identity)))]
    (loop [queue (mapv #(vector % 1 dev-id-kw)
                        (sort (effective-dependencies-for dev-id-kw)))
           visited #{dev-id-kw}
           result []]
      (if (empty? queue)
        result
        (let [[current depth via] (first queue)
              rest-queue (subvec queue 1)
              already-seen? (contains? visited current)
              {:keys [input context-deps]} (classify-context (ot/context-for current))
              entry {:dep/dev-id current
                     :dep/type (entity-type-for current)
                     :dep/depth depth
                     :dep/via via
                     :dep/already-seen? already-seen?
                     :dep/input input
                     :dep/context-deps context-deps}]
          (if already-seen?
            (recur rest-queue visited (conj result entry))
            (let [deps (sort (effective-dependencies-for current))
                  new-deps (remove visited deps)]
              (recur
               (into rest-queue (map #(vector % (inc depth) current) new-deps))
               (conj visited current)
               (conj result entry)))))))))

(defn recursive-dependencies-summary
  "Get a flat summary of all transitive dependencies.
   Returns :summary/root, :summary/context, :summary/deps, :summary/data-keys."
  [dev-id]
  (let [dev-id-kw (ensure-keyword dev-id)
        {:keys [input context-deps]} (classify-context (ot/context-for dev-id-kw))
        tree (recursive-dependencies-of dev-id-kw)
        unique-entries (remove :dep/already-seen? tree)
        all-deps (->> unique-entries
                      (mapcat (fn [e] (cons (:dep/dev-id e) (:dep/context-deps e))))
                      (into (set context-deps))
                      sort
                      vec)
        all-data-keys (->> unique-entries
                           (mapcat :dep/input)
                           (into (set input))
                           sort
                           vec)]
    {:summary/root dev-id-kw
     :summary/context input
     :summary/deps all-deps
     :summary/data-keys all-data-keys}))

(defn producers-of
  "Find functions that produce a data key."
  [data-key]
  (let [data-key-kw (ensure-keyword data-key)
        cache (get-data-key-cache)
        produces (:entity/produces cache)]
    (sorted-vec (get produces data-key-kw #{}))))

(defn consumers-of
  "Find functions that consume a data key."
  [data-key]
  (let [data-key-kw (ensure-keyword data-key)
        cache (get-data-key-cache)
        consumes (:entity/consumes cache)]
    (sorted-vec (get consumes data-key-kw #{}))))

(defn trace-data-flow
  "Trace how a data key flows through the system: who produces it, who consumes it."
  [data-key]
  (let [data-key-kw (ensure-keyword data-key)
        produced-by (producers-of data-key-kw)
        consumed-by (consumers-of data-key-kw)]
    {:dataflow/data-key data-key-kw
     :dataflow/produced-by produced-by
     :dataflow/consumed-by consumed-by
     :dataflow/connected? (boolean (and (seq produced-by) (seq consumed-by)))}))

(defn impact-of-change
  "Show what would be affected if an entity changes: its outputs and direct dependents."
  [entity-id]
  (let [result (query/impact-of-change @cid/registry (ensure-keyword entity-id) :atlas/dev-id :execution-function/deps :interface-endpoint/response)
        entity (:entity result)
        produces (:entity/produces result)
        direct-dependents (:direct-dependents result)]
    {:impact/entity entity
     :impact/produces (sorted-vec produces)
     :impact/direct-dependents (sorted-vec direct-dependents)}))

;; =============================================================================
;; SELF-REGISTRATION
;; =============================================================================

(cid/register!
 :fn.ide/data-flow :atlas/execution-function
 #{:domain/ide :intent/trace :operation/lookup :subject/entity-dataflow}
 {:execution-function/context [:entity/dev-id]
  :execution-function/response [:dataflow/needs :dataflow/produced-by :dataflow/satisfied?]
  :execution-function/deps #{}
  :atlas/docs "Analyze data flow for a function: what data keys it needs, which entities produce them, and whether each need is satisfied. Use this to detect broken data chains or missing producers."
  :atlas/impl #(data-flow (:entity/dev-id %))})

(cid/register!
 :fn.ide/dependents-of :atlas/execution-function
 #{:domain/ide :intent/trace :operation/lookup :subject/dependent}
 {:execution-function/context [:entity/dev-id]
  :execution-function/response [:entity/dev-id-list]
  :execution-function/deps #{}
  :atlas/docs "Find all entities that depend on the given entity (reverse dependency lookup). Returns a sorted list of dev-ids. Use this to assess blast radius before changing an entity."
  :atlas/impl #(dependents-of (:entity/dev-id %))})

(cid/register!
 :fn.ide/dependencies-of :atlas/execution-function
 #{:domain/ide :intent/trace :operation/lookup :subject/dependency}
 {:execution-function/context [:entity/dev-id]
  :execution-function/response [:entity/dev-id-list]
  :execution-function/deps #{}
  :atlas/docs "Find all direct dependencies of an entity (explicit deps + dataflow-derived deps). Returns a sorted list of dev-ids the entity depends on."
  :atlas/impl #(dependencies-of (:entity/dev-id %))})

(cid/register!
 :fn.ide/recursive-dependencies-of :atlas/execution-function
 #{:domain/ide :intent/trace :operation/lookup :subject/dependency-tree}
 {:execution-function/context [:entity/dev-id]
  :execution-function/response [:dep/tree]
  :execution-function/deps #{}
  :atlas/docs "Get the full transitive dependency tree for an entity via BFS. Each entry includes depth, via (parent), input keys, context deps, and whether the node was already visited. Use this to understand the complete dependency chain."
  :atlas/impl #(recursive-dependencies-of (:entity/dev-id %))})

(cid/register!
 :fn.ide/recursive-dependencies-summary :atlas/execution-function
 #{:domain/ide :intent/trace :operation/summary :subject/dependency}
 {:execution-function/context [:entity/dev-id]
  :execution-function/response [:summary/deps :summary/context :summary/data-keys]
  :execution-function/deps #{}
  :atlas/docs "Summarize all transitive dependencies as flat lists: entity deps, data keys needed, and the root entity's own context. Simpler than the full tree — good for quick dependency audits."
  :atlas/impl #(recursive-dependencies-summary (:entity/dev-id %))})

(cid/register!
 :fn.ide/producers-of :atlas/execution-function
 #{:domain/ide :intent/trace :operation/lookup :subject/producer}
 {:execution-function/context [:dataflow/data-key]
  :execution-function/response [:entity/dev-id-list]
  :execution-function/deps #{}
  :atlas/docs "Find all entities that produce (output) a given data key. Use this to discover where a piece of data originates in the system."
  :atlas/impl #(producers-of (:dataflow/data-key %))})

(cid/register!
 :fn.ide/consumers-of :atlas/execution-function
 #{:domain/ide :intent/trace :operation/lookup :subject/consumer}
 {:execution-function/context [:dataflow/data-key]
  :execution-function/response [:entity/dev-id-list]
  :execution-function/deps #{}
  :atlas/docs "Find all entities that consume (require as input) a given data key. Use this to understand who depends on a piece of data."
  :atlas/impl #(consumers-of (:dataflow/data-key %))})

(cid/register!
 :fn.ide/trace-data-flow :atlas/execution-function
 #{:domain/ide :intent/trace :operation/lookup :subject/data-key-flow}
 {:execution-function/context [:dataflow/data-key]
  :execution-function/response [:dataflow/produced-by :dataflow/consumed-by :dataflow/connected?]
  :execution-function/deps #{}
  :atlas/docs "Trace how a data key flows through the system: who produces it, who consumes it, and whether the flow is connected (has both producers and consumers). Use this to find orphaned data or broken pipelines."
  :atlas/impl #(trace-data-flow (:dataflow/data-key %))})

(cid/register!
 :fn.ide/impact-of-change :atlas/execution-function
 #{:domain/ide :intent/trace :operation/lookup :subject/impact}
 {:execution-function/context [:entity/dev-id]
  :execution-function/response [:impact/entity :impact/produces :impact/direct-dependents]
  :execution-function/deps #{}
  :atlas/docs "Assess the impact of changing an entity: what data keys it produces and which entities directly depend on it. Use this before refactoring to understand blast radius."
  :atlas/impl #(impact-of-change (:entity/dev-id %))})

(cid/register!
 :fn.ide/execution-order :atlas/execution-function
 #{:domain/ide :intent/trace :operation/list :subject/execution-order}
 {:execution-function/context []
  :execution-function/response [:execution/order]
  :execution-function/deps #{}
  :atlas/docs "Get the topologically sorted execution order of all execution functions. Functions appear in an order where each function's dependencies come before it. Use this to understand system initialization or execution sequencing."
  :atlas/impl (fn [_] (execution-order))})
