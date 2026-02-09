(ns atlas.adapter.integrant
  "Adapter to convert Integrant system configurations to Atlas registry definitions.

   ## Overview

   Integrant configs define component dependencies implicitly via `{:key :component/id}`
   references. This adapter extracts:
   - Components as `:atlas/structure-component` entities
   - Dependencies from `:key` references
   - Compound identities: `#{dev-id}` by default, or from optional dictionary

   ## Usage

   ```clojure
   (require '[atlas.adapter.integrant :as ig-adapter])

   ;; Simple: compound-identity = #{dev-id}
   (ig-adapter/register-config! integrant-config)

   ;; With semantic mapping
   (def identities
     {:accounts-by-email/logger #{:domain/logging :tier/foundation}
      :accounts-by-email/cache  #{:domain/caching :tier/foundation :protocol/redis}
      :co.yorba.member-connection/api #{:domain/member :tier/api}})

   (ig-adapter/register-config! integrant-config {:identities identities})
   ```

   ## Composite Keys

   For composite/derived Integrant keys like `[:base/key :specific/key]`:
   - dev-id = `:specific/key` (the more specific one)
   - compound-identity = `#{:base/key :specific/key}` (both)

   ## Runtime Resolution

   Two functions help resolve dependencies at runtime:

   ```clojure
   ;; Option 1: Resolve {:key :x} references in original config
   (resolve-config
     {:db {:key :component/db} :timeout 5000}
     {:component/db db-instance})
   ;; => {:db db-instance :timeout 5000}

   ;; Option 2: Build namespaced map from atlas def's deps
   (deps->ns-map
     {:structure-component/deps #{:component/db :component/logger}}
     {:component/db db-instance :component/logger logger-instance})
   ;; => #:component{:db db-instance :logger logger-instance}
   ```"
  (:require [atlas.registry :as registry]
            [clojure.walk :as walk]))

;; =============================================================================
;; DEPENDENCY EXTRACTION
;; =============================================================================

(defn- key-reference?
  "Check if a value is an Integrant key reference: {:key :some/component}"
  [v]
  (and (map? v)
       (= 1 (count v))
       (contains? v :key)
       (keyword? (:key v))))

(defn- extract-key-refs
  "Recursively extract all {:key :x} references from a value."
  [v]
  (cond
    (key-reference? v)
    [(:key v)]

    (map? v)
    (mapcat extract-key-refs (vals v))

    (sequential? v)
    (mapcat extract-key-refs v)

    :else
    []))

(defn- composite-key?
  "Check if this is a composite/derived Integrant key."
  [k]
  (vector? k))

(defn- normalize-integrant-key
  "Normalize Integrant key to a dev-id.
   - Simple key: :foo/bar -> :foo/bar
   - Composite key: [:base/key :specific/key] -> :specific/key (the more specific)"
  [k]
  (if (composite-key? k)
    (second k)
    k))

(defn- composite-identity
  "For composite keys, return #{base specific} as compound identity."
  [k]
  (when (composite-key? k)
    (set k)))

;; =============================================================================
;; RUNTIME RESOLUTION
;; =============================================================================

(defn resolve-config
  "Replace all {:key :x} references in config with values from components map.

   This preserves the original config structure, just replacing references
   with actual component instances.

   Example:
   (resolve-config
     {:db {:key :component/db} :timeout 5000}
     {:component/db <db-instance>})
   ;; => {:db <db-instance> :timeout 5000}"
  [config components]
  (walk/postwalk
   (fn [v]
     (if (key-reference? v)
       (get components (:key v))
       v))
   config))

(defn deps->ns-map
  "Build a namespaced map from an atlas definition's dependencies.

   Takes the :structure-component/deps from an atlas def and resolves
   each to its value in the components map. Returns a namespaced map
   suitable for destructuring with :keys.

   Example:
   (deps->ns-map
     {:structure-component/deps #{:component/db :component/logger}}
     {:component/db <db> :component/logger <logger>})
   ;; => #:component{:db <db> :logger <logger>}

   With destructuring:
   (let [{:component/keys [db logger]} (deps->ns-map atlas-def components)]
     ...)"
  [atlas-def components]
  (into {}
        (map (fn [dep-id] [dep-id (get components dep-id)]))
        (:structure-component/deps atlas-def)))

(defn build-init-config
  "Build init config for a component by looking up its shape in the
   original Integrant config and resolving {:key :x} refs.

   This allows starting components via ig/init-key using:
   1. The original Integrant config (for config shape)
   2. Running component instances (for dependency injection)

   Example:
   (def ig-config
     {:component/db {:pool-size 10}
      :service/users {:db {:key :component/db}
                      :timeout 5000}})

   (def running {:component/db <db-instance>})

   (build-init-config ig-config :service/users running)
   ;; => {:db <db-instance> :timeout 5000}

   ;; Then start via Integrant:
   (ig/init-key :service/users *1)"
  [integrant-config ig-key running-components]
  (let [component-config (get integrant-config ig-key)]
    (resolve-config component-config running-components)))

;; =============================================================================
;; CONVERSION
;; =============================================================================

(defn integrant-key->atlas-def
  "Convert a single Integrant key-value pair to an Atlas definition.

   Options:
   - :identities - map of {dev-id compound-identity} for custom aspects

   Returns:
   {:atlas/dev-id :component/id
    :atlas/type :atlas/structure-component
    :atlas/aspects #{...}
    :structure-component/deps #{:dep1 :dep2}
    :integrant/config {...}  ;; if :include-config? true}"
  ([ig-key ig-value]
   (integrant-key->atlas-def ig-key ig-value {}))
  ([ig-key ig-value opts]
   (let [dev-id (normalize-integrant-key ig-key)
         deps (set (extract-key-refs ig-value))
         identities (:identities opts {})

         ;; Determine compound identity:
         ;; 1. From identities dictionary if provided
         ;; 2. For composite keys: #{base specific}
         ;; 3. Default: #{dev-id}
         aspects (or (get identities dev-id)
                     (composite-identity ig-key)
                     #{dev-id})]

     (cond-> {:atlas/dev-id dev-id
              :atlas/type :atlas/structure-component
              :atlas/aspects aspects
              :structure-component/deps deps}
       ;; Include original config for debugging/reference
       (:include-config? opts) (assoc :integrant/config ig-value)))))

(defn config->atlas-defs
  "Convert entire Integrant config to Atlas definitions.

   Options:
   - :identities - map of {dev-id compound-identity} for custom aspects
   - :include-config? boolean - include original config in output
   - :filter-fn (fn [ig-key ig-value] -> boolean) - filter components

   Returns vector of atlas definition maps."
  ([config]
   (config->atlas-defs config {}))
  ([config opts]
   (let [filter-fn (or (:filter-fn opts) (constantly true))]
     (->> config
          (filter (fn [[k v]] (filter-fn k v)))
          (map (fn [[k v]] (integrant-key->atlas-def k v opts)))
          vec))))

;; =============================================================================
;; REGISTRATION
;; =============================================================================

(defn register-def!
  "Register a single Atlas definition to the registry."
  [{:keys [atlas/dev-id atlas/aspects structure-component/deps]}]
  (registry/register!
   dev-id
   :atlas/structure-component
   aspects
   {:structure-component/deps deps}))

(defn register-config!
  "Convert and register all components from an Integrant config.

   Options: same as config->atlas-defs

   Returns count of registered components."
  ([config]
   (register-config! config {}))
  ([config opts]
   (let [defs (config->atlas-defs config opts)]
     (doseq [d defs]
       (register-def! d))
     (count defs))))

;; =============================================================================
;; ANALYSIS HELPERS
;; =============================================================================

(defn dependency-graph
  "Build dependency graph from config.
   Returns {:nodes #{all-keys} :edges [[from to] ...]}"
  [config]
  (let [defs (config->atlas-defs config)
        nodes (set (map :atlas/dev-id defs))
        edges (for [d defs
                    dep (:structure-component/deps d)]
                [(:atlas/dev-id d) dep])]
    {:nodes nodes
     :edges (vec edges)}))

(defn find-roots
  "Find components with no dependents (entry points)."
  [config]
  (let [{:keys [nodes edges]} (dependency-graph config)
        deps-of-others (set (map second edges))]
    (remove deps-of-others nodes)))

(defn find-leaves
  "Find components with no dependencies (infrastructure)."
  [config]
  (let [defs (config->atlas-defs config)]
    (->> defs
         (filter #(empty? (:structure-component/deps %)))
         (map :atlas/dev-id))))

(defn summarize-config
  "Get summary statistics of an Integrant config."
  [config]
  (let [defs (config->atlas-defs config)]
    {:component-count (count defs)
     :roots (vec (find-roots config))
     :leaves (vec (find-leaves config))}))

;; =============================================================================
;; TOPOLOGICAL SORT
;; =============================================================================

(defn- build-deps-map
  "Build {node -> #{deps}} map from edges [[from to] ...]."
  [nodes edges]
  (let [base (into {} (map (fn [n] [n #{}]) nodes))]
    (reduce (fn [m [from to]]
              (update m from (fnil conj #{}) to))
            base
            edges)))

(defn topo-sort
  "Topological sort of nodes by edges. Returns nodes in dependency order
   (leaves/no-deps first, roots/dependents last).

   nodes - set of all node ids
   edges - [[from to] ...] where 'from' depends on 'to'"
  [nodes edges]
  (let [deps-map (build-deps-map nodes edges)
        sorted (atom [])
        visited (atom #{})]
    (letfn [(visit [node]
              (when-not (@visited node)
                (swap! visited conj node)
                (doseq [dep (get deps-map node #{})]
                  (visit dep))
                (swap! sorted conj node)))]
      (doseq [node nodes]
        (visit node)))
    @sorted))

;; =============================================================================
;; SYSTEM LIFECYCLE
;; =============================================================================

(defn init-order
  "Return integrant keys in dependency order (leaves first, roots last).
   This is the order in which components should be started."
  [integrant-config]
  (let [{:keys [edges]} (dependency-graph integrant-config)
        ;; Use original keys (including composite) for init
        ig-keys (keys integrant-config)
        ;; Build edges using dev-ids for the graph
        dev-id-set (set (map normalize-integrant-key ig-keys))
        ;; Map from dev-id back to original ig-key
        dev-id->ig-key (into {} (map (fn [k] [(normalize-integrant-key k) k]) ig-keys))
        sorted-dev-ids (topo-sort dev-id-set edges)]
    (mapv dev-id->ig-key sorted-dev-ids)))

(defn halt-order
  "Return integrant keys in reverse dependency order (roots first, leaves last).
   This is the order in which components should be stopped."
  [integrant-config]
  (vec (reverse (init-order integrant-config))))

(defn start-system
  "Start all components in dependency order.

   init-key-fn - function to initialize a component: (fn [ig-key config] -> instance)
                 Typically integrant.core/init-key

   Returns map of {dev-id -> instance}."
  [integrant-config init-key-fn]
  (reduce (fn [running ig-key]
            (let [dev-id (normalize-integrant-key ig-key)
                  config (build-init-config integrant-config ig-key running)
                  instance (init-key-fn ig-key config)]
              (assoc running dev-id instance)))
          {}
          (init-order integrant-config)))

(defn stop-system
  "Stop all components in reverse dependency order.

   halt-key-fn - function to stop a component: (fn [ig-key instance] -> nil)
                 Typically integrant.core/halt-key!"
  [integrant-config running halt-key-fn]
  (doseq [ig-key (halt-order integrant-config)]
    (let [dev-id (normalize-integrant-key ig-key)]
      (when-let [instance (get running dev-id)]
        (halt-key-fn ig-key instance)))))
