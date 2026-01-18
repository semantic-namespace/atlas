(ns atlas.registry
  "Semantic kernel providing compound-identity algebra.
   Each identity is a set of qualified keywords representing composed meaning.
   All operations are deterministic, data-oriented, and side-effect free,
   except for explicit registry mutation helpers."
  (:refer-clojure :exclude [exists? remove])
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [atlas.query :as query]
            [taoensso.telemere :as tel]
            [clojure.walk :as walk]
            #?(:cljs [goog.string :as gstring])))

;; =============================================================================
;; Registry
;; =============================================================================

(def registry
  (atom {}))

;; =============================================================================
;; Validation & Registry
;; =============================================================================

#?(:clj (defn- format* [fmt & args]
          (apply format fmt args))
   :cljs (defn- format* [fmt & args]
           (apply gstring/format fmt args)))

(defn- normalize-callables
  "Replaces anonymous / inline functions with :fun,
   preserves named vars that refer to functions
  Relaces reifies by :reify."
  [m]
  (walk/postwalk
    (fn [v]
      (cond
        ;; named function (var)
        (and (var? v) (fn? @v)) v

        ;; anonymous or inline function
        (fn? v) :fun
         ;; Inline reify objects (JVM only)
        #?(:clj
           (let [^Class c (class v)
                 n (.getName c)]
             (or (.isAnonymousClass c)
                 (boolean (re-find #"\$reify__\d+" n))))
           :cljs false)
        :reify
        :else v))
    m))

(defn valid?
  "Returns true if `id` is a valid compound identity.
   A valid identity is a set of at least 1 qualified keyword."
  ([id]
   (valid? id false))
  ([id verbose?]
   (cond
     (not (set? id)) (if verbose? [:error "not-a-set"] false)
     (not-every? qualified-keyword? id) (if verbose? [:error "unqualified-keywords"] false)
     :else true)))

(defn fetch
  "Fetch the value associated with a compound identity."
  [identity]
  (get @registry identity))

(defn exists?
  "Check if a compound identity exists in the registry and whether its value matches `value`.
   Returns {:exists bool :matches bool}."
  [compound-identity value]
  (let [found (get @registry compound-identity)]
    {:exists (contains? @registry compound-identity)
     :matches (= found value)}))

(defn remove
  "Remove a compound identity from the registry."
  [compound-identity]
  (swap! registry dissoc compound-identity))

(defn- find-by-dev-id
  "Find the compound identity that has the given dev-id.
   Returns [compound-id value] or nil if not found.
   PRIVATE: Used internally by register! for dev-id uniqueness checking."
  [dev-id]
  (when dev-id
    (first (filter (fn [[_k v]] (= (:atlas/dev-id v) dev-id))
                   @registry))))

(defn generate-dev-id
  "Generate a deterministic dev-id from a compound identity (aspect set).
   Sorts aspects alphabetically and creates a keyword in the 'auto' namespace."
  [aspect-set]
  (let [sorted-aspects (sort aspect-set)
        aspect-names (map (fn [kw]
                            (str (namespace kw) "--" (name kw)))
                          sorted-aspects)
        joined (str/join "--" aspect-names)]
    (keyword "auto" joined)))

(defn register!
  "Register a new compound identity with explicit entity type and aspects.

   The entity type and aspects are combined to form the complete compound identity.
   Entity types are registered with :atlas/type and can be validated after load.

   Arities:
   - [type aspects value]: Register with auto-generated dev-id
   - [dev-id type aspects value]: Register with explicit dev-id

   Parameters:
   - dev-id: Developer ID (keyword, auto-generated if not provided)
   - type: Entity type keyword (e.g., :atlas/execution-function, :semantic-namespace/flow)
   - aspects: Set of aspect keywords (e.g., #{:domain/cart :tier/service})
   - value: Value map with entity-specific keys"
  ([type aspects value]
   (register! (generate-dev-id (conj aspects type)) type aspects value)) 
  ([dev-id type aspects value]
   ;; 4-arity: (register! dev-id type aspects value) with explicit dev-id
   (assert (qualified-keyword? type)
           (format* "Entity type must be a qualified keyword, got: %s" type))
   (assert (set? aspects)
           (format* "Aspects must be a set, got: %s" aspects))
   (let [id (conj aspects type)]
     (assert (valid? id) (format* "Invalid compound identity: %s" id))
     (when (and (contains? @registry id)
                (not= (get @registry id) (assoc value :atlas/dev-id dev-id)))
       (tel/log! {:level :warn} ["Compound identity already exists in registry"
                                 {:compound-id id
                                  :existing-value (get @registry id)
                                  :attempted-value (assoc value :atlas/dev-id dev-id)}]))
     (when-let [[existing-id existing-value] (find-by-dev-id dev-id)]
       (when (not= existing-id id)
         (tel/log! {:level :warn} ["dev-id already exists in registry"
                                   {:dev-id dev-id
                                    :existing-compound-id existing-id
                                    :existing-value existing-value
                                    :attempted-compound-id id
                                    :attempted-value (assoc value :atlas/dev-id dev-id)}])))
     (swap! registry assoc id (assoc value :atlas/dev-id dev-id :atlas/type type))
     id))) 

;; =============================================================================
;; Entity Type Helpers
;; =============================================================================

(defn registered-types
  "Return all registered entity types (those with :atlas/type in their identity)."
  []
  (->> @registry
       keys
       (filter #(contains? % :atlas/type))
       (map (fn [id] (first (disj id :atlas/type))))
       set))

(defn entity-type
  "Extract the entity type from a compound identity.

  An entity type is a member that is registered with :atlas/type,
  or if none found, returns the first qualified keyword that could be a type.

  Returns the entity type keyword or nil."
  [identity]
  (let [types (registered-types)
        type-in-id (first (set/intersection identity types))]
    (or type-in-id
        ;; Fallback: return first keyword if no registered type found
        (first identity))))

(defn aspects
  "Extract all aspects (non-entity-type members) from identity.

  Returns a set of aspect keywords."
  [identity]
  (let [type (entity-type identity)]
    (when type
      (disj identity type))))

(defn with-entity-type
  "Create identity pattern with entity type + aspects.

  Example:
    (with-entity-type :atlas/execution-function
                      :tier/service
                      :domain/cart)
    => #{:atlas/execution-function :tier/service :domain/cart}"
  [entity-type & aspects]
  (apply conj #{entity-type} aspects))

;; =============================================================================
;; Post-Load Validation
;; =============================================================================

(defn validate-registry-types
  "Validate that all registered entities use known types.

  Returns:
    {:valid? true} or
    {:valid? false :violations [...]}

  Call this after all code is loaded to check type consistency."
  []
  (let [types (registered-types)
        all-identities (query/all-identities @registry)
        violations (for [id all-identities
                         :when (not (contains? id :atlas/type))
                         :let [type (entity-type id)]
                         :when (and type (not (contains? types type)))]
                     {:identity id
                      :type type
                      :dev-id (:atlas/dev-id (fetch id))
                      :message (format* "Entity uses unregistered type: %s" type)})]
    (if (seq violations)
      {:valid? false
       :violations violations
       :registered-types types}
      {:valid? true
       :registered-types types})))

(defn validate-with-specs
  "Optionally validate entities using clojure.spec if specs exist.

  Returns:
    {:valid? true} or
    {:valid? false :violations [...]}

  Only validates entities that have corresponding specs registered."
  []
  ;; TODO: Implement spec validation
  ;; For now, just return valid
  {:valid? true
   :note "Spec validation not yet implemented"})

;; =============================================================================
;; Analytical Utilities (Domain-Specific to Registry)
;; =============================================================================

(defn clusters
  "Group identities by primary namespace, returning a map {:ns [identities]}."
  []
  (reduce (fn [acc id-set]
            (reduce (fn [inner aspect]
                      (let [ns-part (namespace aspect)]
                        (update inner (keyword ns-part)
                                (fnil conj #{}) id-set)))
                    acc id-set))
          {}
          (query/all-identities @registry)))

(defn correlation-matrix
  "Return normalized correlation matrix of aspect co-occurrence (as 0–1 floats)."
  []
  (let [aspects (keys (query/aspect-frequency @registry))
        all-ids (query/all-identities @registry)
        total (max 1 (count all-ids))]
    (into {}
          (for [a1 aspects]
            [a1 (into {}
                      (for [a2 aspects]
                        [a2 (/ (count (filter #(and (contains? % a1)
                                                    (contains? % a2))
                                              all-ids))
                               total)]))]))))

;; =============================================================================
;; Heuristics & Diagnostics
;; =============================================================================

(defn missing-aspects
  "Suggest aspects potentially missing from an identity based on correlation."
  [identity-set]
  (let [similar (query/semantic-similarity @registry identity-set 0.0)
        all-aspects (->> similar
                         (mapcat :identity)
                         (frequencies)
                         (sort-by val >))]
    (->> all-aspects
         (clojure.core/remove (fn [[aspect _]] (contains? identity-set aspect)))
         (take 5)
         (mapv (fn [[aspect freq]]
                 {:aspect aspect
                  :correlation (/ freq (count similar))})))))

(defn find-anomalies
  "Return list of identities that violate expected semantic constraints."
  []
  (let [all-ids (query/all-identities @registry)]
    (->> all-ids
         (filter (fn [id]
                   (or (and (contains? id :sync/operation)
                            (contains? id :async/operation))
                       (and (contains? id :api/endpoint)
                            (not (contains? id :auth/required)))))))))

;; =============================================================================
;; Visualization
;; =============================================================================

(defn to-graphviz
  "Return a Graphviz DOT string representing compound-identity relationships."
  []
  (let [edges (for [id (query/all-identities @registry)
                    aspect id]
                [aspect id])]
    (str "digraph CompoundIdentity {\n"
         "  rankdir=LR;\n"
         "  node [shape=box];\n"
         (str/join "\n"
                   (for [[aspect id] edges]
                      (format* "  \"%s\" -> \"%s\";"
                               (pr-str aspect)
                               (pr-str id))))
         "\n}")))

(defn to-mermaid
  "Return a Mermaid diagram (graph TD) for namespace-based clusters."
  []
  (let [clusters (clusters)]
    (str "graph TD\n"
         (str/join "\n"
                   (for [[ns-key ids] clusters]
                     (str "  subgraph " (name ns-key) "\n"
                          (str/join "\n"
                                     (for [id ids]
                                       (format* "    %s[\"%s\"]"
                                                (hash id)
                                                (str/join ", " (map name id)))))
                          "\n  end"))))))

;; =============================================================================
;; Summary
;; =============================================================================

(defn summary
  "Return data summary of the current identity registry:
   {:total :unique-aspects :namespaces :top-aspects :largest :anomalies}"
  []
  (let [ids (query/all-identities @registry)
        aspects (query/aspect-frequency @registry)
        clusters (clusters)]
    {:total (count ids)
     :unique-aspects (count aspects)
     :namespaces (keys clusters)
     :top-aspects (take 5 aspects)
     :largest (take 3 (sort-by count > ids))
     :anomalies (find-anomalies)}))
