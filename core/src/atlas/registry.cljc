(ns atlas.registry
  "Semantic kernel providing compound-identity algebra.
   Each identity is a set of qualified keywords representing composed meaning.
   All operations are deterministic, data-oriented, and side-effect free,
   except for explicit registry mutation helpers."
  (:refer-clojure :exclude [exists? remove])
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.spec.alpha :as s]
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

(defn- get-not-serialisable-keys
  "Get not-serialisable keys for a compound identity by reading ontology from registry.
   Avoids circular dependency with atlas.ontology by reading registry directly."
  [compound-id]
  (let [entity-types (filter #(= "atlas" (namespace %)) compound-id)
        ontology-entries (filter #(contains? % :atlas/ontology) (keys @registry))]
    (set (mapcat (fn [entity-type]
                   ;; Find ontology entry that has both :atlas/ontology and this entity-type
                   (when-let [ontology-id (first (filter #(contains? % entity-type)
                                                         ontology-entries))]
                     (let [ontology-props (get @registry ontology-id)]
                       (:ontology/not-serialisable-keys ontology-props))))
                 entity-types))))

(defn- serialisable-value
  "Filter value to only include serialisable keys (exclude :ontology/not-serialisable-keys).
   Used for comparing entity definitions to detect semantic conflicts.
   Reads ontology definitions directly from registry to avoid circular dependency."
  [compound-id value]
  (let [not-serialisable-keys (get-not-serialisable-keys compound-id)]
    (apply dissoc value not-serialisable-keys)))

(def ^:dynamic *error-on-register* false)

(defn value-changed? [dev-id compound-id enriched-value]
  (when (contains? @registry compound-id)
    (let [existing-value (get @registry compound-id)
          existing-serialisable (serialisable-value compound-id existing-value)
          new-serialisable (serialisable-value compound-id enriched-value)]
      (when-let [error (and (not= existing-serialisable new-serialisable)
                            [(format* "%s entity value v0 v1 conflict" dev-id)
                             {:compound-id compound-id
                              :dev-id dev-id
                              :error-type :compound-id
                              :v0 existing-serialisable
                              :v1 new-serialisable}])]
        (if *error-on-register*
          (tel/log! {:level :warn} error)
          (ex-info (first error) (last error)))))))

(defn compound-id-changed? [dev-id compound-id _enriched-value]
  (when-let [[existing-compound-id _existing-value]
             (first (filter (fn [[_k v]] (= (:atlas/dev-id v) dev-id))
                            @registry))]
    (when-let [error (and (not= existing-compound-id compound-id)
                          [(format* "%s compound-id v0 v1 conflict" dev-id)
                           {:dev-id dev-id
                            :error-type :dev-id
                            :v0 existing-compound-id
                            :v1 compound-id}])]
      (if *error-on-register*
        (tel/log! {:level :warn} error)
        (ex-info (first error) (last error))))))

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
   (assert (and (set? aspects) (every? qualified-keyword? aspects))
           (format* "Aspects must be a set of qualified kws got: %s" aspects))
   (let [compound-id (conj aspects type)
         value-with-meta (assoc value :atlas/dev-id dev-id :atlas/type type)]
     (when-let [e  (and *error-on-register* (or (compound-id-changed? dev-id compound-id value-with-meta)
                                                (value-changed? dev-id compound-id value-with-meta)))]
       (throw e))
     (swap! registry assoc compound-id value-with-meta)
     compound-id))) 

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
        all-identities (vec (keys @registry))
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

(defn- dyn-spec
  "Create a dynamic spec that requires the given keys."
  [keys*]
  (eval `(s/keys :req ~keys*)))

(s/def :atlas/compound-id (s/coll-of qualified-keyword? ;;:min-count 1
                                     ))
(s/def :atlas/dev-id qualified-keyword?)

(defn validate-ontology-specs
  "Validate entities against their ontology definitions using dynamic specs.

  Returns true if all entities are valid, false otherwise."
  []
  (let [ontologies (into {} (filter (fn [[id _]] (contains? id :atlas/ontology)) @registry))
        ontology-specs (into {}
                             (for [[_compound-id props] ontologies
                                   :let [entity-type (:ontology/for props)
                                         required-keys (:ontology/keys props)]
                                   :when (and entity-type required-keys)]
                               [entity-type required-keys]))]
    (every? (fn [[compound-id props]]
              (or (contains? compound-id :atlas/ontology)
                  (contains? compound-id :atlas/type)
                  (let [etype (entity-type compound-id)
                        required-keys (get ontology-specs etype)]
                    (if required-keys
                      (if (s/valid? (dyn-spec required-keys) props)
                        true
                        (do (tel/log! {:level :warn}
                                   ["NOT VALID"
                                    {:dev-id (:atlas/dev-id props)
                                     :compound-id compound-id
                                     :message (s/explain-str (dyn-spec required-keys) props)
                                     }])
                            false))
                      true))))
            @registry)))

;; =============================================================================
;; Analytical Utilities — delegated to atlas.registry.analysis
;; =============================================================================
;; These require atlas.query which the kernel intentionally does not depend on.
;; The functions are kept here as delegation stubs for backward compatibility.
;; New code should require atlas.registry.analysis directly.

(defn clusters           [] ((requiring-resolve 'atlas.registry.analysis/clusters)))
(defn correlation-matrix [] ((requiring-resolve 'atlas.registry.analysis/correlation-matrix)))
(defn missing-aspects    [identity-set] ((requiring-resolve 'atlas.registry.analysis/missing-aspects) identity-set))
(defn find-anomalies     [] ((requiring-resolve 'atlas.registry.analysis/find-anomalies)))
(defn to-graphviz        [] ((requiring-resolve 'atlas.registry.analysis/to-graphviz)))
(defn to-mermaid         [] ((requiring-resolve 'atlas.registry.analysis/to-mermaid)))
(defn summary            [] ((requiring-resolve 'atlas.registry.analysis/summary)))
