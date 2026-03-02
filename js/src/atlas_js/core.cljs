(ns atlas-js.core
  "Atlas JS client — load an EDN registry and use the full Atlas API from JavaScript.

   All public functions are exported as ES module exports via shadow-cljs :esm target.
   Clojure data structures are automatically converted to/from JS at the boundary.

   Accepts js-qualified-keywords Keyword instances wherever a string keyword is
   expected. Detection is by duck-typing (.fqn property), so no static import of
   the library is needed at compile time."
  (:require [atlas.registry :as registry]
            [atlas.query :as query]
            [atlas.invariant :as inv]
            [atlas.ontology :as ont]
            [atlas.datalog :as datalog]
            [clojure.edn :as edn]
            [clojure.set :as set]))

;; =============================================================================
;; JS <-> CLJ Conversion Helpers
;; =============================================================================

(defn- js-keyword?
  "Duck-type check for js-qualified-keywords Keyword instances.
   Uses aget (string key) so Closure Compiler does not rename the property."
  [x]
  (and (some? x)
       (not (string? x))
       (not (keyword? x))
       (string? (aget x "fqn"))))

(defn- ->clj-kw
  "Convert a JS Keyword instance, string, or CLJ keyword to a CLJ keyword."
  [x]
  (cond
    (keyword? x)    x
    (js-keyword? x) (keyword (aget x "fqn"))
    (string? x)     (keyword x)
    :else           (keyword (str x))))

(defn to-js
  "Convert any Clojure data structure to JavaScript.
   Keywords become 'ns/name' strings.
   Sets become Arrays. Maps become Objects."
  [x]
  (clj->js x :keyword-fn (fn [k]
                            (if (namespace k)
                              (str (namespace k) "/" (name k))
                              (name k)))))

(defn to-clj
  "Convert JavaScript data to Clojure.
   JS Keyword instances and qualified strings ('ns/name') become CLJ keywords."
  [x]
  (cond
    (nil? x)            nil
    (boolean? x)        x
    (number? x)         x
    (string? x)         (if (re-find #"^[a-z].*/" x) (keyword x) x)
    (keyword? x)        x
    (js-keyword? x)     (keyword (aget x "fqn"))
    (array? x)          (mapv to-clj (array-seq x))
    :else
    (into {}
          (map (fn [k] [(->clj-kw k) (to-clj (aget x k))])
               (array-seq (js/Object.keys x))))))

(defn- js-set->clj-set
  "Convert a JS array to a Clojure set of keywords.
   Accepts JS Keyword instances, strings, or CLJ keywords."
  [js-val]
  (cond
    (set? js-val)       js-val
    (keyword? js-val)   #{js-val}
    (js-keyword? js-val) #{(->clj-kw js-val)}
    (string? js-val)    #{(keyword js-val)}
    (array? js-val)     (into #{} (map ->clj-kw) (array-seq js-val))
    :else               (into #{} (map ->clj-kw) (js->clj js-val))))

(defn- js-map->clj-map
  "Convert a JS object to a Clojure map with keyword keys (shallow)."
  [js-val]
  (if (map? js-val)
    js-val
    (let [raw (js->clj js-val)]
      (into {}
            (map (fn [[k v]] [(->clj-kw k) v]) raw)))))

(defn- js-value->clj-value
  "Deep-convert a JS value map for registration."
  [js-val]
  (if (map? js-val)
    js-val
    (to-clj js-val)))

;; =============================================================================
;; Registry Lifecycle
;; =============================================================================

(defn load-registry!
  "Load a registry from an EDN string. Replaces the current registry.
   Returns the number of entities loaded."
  [edn-string]
  (let [data (edn/read-string edn-string)]
    (reset! registry/registry data)
    (datalog/reset-db-cache!)
    (count data)))

(defn get-registry
  "Return the current registry as a JS object."
  []
  (to-js @registry/registry))

(defn reset-registry!
  "Reset the registry to empty."
  []
  (reset! registry/registry {})
  (datalog/reset-db-cache!)
  nil)

;; =============================================================================
;; Registration
;; =============================================================================

(defn register!
  "Register an entity in the registry.

   Accepts strings or js-qualified-keywords Keyword instances for all keyword args.

   Usage from JS:
     register(:fn/validate-token, :atlas/execution-function,
              [:domain/auth, :tier/service],
              {'execution-function/context': ['auth/token']})"
  ([type aspects value]
   (let [type-kw     (->clj-kw type)
         aspects-set (js-set->clj-set aspects)
         value-map   (js-value->clj-value value)
         result      (registry/register! type-kw aspects-set value-map)]
     (to-js result)))
  ([dev-id type aspects value]
   (let [dev-id-kw   (->clj-kw dev-id)
         type-kw     (->clj-kw type)
         aspects-set (js-set->clj-set aspects)
         value-map   (js-value->clj-value value)
         result      (registry/register! dev-id-kw type-kw aspects-set value-map)]
     (to-js result))))

;; =============================================================================
;; Query API
;; =============================================================================

(defn find-by-aspect
  "Find all entities containing the given aspect(s).
   Returns a JS object of {compound-id: entity-value}."
  [aspect]
  (let [aspect-val (if (or (string? aspect) (js-keyword? aspect))
                     (->clj-kw aspect)
                     (js-set->clj-set aspect))]
    (to-js (query/find-by-aspect @registry/registry aspect-val))))

(defn find-by-dev-id
  "Find entity by its dev-id. Returns [compound-id, entity-value] or null."
  [dev-id]
  (when-let [result (query/find-by-dev-id @registry/registry (->clj-kw dev-id))]
    (to-js (vec result))))

(defn find-dev-ids-with-aspect
  "Find all dev-ids having the given aspect(s). Returns a JS array."
  [aspect]
  (let [aspect-val (if (or (string? aspect) (js-keyword? aspect))
                     (->clj-kw aspect)
                     (js-set->clj-set aspect))]
    (to-js (query/find-dev-ids-with-aspect @registry/registry aspect-val))))

(defn find-exact
  "Find entity by exact compound identity match. Returns entity value or null."
  [identity-array]
  (let [id-set (js-set->clj-set identity-array)]
    (to-js (query/find-exact @registry/registry id-set))))

(defn where-js
  "Filter registry by a JS predicate function.
   The predicate receives (compoundIdArray, valueObject)."
  [pred-fn]
  (let [results (query/where @registry/registry
                  (fn [id v]
                    (pred-fn (to-js id) (to-js v))))]
    (to-js results)))

(defn all-identities
  "Return all compound identities as a JS array of arrays."
  []
  (to-js (query/all-identities @registry/registry)))

;; =============================================================================
;; Scoring & Matching
;; =============================================================================

(defn match-score
  "Return match score (0.0 to 1.0) for an identity against aspects.
   mode: 'and' | 'or' | 'count'"
  [identity aspects mode]
  (query/match-score (js-set->clj-set identity)
                     (js-set->clj-set aspects)
                     (keyword mode)))

(defn query-matches?
  "Check if an identity matches a query map."
  [identity query-map]
  (let [q (js-map->clj-map query-map)]
    (query/query-matches? (js-set->clj-set identity) q)))

(defn semantic-similarity
  "Find entities similar to a target identity using Jaccard similarity."
  ([target-identity]
   (semantic-similarity target-identity 0.0))
  ([target-identity min-similarity]
   (to-js (query/semantic-similarity @registry/registry
                                     (js-set->clj-set target-identity)
                                     min-similarity))))

;; =============================================================================
;; Analytics
;; =============================================================================

(defn aspect-frequency [] (to-js (query/aspect-frequency @registry/registry)))
(defn related-aspects [aspect] (to-js (query/related-aspects @registry/registry (->clj-kw aspect))))
(defn identity-stats [] (to-js (query/identity-stats @registry/registry)))

;; =============================================================================
;; Architecture Analysis
;; =============================================================================

(defn dependency-graph [id-key deps-key]
  (to-js (query/dependency-graph @registry/registry (->clj-kw id-key) (->clj-kw deps-key))))

(defn by-tier [id-key]
  (to-js (query/by-tier @registry/registry (->clj-kw id-key))))

(defn domain-coupling [id-key deps-key]
  (to-js (query/domain-coupling @registry/registry (->clj-kw id-key) (->clj-kw deps-key))))

(defn impact-of-change [entity-id id-key deps-key response-key]
  (to-js (query/impact-of-change @registry/registry
                                  (->clj-kw entity-id)
                                  (->clj-kw id-key)
                                  (->clj-kw deps-key)
                                  (->clj-kw response-key))))

;; =============================================================================
;; Data Flow
;; =============================================================================

(defn find-producers [data-key property-key]
  (to-js (query/find-producers @registry/registry (->clj-kw data-key) (->clj-kw property-key))))

(defn find-consumers [data-key property-key]
  (to-js (query/find-consumers @registry/registry (->clj-kw data-key) (->clj-kw property-key))))

(defn trace-data-flow [data-key producer-key consumer-key]
  (to-js (query/trace-data-flow @registry/registry
                                 (->clj-kw data-key)
                                 (->clj-kw producer-key)
                                 (->clj-kw consumer-key))))

;; =============================================================================
;; Set Algebra
;; =============================================================================

(defn query-algebra [ops]
  (let [ops-clj (js-map->clj-map ops)
        convert-sets (fn [coll] (mapv js-set->clj-set coll))
        converted (cond
                    (:intersection ops-clj) {:intersection (convert-sets (:intersection ops-clj))}
                    (:union ops-clj)        {:union (convert-sets (:union ops-clj))}
                    (:difference ops-clj)   (let [[inc exc] (:difference ops-clj)]
                                              {:difference [(js-set->clj-set inc)
                                                            (js-set->clj-set exc)]})
                    :else ops-clj)]
    (to-js (query/query-algebra @registry/registry converted))))

;; =============================================================================
;; Registry Introspection
;; =============================================================================

(defn registered-types [] (to-js (registry/registered-types)))
(defn entity-type [identity] (to-js (registry/entity-type (js-set->clj-set identity))))
(defn aspects [identity] (to-js (registry/aspects (js-set->clj-set identity))))
(defn summary [] (to-js (registry/summary)))
(defn validate-types [] (to-js (registry/validate-registry-types)))

;; =============================================================================
;; Invariants
;; =============================================================================

(defn check-invariants [] (to-js (inv/check-all)))

;; =============================================================================
;; Ontology
;; =============================================================================

(defn all-ontologies [] (to-js (ont/all-ontologies)))
(defn ontology-for [entity-type] (to-js (ont/ontology-for (->clj-kw entity-type))))

;; =============================================================================
;; Datalog Queries
;; =============================================================================

(defn rebuild-datalog! []
  (datalog/reset-db-cache!)
  (datalog/get-db)
  nil)

(defn query-entities-with-aspect [aspect]
  (to-js (datalog/query-entities-with-aspect (datalog/get-db) (->clj-kw aspect))))

(defn query-dependencies [dev-id]
  (to-js (datalog/query-dependencies (datalog/get-db) (->clj-kw dev-id))))

(defn query-reverse-dependencies [dev-id]
  (to-js (datalog/query-reverse-dependencies (datalog/get-db) (->clj-kw dev-id))))

(defn query-upstream-closure [start-ids max-hops]
  (let [ids (if (or (string? start-ids) (js-keyword? start-ids))
              (->clj-kw start-ids)
              (set (map ->clj-kw (array-seq start-ids))))]
    (to-js (datalog/query-upstream-closure (datalog/get-db) ids max-hops))))

(defn query-downstream-closure [start-ids max-hops]
  (let [ids (if (or (string? start-ids) (js-keyword? start-ids))
              (->clj-kw start-ids)
              (set (map ->clj-kw (array-seq start-ids))))]
    (to-js (datalog/query-downstream-closure (datalog/get-db) ids max-hops))))

(defn query-producers-of [key]
  (to-js (datalog/query-producers-of (datalog/get-db) (->clj-kw key))))

(defn query-consumers-of [key]
  (to-js (datalog/query-consumers-of (datalog/get-db) (->clj-kw key))))
