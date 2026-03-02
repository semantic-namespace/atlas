(ns atlas-js.core-slim
  "Atlas JS slim client — registry + query + ontology, no datascript/datalog.

   Same API as atlas-js.core minus the datalog query functions.
   Significantly smaller bundle (~125KB less)."
  (:require [atlas.registry :as registry]
            [atlas.query :as query]
            [atlas.ontology :as ont]
            [clojure.edn :as edn]
            [clojure.set :as set]))

;; =============================================================================
;; JS <-> CLJ Conversion Helpers
;; =============================================================================

(defn- js-keyword? [x]
  (and (some? x) (not (string? x)) (not (keyword? x)) (string? (aget x "fqn"))))

(defn- ->clj-kw [x]
  (cond
    (keyword? x)    x
    (js-keyword? x) (keyword (aget x "fqn"))
    (string? x)     (keyword x)
    :else           (keyword (str x))))

(defn to-js [x]
  (clj->js x :keyword-fn (fn [k]
                            (if (namespace k)
                              (str (namespace k) "/" (name k))
                              (name k)))))

(defn to-clj [x]
  (cond
    (nil? x)        nil
    (boolean? x)    x
    (number? x)     x
    (string? x)     (if (re-find #"^[a-z].*/" x) (keyword x) x)
    (keyword? x)    x
    (js-keyword? x) (keyword (aget x "fqn"))
    (array? x)      (mapv to-clj (array-seq x))
    :else
    (into {}
          (map (fn [k] [(->clj-kw k) (to-clj (aget x k))])
               (array-seq (js/Object.keys x))))))

(defn- js-set->clj-set [js-val]
  (cond
    (set? js-val)        js-val
    (keyword? js-val)    #{js-val}
    (js-keyword? js-val) #{(->clj-kw js-val)}
    (string? js-val)     #{(keyword js-val)}
    (array? js-val)      (into #{} (map ->clj-kw) (array-seq js-val))
    :else                (into #{} (map ->clj-kw) (js->clj js-val))))

(defn- js-map->clj-map [js-val]
  (if (map? js-val)
    js-val
    (let [raw (js->clj js-val)]
      (into {} (map (fn [[k v]] [(->clj-kw k) v]) raw)))))

(defn- js-value->clj-value [js-val]
  (if (map? js-val) js-val (to-clj js-val)))

;; =============================================================================
;; Registry Lifecycle
;; =============================================================================

(defn load-registry! [edn-string]
  (let [data (edn/read-string edn-string)]
    (reset! registry/registry data)
    (count data)))

(defn get-registry [] (to-js @registry/registry))

(defn reset-registry! []
  (reset! registry/registry {})
  nil)

;; =============================================================================
;; Registration
;; =============================================================================

(defn register!
  ([type aspects value]
   (let [type-kw     (->clj-kw type)
         aspects-set (js-set->clj-set aspects)
         value-map   (js-value->clj-value value)]
     (to-js (registry/register! type-kw aspects-set value-map))))
  ([dev-id type aspects value]
   (let [dev-id-kw   (->clj-kw dev-id)
         type-kw     (->clj-kw type)
         aspects-set (js-set->clj-set aspects)
         value-map   (js-value->clj-value value)]
     (to-js (registry/register! dev-id-kw type-kw aspects-set value-map)))))

;; =============================================================================
;; Query API
;; =============================================================================

(defn find-by-aspect [aspect]
  (let [aspect-val (if (or (string? aspect) (js-keyword? aspect))
                     (->clj-kw aspect)
                     (js-set->clj-set aspect))]
    (to-js (query/find-by-aspect @registry/registry aspect-val))))

(defn find-by-dev-id [dev-id]
  (when-let [result (query/find-by-dev-id @registry/registry (->clj-kw dev-id))]
    (to-js (vec result))))

(defn find-dev-ids-with-aspect [aspect]
  (let [aspect-val (if (or (string? aspect) (js-keyword? aspect))
                     (->clj-kw aspect)
                     (js-set->clj-set aspect))]
    (to-js (query/find-dev-ids-with-aspect @registry/registry aspect-val))))

(defn find-exact [identity-array]
  (to-js (query/find-exact @registry/registry (js-set->clj-set identity-array))))

(defn where-js [pred-fn]
  (to-js (query/where @registry/registry
           (fn [id v] (pred-fn (to-js id) (to-js v))))))

(defn all-identities [] (to-js (query/all-identities @registry/registry)))

;; =============================================================================
;; Scoring & Matching
;; =============================================================================

(defn match-score [identity aspects mode]
  (query/match-score (js-set->clj-set identity) (js-set->clj-set aspects) (keyword mode)))

(defn query-matches? [identity query-map]
  (query/query-matches? (js-set->clj-set identity) (js-map->clj-map query-map)))

(defn semantic-similarity
  ([target-identity] (semantic-similarity target-identity 0.0))
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
                                  (->clj-kw entity-id) (->clj-kw id-key)
                                  (->clj-kw deps-key) (->clj-kw response-key))))

;; =============================================================================
;; Data Flow
;; =============================================================================

(defn find-producers [data-key property-key]
  (to-js (query/find-producers @registry/registry (->clj-kw data-key) (->clj-kw property-key))))

(defn find-consumers [data-key property-key]
  (to-js (query/find-consumers @registry/registry (->clj-kw data-key) (->clj-kw property-key))))

(defn trace-data-flow [data-key producer-key consumer-key]
  (to-js (query/trace-data-flow @registry/registry
                                 (->clj-kw data-key) (->clj-kw producer-key) (->clj-kw consumer-key))))

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
                                              {:difference [(js-set->clj-set inc) (js-set->clj-set exc)]})
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
;; Ontology
;; =============================================================================

(defn all-ontologies [] (to-js (ont/all-ontologies)))
(defn ontology-for [entity-type] (to-js (ont/ontology-for (->clj-kw entity-type))))
