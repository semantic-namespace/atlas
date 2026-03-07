(ns atlas.query
  "Unified query interface for compound identity registries.
   Pure set-based query operations (no datalog).

   All functions take registry as first parameter for composability.
   This namespace consolidates duplicated query logic from:
   - atlas.registry
   - atlas.semantic-queries
   - atlas.registry.graph"
  (:require [clojure.set :as set]))

;; =============================================================================
;; Core Query Functions
;; =============================================================================

(defn find-by-aspect
  "Find all entries containing aspect(s).
   Returns map of identities→values, sorted by identity size.

   UNIFIES:
   - atlas.registry/find-with
   - atlas.semantic-queries/find-by"
  [registry aspect]
  (let [aspect-set (if (set? aspect) aspect #{aspect})]
    (->> registry
         (filter (fn [[id _]] (set/superset? id aspect-set)))
         (sort-by (comp count key))
         (into (array-map)))))

(defn find-by-dev-id
  "Find the [identity value] pair for a given dev-id.
   Returns nil if not found.

   UNIFIES:
   - atlas.registry/find-by-dev-id
   - atlas.registry.graph/fetch-by-dev-id"
  [registry dev-id]
  (when dev-id
    (first (filter (fn [[_ v]] (= (:atlas/dev-id v) dev-id))
                   registry))))

(defn find-dev-ids-with-aspect
  "Find all dev-ids with given aspect(s). Returns set of dev-ids.

   Derived from find-by-aspect for convenience.
   Replaces atlas.registry.graph/all-with-aspect."
  [registry aspect]
  (->> (find-by-aspect registry aspect)
       (map (fn [[_ v]] (:atlas/dev-id v)))
       set))

(defn find-exact
  "Find exact identity match. Returns value or nil.

   From atlas.registry/find-exact"
  [registry identity-set]
  (get registry identity-set))

(defn where
  "Filter registry entries by predicate (pred identity value).
   Returns filtered map.

   From atlas.registry/where"
  [registry pred]
  (->> registry
       (filter (fn [[id v]] (pred id v)))
       (into {})))

(defn all-identities
  "Return all compound identities in registry as vector.

   From atlas.registry/all-identities"
  [registry]
  (vec (keys registry)))

;; =============================================================================
;; Match Scoring
;; =============================================================================

(defn match-count
  "Return how many of `selected-aspects` are present in `identity`."
  [identity selected-aspects]
  (let [identity-set (if (set? identity) identity (set identity))
        selected-set (if (set? selected-aspects) selected-aspects (set selected-aspects))]
    (count (set/intersection identity-set selected-set))))

(defn match-score
  "Return match score (0.0 to 1.0) for `identity` against `selected-aspects`.

  query-mode:
  - :and   => 1.0 iff identity contains ALL selected aspects, else 0.0
  - :or    => 1.0 iff identity contains ANY selected aspect, else 0.0
  - :count => (match-count / total-selected)

  Returns 0.0 when `selected-aspects` is empty."
  [identity selected-aspects query-mode]
  (let [identity-set (if (set? identity) identity (set identity))
        selected-set (if (set? selected-aspects) selected-aspects (set selected-aspects))
        total-selected (count selected-set)]
    (if (zero? total-selected)
      0.0
      (let [matched-count (count (set/intersection identity-set selected-set))]
        (case query-mode
          :and (if (= matched-count total-selected) 1.0 0.0)
          :or (if (pos? matched-count) 1.0 0.0)
          :count (/ matched-count (double total-selected))
          0.0)))))

(defn matches?
  "Return true if `identity` matches `selected-aspects` under `query-mode`.

  query-mode:
  - :and => ALL selected aspects must be present
  - :or  => ANY selected aspect must be present
  - :count => ANY selected aspect must be present

  Returns false when `selected-aspects` is empty."
  [identity selected-aspects query-mode]
  (let [identity-set (if (set? identity) identity (set identity))
        selected-set (if (set? selected-aspects) selected-aspects (set selected-aspects))
        total-selected (count selected-set)]
    (if (pos? total-selected)
      (let [matched-count (count (set/intersection identity-set selected-set))]
        (case query-mode
          :and (= matched-count total-selected)
          :or (pos? matched-count)
          :count (pos? matched-count)
          false))
      false)))

;; =============================================================================
;; Query Semantics (Negation & Thresholds)
;; =============================================================================

(defn- ->aspect-set [aspects]
  (cond
    (nil? aspects) #{}
    (set? aspects) aspects
    :else (set aspects)))

(defn- qget
  "Get a query value from either a namespaced key (preferred) or a legacy
  unqualified key."
  [query namespaced-key legacy-key]
  (if (contains? query namespaced-key)
    (get query namespaced-key)
    (get query legacy-key)))

(defn excluded-by-negation?
  "Return true if identity contains any of the `negated-aspects`."
  [identity negated-aspects]
  (let [identity-set (if (set? identity) identity (set identity))
        negated-set (->aspect-set negated-aspects)]
    (boolean (seq (set/intersection identity-set negated-set)))))

(defn query-score
  "Return match score (0.0-1.0) for `identity` against a query map.

  Query map:
  - ::selected  collection of aspects (may be empty)
  - ::negated   collection of aspects (may be empty)
  - ::mode      :and | :or | :count (defaults to :count)

  If `identity` contains any `:negated` aspect, the score is 0.0."
  [identity query]
  (let [identity-set (if (set? identity) identity (set identity))
        selected-set (->aspect-set (qget query ::selected :selected))
        negated-set (->aspect-set (qget query ::negated :negated))
        mode (or (qget query ::mode :mode) :count)]
    (if (seq (set/intersection identity-set negated-set))
      0.0
      (match-score identity-set selected-set mode))))

(defn query-matches?
  "Return true if `identity` matches a query map.

  Query map:
  - ::selected  collection of aspects (may be empty)
  - ::negated   collection of aspects (may be empty)
  - ::mode      :and | :or | :count (defaults to :count)
  - ::min-score minimum score threshold (defaults to 0.0)

  Notes:
  - If `:selected` is empty, the query matches as long as negations don't exclude.
  - `:min-score` is only applied when `:selected` is non-empty."
  [identity query]
  (let [identity-set (if (set? identity) identity (set identity))
        selected-set (->aspect-set (qget query ::selected :selected))
        negated-set (->aspect-set (qget query ::negated :negated))
        mode (or (qget query ::mode :mode) :count)
        min-score (double (or (qget query ::min-score :min-score) 0.0))
        excluded? (seq (set/intersection identity-set negated-set))
        selected-empty? (empty? selected-set)
        matched-count (count (set/intersection identity-set selected-set))
        positive-match?
        (case mode
          :and (set/superset? identity-set selected-set)
          :or (or selected-empty? (pos? matched-count))
          :count (or selected-empty? (pos? matched-count))
          (or selected-empty? (pos? matched-count)))]
    (and (not excluded?)
         positive-match?
         (or selected-empty?
             (>= (query-score identity-set {:selected selected-set
                                            :negated negated-set
                                            :mode mode})
                 min-score)))))

;; =============================================================================
;; Similarity & Discovery
;; =============================================================================

(defn semantic-similarity
  "Find identities similar to target identity using Jaccard similarity.
   Returns sorted list of {:identity :shared :similarity} maps.
   min-similarity is threshold (0.0-1.0), defaults to 0.0.

   UNIFIES:
   - atlas.registry/semantic-neighbors
   - atlas.semantic-queries/semantic-similarity"
  ([registry target-identity]
   (semantic-similarity registry target-identity 0.0))
  ([registry target-identity min-similarity]
   (->> registry
        (keys)
        (keep (fn [other]
                (when (not= target-identity other)
                  (let [shared (set/intersection target-identity other)
                        union (set/union target-identity other)
                        sim (if (seq union)
                              (/ (count shared) (count union))
                              0)]
                    (when (>= sim min-similarity)
                      {:identity other
                       :shared shared
                       :similarity sim})))))
        (sort-by :similarity >))))

(defn related-aspects
  "Return frequency map of co-occurring aspects for given aspect.
   Sorted by frequency descending.

   From atlas.registry/related-to"
  [registry aspect]
  (->> (all-identities registry)
       (filter #(contains? % aspect))
       (mapcat identity)
       (remove #{aspect})
       (frequencies)
       (sort-by val >)
       (into {})))

;; =============================================================================
;; Analytics
;; =============================================================================

(defn aspect-frequency
  "Return frequency map of aspects across all identities, sorted descending.

   From atlas.registry/aspect-frequency"
  [registry]
  (->> registry
       (keys)
       (mapcat identity)
       (frequencies)
       (sort-by val >)
       (into {})))

(defn identity-stats
  "Return list of {:identity :size :value} sorted by descending size.

   From atlas.registry/identity-stats"
  [registry]
  (->> (all-identities registry)
       (map (fn [id]
              {:identity id
               :size (count id)
               :value (get registry id)}))
       (sort-by :size >)))

;; =============================================================================
;; Data Flow Queries — delegated to atlas.query.dataflow
;; =============================================================================

(defn find-producers  [registry data-key property-key]              ((requiring-resolve 'atlas.query.dataflow/find-producers) registry data-key property-key))
(defn find-consumers  [registry data-key property-key]              ((requiring-resolve 'atlas.query.dataflow/find-consumers) registry data-key property-key))
(defn trace-data-flow [registry data-key producer-key consumer-key] ((requiring-resolve 'atlas.query.dataflow/trace-data-flow) registry data-key producer-key consumer-key))

;; =============================================================================
;; Algebraic Operations
;; =============================================================================

(defn query-algebra
  "Perform set-algebraic queries over compound identities.
   Supported ops:
     {:intersection [aspect-sets]}  - Find identities containing ALL aspect sets
     {:union [aspect-sets]}         - Find identities containing ANY aspect set
     {:difference [include exclude]} - Find identities with include but not exclude

   Returns vector of sorted identities.

   From atlas.registry/query-algebra"
  [registry ops]
  (let [all-ids (set (all-identities registry))
        results
        (cond
          (:intersection ops)
          (filter #(every? (fn [x] (set/superset? % x)) (:intersection ops)) all-ids)

          (:union ops)
          (filter #(some (fn [x] (set/superset? % x)) (:union ops)) all-ids)

          (:difference ops)
          (let [[include exclude] (:difference ops)]
            (filter #(and (set/superset? % include)
                          (empty? (set/intersection % exclude)))
                    all-ids))

          :else
          (throw (ex-info "Unknown operation" {:ops ops})))]
    (->> results (map #(vec (sort %))) sort vec)))

(defn query-superset
  "Return all compound identities that are supersets of find*.
   Returns vector of sorted identities.

   From atlas.registry/query"
  [registry find*]
  (let [find* (set find*)]
    (->> (keys registry)
         (filter #(set/superset? % find*))
         (map #(vec (sort %)))
         (sort)
         (vec))))

;; =============================================================================
;; Architecture Analysis — delegated to atlas.query.architecture
;; =============================================================================

(defn dependency-graph  [registry id-key deps-key]                          ((requiring-resolve 'atlas.query.architecture/dependency-graph) registry id-key deps-key))
(defn by-tier           [registry id-key]                                   ((requiring-resolve 'atlas.query.architecture/by-tier) registry id-key))
(defn domain-coupling   [registry id-key deps-key]                          ((requiring-resolve 'atlas.query.architecture/domain-coupling) registry id-key deps-key))
(defn impact-of-change  [registry entity-id id-key deps-key response-key]   ((requiring-resolve 'atlas.query.architecture/impact-of-change) registry entity-id id-key deps-key response-key))

;; =============================================================================
;; Compliance & Coverage — delegated to atlas.query.compliance
;; =============================================================================

(defn pii-surface              [registry id-key context-key response-key]  ((requiring-resolve 'atlas.query.compliance/pii-surface) registry id-key context-key response-key))
(defn aspect-coverage          [registry entity-type id-key aspects]       ((requiring-resolve 'atlas.query.compliance/aspect-coverage) registry entity-type id-key aspects))
(defn error-handler-coverage   [registry id-key]                           ((requiring-resolve 'atlas.query.compliance/error-handler-coverage) registry id-key))
(defn decisions-by-category    [registry id-key]                           ((requiring-resolve 'atlas.query.compliance/decisions-by-category) registry id-key))
