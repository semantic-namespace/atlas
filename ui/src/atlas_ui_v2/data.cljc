(ns atlas-ui-v2.data
  "Data transformations for atlas-ui-v2 dual-map visualization.

   Builds two complementary hierarchical views:
   1. Aspects map: namespace -> aspect names
   2. Entities map: atlas-type -> dev-id -> compound identity

   These maps support bidirectional filtering."
  (:require [clojure.set :as set]
            [clojure.string :as str]))

;; =============================================================================
;; ASPECTS MAP: namespace -> #{names}
;; =============================================================================

(defn extract-all-aspects
  "Extract all unique aspects from registry.
   Returns set of namespaced keywords."
  [registry]
  (->> registry
       keys
       (mapcat identity)
       (filter keyword?)
       set))

(defn build-aspects-map
  "Build hierarchical map of aspects grouped by namespace.

   Input: registry
   Output: {:domain #{:users :auth :scheduling}
            :tier #{:service :api :foundation}
            :protocol #{:oauth :user-repository}
            ...}"
  [registry]
  (let [aspects (extract-all-aspects registry)]
    (->> aspects
         (group-by #(keyword (namespace %)))
         (map (fn [[ns-kw aspects-list]]
                [ns-kw (set (map #(keyword (name %)) aspects-list))]))
         (into (sorted-map)))))

;; =============================================================================
;; ENTITIES MAP: atlas-type -> dev-id -> compound-identity
;; =============================================================================

(defn extract-entity-type
  "Extract the atlas entity type from a compound identity.
   Returns the :atlas/* keyword if present."
  [compound-identity]
  (->> compound-identity
       (filter #(and (keyword? %)
                     (= "atlas" (namespace %))))
       first))

(defn build-entities-map
  "Build hierarchical map of entities grouped by type.

   Input: registry
   Output: {:atlas/execution-function
              {:fn/find-users-by-language #{:tier/service :domain/users ...}
               :fn/refresh-oauth-token #{:tier/service :domain/google ...}}
            :atlas/structure-component
              {:component/db #{:tier/foundation :domain/users ...}}
            ...}"
  [registry]
  (->> registry
       (group-by (fn [[identity _props]]
                   (extract-entity-type identity)))
       (map (fn [[entity-type entries]]
              [entity-type
               (->> entries
                    (map (fn [[identity props]]
                           [(:atlas/dev-id props) identity]))
                    (filter (fn [[dev-id _]] dev-id))
                    (into (sorted-map)))]))
       (filter (fn [[entity-type _]] entity-type))
       (into (sorted-map))))

;; =============================================================================
;; STATS
;; =============================================================================

(defn count-entities-by-aspect
  "Count how many entities have each aspect.
   Returns map of {aspect-keyword -> count}"
  [registry]
  (->> registry
       keys  ; Get all compound identities
       (mapcat identity)  ; Flatten to individual aspects
       (filter keyword?)  ; Only keywords
       frequencies))  ; Count occurrences

(defn stats
  "Compute statistics about the registry"
  [registry]
  (let [aspects-map (build-aspects-map registry)
        entities-map (build-entities-map registry)]
    {:aspect-count (count (extract-all-aspects registry))
     :namespace-count (count aspects-map)
     :entity-count (->> entities-map vals (mapcat keys) count)
     :type-count (count entities-map)}))

;; =============================================================================
;; ASPECT STATS HELPERS (from backend api)
;; =============================================================================

(defn aspect-stats->map
  "Convert aspect-stats from API (vector of {:aspect/aspect kw :aspect/count N})
   to a map of {aspect-kw -> count} for easier lookup."
  [aspect-stats]
  (into {}
        (map (fn [entry]
               [(:aspect/aspect entry) (:aspect/count entry)]))
        aspect-stats))

(defn namespace-stats
  "Compute statistics for each namespace from aspect-stats.
   Returns map of ns-keyword -> {:total-usage N :aspect-count N}"
  [aspect-stats-map]
  (reduce (fn [acc [aspect cnt]]
            (if-let [ns (namespace aspect)]
              (let [ns-key (keyword ns)]
                (-> acc
                    (update-in [ns-key :total-usage] (fnil + 0) cnt)
                    (update-in [ns-key :aspect-count] (fnil inc 0))))
              acc))  ; Return acc unchanged if no namespace
          {}
          aspect-stats-map))

;; =============================================================================
;; SORTING
;; =============================================================================

(defn sort-aspects-map
  "Sort the aspects map based on sort-by option.
   aspect-stats-map: {aspect-kw -> count} from aspect-stats->map

   Options:
   - :alpha-asc - Alphabetical A-Z (default)
   - :alpha-desc - Alphabetical Z-A
   - :usage-desc - By total usage (most used first)
   - :usage-asc - By total usage (least used first)
   - :count-desc - By aspect count (most aspects first)
   - :count-asc - By aspect count (fewest aspects first)"
  [aspects-map aspect-stats-map sort-by]
  (let [ns-stats (namespace-stats aspect-stats-map)]
    #?(:cljs (js/console.log "sort-aspects-map ns-stats sample:"
                             (clj->js (take 3 ns-stats))))
    (case sort-by
      :alpha-asc
      (into (sorted-map) aspects-map)

      :alpha-desc
      (into (sorted-map-by #(compare %2 %1)) aspects-map)

      :usage-desc
      (into (sorted-map-by
             (fn [k1 k2]
               (let [u1 (get-in ns-stats [k1 :total-usage] 0)
                     u2 (get-in ns-stats [k2 :total-usage] 0)]
                 (if (= u1 u2)
                   (compare k1 k2)
                   (compare u2 u1)))))
            aspects-map)

      :usage-asc
      (into (sorted-map-by
             (fn [k1 k2]
               (let [u1 (get-in ns-stats [k1 :total-usage] 0)
                     u2 (get-in ns-stats [k2 :total-usage] 0)]
                 (if (= u1 u2)
                   (compare k1 k2)
                   (compare u1 u2)))))
            aspects-map)

      :count-desc
      (into (sorted-map-by
             (fn [k1 k2]
               (let [c1 (get-in ns-stats [k1 :aspect-count] 0)
                     c2 (get-in ns-stats [k2 :aspect-count] 0)]
                 (if (= c1 c2)
                   (compare k1 k2)
                   (compare c2 c1)))))
            aspects-map)

      :count-asc
      (into (sorted-map-by
             (fn [k1 k2]
               (let [c1 (get-in ns-stats [k1 :aspect-count] 0)
                     c2 (get-in ns-stats [k2 :aspect-count] 0)]
                 (if (= c1 c2)
                   (compare k1 k2)
                   (compare c1 c2)))))
            aspects-map)

      ;; Default to alphabetical
      (into (sorted-map) aspects-map))))

(defn sort-aspect-names
  "Sort aspect names within a namespace.

   Options:
   - :alpha-asc - Alphabetical A-Z (default)
   - :alpha-desc - Alphabetical Z-A
   - :usage-desc - By usage count (most used first)
   - :usage-asc - By usage count (least used first)"
  [ns-key aspect-names aspect-stats-map sort-by]
  (let [result (case sort-by
                 :alpha-asc
                 (sort aspect-names)

                 :alpha-desc
                 (sort #(compare %2 %1) aspect-names)

                 :usage-desc
                 (reverse (sort-by (fn [aspect-name]
                                     (let [full-aspect (keyword (name ns-key) (name aspect-name))]
                                       (get aspect-stats-map full-aspect 0)))
                                   aspect-names))

                 :usage-asc
                 (sort-by (fn [aspect-name]
                            (let [full-aspect (keyword (name ns-key) (name aspect-name))]
                              (get aspect-stats-map full-aspect 0)))
                          aspect-names)

                 ;; Default
                 (sort aspect-names))]
    #?(:cljs (when (and (= ns-key :domain) (contains? #{:usage-desc :usage-asc} sort-by))
               (let [first-3 (take 3 result)
                     counts-str (->> first-3
                                     (map (fn [aspect-name]
                                            (let [full-aspect (keyword (name ns-key) (name aspect-name))
                                                  cnt (get aspect-stats-map full-aspect 0)]
                                              (str (name aspect-name) ":" cnt))))
                                     (str/join ", "))]
                 (js/console.log "sort-aspect-names" (name ns-key) "sort-by:" (name sort-by)
                                "first-3:" counts-str))))
    result))

(defn sort-entities-map
  "Sort the entities map based on sort-by option.

   Options:
   - :alpha-asc - Alphabetical A-Z by type name (default)
   - :alpha-desc - Alphabetical Z-A by type name
   - :count-desc - By count (most entities first)
   - :count-asc - By count (fewest entities first)"
  [entities-map sort-by]
  #?(:cljs (js/console.log "sort-entities-map counts:"
                           (clj->js (into {} (map (fn [[k v]] [k (count v)]) entities-map)))))
  (case sort-by
    :alpha-asc
    (into (sorted-map) entities-map)

    :alpha-desc
    (into (sorted-map-by #(compare %2 %1)) entities-map)

    :count-desc
    (into (sorted-map-by
           (fn [k1 k2]
             (let [c1 (count (get entities-map k1))
                   c2 (count (get entities-map k2))]
               (if (= c1 c2)
                 (compare k1 k2)
                 (compare c2 c1)))))
          entities-map)

    :count-asc
    (into (sorted-map-by
           (fn [k1 k2]
             (let [c1 (count (get entities-map k1))
                   c2 (count (get entities-map k2))]
               (if (= c1 c2)
                 (compare k1 k2)
                 (compare c1 c2)))))
          entities-map)

    ;; Default to alphabetical
    (into (sorted-map) entities-map)))

(defn sort-dev-ids
  "Sort dev-ids within an entity type.

   Options:
   - :alpha-asc - Alphabetical A-Z (default)
   - :alpha-desc - Alphabetical Z-A
   - :aspect-count-desc - By number of aspects (most first)
   - :aspect-count-asc - By number of aspects (fewest first)"
  [dev-id-map sort-by]
  (let [result (case sort-by
                 :alpha-asc
                 (into (sorted-map) dev-id-map)

                 :alpha-desc
                 (into (sorted-map-by #(compare %2 %1)) dev-id-map)

                 :aspect-count-desc
                 (reverse (sort-by (fn [[_dev-id identity]]
                                     (count (filter keyword? identity)))
                                   dev-id-map))

                 :aspect-count-asc
                 (sort-by (fn [[_dev-id identity]]
                            (count (filter keyword? identity)))
                          dev-id-map)

                 ;; Default to alphabetical
                 (into (sorted-map) dev-id-map))]
    #?(:cljs (when (contains? #{:aspect-count-desc :aspect-count-asc} sort-by)
               (let [first-3 (take 3 result)
                     counts-str (->> first-3
                                     (map (fn [[dev-id identity]]
                                            (let [cnt (count (filter keyword? identity))]
                                              (str (name dev-id) ":" cnt))))
                                     (str/join ", "))]
                 (js/console.log "sort-dev-ids sort-by:" (name sort-by)
                                "first-3:" counts-str
                                "total:" (count dev-id-map)))))
    result))
