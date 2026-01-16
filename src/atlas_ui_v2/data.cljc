(ns atlas-ui-v2.data
  "Data transformations for atlas-ui-v2 dual-map visualization.

   Builds two complementary hierarchical views:
   1. Aspects map: namespace -> aspect names
   2. Entities map: atlas-type -> dev-id -> compound identity

   These maps support bidirectional filtering."
  (:require [clojure.set :as set]))

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

(defn stats
  "Compute statistics about the registry"
  [registry]
  (let [aspects-map (build-aspects-map registry)
        entities-map (build-entities-map registry)]
    {:aspect-count (count (extract-all-aspects registry))
     :namespace-count (count aspects-map)
     :entity-count (->> entities-map vals (mapcat keys) count)
     :type-count (count entities-map)}))
