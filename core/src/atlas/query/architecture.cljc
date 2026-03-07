(ns atlas.query.architecture
  "Architecture analysis queries: dependency graphs, domain coupling,
   impact analysis, tier grouping.
   These are domain-specific query patterns built on top of atlas.query."
  (:require [atlas.query :as query]
            [clojure.set :as set]))

(defn dependency-graph
  "Build dependency graph from entities with :deps key.
   Returns seq of {:id :deps :identity} maps."
  [registry id-key deps-key]
  (->> registry
       (filter (fn [[_ v]] (get v deps-key)))
       (map (fn [[id v]]
              {:id (get v id-key)
               :deps (get v deps-key)
               :identity id}))
       (remove #(empty? (:deps %)))))

(defn by-tier
  "Group entities by architectural tier.
   Returns map of tier->[dev-ids]."
  [registry id-key]
  (let [tiers [:tier/foundation :tier/service :tier/api]]
    (into {}
          (for [tier tiers]
            [tier (->> (query/find-by-aspect registry tier)
                       vals
                       (map #(get % id-key))
                       (remove nil?)
                       vec)]))))

(defn domain-coupling
  "Analyze inter-domain dependencies.
   Returns seq of {:domain :depends-on :entity-count} maps."
  [registry id-key deps-key]
  (let [domains (->> registry
                     keys
                     (mapcat identity)
                     (filter #(= "domain" (namespace %)))
                     set)]
    (for [domain domains
          :let [domain-entities (query/find-by-aspect registry domain)
                deps (->> domain-entities vals (mapcat #(get % deps-key)) set)
                dep-domains (->> registry
                                 (filter (fn [[_ v]] (some deps #{(get v id-key)})))
                                 (mapcat (fn [[id _]]
                                           (filter #(= "domain" (namespace %)) id)))
                                 set)]]
      {:domain domain
       :depends-on (disj dep-domains domain)
       :entity-count (count domain-entities)})))

(defn impact-of-change
  "Analyze what would be affected if entity changes.
   Returns {:entity :entity/produces :direct-dependents}."
  [registry entity-id id-key deps-key response-key]
  (let [entity-entry (->> registry
                          (filter (fn [[_ v]] (= entity-id (get v id-key))))
                          first)
        entity-response (get (second entity-entry) response-key)
        direct-deps (->> registry
                         (filter (fn [[_ v]]
                                   (some #{entity-id} (get v deps-key))))
                         (map (fn [[_ v]] (get v id-key))))]
    {:entity entity-id
     :entity/produces entity-response
     :direct-dependents (vec direct-deps)}))
