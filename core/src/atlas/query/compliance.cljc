(ns atlas.query.compliance
  "Compliance and coverage queries: PII surface, error handler coverage,
   aspect coverage, architectural decisions.
   These hardcode specific aspect conventions and belong outside the kernel."
  (:require [atlas.query :as query]
            [clojure.set :as set]))

(defn pii-surface
  "Find all entities handling PII with audit status.
   Returns seq of {:id :audited? :context :response} maps."
  [registry id-key context-key response-key]
  (->> (query/find-by-aspect registry :compliance/pii)
       (map (fn [[id v]]
              {:id (get v id-key)
               :audited? (contains? id :compliance/audited)
               :context (get v context-key)
               :response (get v response-key)}))))

(defn aspect-coverage
  "Show which entities have specific cross-cutting concerns.
   Returns seq of {:id :has} where :has is map of aspect->boolean."
  [registry entity-type id-key aspects]
  (for [[id v] (query/find-by-aspect registry entity-type)]
    {:id (get v id-key)
     :has (into {} (for [a aspects]
                     [a (contains? id a)]))}))

(defn error-handler-coverage
  "Check if error handlers exist for marked concerns.
   Returns {:handlers :coverage}."
  [registry id-key]
  (let [handlers (query/find-by-aspect registry :semantic-namespace/error-handler)
        needs-handling #{:protocol/http :temporal/timeout
                         :authorization/required :capacity/rate-limited}]
    {:handlers (map (fn [[id v]]
                      {:id (get v id-key)
                       :handles (set/intersection id needs-handling)})
                    handlers)
     :coverage (for [concern needs-handling
                     [id v] (query/find-by-aspect registry concern)]
                 {:entity (get v id-key)
                  :concern concern
                  :has-handler? (boolean (some #(contains? % concern) (keys handlers)))})}))

(defn decisions-by-category
  "Group architectural decisions by category.
   Returns map of category->[decision-maps]."
  [registry id-key]
  (->> (query/find-by-aspect registry :semantic-namespace/architectural-decision)
       (map (fn [[id v]]
              {:id (get v id-key)
               :category (first (filter #(= "decision-category" (namespace %)) id))
               :question (:decision/question v)
               :chosen (:decision/chosen v)
               :priority (:decision/priority v)}))
       (group-by :category)))
