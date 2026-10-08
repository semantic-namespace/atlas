(ns atlas.review.risk
  (:require [atlas.ide :as ide]
            [atlas.registry :as registry]
            [atlas.registry.lookup :as lookup]))

(defonce surface-types
  (atom #(boolean (re-find #"endpoint|mcp-tool|workflow|llm-prompt" (name %)))))

(defn- type-of [id] (:atlas/type (lookup/props-for id)))

(defn- tests-targeting [ids]
  (let [ids (set ids)]
    (sort (for [[_ e] (registry/current-registry)
                :when (and (= :atlas/test-case (:atlas/type e)) (ids (:test-case/target e)))]
            (:atlas/dev-id e)))))

(defn- external [aspects]
  (sort (map name (filter #(= "services" (namespace %)) aspects))))

(defn- writes? [aspects]
  (some #(and (#{"action" "effect" "operation"} (namespace %))
              (re-find #"write|create|delete|update|register|notify|send|remove" (name %)))
        aspects))

(defn assess
  "Risk of changing `id`, from the registry bound as current. `change` is
  :contract, :code or :deleted. Returns the level, the score and every reason
  that produced it, so the level can be checked rather than trusted."
  [id change]
  (let [aspects (lookup/identity-for id)
        affected (:summary/affected (ide/recursive-dependents-summary id))
        surfaces (filter #(@surface-types (or (type-of %) :none)) (cons id affected))
        surface-types* (frequencies (map type-of surfaces))
        tests (tests-targeting (cons id affected))
        ext (external aspects)
        reasons (cond-> []
                  (= :deleted change) (conj [3 "deleted"])
                  (= :contract change) (conj [2 "its contract changed"])
                  (seq surfaces) (conj [2 (str "reaches " (count surfaces) " entry point" (when (> (count surfaces) 1) "s") " ("
                                               (apply str (interpose ", " (for [[t n] (sort-by (comp - val) surface-types*)] (str n " " (name t))))) ")")])
                  (>= (count affected) 10) (conj [1 (str (count affected) " entities depend on it, directly or not")])
                  (>= (count affected) 30) (conj [1 "a wide blast radius"])
                  (seq ext) (conj [1 (str "talks to " (apply str (interpose ", " ext)))])
                  (writes? aspects) (conj [1 "it writes or notifies"])
                  (empty? tests) (conj [1 "no test case covers it or what depends on it"]))
        score (reduce + (map first reasons))]
    {:id id
     :level (cond (>= score 5) :high (>= score 3) :medium :else :low)
     :score score
     :reasons (mapv second reasons)
     :affected (count affected)
     :surfaces (vec surfaces)
     :tests (count tests)}))
