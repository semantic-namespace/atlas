(ns atlas.datalog.graph
  "Datascript-free view of the registry's datalog facts: the same entities,
   attribute cardinalities and edge queries as `atlas.datalog`, held as plain maps."
  (:require [atlas.datalog.facts :as df]
            [clojure.set :as set]))

(defn- many? [schema attr]
  (= :db.cardinality/many (get-in schema [attr :db/cardinality])))

(defn from-facts
  [schema facts]
  (let [entities (reduce (fn [m [_ id attr v]]
                           (if (many? schema attr)
                             (update-in m [id attr] (fnil conj #{}) v)
                             (assoc-in m [id attr] v)))
                         {}
                         facts)
        index (reduce-kv (fn [idx id attrs]
                           (reduce-kv (fn [idx attr v]
                                        (reduce #(update-in %1 [attr %2] (fnil conj #{}) id)
                                                idx
                                                (if (many? schema attr) v [v])))
                                      idx
                                      attrs))
                         {}
                         entities)]
    {:schema schema
     :entities entities
     :index index}))

(defn from-registry
  [reg]
  (from-facts (df/schema reg) (df/all-facts reg)))

(defonce ^:private cache (atom {:registry nil :graph nil}))

(defn for-registry
  [reg]
  (let [{cached-reg :registry cached :graph} @cache]
    (if (and cached (identical? reg cached-reg))
      cached
      (let [g (from-registry reg)]
        (reset! cache {:registry reg :graph g})
        g))))

(defn entity-ids [g]
  (keys (:entities g)))

(defn attr-values [g id attr]
  (let [v (get-in g [:entities id attr])]
    (cond (many? (:schema g) attr) (or v #{})
          (nil? v) #{}
          :else #{v})))

(defn holders [g attr v]
  (get-in g [:index attr v] #{}))

(defn entity-aspects [g id] (attr-values g id :entity/aspect))
(defn dependencies [g id] (attr-values g id :entity/depends))
(defn reverse-dependencies [g id] (holders g :entity/depends id))
(defn produces [g id] (attr-values g id :entity/produces))
(defn consumes [g id] (attr-values g id :entity/consumes))
(defn producers-of [g k] (holders g :entity/produces k))
(defn consumers-of [g k] (holders g :entity/consumes k))

(defn effective-dependencies
  "What `id` declares it depends on, and the producers of the data it
   consumes: an entity reading a key depends on whatever writes it."
  [g id]
  (-> (dependencies g id)
      (into (mapcat #(producers-of g %) (consumes g id)))
      (disj id)))

(defn effective-dependents
  "What declares a dependency on `id`, and the consumers of the data it
   produces."
  [g id]
  (-> (reverse-dependencies g id)
      (into (mapcat #(consumers-of g %) (produces g id)))
      (disj id)))

(defn closure
  [step start-ids max-hops]
  (let [start-set (if (set? start-ids) start-ids #{start-ids})]
    (loop [frontier start-set
           visited start-set
           result []
           hop 0]
      (if (or (empty? frontier)
              (and max-hops (>= hop max-hops)))
        result
        (let [next-frontier (set/difference (set (mapcat step frontier)) visited)]
          (recur next-frontier
                 (into visited next-frontier)
                 (into result (map (fn [e] {:entity e :hops (inc hop)})) next-frontier)
                 (inc hop)))))))

(defn upstream-closure [g start-ids max-hops]
  (closure #(effective-dependencies g %) start-ids max-hops))

(defn downstream-closure [g start-ids max-hops]
  (closure #(effective-dependents g %) start-ids max-hops))

(defn gaps
  [dev-ids aspects-of deps-of min-similarity max-results]
  (let [ids (vec (sort-by str dev-ids))
        aspects (mapv #(or (aspects-of %) #{}) ids)
        deps (mapv #(or (deps-of %) #{}) ids)
        n (count ids)
        found (for [i (range n)
                    j (range (inc i) n)
                    :let [aspects-a (aspects i)
                          aspects-b (aspects j)
                          shared (set/intersection aspects-a aspects-b)
                          union-count (- (+ (count aspects-a) (count aspects-b)) (count shared))
                          similarity (if (pos? union-count)
                                       (/ (count shared) union-count)
                                       0)]
                    :when (and (> similarity min-similarity)
                               (not (contains? (deps i) (ids j)))
                               (not (contains? (deps j) (ids i))))]
                {:a (ids i)
                 :b (ids j)
                 :similarity (double similarity)
                 :shared-aspects (vec (sort shared))})]
    (vec (take max-results (sort-by (comp - :similarity) found)))))

(defn structural-gaps
  ([g] (structural-gaps g 0.5 20))
  ([g min-similarity] (structural-gaps g min-similarity 20))
  ([g min-similarity max-results]
   (gaps (entity-ids g)
         #(entity-aspects g %)
         #(dependencies g %)
         min-similarity
         max-results)))
