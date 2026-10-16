(ns atlas.ide.analysis
  "Structural analysis of the current registry: reuse, drift, connectivity and
   aspect lifecycle. Each function takes the argument map of the llm-ide tool of
   the same name and reads `registry/current-registry`, so binding
   `registry/*registry-override*` runs it against a stored snapshot. Pure, so it
   loads in babashka."
  (:require [atlas.registry :as registry]
            [atlas.registry.lookup :as lookup]
            [atlas.query :as query]
            [atlas.invariant :as inv]
            [atlas.ide.entity :as ide.entity]
            [atlas.ide.trace :as ide.trace]
            [clojure.set :as set]))

(defn- ensure-keyword
  [x]
  (cond
    (keyword? x) x
    (string? x)  (keyword (cond-> x (= \: (first x)) (subs 1)))
    :else        x))

(defn- in-degree [dev-id] (count (ide.trace/dependents-of dev-id)))

(defn- out-degree [dev-id] (count (ide.trace/dependencies-of dev-id)))

(defn- format-2 [x]
  #?(:cljs (.toFixed x 2)
     :default (format "%.2f" (double x))))

(defn- log [x]
  #?(:cljs (js/Math.log x)
     :default (Math/log x)))

(defn- sqrt [x]
  #?(:cljs (js/Math.sqrt x)
     :default (Math/sqrt x)))

(defn- abs* [x]
  #?(:cljs (js/Math.abs x)
     :default (Math/abs (long x))))

;; =============================================================================
;; Query
;; =============================================================================

(defn overview
  "Entity count, entities per type, and the twenty most used aspects."
  [{:keys [version]}]
  (let [reg      (registry/current-registry)
        by-type  (group-by :atlas/type (vals reg))
        aspects  (frequencies (mapcat identity (keys reg)))]
    {:version      version
     :entity-count (count reg)
     :by-type      (into {} (map (fn [[t es]] [t (count es)]) by-type))
     :aspect-count (count aspects)
     :top-aspects  (->> aspects (sort-by val >) (take 20) vec)}))

(defn by-aspect
  [{:keys [query/aspect]}]
  {:entities (vec (ide.entity/entities-with-aspect aspect))})

(defn entities-by-type
  [{:keys [entity/type]}]
  (let [entity-type (if (keyword? type) type (keyword type))
        entities    (vec (ide.entity/entities-with-aspect entity-type))]
    {:entity-type entity-type
     :entities    entities
     :count       (count entities)}))

(defn entity-detail
  [{:keys [entity/dev-id]}]
  (ide.entity/entity-info dev-id))

(defn suggest-placement
  [{:keys [entity/intended-aspects dataflow/consumes]}]
  (let [aspect-set     (if (set? intended-aspects) intended-aspects (set intended-aspects))
        similar        (->> (registry/current-registry)
                            (map (fn [[id props]]
                                   (let [entity-aspects (registry/aspects id)
                                         shared         (set/intersection aspect-set entity-aspects)
                                         missing        (set/difference aspect-set entity-aspects)]
                                     {:entity  (:atlas/dev-id props)
                                      :overlap (if (seq aspect-set)
                                                 (/ (count shared) (count aspect-set))
                                                 0)
                                      :shared  (vec (sort shared))
                                      :missing (vec (sort missing))})))
                            (filter #(> (:overlap %) 0))
                            (sort-by :overlap >)
                            (take 10)
                            vec)
        suggested-deps (when (seq consumes)
                         (->> consumes (mapcat ide.trace/producers-of) distinct vec))]
    {:similar        similar
     :suggested-deps (or suggested-deps [])
     :patterns       (->> similar (take 3) (map :entity) vec)}))

;; =============================================================================
;; Similarity
;; =============================================================================

(defn- jaccard
  [a b]
  (let [u (set/union a b)]
    (if (empty? u) 0.0 (/ (count (set/intersection a b)) (double (count u))))))

(defn- mean-jaccard
  "Mean Jaccard similarity among a collection of aspect sets (sample up to k)."
  [aspect-sets k]
  (let [sample (vec (take k aspect-sets))
        n      (count sample)]
    (if (< n 2)
      1.0
      (let [pairs (for [i (range n) j (range n) :when (< i j)]
                    (let [a (nth sample i) b (nth sample j)
                          u (set/union a b)]
                      (if (seq u) (/ (count (set/intersection a b)) (double (count u))) 1.0)))]
        (/ (reduce + pairs) (max 1 (count pairs)))))))

(defn- build-aspect-freq
  [reg]
  (->> (keys reg) (mapcat identity) frequencies))

(defn- tfidf-vec
  "Sparse TF-IDF vector for an aspect set: {aspect -> 1/log(freq)}."
  [aspects freq-map]
  (into {} (map (fn [a] [a (/ 1.0 (log (max 2.0 (double (get freq-map a 1)))))]))
        aspects))

(defn- cosine
  [v1 v2]
  (if (or (empty? v1) (empty? v2))
    0.0
    (let [dot  (reduce-kv (fn [acc k w1] (+ acc (* w1 (get v2 k 0.0)))) 0.0 v1)
          mag1 (sqrt (reduce-kv (fn [acc _ w] (+ acc (* w w))) 0.0 v1))
          mag2 (sqrt (reduce-kv (fn [acc _ w] (+ acc (* w w))) 0.0 v2))]
      (if (or (zero? mag1) (zero? mag2))
        0.0
        (/ dot (* mag1 mag2))))))

(defn near-intent
  "The k entities nearest to an aspect set by TF-IDF weighted cosine, with
   Jaccard alongside for comparison."
  [{:keys [entity/intended-aspects query/k]}]
  (let [k        (or k 10)
        reg      (registry/current-registry)
        freq-map (build-aspect-freq reg)
        q-set    (set intended-aspects)
        q-vec    (tfidf-vec q-set freq-map)
        results  (->> (vals reg)
                      (keep (fn [props]
                              (let [dev-id  (:atlas/dev-id props)
                                    [cid _] (when dev-id (query/find-by-dev-id reg dev-id))]
                                (when cid
                                  (let [aspects (registry/aspects cid)
                                        sim     (cosine q-vec (tfidf-vec aspects freq-map))]
                                    (when (pos? sim)
                                      {:dev-id            dev-id
                                       :entity-type       (:atlas/type props)
                                       :cosine-similarity (double sim)
                                       :jaccard           (double (jaccard q-set aspects))
                                       :shared-aspects    (vec (sort (set/intersection q-set aspects)))
                                       :unique-to-entity  (vec (sort (set/difference aspects q-set)))}))))))
                      (sort-by :cosine-similarity >)
                      (take k)
                      vec)]
    {:query/similarity results}))

;; =============================================================================
;; Diagnose
;; =============================================================================

(defn aspect-anomalies
  "Aspects used once, and pairs in one namespace whose names differ in length
   by less than two characters."
  [_]
  (let [freq-map     (into {} (map (fn [{:keys [aspect/aspect aspect/count]}] [aspect count]))
                           (ide.entity/list-aspects))
        all-sparse   (->> freq-map (filter #(= 1 (val %))) (map key) vec)
        aspect-names (keys freq-map)
        all-similar  (->> (for [a aspect-names
                                b aspect-names
                                :when (and (not= a b)
                                           (= (namespace a) (namespace b))
                                           (< (abs* (- (count (name a)) (count (name b)))) 2))]
                            #{a b})
                          distinct
                          (map vec)
                          vec)]
    {:sparse              (vec (take 50 all-sparse))
     :sparse-count        (count all-sparse)
     :similar-names       (vec (take 50 all-similar))
     :similar-names-count (count all-similar)}))

(defn bottlenecks
  "The twenty entities with the highest in-degree × out-degree."
  [_]
  {:bottlenecks (->> (inv/all-dev-ids)
                     (map (fn [e]
                            (let [in  (in-degree e)
                                  out (out-degree e)]
                              {:entity e :in-degree in :out-degree out :score (* in out)})))
                     (filter #(> (:score %) 0))
                     (sort-by :score >)
                     (take 20)
                     vec)})

(defn islands
  "Connected components of the dependency graph, largest first."
  [_]
  (let [all-ids        (inv/all-dev-ids)
        adjacency      (reduce (fn [adj id]
                                 (assoc adj id (set/union (set (ide.trace/dependencies-of id))
                                                          (set (ide.trace/dependents-of id)))))
                               {}
                               all-ids)
        find-component (fn [start visited]
                         (loop [queue [start] component #{} seen visited]
                           (if (empty? queue)
                             [component seen]
                             (let [node          (first queue)
                                   new-neighbors (set/difference (get adjacency node #{}) seen)]
                               (recur (into (rest queue) new-neighbors)
                                      (conj component node)
                                      (conj seen node))))))
        components     (loop [remaining all-ids visited #{} result []]
                         (if (empty? remaining)
                           result
                           (let [start (first remaining)]
                             (if (visited start)
                               (recur (rest remaining) visited result)
                               (let [[component new-visited] (find-component start visited)]
                                 (recur (rest remaining) new-visited (conj result (vec component))))))))]
    {:islands (vec (sort-by (comp - count) components))
     :count   (count components)}))

(defn orphans
  "Entities nothing depends on, except those carrying the excluded aspect."
  [{:keys [aspect/exclude-from-orphan-check aspect/type-namespace]}]
  (let [exclude        (or exclude-from-orphan-check :tier/api)
        type-namespace (or type-namespace "atlas")
        orphaned       (->> (inv/all-dev-ids)
                            (remove #(lookup/has-aspect? % exclude))
                            (filter #(zero? (in-degree %)))
                            (map (fn [e]
                                   {:entity e
                                    :type   (first (filter #(= type-namespace (namespace %))
                                                           (lookup/identity-for e)))}))
                            vec)]
    {:orphans orphaned
     :count   (count orphaned)}))

;; =============================================================================
;; Position and peers
;; =============================================================================

(defn- aspect-rarity
  "Mean TF-IDF weight of the entity's aspects; high means distinctive."
  [aspects freq-map]
  (if (empty? aspects)
    0.0
    (let [weights (map (fn [a] (/ 1.0 (log (max 2.0 (double (get freq-map a 1)))))) aspects)]
      (/ (reduce + weights) (count weights)))))

(defn- namespace-siblings
  [reg dev-id]
  (let [ns-str (namespace dev-id)]
    (->> (vals reg)
         (keep :atlas/dev-id)
         (filter (fn [d] (and d (not= d dev-id) (= (namespace d) ns-str)))))))

(defn entity-position
  "In/out degree, aspect rarity and namespace cohesion of an entity."
  [{:keys [entity/dev-id]}]
  (let [dev-id          (ensure-keyword dev-id)
        reg             (registry/current-registry)
        [compound-id _] (query/find-by-dev-id reg dev-id)]
    (when compound-id
      (let [aspects  (registry/aspects compound-id)
            freq-map (into {} (map (fn [{:keys [aspect/aspect aspect/count]}] [aspect count]))
                           (ide.entity/list-aspects))
            siblings (namespace-siblings reg dev-id)
            sibling-aspect-sets (map (fn [sid]
                                       (let [[sid-id _] (query/find-by-dev-id reg sid)]
                                         (if sid-id (registry/aspects sid-id) #{})))
                                     (take 20 siblings))]
        {:position/in-degree               (in-degree dev-id)
         :position/out-degree              (out-degree dev-id)
         :position/aspect-rarity           (aspect-rarity aspects freq-map)
         :position/namespace-sibling-count (count siblings)
         :position/namespace-cohesion      (mean-jaccard sibling-aspect-sets 10)}))))

(defn- median
  [nums]
  (when (seq nums)
    (let [s   (vec (sort nums))
          n   (count s)
          mid (quot n 2)]
      (if (odd? n)
        (double (nth s mid))
        (/ (+ (double (nth s (dec mid))) (double (nth s mid))) 2.0)))))

(defn- consumer-types-of
  [reg dev-id]
  (->> (ide.trace/dependents-of dev-id)
       (keep (fn [d] (:atlas/type (second (query/find-by-dev-id reg d)))))
       frequencies))

(defn consumer-pattern
  "Whether an entity's in-degree matches its k nearest semantic peers:
   :isolated, :matches-peers, :outlier-low, :outlier-high or :no-peers."
  [{:keys [entity/dev-id query/k query/min-cosine]}]
  (let [dev-id     (ensure-keyword dev-id)
        k          (or k 8)
        min-cosine (or min-cosine 0.4)
        reg        (registry/current-registry)
        [tcid _]   (query/find-by-dev-id reg dev-id)]
    (when tcid
      (let [freq-map     (build-aspect-freq reg)
            tvec         (tfidf-vec (registry/aspects tcid) freq-map)
            candidates   (->> (vals reg)
                              (keep (fn [props]
                                      (let [d (:atlas/dev-id props)]
                                        (when (and d (not= d dev-id))
                                          (let [[cid _] (query/find-by-dev-id reg d)]
                                            (when cid
                                              (let [sim (cosine tvec (tfidf-vec (registry/aspects cid) freq-map))]
                                                (when (>= sim min-cosine)
                                                  {:dev-id      d
                                                   :entity-type (:atlas/type props)
                                                   :cosine      (double sim)}))))))))
                              (sort-by :cosine >)
                              (take k)
                              vec)
            neighbours   (mapv (fn [n]
                                 (assoc n
                                        :in-degree      (in-degree (:dev-id n))
                                        :out-degree     (out-degree (:dev-id n))
                                        :consumer-types (consumer-types-of reg (:dev-id n))))
                               candidates)
            target-in    (in-degree dev-id)
            target-out   (out-degree dev-id)
            peer-ins     (mapv :in-degree neighbours)
            peer-min     (when (seq peer-ins) (apply min peer-ins))
            peer-max     (when (seq peer-ins) (apply max peer-ins))
            peer-med-in  (median peer-ins)
            peer-med-out (median (mapv :out-degree neighbours))
            modal-cons   (->> neighbours
                              (mapcat (comp seq :consumer-types))
                              (reduce (fn [acc [t n]] (update acc t (fnil + 0) n)) {}))
            verdict      (cond
                           (empty? neighbours)                               :no-peers
                           (and (zero? target-in) (pos? (or peer-med-in 0))) :isolated
                           (and peer-max (> target-in peer-max))             :outlier-high
                           (and peer-min (< target-in peer-min))             :outlier-low
                           :else                                             :matches-peers)
            diagnosis    (case verdict
                           :no-peers
                           (str "No baseline entity reaches cosine " min-cosine
                                " — target's aspect set is too distinctive for peer-pattern analysis.")
                           :isolated
                           (str "Target has 0 dependents; " (count neighbours)
                                " peer(s) have in-degree " peer-min "–" peer-max
                                " (median " peer-med-in
                                "). Likely missing a caller (workflow producer / endpoint dep / direct consumer) "
                                "rather than intentional foundation.")
                           :outlier-high
                           (str "Target's in-degree (" target-in ") exceeds peer max ("
                                peer-max "). Worth checking if this entity is becoming a bottleneck.")
                           :outlier-low
                           (str "Target's in-degree (" target-in ") is below peer min ("
                                peer-min ", median " peer-med-in
                                "). Under-connected vs peers — verify all expected consumers are wired.")
                           :matches-peers
                           (str "Target's in-degree (" target-in
                                ") sits within peer range (" peer-min "–" peer-max
                                ", median " peer-med-in
                                "). Connectivity is consistent with semantic peers."))]
        {:pattern/verdict              verdict
         :pattern/diagnosis            diagnosis
         :pattern/target-in-degree     target-in
         :pattern/target-out-degree    target-out
         :pattern/peer-stats           {:count      (count neighbours)
                                        :in-min     peer-min
                                        :in-max     peer-max
                                        :in-median  peer-med-in
                                        :out-median peer-med-out
                                        :min-cosine (or (some-> neighbours last :cosine) 0.0)
                                        :max-cosine (or (some-> neighbours first :cosine) 0.0)}
         :pattern/neighbours           (mapv #(select-keys % [:dev-id :entity-type :cosine
                                                              :in-degree :out-degree :consumer-types])
                                             neighbours)
         :pattern/modal-consumer-types modal-cons}))))

;; =============================================================================
;; Aspect signature and lifecycle
;; =============================================================================

(defn- aspect-fingerprint
  "Entity-type distribution (:type/* keys) and companion-namespace distribution
   (:ns/* keys) across the carriers of an aspect, excluding its own namespace and
   atlas/*."
  [reg aspect]
  (let [carriers     (filter #(contains? % aspect) (keys reg))
        type-counts  (frequencies (keep (fn [cid] (or (:atlas/type (get reg cid))
                                                      (registry/entity-type cid)))
                                        carriers))
        n            (max 1 (reduce + (vals type-counts)))
        type-weights (into {} (map (fn [[t c]] [(keyword "type" (name t)) (/ (double c) n)]))
                           type-counts)
        own-ns       (namespace aspect)
        ns-counts    (->> carriers
                          (mapcat (fn [cid] (disj cid aspect)))
                          (keep namespace)
                          (remove #{own-ns "atlas"})
                          frequencies)
        m            (max 1 (reduce + (vals ns-counts)))
        ns-weights   (into {} (map (fn [[ns c]] [(keyword "ns" ns) (/ (double c) m)])) ns-counts)]
    (merge type-weights ns-weights)))

(defn- mean-fingerprint
  [fps]
  (when (seq fps)
    (let [n   (count fps)
          sum (reduce (fn [acc fp] (reduce-kv (fn [a k v] (update a k (fnil + 0.0) v)) acc fp))
                      {} fps)]
      (into {} (map (fn [[k v]] [k (/ v (double n))])) sum))))

(defn aspect-signature
  "Which aspect namespace an aspect structurally resembles, by cosine between
   its fingerprint and each namespace's mean fingerprint. Namespaces with fewer
   than three aspects are skipped."
  [{:keys [query/aspect]}]
  (let [aspect      (ensure-keyword aspect)
        reg         (registry/current-registry)
        n           (count (filter #(contains? % aspect) (keys reg)))
        target-fp   (aspect-fingerprint reg aspect)
        all-aspects (->> reg keys (mapcat identity) distinct (remove #(= "atlas" (namespace %))))
        ns-fits     (->> (group-by namespace all-aspects)
                         (keep (fn [[ns aspects]]
                                 (when (and (>= (count aspects) 3) (some? ns))
                                   {:namespace   ns
                                    :sample-size (count aspects)
                                    :cosine      (cosine target-fp
                                                         (mean-fingerprint
                                                          (map #(aspect-fingerprint reg %) aspects)))})))
                         (sort-by :cosine >)
                         vec)
        own         (first (filter #(= (:namespace %) (namespace aspect)) ns-fits))
        best-other  (first (remove #(= (:namespace %) (namespace aspect)) ns-fits))
        recommend   (cond
                      (zero? n)
                      (str "Aspect " aspect " is unused in this snapshot.")
                      (and best-other own (> (- (:cosine best-other) (:cosine own)) 0.15))
                      (str "Aspect's structural fingerprint matches :"
                           (:namespace best-other) "/* (cosine " (format-2 (:cosine best-other))
                           ") more than its own :" (:namespace own) "/* (cosine "
                           (format-2 (:cosine own))
                           "). Consider :" (:namespace best-other) "/" (name aspect) ".")
                      (and best-other own)
                      (str "Aspect's fingerprint is consistent with its current namespace :"
                           (:namespace own) "/* (" (format-2 (:cosine own))
                           " vs best alternative :" (:namespace best-other) "/* "
                           (format-2 (:cosine best-other)) ").")
                      :else
                      "Insufficient namespace samples for comparison.")]
    {:signature/own-namespace            (namespace aspect)
     :signature/best-fit-namespace       (:namespace best-other)
     :signature/recommendation           recommend
     :signature/usage-count              n
     :signature/entity-type-distribution (->> target-fp
                                              (filter (fn [[k _]] (= "type" (namespace k))))
                                              (map (fn [[k v]] [(keyword "atlas" (name k)) v]))
                                              (into {}))
     :signature/companion-namespaces     (->> target-fp
                                              (filter (fn [[k _]] (= "ns" (namespace k))))
                                              (map (fn [[k v]] [(name k) v]))
                                              (sort-by second >)
                                              (take 10)
                                              vec)
     :signature/peer-namespace-fits      ns-fits}))

(defn- carriers-with-aspect
  [reg aspect]
  (->> reg
       (keep (fn [[cid props]] (when (contains? cid aspect) (:atlas/dev-id props))))
       set))

(defn- longest-true-run
  [bools]
  (->> bools (partition-by identity) (filter (comp true? first)) (map count) (reduce max 0)))

(defn aspect-lifecycle
  "How an aspect's carriers persist across `versions`, a sequence of
   [label registry] pairs in order: persistence class (:stable, :transient,
   :growing, :shrinking, :fluctuating, :never-existed), first/last/peak versions
   and churn rate."
  [{:keys [query/aspect]} versions]
  (let [aspect  (ensure-keyword aspect)
        per-ver (mapv (fn [[v reg]] {:version v :carriers (carriers-with-aspect reg aspect)}) versions)
        counts  (mapv (fn [{:keys [version carriers]}] {:version version :count (count carriers)}) per-ver)
        present (filter #(pos? (:count %)) counts)]
    (if (empty? present)
      {:lifecycle/persistence-class :never-existed
       :lifecycle/timeline           counts}
      (let [peak            (apply max-key :count counts)
            all-carriers    (apply set/union (map :carriers per-ver))
            presence-trails (mapv (fn [d] (mapv (fn [{:keys [carriers]}] (boolean (carriers d))) per-ver))
                                  (vec all-carriers))
            run-lengths     (mapv longest-true-run presence-trails)
            mean-runs       (when (seq run-lengths)
                              (double (/ (reduce + run-lengths) (count run-lengths))))
            dropped-count   (count (filter (fn [trail] (and (some true? trail) (not (last trail))))
                                           presence-trails))
            churn-rate      (when (seq all-carriers)
                              (double (/ dropped-count (count all-carriers))))
            counts-vec      (mapv :count counts)
            monotone-up?    (and (>= (count counts-vec) 2) (apply <= counts-vec))
            monotone-down?  (and (>= (count counts-vec) 2) (apply >= counts-vec))
            persistence     (cond
                              (and monotone-up? (not monotone-down?))                    :growing
                              (and monotone-down? (not monotone-up?))                    :shrinking
                              (and mean-runs (>= mean-runs (* 0.8 (count versions))))    :stable
                              (and mean-runs (<= mean-runs 2.0))                         :transient
                              :else                                                      :fluctuating)]
        {:lifecycle/persistence-class     persistence
         :lifecycle/first-seen-at         (:version (first present))
         :lifecycle/last-seen-at          (:version (last present))
         :lifecycle/peak-version          (:version peak)
         :lifecycle/peak-count            (:count peak)
         :lifecycle/current-count         (:count (last counts))
         :lifecycle/mean-versions-carried mean-runs
         :lifecycle/churn-rate            churn-rate
         :lifecycle/timeline              counts}))))
