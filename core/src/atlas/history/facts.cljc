(ns atlas.history.facts
  "The facts atlas.history indexes, and its queries, as plain data.

   version-facts computes what snapshot-version! transacts. A facts db is
   built with transact, which numbers facts the way datascript allocates
   entity ids, so every query returns what its atlas.history counterpart
   returns, ordering included. No datascript, so it loads in babashka."
  (:require [clojure.set :as set]))

(defn extract-targets
  [value]
  (cond
    (keyword? value) [value]
    (set? value)     (vec value)
    :else            (->> (flatten (seq value))
                          (filter keyword?))))

(defn- type-ref-index
  [registry]
  (->> (vals registry)
       (filter #(= :atlas/type-ref (:atlas/type %)))
       (group-by :type-ref/source)))

(defn- dataflow-index
  [registry]
  (->> (vals registry)
       (filter #(= :atlas/ontology (:atlas/type %)))
       (keep (fn [ont]
               (let [ctx-key   (:dataflow/context-key ont)
                     ctx-verb  (:dataflow/context-verb ont)
                     resp-key  (:dataflow/response-key ont)
                     resp-verb (:dataflow/response-verb ont)
                     etype     (:ontology/for ont)]
                 (when (or ctx-key resp-key)
                   {:source etype
                    :flows (cond-> []
                             (and ctx-key ctx-verb)   (conj {:property ctx-key  :verb ctx-verb})
                             (and resp-key resp-verb) (conj {:property resp-key :verb resp-verb}))}))))
       (group-by :source)
       (map (fn [[k vs]] [k (mapcat :flows vs)]))
       (into {})))

(defn- prop-facts
  [dev-id version etype props type-refs-by-source dataflow-by-source]
  (let [facts-for (fn [property verb]
                    (when-let [value (get props property)]
                      (keep (fn [target]
                              (when (keyword? target)
                                {:prop/dev-id  dev-id
                                 :prop/version version
                                 :prop/key     property
                                 :prop/verb    verb
                                 :prop/target  target}))
                            (extract-targets value))))]
    (concat
     (mapcat #(facts-for (:type-ref/property %) (:type-ref/datalog-verb %))
             (get type-refs-by-source etype))
     (mapcat #(facts-for (:property %) (:verb %))
             (get dataflow-by-source etype)))))

(defn- snap-facts
  [registry version prev-registry rename-dev-ids]
  (let [inv-renames    (set/map-invert (or rename-dev-ids {}))
        prev-by-dev-id (when prev-registry
                         (reduce (fn [m [k v]]
                                   (update m (:atlas/dev-id v) (fnil conj #{}) k))
                                 {} prev-registry))]
    (map
     (fn [[compound-id props]]
       (let [dev-id        (:atlas/dev-id props)
             etype         (:atlas/type props)
             aspects       (disj compound-id etype)
             prev-dev-id   (get inv-renames dev-id)
             lookup-id     (or prev-dev-id dev-id)
             prev-cid      (when-let [cids (get prev-by-dev-id lookup-id)]
                             (if (contains? cids compound-id)
                               compound-id
                               (first cids)))
             prev-props    (when prev-cid (get prev-registry prev-cid))
             prev-etype    (when prev-props (:atlas/type prev-props))
             prev-asp      (when prev-cid (disj prev-cid (or prev-etype etype)))
             type-changed? (and prev-etype (not= prev-etype etype))
             added         (cond
                             prev-asp       (set/difference aspects prev-asp)
                             prev-by-dev-id aspects
                             :else          #{})
             removed       (if prev-asp (set/difference prev-asp aspects) #{})]
         (cond-> {:snap/dev-id  dev-id
                  :snap/version version
                  :snap/type    etype
                  :snap/aspect  aspects}
           (seq added)   (assoc :snap/added added)
           (seq removed) (assoc :snap/removed removed)
           prev-dev-id   (assoc :snap/prev-dev-id prev-dev-id)
           type-changed? (assoc :snap/prev-type prev-etype))))
     registry)))

(defn- deleted-facts
  [registry version prev-registry]
  (when prev-registry
    (let [current-dev-ids (set (map (fn [[_ v]] (:atlas/dev-id v)) registry))]
      (keep (fn [[_ prev-props]]
              (let [dev-id (:atlas/dev-id prev-props)]
                (when-not (current-dev-ids dev-id)
                  {:snap/dev-id  dev-id
                   :snap/version version
                   :snap/type    (:atlas/type prev-props)
                   :snap/deleted true})))
            prev-registry))))

(defn version-facts
  "Snapshot facts of registry as version, diffed against prev-registry:
   {:snaps [live... deleted...] :props [...]}, in transact order."
  [registry version prev-registry rename-dev-ids]
  (let [type-refs-by-source (type-ref-index registry)
        dataflow-by-source  (dataflow-index registry)]
    {:snaps (vec (concat (snap-facts registry version prev-registry rename-dev-ids)
                         (deleted-facts registry version prev-registry)))
     :props (vec (mapcat (fn [[_ props]]
                           (prop-facts (:atlas/dev-id props) version (:atlas/type props) props
                                       type-refs-by-source dataflow-by-source))
                         registry))}))

(def empty-db {:max-eid 0 :snaps {} :props {} :dev-ids {}})

(defn transact
  [db facts]
  (reduce (fn [{:keys [max-eid] :as db} fact]
            (let [eid (inc max-eid)
                  db  (assoc db :max-eid eid)]
              (cond
                (contains? fact :snap/version) (-> db
                                                   (update-in [:snaps (:snap/version fact)] (fnil conj []) [eid fact])
                                                   (update-in [:dev-ids (:snap/dev-id fact)] (fnil conj []) [eid fact]))
                (contains? fact :prop/version) (update-in db [:props (:prop/version fact)] (fnil conj []) [eid fact])
                :else db)))
          (or db empty-db)
          facts))

(defn snapshot
  [db registry version prev-registry rename-dev-ids]
  (let [{:keys [snaps props]} (version-facts registry version prev-registry rename-dev-ids)]
    (transact db (concat snaps props))))

(defn- resultset
  "The set of tuples datascript collects; its iteration order is the query's result order."
  [tuple-fn eid+facts]
  (into #{} (map (fn [[eid fact]] (tuple-fn eid fact))) eid+facts))

(defn- pull-facts
  [eid+facts]
  (let [by-eid (into {} eid+facts)]
    (map (fn [[eid]] (by-eid eid)) (resultset (fn [eid _] [eid]) eid+facts))))

(defn- pull
  [ks fact]
  (reduce (fn [m k]
            (let [v (get fact k)]
              (cond
                (nil? v) m
                (set? v) (if (seq v) (assoc m k (vec (sort v))) m)
                :else    (assoc m k v))))
          {} ks))

(defn- count-by
  [k eid+facts]
  (for [[_ tuples] (group-by (fn [[x]] [x]) (resultset (fn [eid fact] [(get fact k) eid]) eid+facts))]
    [(ffirst tuples) (count tuples)]))

(defn- snaps-at [db version] (get-in db [:snaps version]))

(defn versions
  [db]
  (->> (:snaps db)
       (keep (fn [[v facts]] (when (seq facts) v)))
       sort vec))

(defn entity-timeline
  [db dev-id]
  (->> (get-in db [:dev-ids dev-id])
       pull-facts
       (map (partial pull [:snap/dev-id :snap/version :snap/type
                           :snap/aspect :snap/added :snap/removed
                           :snap/prev-dev-id]))
       (sort-by :snap/version)))

(defn full-timeline
  [db current-dev-id]
  (loop [dev-id current-dev-id, acc []]
    (let [snaps (entity-timeline db dev-id)
          prev  (:snap/prev-dev-id (first snaps))]
      (if prev
        (recur prev (into acc snaps))
        (sort-by :snap/version (into acc snaps))))))

(defn version-diff
  [db version]
  (->> (snaps-at db version)
       (filter (fn [[_ s]] (or (seq (:snap/added s))
                               (seq (:snap/removed s))
                               (some? (:snap/prev-type s)))))
       pull-facts
       (map (partial pull [:snap/dev-id :snap/type :snap/added :snap/removed
                           :snap/prev-dev-id :snap/prev-type]))
       (sort-by :snap/dev-id)))

(defn version-summary
  [db version]
  (->> (snaps-at db version)
       (filter (fn [[_ s]] (and (or (seq (:snap/added s)) (seq (:snap/removed s)))
                                (some? (:snap/type s)))))
       (count-by :snap/type)
       (sort-by second >)))

(defn version-renames
  [db version]
  (->> (snaps-at db version)
       (filter (fn [[_ s]] (some? (:snap/prev-dev-id s))))
       pull-facts
       (map (partial pull [:snap/dev-id :snap/prev-dev-id]))
       (sort-by :snap/dev-id)))

(defn version-deleted
  [db version]
  (->> (snaps-at db version)
       (filter (fn [[_ s]] (true? (:snap/deleted s))))
       pull-facts
       (map (partial pull [:snap/dev-id :snap/type]))
       (sort-by :snap/dev-id)))

(defn vocabulary-at-version
  [db version]
  (->> (snaps-at db version)
       (mapcat (fn [[eid s]] (map (fn [a] [eid {:aspect a}]) (:snap/aspect s))))
       (count-by :aspect)
       (sort-by second >)))

(defn vocabulary-diff
  [db v-old v-new]
  (let [old-vocab   (into {} (vocabulary-at-version db v-old))
        new-vocab   (into {} (vocabulary-at-version db v-new))
        all-aspects (set/union (set (keys old-vocab)) (set (keys new-vocab)))]
    {:added   (set/difference (set (keys new-vocab)) (set (keys old-vocab)))
     :removed (set/difference (set (keys old-vocab)) (set (keys new-vocab)))
     :grew    (->> all-aspects
                   (keep (fn [a]
                           (let [old-n (get old-vocab a 0)
                                 new-n (get new-vocab a 0)]
                             (when (> new-n old-n)
                               {:aspect a :old-count old-n :new-count new-n}))))
                   (sort-by (comp - :new-count)))
     :shrank  (->> all-aspects
                   (keep (fn [a]
                           (let [old-n (get old-vocab a 0)
                                 new-n (get new-vocab a 0)]
                             (when (< new-n old-n)
                               {:aspect a :old-count old-n :new-count new-n}))))
                   (sort-by :new-count))}))

(defn- props-minus
  [db v-from v-other]
  (let [other (into #{} (map (fn [[_ p]] [(:prop/dev-id p) (:prop/key p) (:prop/target p)]))
                    (get-in db [:props v-other]))]
    (into #{}
          (keep (fn [[_ p]]
                  (let [{:prop/keys [dev-id key verb target]} p]
                    (when (and (some? dev-id) (some? key) (some? verb) (some? target)
                               (not (other [dev-id key target])))
                      [dev-id key verb target]))))
          (get-in db [:props v-from]))))

(defn property-diff
  [db v-old v-new]
  {:added   (props-minus db v-new v-old)
   :removed (props-minus db v-old v-new)})
