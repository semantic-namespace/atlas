(ns atlas.history
  "Registry history — tracks semantic movement of Atlas entities across versions.

   Phase 1: snaps (aspects, added/removed, prev-dev-id).
   Phase 2: edges (cross-entity type-ref connections).

   Usage:

     ;; Create history db and snapshot versions in sequence
     (def conn (create-conn))
     (snapshot-version! conn v1-reg \"v1\" nil nil)
     (snapshot-version! conn v2-reg \"v2-serial\" v1-reg nil)
     (snapshot-edges!   conn v2-reg \"v2-serial\")

     ;; Query snaps
     (entity-timeline @conn :serial/fees-all)
     (full-timeline @conn :serial/fees-all)
     (version-diff @conn \"v2-serial\")
     (aspect-adoption @conn \"v2-serial\" :cardinality/many)

     ;; Query edges
     (edges-at @conn \"v2-serial\" :cache/fees-all)
     (edge-diff @conn \"v1\" \"v2-serial\")
     (dependents-of @conn \"v2-serial\" :serial/fees-all)"
  (:require [datascript.core :as d]
            [clojure.set :as set]
            [atlas.ontology.type-ref :as type-ref]
            [atlas.registry.lookup :as entity]))

;; =============================================================================
;; Schema
;; =============================================================================

(def schema
  {;; Snapshot: one entity per [dev-id × version]
   :snap/dev-id      {:db/index true}
   :snap/version     {:db/index true}
   :snap/type        {:db/index true}
   :snap/aspect      {:db/cardinality :db.cardinality/many
                      :db/index true}
   :snap/added       {:db/cardinality :db.cardinality/many
                      :db/index true}
   :snap/removed     {:db/cardinality :db.cardinality/many
                      :db/index true}
   :snap/prev-dev-id {:db/index true}
   :snap/deleted     {:db/index true}

   ;; Edge: one entity per [source × target × property × version]
   :edge/source      {:db/index true}
   :edge/target      {:db/index true}
   :edge/verb        {:db/index true}
   :edge/property    {:db/index true}
   :edge/version     {:db/index true}})

;; =============================================================================
;; Connection lifecycle
;; =============================================================================

(defn create-conn
  "Create a fresh history db connection."
  []
  (d/create-conn schema))

;; =============================================================================
;; Snapshot ingestion
;; =============================================================================

(defn snapshot-version!
  "Transact a version snapshot into the history db.

   registry:        the registry map at this version
   version:         version string, e.g. \"v2-serial\"
   prev-registry:   the registry map at the previous version (nil for first)
   rename-dev-ids:  map of {old-dev-id new-dev-id} from the transform spec (nil if none)"
  [conn registry version prev-registry rename-dev-ids]
  (let [inv-renames (set/map-invert (or rename-dev-ids {}))
        ;; Build index of prev-registry by dev-id for fast lookup
        prev-by-dev-id (when prev-registry
                         (into {} (map (fn [[k v]] [(:atlas/dev-id v) k]) prev-registry)))
        ;; Snap txdata for entities present in this version
        snap-txdata
        (mapcat
         (fn [[compound-id props]]
           (let [dev-id      (:atlas/dev-id props)
                 etype       (:atlas/type props)
                 aspects     (disj compound-id etype)
                 prev-dev-id (get inv-renames dev-id)
                 lookup-id   (or prev-dev-id dev-id)
                 prev-cid    (get prev-by-dev-id lookup-id)
                 prev-asp    (when prev-cid (disj prev-cid etype))
                 ;; For new entities in a subsequent snapshot, all aspects are "added".
                 ;; For the first snapshot (no prev-by-dev-id), nothing is "added" yet.
                 added       (cond
                               prev-asp       (set/difference aspects prev-asp)
                               prev-by-dev-id aspects
                               :else          #{})
                 removed     (if prev-asp (set/difference prev-asp aspects) #{})

                 snap-tx (cond-> {:snap/dev-id  dev-id
                                  :snap/version version
                                  :snap/type    etype
                                  :snap/aspect  aspects}
                           (seq added)   (assoc :snap/added added)
                           (seq removed) (assoc :snap/removed removed)
                           prev-dev-id   (assoc :snap/prev-dev-id prev-dev-id))]
             [snap-tx]))
         registry)
        ;; Deleted entities: present in prev-registry but absent in this version
        current-dev-ids (set (map (fn [[_ v]] (:atlas/dev-id v)) registry))
        deleted-txdata
        (when prev-registry
          (keep (fn [[_ prev-props]]
                  (let [dev-id (:atlas/dev-id prev-props)]
                    (when-not (current-dev-ids dev-id)
                      {:snap/dev-id  dev-id
                       :snap/version version
                       :snap/type    (:atlas/type prev-props)
                       :snap/deleted true})))
                prev-registry))
        txdata (concat snap-txdata deleted-txdata)]
    (d/transact! conn txdata)
    {:version version
     :entities (count registry)
     :changed (count (filter :snap/added txdata))
     :deleted (count (filter :snap/deleted txdata))}))

;; =============================================================================
;; Version queries
;; =============================================================================

(defn versions
  "All version strings in the history db, sorted."
  [db]
  (->> (d/q '[:find [?v ...] :where [_ :snap/version ?v]] db)
       sort vec))

;; =============================================================================
;; Entity-level queries
;; =============================================================================

(defn entity-timeline
  "All versions of a single entity under one dev-id, sorted by version."
  [db dev-id]
  (->> (d/q '[:find (pull ?e [:snap/dev-id :snap/version :snap/type
                               :snap/aspect :snap/added :snap/removed
                               :snap/prev-dev-id])
              :in $ ?dev-id
              :where [?e :snap/dev-id ?dev-id]]
            db dev-id)
       (map first)
       (sort-by :snap/version)))

(defn full-timeline
  "Complete history of an entity across dev-id renames.
   Follows :snap/prev-dev-id chain back to the original name."
  [db current-dev-id]
  (loop [dev-id current-dev-id, acc []]
    (let [snaps (entity-timeline db dev-id)
          prev  (:snap/prev-dev-id (first snaps))]
      (if prev
        (recur prev (into acc snaps))
        (sort-by :snap/version (into acc snaps))))))

;; =============================================================================
;; Version-level queries
;; =============================================================================

(defn version-diff
  "All entities that changed in a given version (have added or removed aspects).
   Returns seq of pulled snapshots sorted by dev-id."
  [db version]
  (->> (d/q '[:find (pull ?e [:snap/dev-id :snap/type :snap/added :snap/removed
                               :snap/prev-dev-id])
              :in $ ?version
              :where
              [?e :snap/version ?version]
              (or [?e :snap/added _]
                  [?e :snap/removed _])]
            db version)
       (map first)
       (sort-by :snap/dev-id)))

(defn version-summary
  "Aggregate change counts per entity type for a version."
  [db version]
  (->> (d/q '[:find ?type (count ?e)
              :in $ ?version
              :where
              [?e :snap/version ?version]
              (or [?e :snap/added _]
                  [?e :snap/removed _])
              [?e :snap/type ?type]]
            db version)
       (sort-by second >)))

(defn version-renames
  "Find all dev-id renames in a given version."
  [db version]
  (->> (d/q '[:find (pull ?e [:snap/dev-id :snap/prev-dev-id])
              :in $ ?version
              :where
              [?e :snap/version ?version]
              [?e :snap/prev-dev-id _]]
            db version)
       (map first)
       (sort-by :snap/dev-id)))

(defn version-deleted
  "Entities deleted in a given version (present in previous snapshot, absent now).
   Returns seq of {:snap/dev-id ... :snap/type ...} sorted by dev-id."
  [db version]
  (->> (d/q '[:find (pull ?e [:snap/dev-id :snap/type])
              :in $ ?version
              :where
              [?e :snap/version ?version]
              [?e :snap/deleted true]]
            db version)
       (map first)
       (sort-by :snap/dev-id)))

(defn detect-renames
  "Heuristic rename detection between two versions.
   Pairs deleted entities in v-new with new entities that have the same type
   and exact same aspects from v-old.
   Returns [{:from old-dev-id :to new-dev-id :type etype :aspects #{...}}]."
  [db v-old v-new]
  (let [deleted (version-deleted db v-new)
        ;; New entities: all aspects are in snap/added, no snap/removed
        new-entities (->> (version-diff db v-new)
                          (filter (fn [snap]
                                    (and (:snap/added snap)
                                         (not (:snap/removed snap))))))
        ;; Get aspects for each deleted entity from v-old
        deleted-with-aspects
        (map (fn [{:snap/keys [dev-id type]}]
               (let [aspects (->> (d/q '[:find [?a ...]
                                         :in $ ?dev-id ?version
                                         :where
                                         [?e :snap/dev-id ?dev-id]
                                         [?e :snap/version ?version]
                                         [?e :snap/aspect ?a]]
                                       db dev-id v-old)
                                  set)]
                 {:dev-id dev-id :type type :aspects aspects}))
             deleted)]
    ;; Exact match: same type + same aspects = rename
    (vec
     (for [{del-id :dev-id del-type :type del-aspects :aspects} deleted-with-aspects
           :when (seq del-aspects)
           {:snap/keys [added dev-id type]} new-entities
           :when (and (= del-type type)
                      (= del-aspects (set added)))]
       {:from del-id :to dev-id :type del-type :aspects del-aspects}))))

;; =============================================================================
;; Aspect-level queries
;; =============================================================================

(defn aspect-adoption
  "Which entities adopted a given aspect in a given version."
  [db version aspect]
  (->> (d/q '[:find ?dev-id ?type
              :in $ ?version ?aspect
              :where
              [?e :snap/version ?version]
              [?e :snap/added   ?aspect]
              [?e :snap/dev-id  ?dev-id]
              [?e :snap/type    ?type]]
            db version aspect)
       (sort-by first)))

(defn aspect-removal
  "Which entities lost a given aspect in a given version."
  [db version aspect]
  (->> (d/q '[:find ?dev-id ?type
              :in $ ?version ?aspect
              :where
              [?e :snap/version ?version]
              [?e :snap/removed ?aspect]
              [?e :snap/dev-id  ?dev-id]
              [?e :snap/type    ?type]]
            db version aspect)
       (sort-by first)))

(defn aspect-count-by-version
  "How many entities use a given aspect in each version."
  [db aspect]
  (->> (d/q '[:find ?version (count ?e)
              :in $ ?aspect
              :where
              [?e :snap/aspect ?aspect]
              [?e :snap/version ?version]]
            db aspect)
       (sort-by first)))

(defn aspect-first-seen
  "The earliest version where an aspect appears in :snap/added."
  [db aspect]
  (->> (d/q '[:find ?version
              :in $ ?aspect
              :where
              [?e :snap/added ?aspect]
              [?e :snap/version ?version]]
            db aspect)
       (map first)
       sort
       first))

;; =============================================================================
;; Vocabulary-level queries
;; =============================================================================

(defn vocabulary-at-version
  "All aspects and their entity counts at a given version."
  [db version]
  (->> (d/q '[:find ?aspect (count ?e)
              :in $ ?version
              :where
              [?e :snap/version ?version]
              [?e :snap/aspect  ?aspect]]
            db version)
       (sort-by second >)))

(defn vocabulary-diff
  "Aspects whose entity count changed between two versions.
   Returns {:added #{new-aspects} :removed #{gone-aspects}
            :grew [{:aspect :old-count :new-count}]
            :shrank [{:aspect :old-count :new-count}]}."
  [db v-old v-new]
  (let [old-vocab (into {} (vocabulary-at-version db v-old))
        new-vocab (into {} (vocabulary-at-version db v-new))
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

;; =============================================================================
;; Edge ingestion
;; =============================================================================

(defn- extract-targets
  "Extract target dev-ids from a property value.
   Handles: keyword, vector, list, set — flattens nested structures."
  [value]
  (cond
    (keyword? value) [value]
    (set? value)     (vec value)
    :else            (->> (flatten (seq value))
                          (filter keyword?))))

(defn snapshot-edges!
  "Transact edge entities for cross-entity type-ref connections at a version.
   Reads type-refs from the live registry via (type-ref/all-type-refs).
   Call separately from snapshot-version! — edges are independent of snap diffs."
  [conn registry version]
  (let [type-ref-ids (type-ref/all-type-refs)
        type-refs    (map (fn [id] (entity/props-for id)) type-ref-ids)
        ;; Group type-refs by source entity type for fast lookup
        by-source    (group-by :type-ref/source type-refs)
        txdata
        (mapcat
         (fn [[_ props]]
           (let [dev-id (:atlas/dev-id props)
                 etype  (:atlas/type props)
                 refs   (get by-source etype)]
             (mapcat
              (fn [{:keys [type-ref/property type-ref/datalog-verb]}]
                (let [value (get props property)]
                  (when value
                    (keep (fn [target]
                            (when (keyword? target)
                              {:edge/source   dev-id
                               :edge/target   target
                               :edge/verb     datalog-verb
                               :edge/property property
                               :edge/version  version}))
                          (extract-targets value)))))
              refs)))
         registry)]
    (d/transact! conn txdata)
    {:version version
     :edges (count txdata)
     :type-refs (count type-ref-ids)}))

;; =============================================================================
;; Edge queries
;; =============================================================================

(defn edges-at
  "All edges for a given entity at a given version."
  [db version dev-id]
  (->> (d/q '[:find (pull ?e [:edge/source :edge/target :edge/verb :edge/property])
              :in $ ?version ?dev-id
              :where
              [?e :edge/version ?version]
              [?e :edge/source  ?dev-id]]
            db version dev-id)
       (map first)
       (sort-by :edge/target)))

(defn dependents-of
  "Entities that reference a given target at a version (reverse lookup)."
  [db version target-dev-id]
  (->> (d/q '[:find (pull ?e [:edge/source :edge/verb :edge/property])
              :in $ ?version ?target
              :where
              [?e :edge/version ?version]
              [?e :edge/target  ?target]]
            db version target-dev-id)
       (map first)
       (sort-by :edge/source)))

(defn edges-by-verb
  "All edges with a given verb at a version."
  [db version verb]
  (->> (d/q '[:find (pull ?e [:edge/source :edge/target :edge/property])
              :in $ ?version ?verb
              :where
              [?e :edge/version ?version]
              [?e :edge/verb    ?verb]]
            db version verb)
       (map first)
       (sort-by (juxt :edge/source :edge/target))))

(defn edge-diff
  "Edges added or removed between two versions.
   Returns {:added [...] :removed [...]}."
  [db v-old v-new]
  (let [->tuple (fn [e] [(:edge/source e) (:edge/target e)
                          (:edge/verb e) (:edge/property e)])
        old-edges (->> (d/q '[:find (pull ?e [:edge/source :edge/target :edge/verb :edge/property])
                              :in $ ?v
                              :where [?e :edge/version ?v]]
                            db v-old)
                       (map first))
        new-edges (->> (d/q '[:find (pull ?e [:edge/source :edge/target :edge/verb :edge/property])
                              :in $ ?v
                              :where [?e :edge/version ?v]]
                            db v-new)
                       (map first))
        old-set   (set (map ->tuple old-edges))
        new-set   (set (map ->tuple new-edges))
        added-t   (set/difference new-set old-set)
        removed-t (set/difference old-set new-set)]
    {:added   (->> new-edges (filter #(added-t (->tuple %))) (sort-by :edge/source))
     :removed (->> old-edges (filter #(removed-t (->tuple %))) (sort-by :edge/source))}))

(defn edge-summary
  "Count edges by verb at a version."
  [db version]
  (->> (d/q '[:find ?verb (count ?e)
              :in $ ?version
              :where
              [?e :edge/version ?version]
              [?e :edge/verb    ?verb]]
            db version)
       (sort-by second >)))
