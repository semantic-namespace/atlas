(ns atlas.ide.history
  "IDE history API — version-scoped queries over the history database.
   All functions are self-registered as :atlas/execution-function entities."
  (:require [atlas.registry :as cid]
            [atlas.registry.lookup :as rt]
            [atlas.history :as history]))

;; =============================================================================
;; HELPERS
;; =============================================================================

(defn- ensure-keyword
  [x]
  (cond
    (keyword? x) x
    (string? x)  (keyword (cond-> x (= \: (first x)) (subs 1)))
    :else        x))

(defn- sorted-vec [coll]
  (->> (or coll []) sort vec))

(defn- normalize-sets
  "Walk a data structure converting sets to sorted vectors for Emacs consumption."
  [x]
  (cond
    (set? x)        (sorted-vec x)
    (map? x)        (reduce-kv (fn [m k v] (assoc m k (normalize-sets v))) {} x)
    (sequential? x) (mapv normalize-sets x)
    :else           x))

;; =============================================================================
;; STATE
;; =============================================================================

(defonce ^:private history-conn (atom nil))

;; Stores the registry state at the time of the last snapshot.
;; Used as prev-registry when computing aspect diffs on the next snapshot.
;; Maintained automatically by snapshot! — callers don't need to manage it.
(defonce ^:private last-snapshot-registry (atom nil))

(defn- ensure-conn
  "Return the history db value, or throw if not initialized."
  []
  (or (some-> @history-conn deref)
      (throw (ex-info "History not initialized. Call (atlas.ide.history/init!) first." {}))))

;; =============================================================================
;; PUBLIC API
;; =============================================================================

(defn init!
  "Initialize (or reset) the history db. Clears the snapshot chain.
   Call this to start fresh tracking. Returns status map."
  []
  (reset! history-conn (history/create-conn))
  (reset! last-snapshot-registry nil)
  {:history/status :initialized})

(defn versions
  "All version strings in the history db, sorted."
  []
  (history/versions (ensure-conn)))

(defn entity-timeline
  "All versions of a single entity under one dev-id, sorted by version."
  [dev-id]
  (->> (history/entity-timeline (ensure-conn) (ensure-keyword dev-id))
       normalize-sets))

(defn full-timeline
  "Complete history of an entity across dev-id renames."
  [dev-id]
  (->> (history/full-timeline (ensure-conn) (ensure-keyword dev-id))
       normalize-sets))

(defn version-diff
  "All entities that changed in a given version."
  [version]
  (->> (history/version-diff (ensure-conn) version)
       normalize-sets))

(defn version-diff-full
  "Full version diff: changes + deleted + detected renames.
   Auto-detects the previous version for rename heuristics.
   Returns {:changes [...] :deleted [...] :renames [...]}."
  [version]
  (let [db       (ensure-conn)
        changes  (normalize-sets (history/version-diff db version))
        deleted  (normalize-sets (history/version-deleted db version))
        vs       (history/versions db)
        idx      (.indexOf (vec vs) version)
        prev-v   (when (pos? idx) (nth vs (dec idx)))
        renames  (if prev-v
                   (normalize-sets (history/detect-renames db prev-v version))
                   [])]
    {:changes changes
     :deleted deleted
     :renames renames}))

(defn version-summary
  "Aggregate change counts per entity type for a version."
  [version]
  (history/version-summary (ensure-conn) version))

(defn vocabulary-diff
  "Aspects whose entity count changed between two versions."
  [v-old v-new]
  (->> (history/vocabulary-diff (ensure-conn) v-old v-new)
       normalize-sets))

(defn edges-at
  "All edges for a given entity at a given version."
  [version dev-id]
  (->> (history/edges-at (ensure-conn) version (ensure-keyword dev-id))
       normalize-sets))

(defn dependents-of
  "Entities that reference a given target at a version (reverse lookup)."
  [version dev-id]
  (->> (history/dependents-of (ensure-conn) version (ensure-keyword dev-id))
       normalize-sets))

(defn edge-diff
  "Edges added or removed between two versions."
  [v-old v-new]
  (->> (history/edge-diff (ensure-conn) v-old v-new)
       normalize-sets))

(defn edge-summary
  "Count edges by verb at a version."
  [version]
  (history/edge-summary (ensure-conn) version))

(defn snapshot!
  "Snapshot the current registry state with a version label.
   Auto-initializes history conn if needed.
   Automatically chains snapshots for diff computation — no manual prev-registry needed.

   Optional rename-map: {old-dev-id new-dev-id} to declare explicit renames so
   entity-timeline can follow the rename chain.

   Returns {:version ... :entities ... :changed ... :deleted ...}."
  ([version-label]
   (snapshot! version-label nil))
  ([version-label rename-map]
   (when-not @history-conn
     (init!))
   (let [prev-reg @last-snapshot-registry
         reg      @cid/registry
         result   (history/snapshot-version! @history-conn reg version-label prev-reg rename-map)]
     (history/snapshot-edges! @history-conn reg version-label)
     (reset! last-snapshot-registry reg)
     result)))

(defn get-conn
  "Return the raw history conn atom (for tools that need direct access)."
  []
  @history-conn)

;; =============================================================================
;; SELF-REGISTRATION
;; =============================================================================

(cid/register!
 :fn.ide/history-init! :atlas/execution-function
 #{:domain/ide :intent/history :operation/init}
 {:execution-function/context []
  :execution-function/response [:history/status]
  :execution-function/deps #{}
  :atlas/docs "Initialize or reset the history database. Clears the snapshot chain so you can start fresh tracking. Call this before taking snapshots."
  :atlas/impl (fn [_] (init!))})

(cid/register!
 :fn.ide/history-versions :atlas/execution-function
 #{:domain/ide :intent/history :operation/list :subject/version}
 {:execution-function/context []
  :execution-function/response [:history/versions]
  :execution-function/deps #{}
  :atlas/docs "List all version labels in the history database, sorted chronologically. Use this to see available snapshots before querying diffs or timelines."
  :atlas/impl (fn [_] (versions))})

(cid/register!
 :fn.ide/history-entity-timeline :atlas/execution-function
 #{:domain/ide :intent/history :operation/lookup :subject/entity-timeline}
 {:execution-function/context [:entity/dev-id]
  :execution-function/response [:history/timeline]
  :execution-function/deps #{}
  :atlas/docs "Show all versions of a single entity under one dev-id, sorted by version. Use this to track how an entity's aspects or props evolved over time."
  :atlas/impl #(entity-timeline (:entity/dev-id %))})

(cid/register!
 :fn.ide/history-full-timeline :atlas/execution-function
 #{:domain/ide :intent/history :operation/lookup :subject/full-timeline}
 {:execution-function/context [:entity/dev-id]
  :execution-function/response [:history/timeline]
  :execution-function/deps #{}
  :atlas/docs "Complete history of an entity across dev-id renames. Unlike entity-timeline, this follows rename chains so you can track an entity even after it was renamed."
  :atlas/impl #(full-timeline (:entity/dev-id %))})

(cid/register!
 :fn.ide/history-version-diff :atlas/execution-function
 #{:domain/ide :intent/history :operation/diff :subject/version}
 {:execution-function/context [:history/version]
  :execution-function/response [:history/changes]
  :execution-function/deps #{}
  :atlas/docs "Show all entities that changed in a given version snapshot. Returns the list of changed entity records. Use this to review what was modified in a specific snapshot."
  :atlas/impl #(version-diff (:history/version %))})

(cid/register!
 :fn.ide/history-version-diff-full :atlas/execution-function
 #{:domain/ide :intent/history :operation/diff :subject/version-full}
 {:execution-function/context [:history/version]
  :execution-function/response [:history/changes :history/deleted :history/renames]
  :execution-function/deps #{}
  :atlas/docs "Full version diff including changes, deletions, and auto-detected renames. More comprehensive than version-diff — use this for complete change review including entities that were removed or renamed."
  :atlas/impl #(version-diff-full (:history/version %))})

(cid/register!
 :fn.ide/history-version-summary :atlas/execution-function
 #{:domain/ide :intent/history :operation/summary :subject/version}
 {:execution-function/context [:history/version]
  :execution-function/response [:history/summary]
  :execution-function/deps #{}
  :atlas/docs "Aggregate change counts per entity type for a version. Gives a quick overview of how many entities of each type were affected in a snapshot."
  :atlas/impl #(version-summary (:history/version %))})

(cid/register!
 :fn.ide/history-vocabulary-diff :atlas/execution-function
 #{:domain/ide :intent/history :operation/diff :subject/vocabulary}
 {:execution-function/context [:history/version-old :history/version-new]
  :execution-function/response [:history/vocabulary-diff]
  :execution-function/deps #{}
  :atlas/docs "Compare the aspect vocabulary between two versions: which aspects gained or lost entities. Use this to detect ontology drift or vocabulary expansion over time."
  :atlas/impl #(vocabulary-diff (:history/version-old %) (:history/version-new %))})

(cid/register!
 :fn.ide/history-edges-at :atlas/execution-function
 #{:domain/ide :intent/history :operation/lookup :subject/edge}
 {:execution-function/context [:history/version :entity/dev-id]
  :execution-function/response [:history/edges]
  :execution-function/deps #{}
  :atlas/docs "Get all edges (dependency relationships) for a given entity at a specific version. Use this to see how an entity's connections looked at a point in time."
  :atlas/impl #(edges-at (:history/version %) (:entity/dev-id %))})

(cid/register!
 :fn.ide/history-dependents-of :atlas/execution-function
 #{:domain/ide :intent/history :operation/lookup :subject/dependent}
 {:execution-function/context [:history/version :entity/dev-id]
  :execution-function/response [:history/dependents]
  :execution-function/deps #{}
  :atlas/docs "Find entities that referenced a given target at a specific version (reverse edge lookup). Use this to see who depended on an entity at a point in time."
  :atlas/impl #(dependents-of (:history/version %) (:entity/dev-id %))})

(cid/register!
 :fn.ide/history-edge-diff :atlas/execution-function
 #{:domain/ide :intent/history :operation/diff :subject/edge}
 {:execution-function/context [:history/version-old :history/version-new]
  :execution-function/response [:history/edge-diff]
  :execution-function/deps #{}
  :atlas/docs "Show edges added or removed between two versions. Use this to understand how the dependency graph evolved between snapshots."
  :atlas/impl #(edge-diff (:history/version-old %) (:history/version-new %))})

(cid/register!
 :fn.ide/history-edge-summary :atlas/execution-function
 #{:domain/ide :intent/history :operation/summary :subject/edge}
 {:execution-function/context [:history/version]
  :execution-function/response [:history/edge-summary]
  :execution-function/deps #{}
  :atlas/docs "Count edges by verb (dependency type) at a version. Gives a quick overview of the dependency graph structure at a point in time."
  :atlas/impl #(edge-summary (:history/version %))})

(cid/register!
 :fn.ide/history-snapshot! :atlas/execution-function
 #{:domain/ide :intent/history :operation/snapshot}
 {:execution-function/context [:history/version-label :history/rename-map]
  :execution-function/response [:history/snapshot]
  :execution-function/deps #{}
  :atlas/docs "Snapshot the current registry state with a version label. Auto-initializes history if needed and chains snapshots for diff computation. Optionally accepts a rename-map {old-dev-id new-dev-id} for tracking renames."
  :atlas/impl #(snapshot! (:history/version-label %) (:history/rename-map %))})
