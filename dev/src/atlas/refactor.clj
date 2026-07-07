(ns atlas.refactor
  "Utilities for registry refactoring: transforming compound-ids across
   EDN snapshots, detecting collisions, and summarising changes.

   Usage pattern:

     (def spec
       {:renames          {:old/kw :new/kw}
        :remove-when      {:kw-to-remove #{:must-also-be-present}}
        :remove-always    #{:semantically/empty}
        :decompose        {:one/kw #{:becomes/this :and/this}}
        :remove-by-dev-id {:my/entity #{:aspect/to-remove}}
        :add-by-dev-id    {:my/entity #{:aspect/to-add}}
        :rename-dev-ids   {:old/dev-id :new/dev-id}})

     (let [new-reg (apply-transform spec @registry/registry)]
       (print-collisions (detect-collisions @registry/registry spec))
       (write-edn! \"path/to/v2.edn\" new-reg)
       (print-changes (change-summary @registry/registry new-reg))
       (print-dev-id-renames (dev-id-rename-summary @registry/registry new-reg)))"
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [clojure.pprint :as pprint]))

;; =============================================================================
;; Transform spec keys
;; =============================================================================
;;
;; :renames          {old-kw new-kw}
;;   Global keyword renames applied to every compound-id.
;;   Example: {:serial/collection :cardinality/many}
;;
;; :remove-when      {kw-to-remove #{co-present-kws}}
;;   Remove kw-to-remove only when ALL co-present-kws are also in the compound-id.
;;   Example: {:domain/metrics #{:metrics/counter}}
;;
;; :remove-always    #{kw}
;;   Remove these aspects from every compound-id unconditionally.
;;   Example: #{:filter/none}
;;
;; :decompose        {kw #{replacement-kws}}
;;   Replace one aspect with a set of new aspects.
;;   Example: {:domain/mailing-lists-stats #{:domain/mailing-lists :metrics/stats}}
;;
;; :remove-by-dev-id {dev-id #{kws}}
;;   Remove aspects only from a specific entity (matched by :atlas/dev-id).
;;   Example: {:serial/logins-search-retries #{:domain/searches}}
;;
;; :add-by-dev-id    {dev-id #{kws}}
;;   Add aspects only to a specific entity (matched by :atlas/dev-id).
;;   Example: {:serial/recurring-fees-all #{:temporal/recurring}}
;;
;; :rename-dev-ids   {old-dev-id new-dev-id}
;;   Rename the :atlas/dev-id value in matched entries (compound-id key unchanged).
;;   Applied after all compound-id transforms.
;;   Example: {:serial/search-raws :cache/search-raws}

;; =============================================================================
;; Core transform
;; =============================================================================

(defn transform-compound-id
  "Apply a transform spec to a single compound-id.
   value must contain :atlas/dev-id for per-entity rules to apply."
  [spec compound-id value]
  (let [{:keys [renames remove-when remove-always decompose
                remove-by-dev-id add-by-dev-id]} spec
        dev-id (:atlas/dev-id value)

        ;; 1. Global renames
        step1 (reduce (fn [id [old new]]
                        (if (contains? id old)
                          (-> id (disj old) (conj new))
                          id))
                      compound-id
                      (or renames {}))

        ;; 2. Conditional removals
        step2 (reduce (fn [id [kw co-present]]
                        (if (and (contains? id kw)
                                 (set/superset? id co-present))
                          (disj id kw)
                          id))
                      step1
                      (or remove-when {}))

        ;; 3. Unconditional removals
        step3 (reduce disj step2 (or remove-always #{}))

        ;; 4. Decompositions
        step4 (reduce (fn [id [kw replacements]]
                        (if (contains? id kw)
                          (-> id (disj kw) (set/union replacements))
                          id))
                      step3
                      (or decompose {}))

        ;; 5. Per-entity targeted removals
        step5 (reduce disj step4 (get (or remove-by-dev-id {}) dev-id #{}))

        ;; 6. Per-entity targeted additions
        step6 (reduce conj step5 (get (or add-by-dev-id {}) dev-id #{}))]
    step6))

(defn apply-dev-id-renames
  "Apply :rename-dev-ids from spec to the :atlas/dev-id value in each registry entry.
   The compound-id keys are not affected — only the stored dev-id in the value map."
  [spec registry]
  (let [renames (or (:rename-dev-ids spec) {})]
    (if (empty? renames)
      registry
      (->> registry
           (map (fn [[compound-id value]]
                  (let [dev-id     (:atlas/dev-id value)
                        new-dev-id (get renames dev-id dev-id)]
                    [compound-id (assoc value :atlas/dev-id new-dev-id)])))
           (into {})))))

(defn apply-transform
  "Apply a transform spec to an entire registry map.
   Returns a new registry with transformed compound-ids and renamed dev-ids."
  [spec registry]
  (->> registry
       (map (fn [[compound-id value]]
              [(transform-compound-id spec compound-id value) value]))
       (into {})
       (apply-dev-id-renames spec)))

;; =============================================================================
;; Collision detection
;; =============================================================================

(defn detect-collisions
  "Find cases where two distinct old compound-ids transform to the same new one.
   Returns a seq of collision groups, each a seq of {:dev-id :old-id :new-id}.
   Empty seq means no collisions — safe to write."
  [old-registry spec]
  (->> old-registry
       (map (fn [[k v]] {:new-id (transform-compound-id spec k v)
                         :old-id k
                         :dev-id (:atlas/dev-id v)}))
       (group-by :new-id)
       (filter (fn [[_ entries]] (> (count entries) 1)))
       vals))

;; =============================================================================
;; Change summary
;; =============================================================================

(defn change-summary
  "Diff old-registry against new-registry.
   Returns a seq of changed entries, sorted by dev-id, each with:
     {:dev-id  keyword
      :old     old compound-id set
      :new     new compound-id set
      :added   aspects added
      :removed aspects removed}"
  [old-registry new-registry]
  (->> new-registry
       (keep (fn [[new-cid value]]
               (let [dev-id (:atlas/dev-id value)
                     [old-cid _] (first (filter (fn [[_k v]]
                                                  (= (:atlas/dev-id v) dev-id))
                                                old-registry))]
                 (when (and old-cid (not= old-cid new-cid))
                   {:dev-id  dev-id
                    :old     old-cid
                    :new     new-cid
                    :added   (set/difference new-cid old-cid)
                    :removed (set/difference old-cid new-cid)}))))
       (sort-by (comp str :dev-id))))

(defn dev-id-rename-summary
  "Find entities whose :atlas/dev-id changed between old and new registry.
   Matches entries by compound-id key — reliable when the compound-id itself
   did not change in the same pass. Returns a seq of {:old-dev-id :new-dev-id
   :compound-id}, sorted by new-dev-id."
  [old-registry new-registry]
  (->> new-registry
       (keep (fn [[compound-id new-value]]
               (let [old-value  (get old-registry compound-id)
                     old-dev-id (:atlas/dev-id old-value)
                     new-dev-id (:atlas/dev-id new-value)]
                 (when (and old-dev-id (not= old-dev-id new-dev-id))
                   {:old-dev-id  old-dev-id
                    :new-dev-id  new-dev-id
                    :compound-id compound-id}))))
       (sort-by (comp str :new-dev-id))))

;; =============================================================================
;; I/O
;; =============================================================================

(defn write-edn!
  "Write registry to an EDN file using pretty-print."
  [path registry]
  (spit path (with-out-str (pprint/pprint registry)))
  (println "Written" path (str "(" (count registry) " entries)")))

;; =============================================================================
;; Display helpers
;; =============================================================================

(defn sorted-kws
  "Return a compound-id as a sorted string for display/docs."
  [compound-id]
  (str "#{" (str/join " " (sort (map str compound-id))) "}"))

(defn print-changes
  "Print a change-summary result in a readable format."
  [changes]
  (println "Changed:" (count changes))
  (println)
  (doseq [{:keys [dev-id removed added]} changes]
    (println dev-id)
    (when (seq removed) (println " -" removed))
    (when (seq added)   (println " +" added))
    (println)))

(defn print-collisions
  "Print detected collisions with context."
  [collisions]
  (if (empty? collisions)
    (println "No collisions.")
    (do
      (println "COLLISIONS DETECTED:" (count collisions))
      (doseq [group collisions]
        (println " new-id:" (:new-id (first group)))
        (doseq [{:keys [dev-id old-id]} group]
          (println "  " dev-id old-id))))))

(defn print-dev-id-renames
  "Print a dev-id-rename-summary result."
  [renames]
  (if (empty? renames)
    (println "No dev-id renames.")
    (do
      (println "Dev-id renames:" (count renames))
      (println)
      (doseq [{:keys [old-dev-id new-dev-id]} renames]
        (println old-dev-id "→" new-dev-id)))))

;; =============================================================================
;; Aspect analysis
;; =============================================================================

(defn aspect-frequency
  "Return aspects used in entities of a given type, sorted by total registry
   frequency descending. Useful for spotting overloaded or sparse dimensions.

   Returns a seq of {:aspect :type-count :total-count}."
  [registry entity-type]
  (let [total-freq (->> registry keys (mapcat identity) frequencies)
        type-freq  (->> registry
                        (filter (fn [[_k v]] (= entity-type (:atlas/type v))))
                        (mapcat (fn [[k _]] (disj k entity-type)))
                        frequencies)]
    (->> type-freq
         (map (fn [[aspect n]]
                {:aspect      aspect
                 :type-count  n
                 :total-count (get total-freq aspect 0)}))
         (sort-by :total-count >))))

(defn print-aspect-frequency
  "Print aspect-frequency results in a tabular format."
  [rows]
  (doseq [{:keys [aspect type-count total-count]} rows]
    (println (format "%-45s type: %2d  total: %3d"
                     (str aspect) type-count total-count))))
