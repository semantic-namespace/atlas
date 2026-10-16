(ns atlas.source
  "The registry as the merge of its sources.

   A source is anything that says something about entities: an EDN file
   written by a person or an LLM, a generator run at startup, an adapter
   reading config. Each gives a map of dev-id to entry,

     {:fn/get-cart {:atlas/type    :atlas/execution-function
                    :atlas/aspects #{:tier/service :domain/cart}
                    :execution-function/context [:session/data]
                    ...}}

   and an entity may be described by several of them, each giving the keys
   it owns. Keys are qualified, so they say who owns them, and the merge
   does not depend on the order the sources are read in:

   - different keys from different sources are combined;
   - a key given twice with the same value is fine;
   - :atlas/aspects is the union of every source's aspects;
   - any other key given with different values is a conflict, reported
     with every source that gave it; :atlas/type included.

   Code links to entities with metadata on a plain var,

     (defn ^{:atlas.source/id :fn/get-cart} get-cart [ctx deps] ...)

   and loading registers each merged entity with that var as :atlas/impl
   (the var, so redefining at the REPL is seen) and the sources that gave it
   as :atlas/source."
  (:require [atlas.registry :as registry]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [clojure.edn :as edn]
               :cljs [cljs.reader :as edn])))

(def impl-types
  "Entity types whose entries must be backed by an annotated var."
  #{:atlas/execution-function})

;; =============================================================================
;; Reading
;; =============================================================================

(defn read-entities
  "The entries of one EDN registry file, as {dev-id entry}."
  [text]
  (let [m (edn/read-string text)]
    (when-not (map? m)
      (throw (ex-info "A registry file must hold a map of dev-id to entry" {:read m})))
    m))

;; =============================================================================
;; Merging
;; =============================================================================

(defn- labels
  "A set of source labels, sorted by how they print: a label may be a file
   path (string) or a generator (symbol), which don't compare."
  [xs]
  (into (sorted-set-by #(compare (pr-str %1) (pr-str %2))) xs))

(defn merge-sources
  "Merges `sources`, each {:source label :entities {dev-id entry}}, into
   {:entities   {dev-id entry}
    :provenance {dev-id {key #{label ...}}}
    :conflicts  [{:problem :conflicting-values :dev-id :key :values [{:value :sources}]}]}.
   The result does not depend on the order of `sources`. A conflicting key
   is left out of its entity; the rest of the entity is kept."
  [sources]
  (let [given (for [{:keys [source entities]} sources
                    [id entry] entities
                    [k v] entry]
                {:id id :k k :v v :source source})
        by-key (group-by (juxt :id :k) given)
        merged-value (fn [[_ k] gs]
                       (if (= k :atlas/aspects)
                         {:value (into #{} (mapcat :v) gs)}
                         (let [by-value (group-by :v gs)]
                           (if (= 1 (count by-value))
                             {:value (:v (first gs))}
                             {:conflict (vec (sort-by (comp pr-str :sources)
                                                      (for [[v vgs] by-value]
                                                        {:value v :sources (labels (map :source vgs))})))}))))
        results (into {} (for [[idk gs] by-key] [idk (merged-value idk gs)]))
        conflicts (vec (for [[[id k] {:keys [conflict]}] (sort-by (comp pr-str key) results) :when conflict]
                         {:problem :conflicting-values :dev-id id :key k :values conflict}))]
    {:entities (reduce (fn [acc [[id k] {:keys [value conflict]}]]
                         (cond-> (update acc id #(or % {}))
                           (not conflict) (assoc-in [id k] value)))
                       {}
                       results)
     :provenance (reduce (fn [acc [[id k] gs]] (assoc-in acc [id k] (labels (map :source gs))))
                         {}
                         by-key)
     :conflicts conflicts}))

(defn sources-of
  "The labels of every source that gave `id` something, sorted."
  [provenance id]
  (vec (labels (apply concat (vals (get provenance id))))))

;; =============================================================================
;; Annotated vars
;; =============================================================================

#?(:clj
   (defn annotated-vars
     "{dev-id var} for every public var in `nss` whose metadata has
      :atlas.source/id. Two vars claiming one dev-id is an error."
     [nss]
     (reduce (fn [acc v]
               (let [id (:atlas.source/id (meta v))]
                 (when-let [other (get acc id)]
                   (throw (ex-info "dev-id claimed by more than one var" {:dev-id id :vars [other v]})))
                 (assoc acc id v)))
             {}
             (for [n nss
                   v (vals (ns-publics (the-ns n)))
                   :when (:atlas.source/id (meta v))]
               v))))

;; =============================================================================
;; Linkage
;; =============================================================================

(defn linkage-problems
  "Where code and the merged entities disagree:
   - :var-without-entity — a var names a dev-id no source defines;
   - :entity-without-type — no source gives the entity an :atlas/type;
   - :entity-without-impl — an entity of an `impl-types` type has no var."
  [entities vars]
  (concat
   (for [[id v] vars :when (not (contains? entities id))]
     {:problem :var-without-entity :dev-id id :var v})
   (for [[id entry] entities :when (not (:atlas/type entry))]
     {:problem :entity-without-type :dev-id id})
   (for [[id {:atlas/keys [type]}] entities
         :when (and (impl-types type) (not (contains? vars id)))]
     {:problem :entity-without-impl :dev-id id :type type})))

;; =============================================================================
;; Registration
;; =============================================================================

(defn register-entities!
  "Registers every merged entity, with its annotated var (if any) as
   :atlas/impl and the labels of its sources as :atlas/source. Returns the
   registered dev-ids."
  [{:keys [entities provenance]} vars]
  (doall
   (for [[id {:atlas/keys [type aspects] :as entry}] (sort-by key entities)]
     (do (registry/register! id type (set aspects)
                             (cond-> (assoc (dissoc entry :atlas/type :atlas/aspects)
                                            :atlas/source (sources-of provenance id))
                               (contains? vars id) (assoc :atlas/impl (get vars id))))
         id))))

#?(:clj
   (defn- ->source
     "A source map from a file path or classpath resource (labelled by the
      string given, so the label is the same on every machine), or a source
      map as given."
     [s]
     (if (map? s)
       s
       (let [f (io/file s)
             in (if (.exists f) f (or (io/resource s) (throw (ex-info "registry file not found" {:source s}))))]
         {:source s :entities (read-entities (slurp in))}))))

#?(:clj
   (defn load!
     "Merges `sources` (registry file paths or resources, or source maps
      {:source label :entities {...}} from a generator), finds the annotated
      vars in `nss`, and registers the merged entities. Throws when sources
      conflict or code and sources disagree."
     [sources nss]
     (let [merged (merge-sources (map ->source sources))
           vars (annotated-vars nss)]
       (when-let [conflicts (seq (:conflicts merged))]
         (throw (ex-info "Registry sources conflict" {:conflicts (vec conflicts)})))
       (when-let [problems (seq (linkage-problems (:entities merged) vars))]
         (throw (ex-info "Registry sources and code disagree" {:problems (vec problems)})))
       (register-entities! merged vars))))

;; =============================================================================
;; Static view (no code loaded)
;; =============================================================================

#?(:clj
   (defn static-links
     "{dev-id {:ns :name :filename :row :end-row}} for the vars under `paths`
      whose metadata names a dev-id, read by clj-kondo without loading any
      code. Needs clj-kondo on the classpath."
     [paths]
     (let [kondo-run! (requiring-resolve 'clj-kondo.core/run!)
           {:keys [analysis]} (kondo-run! {:lint (vec paths)
                                           :config {:output {:canonical-paths true}
                                                    :analysis {:var-definitions {:meta [:atlas.source/id]}}}})]
       (into {}
             (for [{:keys [meta] :as d} (:var-definitions analysis)
                   :let [id (:atlas.source/id meta)]
                   :when id]
               [id (select-keys d [:ns :name :filename :row :end-row])])))))
