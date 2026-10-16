(ns atlas.sidecar
  "Registry definitions kept as EDN files beside plain Clojure code.

   The code carries only a pointer: a var whose metadata names the entity it
   implements,

     (defn ^{:atlas/dev-id :fn/get-cart} get-cart [ctx deps] ...)

   and the full definition lives in data, one map per file keyed by dev-id:

     {:fn/get-cart {:atlas/type    :atlas/execution-function
                    :atlas/aspects #{:tier/service :domain/cart}
                    :execution-function/context [:session/data]
                    ...}}

   Loading registers each entry with `atlas.registry/register!`, attaching the
   annotated var as `:atlas/impl`. The var, not its value, so redefining the fn
   at the REPL is seen without reloading the registry. A var is not EDN, so
   `atlas.registry.serialise` drops it from exported snapshots like any impl."
  (:require [atlas.registry :as registry]
            [clojure.set :as set]
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

(defn merge-entities
  "One {dev-id entry} map from several files' entries. A dev-id defined in
   more than one file is an error, since nothing says which definition wins."
  [entity-maps]
  (reduce (fn [acc m]
            (when-let [dup (seq (set/intersection (set (keys acc)) (set (keys m))))]
              (throw (ex-info "dev-id defined in more than one registry file" {:dev-ids (vec dup)})))
            (merge acc m))
          {}
          entity-maps))

;; =============================================================================
;; Annotated vars
;; =============================================================================

#?(:clj
   (defn annotated-vars
     "{dev-id var} for every public var in `nss` whose metadata has
      :atlas/dev-id. Two vars claiming one dev-id is an error."
     [nss]
     (reduce (fn [acc v]
               (let [id (:atlas/dev-id (meta v))]
                 (when-let [other (get acc id)]
                   (throw (ex-info "dev-id claimed by more than one var" {:dev-id id :vars [other v]})))
                 (assoc acc id v)))
             {}
             (for [n nss
                   v (vals (ns-publics (the-ns n)))
                   :when (:atlas/dev-id (meta v))]
               v))))

;; =============================================================================
;; Linkage
;; =============================================================================

(defn linkage-problems
  "Where code and registry files disagree:
   - :var-without-entity — a var names a dev-id no file defines;
   - :entity-without-impl — an entry of an `impl-types` type has no var."
  [entities vars]
  (concat
   (for [[id v] vars :when (not (contains? entities id))]
     {:problem :var-without-entity :dev-id id :var v})
   (for [[id {:atlas/keys [type]}] entities
         :when (and (impl-types type) (not (contains? vars id)))]
     {:problem :entity-without-impl :dev-id id :type type})))

;; =============================================================================
;; Registration
;; =============================================================================

(defn register-entities!
  "Registers every entry, with its annotated var (if any) as :atlas/impl.
   Returns the registered dev-ids."
  [entities vars]
  (doall
   (for [[id {:atlas/keys [type aspects] :as entry}] (sort-by key entities)]
     (do (registry/register! id type (set aspects)
                             (cond-> (dissoc entry :atlas/type :atlas/aspects)
                               (contains? vars id) (assoc :atlas/impl (get vars id))))
         id))))

#?(:clj
   (defn load!
     "Reads the registry files (anything `slurp` accepts: paths, files,
      resources), finds the annotated vars in `nss`, and registers the entries.
      Throws when code and files disagree; see `linkage-problems`."
     [files nss]
     (let [entities (merge-entities (map (comp read-entities slurp) files))
           vars (annotated-vars nss)]
       (when-let [problems (seq (linkage-problems entities vars))]
         (throw (ex-info "Registry files and code disagree" {:problems (vec problems)})))
       (register-entities! entities vars))))

;; =============================================================================
;; Static view (no code loaded)
;; =============================================================================

#?(:clj
   (defn static-links
     "{dev-id {:ns :name :file :row :end-row}} for the vars under `paths` whose
      metadata names a dev-id, read by clj-kondo without loading any code. This
      is what a reviewer needs to put a changed form next to the entity it
      implements. Needs clj-kondo on the classpath."
     [paths]
     (let [kondo-run! (requiring-resolve 'clj-kondo.core/run!)
           {:keys [analysis]} (kondo-run! {:lint (vec paths)
                                     :config {:output {:canonical-paths true}
                                              :analysis {:var-definitions {:meta [:atlas/dev-id]}}}})]
       (into {}
             (for [{:keys [meta] :as d} (:var-definitions analysis)
                   :let [id (:atlas/dev-id meta)]
                   :when id]
               [id (select-keys d [:ns :name :filename :row :end-row])])))))
