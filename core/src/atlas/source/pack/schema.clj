(ns atlas.source.pack.schema
  "EXPERIMENTAL (see EXPERIMENTAL.md) — the schema pack: data keys and their
   shapes, read from clojure.spec `s/def` calls and from malli registries,
   as data. Nothing is loaded or evaluated.

   `pack` is what atlas.source.index runs (see the Packs section of
   atlas.source.index): enabled by :spec and :malli {:registries [ns/var ...]}
   under atlas-source.edn's :packs, it finds each `s/def` call and each named
   registry var in clj-kondo's analysis, and turns each read form into
   entries. Each entry is keyed by the data key it shapes, in the pack's own
   vocabulary:

     :spec/form   the spec as written           :malli/form   the schema as written
     :spec/req    s/keys :req keys              :malli/keys   qualified keys of its :map children
     :spec/opt    s/keys :opt keys              :malli/unqualified-keys  the others (:qty)
     :spec/req-un, :spec/opt-un                 :malli/refs   other schemas it names
     :spec/refs   other specs it names

   Data keys are global, so these entries merge with whatever else the
   sources say about the same key (an EDN file giving :data/privacy, say).

   Forms are kept as the printed text, with the names of #() arguments
   written back as %: a form holds symbols, regexes and fns, which are not
   values to compare or cache."
  (:require [clojure.string :as str]))

(def kinds
  "The :kind of a source this pack produces."
  #{:spec :malli})

(defn- head-name [form]
  (when (and (seq? form) (symbol? (first form))) (name (first form))))

(defn- qualified-keywords [form]
  (vec (sort (distinct (filter qualified-keyword? (tree-seq coll? seq form))))))

(defn- fn-literal?
  "True for what the reader makes of #(...): (fn* [p1__12#] ...) on the
   JVM, (fn* [%1] ...) in babashka."
  [x]
  (and (seq? x) (= 'fn* (first x)) (vector? (second x))
       (every? #(re-matches #"%\d*|%&|(?:p\d+|rest)__\d+#" (str %)) (remove #{'&} (second x)))))

(defn- form-text
  "`form` printed the way it is written: #(...) as #(...), its arguments
   as %, %1, %&."
  [form]
  (let [walk (fn walk [x]
               (cond
                 (fn-literal? x) (let [body (nth x 2 nil)]
                                   (str "#(" (str/join " " (map walk (if (seq? body) body [body]))) ")"))
                 (seq? x) (str "(" (str/join " " (map walk x)) ")")
                 (vector? x) (str "[" (str/join " " (map walk x)) "]")
                 (set? x) (str "#{" (str/join " " (map walk x)) "}")
                 (map? x) (str "{" (str/join ", " (map (fn [[k v]] (str (walk k) " " (walk v))) x)) "}")
                 :else (pr-str x)))]
    (-> (walk form)
        (str/replace #"p(\d+)__\d+#" "%$1")
        (str/replace #"rest__\d+#" "%&"))))

;; =============================================================================
;; clojure.spec
;; =============================================================================

(defn- keys-parts
  "What an (s/keys ...) form requires and allows. :req may hold (or ...) and
   (and ...) groups; every key in them is listed."
  [form]
  (let [opts (apply hash-map (rest form))]
    (into {} (for [[opt k] [[:req :spec/req] [:opt :spec/opt] [:req-un :spec/req-un] [:opt-un :spec/opt-un]]
                   :let [ks (qualified-keywords (get opts opt))]
                   :when (seq ks)]
               [k ks]))))

(defn spec-def
  "{:dev-id :entry} for an `(s/def k spec)` form, or {:skipped reason}."
  [form]
  (let [[_ k spec] (when (seq? form) form)]
    (cond
      (= form :unreadable) {:skipped "not readable as data"}
      (not= 3 (count form)) {:skipped "not a 2-argument s/def"}
      (not (qualified-keyword? k)) {:skipped "the spec's name is not a qualified keyword"}
      :else (let [refs (vec (remove #{k} (qualified-keywords spec)))]
              {:dev-id k
               :entry (cond-> {:spec/form (form-text spec)}
                        (= "keys" (head-name spec)) (merge (keys-parts spec))
                        (seq refs) (assoc :spec/refs refs))}))))

;; =============================================================================
;; malli
;; =============================================================================

(defn- map-children
  "The keys of every [:map ...] schema inside `schema`. A child is `:k`,
   `[:k schema]` or `[:k props schema]`."
  [schema]
  (for [x (tree-seq coll? seq schema)
        :when (and (vector? x) (= :map (first x)))
        child (rest (if (map? (second x)) (rest x) x))
        :let [k (if (vector? child) (first child) child)]
        :when (keyword? k)]
    k))

(defn- malli-entry [k schema]
  (let [children (distinct (map-children schema))
        qualified (vec (sort (filter qualified-keyword? children)))
        unqualified (vec (sort (remove qualified-keyword? children)))
        refs (vec (remove (set (cons k qualified)) (qualified-keywords schema)))]
    (cond-> {:malli/form (form-text schema)}
      (seq qualified) (assoc :malli/keys qualified)
      (seq unqualified) (assoc :malli/unqualified-keys unqualified)
      (seq refs) (assoc :malli/refs refs))))

(defn- registry-map
  "The map a registry var's value holds: the first map in it whose keys are
   all keywords, some qualified, as in (merge (m/default-schemas) {...}).
   Only its qualified entries: an unqualified key (:map) overrides one of
   malli's own schemas and is not a data key."
  [value]
  (some-> (first (filter #(and (map? %) (seq %) (every? keyword? (keys %)) (some qualified-keyword? (keys %)))
                         (tree-seq coll? seq value)))
          (->> (into {} (filter (comp qualified-keyword? key))))))

(defn malli-registry
  "[{:dev-id :entry} ...] for a `(def name doc? value)` form holding a malli
   registry, or [{:skipped reason}]."
  [form]
  (let [value (when (and (seq? form) (>= (count form) 3)) (last form))
        m (when value (registry-map value))]
    (cond
      (= form :unreadable) [{:skipped "not readable as data"}]
      (nil? m) [{:skipped "no map of qualified keywords to schemas in the value"}]
      :else (vec (for [[k schema] (sort-by key m)]
                   {:dev-id k :entry (malli-entry k schema)})))))

;; =============================================================================
;; Shapes, for the index's views and checks
;; =============================================================================

(defn shapes
  "What a merged entry says about its key's shape, by pack: {:spec text
   :malli text}, only the packs that shape it."
  [entry]
  (cond-> {}
    (:spec/form entry) (assoc :spec (:spec/form entry))
    (:malli/form entry) (assoc :malli (:malli/form entry))))

(defn member-keys
  "The data keys a schema entry is made of: s/keys keys, malli map keys."
  [entry]
  (vec (distinct (concat (:spec/req entry) (:spec/opt entry) (:spec/req-un entry) (:spec/opt-un entry)
                         (:malli/keys entry)))))

;; =============================================================================
;; The pack
;; =============================================================================

(def ^:private spec-nses '#{clojure.spec.alpha cljs.spec.alpha})

(defn- find-forms
  "Each s/def call, and each def of a registry var :malli :registries names."
  [analysis opts]
  (concat
   (when (contains? opts :spec)
     (for [u (:var-usages analysis)
           :when (and (spec-nses (:to u)) (= 'def (:name u)))]
       (assoc (select-keys u [:filename :row :name-row :name-col]) :kind :spec)))
   (let [registries (set (get-in opts [:malli :registries]))]
     (for [d (:var-definitions analysis)
           :when (contains? registries (symbol (str (:ns d)) (str (:name d))))]
       {:filename (:filename d) :row (:row d) :name-row (:row d) :name-col (inc (:col d)) :kind :malli}))))

(defn- read-forms [sites {:keys [form-at]} _opts]
  (vec (for [{:keys [kind row name-row name-col]} sites
             r (let [form (form-at name-row name-col)]
                 (if (= kind :spec) [(spec-def form)] (malli-registry form)))]
         (assoc r :row row :kind kind))))

(defn- inline-data-keys
  "Data keys a malli :map gives a schema inline ([:product/id :uuid]), with
   no registry entry of their own."
  [schema-entities]
  (into #{} (mapcat #(get-in (val %) [:entry :malli/keys])) schema-entities))

(defn- checks
  "All :info:
   - :schema-key-without-shape: an s/keys names a key with no spec of its
     own. spec allows it (presence only), and it is how a typo looks;
   - :unqualified-schema-keys: a malli :map with unqualified keys, which no
     dataflow query can follow;
   - :data-key-without-schema: keys in an entity's contract no schema shapes,
     reported only once some schema shapes something."
  [{:keys [entities of-kinds data-key? loc flow]}]
  (let [schemas (sort-by key (of-kinds kinds))
        shaped (into (set (map key schemas)) (inline-data-keys schemas))]
    (concat
     (for [[id e] schemas
           :let [{:spec/keys [req opt req-un opt-un]} (:entry e)
                 missing (vec (sort (remove shaped (concat req opt req-un opt-un))))]
           :when (seq missing)]
       {:severity :info :problem :schema-key-without-shape :dev-id id :keys missing :definition (loc e)})
     (for [[id e] schemas
           :let [ks (get-in e [:entry :malli/unqualified-keys])]
           :when (seq ks)]
       {:severity :info :problem :unqualified-schema-keys :dev-id id :keys ks :definition (loc e)})
     (when (seq schemas)
       (for [[id e] (sort-by key entities)
             :when (not (data-key? e))
             :let [{:keys [consumes produces]} (flow (:entry e))
                   missing (vec (sort (remove shaped (into consumes produces))))]
             :when (seq missing)]
         {:severity :info :problem :data-key-without-schema :dev-id id :keys missing :definition (loc e)})))))

(def pack
  {:pack/id :schema
   :pack/config-keys #{:spec :malli}
   :pack/kinds kinds
   :pack/vocabulary #{"spec" "malli"}
   :pack/data-keys? true
   :pack/find find-forms
   :pack/read read-forms
   :pack/inline-data-keys inline-data-keys
   :pack/shape (fn [entry kind] (get (shapes entry) kind))
   :pack/members member-keys
   :pack/checks checks})
