(ns atlas.source.pack.schema
  "EXPERIMENTAL (see EXPERIMENTAL.md) — the schema pack: data keys and their
   shapes, read from clojure.spec `s/def` calls and from malli registries,
   as data. Nothing is loaded or evaluated.

   atlas.source.index finds the forms (clj-kondo locates each `s/def` call
   and each registry var named in atlas-source.edn) and reads them; this
   namespace turns a read form into entries. Each entry is keyed by the
   data key it shapes, in the pack's own vocabulary:

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
