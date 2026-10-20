(ns atlas.source.pack.pathom
  "EXPERIMENTAL (see EXPERIMENTAL.md) — the Pathom pack: resolvers and
   mutations, read as data from `defresolver` and `defmutation` forms
   (Pathom 3, com.wsscode.pathom3.connect.operation, and Pathom 2,
   com.wsscode.pathom.connect). Nothing is loaded or evaluated.

   An operation is keyed by its op-name, `ns/name` as a keyword, in the
   pack's own vocabulary:

     :pathom/op        :resolver or :mutation
     :pathom/input     data keys it needs              (::pco/input)
     :pathom/params    data keys a mutation takes      (::pco/params)
     :pathom/output    data keys it gives, joins and nested keys included
     :pathom/inferred  #{:input :output}: what was read from the code, as
                       Pathom 3 does, rather than declared

   Pathom 3 infers an undeclared input from the destructuring of the input
   argument, and an undeclared output from a map literal returned last. So
   does this pack. Pathom 2 declares both.

   `pack` is what atlas.source.index runs (see the Packs section of
   atlas.source.index), enabled by :pathom under atlas-source.edn's :packs.
   A project's own macros that take defresolver's or defmutation's arguments
   are read when named: :pathom {:resolver-macros [app.connect/defresolver]
   :mutation-macros [...]}. Its :pack/dataflow tells the index which of
   these keys list data consumed and produced."
  (:require [clojure.string :as str]
            [clojure.walk :as walk]))

(def kind :pathom)

(def dataflow
  {:consumes [:pathom/input :pathom/params]
   :produces [:pathom/output]})

(def operation-nses '#{com.wsscode.pathom3.connect.operation com.wsscode.pathom.connect})

(def operation-macros '#{defresolver defmutation})

(defn- option
  "The value of the Pathom option named `n` (input, output, params) in an
   options map, whichever Pathom namespace its key is in."
  [opts n]
  (some (fn [[k v]] (when (and (keyword? k) (= n (name k))
                               (contains? operation-nses (some-> (namespace k) symbol)))
                      v))
        opts))

;; =============================================================================
;; EQL and destructuring
;; =============================================================================

(defn eql-keys
  "Every attribute an EQL vector names: plain keys, join keys and the keys
   inside joins, and keys given parameters as (:k {...}) or Pathom 3's
   optional (pco/? :k). A set (Pathom 2's ::pc/input #{...}) works too."
  [eql]
  (vec (distinct
        (for [x (tree-seq #(or (sequential? %) (set? %) (map? %) (map-entry? %)) seq eql)
              :when (qualified-keyword? x)]
          x))))

(defn- destructured-keys
  "The qualified keys a map destructuring form binds: {:user/keys [id]},
   {:keys [user/id :user/id]}, {id :user/id}."
  [binding]
  (when (map? binding)
    (vec (distinct
          (for [[k v] binding
                x (cond
                    (and (keyword? k) (= "keys" (name k)) (namespace k) (vector? v))
                    (for [s v] (keyword (namespace k) (name s)))
                    (= :keys k)
                    (for [s v :let [kw (if (keyword? s) s (keyword (namespace s) (name s)))]
                          :when (namespace kw)]
                      kw)
                    (and (symbol? k) (qualified-keyword? v)) [v]
                    (and (map? k) (qualified-keyword? v)) [v])]
            x)))))

(defn- returned-keys
  "The qualified keys of a map literal, nested maps included."
  [form]
  (when (map? form)
    (vec (distinct (for [m (tree-seq map? vals form) :when (map? m) k (keys m) :when (qualified-keyword? k)] k)))))

;; =============================================================================
;; defresolver / defmutation
;; =============================================================================

(defn- parts
  "{:name :args :opts :body} of `(defresolver name doc? [args] opts? body*)`.
   A map right after the args is options only when more forms follow; on its
   own it is the body, the map the resolver returns."
  [form]
  (let [[_ nm & more] form
        more (if (string? (first more)) (rest more) more)
        [args & tail] more
        [opts body] (if (and (map? (first tail)) (next tail))
                      [(first tail) (rest tail)]
                      [nil tail])]
    {:name nm :args args :opts opts :body body}))

(defn- resolve-names
  "`opts` with each symbol that names a def in `consts` replaced by its
   value, wherever it is: {::pco/output concept-properties}, or inside a
   join, [{:concept/descriptions description-properties}], where (def
   concept-properties [...]) is written in the same file."
  [opts consts own-ns]
  (walk/postwalk (fn [x]
                   (or (when (and (symbol? x) (or (nil? (namespace x)) (= own-ns (namespace x))))
                         (get consts (symbol (name x))))
                       x))
                 opts))

(defn- computed?
  "True when an option's value is code rather than data: a call, or a name
   that isn't a def in the same file. (pco/? :k) is data."
  [v]
  (boolean (some #(and (symbol? %) (not= "?" (name %))) (tree-seq coll? seq v))))

(defn operation
  "{:dev-id :entry} for a defresolver or defmutation form read as data, or
   {:skipped reason}. `own-ns` is the file's namespace; `consts` is {name
   value} for its defs whose values are data, so options may name them;
   `op` (:resolver or :mutation) is given for a project's own wrapper
   macro, and otherwise read from the form's head.

   An option written as code, like (->> registry keys vec), is not guessed:
   :pathom/not-read lists it, and its keys are left out."
  [form {:keys [own-ns consts op]}]
  (if (= form :unreadable)
    {:skipped "not readable as data"}
    (let [op (or op (if (= "defmutation" (name (first form))) :mutation :resolver))
          {nm :name args :args opts :opts body :body} (update (parts form) :opts #(when % (resolve-names % consts own-ns)))
          input-arg (when (vector? args) (last args))
          in-key (if (and (= op :mutation) (not (option opts "input"))) "params" "input")
          declared-in (option opts in-key)
          declared-out (option opts "output")
          not-read (cond-> #{}
                     (and declared-in (computed? declared-in)) (conj (keyword in-key))
                     (and declared-out (computed? declared-out)) (conj :output))
          in (cond (:input not-read) [] (:params not-read) [] declared-in (eql-keys declared-in) :else (destructured-keys input-arg))
          out (cond (:output not-read) [] declared-out (eql-keys declared-out) :else (returned-keys (last body)))
          inferred (cond-> #{}
                     (and (not declared-in) (seq in)) (conj :input)
                     (and (not declared-out) (seq out)) (conj :output))]
      (cond
        (not (symbol? nm)) {:skipped "the operation's name is not a symbol"}
        (not (vector? args)) {:skipped "no argument vector"}
        :else {:dev-id (keyword own-ns (name nm))
               :entry (cond-> {:pathom/op op}
                        (seq in) (assoc (if (= op :mutation) :pathom/params :pathom/input) in)
                        (seq out) (assoc :pathom/output out)
                        (seq inferred) (assoc :pathom/inferred inferred)
                        (seq not-read) (assoc :pathom/not-read not-read))}))))

(defn summary-line
  "One line for an editor: [inputs] → [outputs]."
  [entry]
  (str (name (:pathom/op entry)) "  ["
       (str/join " " (map pr-str (concat (:pathom/input entry) (:pathom/params entry))))
       "] → [" (str/join " " (map pr-str (:pathom/output entry))) "]"
       (when (seq (:pathom/inferred entry))
         (str "  (inferred: " (str/join ", " (map name (sort (:pathom/inferred entry)))) ")"))))

;; =============================================================================
;; The running environment
;; =============================================================================

(defn- runtime-entry [op config]
  (let [in (option config (if (= op :mutation) "params" "input"))
        out (option config "output")]
    (cond-> {:pathom/op op}
      (seq in) (assoc (if (= op :mutation) :pathom/params :pathom/input) (eql-keys in))
      (seq out) (assoc :pathom/output (eql-keys out)))))

(defn runtime-entities
  "{dev-id entry} for every operation the running Pathom environments in
   `envs` have registered, in the pack's vocabulary. An env is what Pathom
   3's pci/register returns, or Pathom 2's indexes (what pc/register
   returns, or a map holding them under ::pc/indexes). Read by keyword:
   Pathom need not be on atlas's classpath."
  [envs]
  (into (sorted-map)
        (for [env envs
              :let [p2 (or (:com.wsscode.pathom.connect/indexes env) env)]
              [op index config-of] [[:resolver (:com.wsscode.pathom3.connect.indexes/index-resolvers env) :config]
                                    [:mutation (:com.wsscode.pathom3.connect.indexes/index-mutations env) :config]
                                    [:resolver (:com.wsscode.pathom.connect/index-resolvers p2) identity]
                                    [:mutation (:com.wsscode.pathom.connect/index-mutations p2) identity]]
              [sym operation] index
              :when (qualified-symbol? sym)]
          [(keyword (namespace sym) (name sym)) (runtime-entry op (config-of operation))])))

(defn snapshot!
  "Writes what the running Pathom environments in `envs` have registered to
   `file` (default .atlas/packs/pathom.edn), for atlas.source.index to read
   beside what it reads from the code. Call it from the app's REPL, or after
   the env is built:

     (atlas.source.pack.pathom/snapshot! [env])"
  ([envs] (snapshot! envs {}))
  ([envs {:keys [file] :or {file ".atlas/packs/pathom.edn"}}]
   (let [f (java.io.File. ^String file)]
     (some-> (.getParentFile f) .mkdirs)
     (spit f (with-out-str
               (prn {:pack :pathom
                     :written (System/currentTimeMillis)
                     :entities (runtime-entities envs)})))
     file)))

(def ^:private slots {:input :pathom/input :params :pathom/params :output :pathom/output})

(defn- runtime-fill
  "`entry` with the slots static reading couldn't read (:pathom/not-read)
   taken from the running environment's `runtime` entry, and listed under
   :pathom/from-runtime."
  [entry runtime]
  (let [unread (:pathom/not-read entry)
        filled (filter #(contains? runtime (slots %)) unread)
        left (reduce disj (set unread) filled)]
    (if (empty? filled)
      entry
      (cond-> (-> (reduce (fn [e k] (assoc e (slots k) (get runtime (slots k)))) entry filled)
                  (assoc :pathom/from-runtime (set filled))
                  (dissoc :pathom/not-read))
        (seq left) (assoc :pathom/not-read left)))))

(defn- runtime-diff
  "{slot {:only-in-code [...] :only-at-runtime [...]}} where what the code
   says and what the running environment registered differ; nil when they
   agree."
  [entry runtime]
  (not-empty
   (into (sorted-map)
         (for [k (vals slots)
               :let [code (set (get entry k)) live (set (get runtime k))]
               :when (not= code live)]
           [k (cond-> {}
                (seq (remove live code)) (assoc :only-in-code (vec (sort (remove live code))))
                (seq (remove code live)) (assoc :only-at-runtime (vec (sort (remove code live)))))]))))

;; =============================================================================
;; The pack
;; =============================================================================

(defn- wrapper-ops
  "{macro-symbol op} for a project's own wrapper macros, from the pack's
   options."
  [opts]
  (merge (zipmap (get-in opts [:pathom :resolver-macros]) (repeat :resolver))
         (zipmap (get-in opts [:pathom :mutation-macros]) (repeat :mutation))))

(defn- find-forms
  "Each defresolver and defmutation call, and each call to a wrapper macro
   the options name. A file's plain defs come along, as constants an
   operation's options may name."
  [analysis opts]
  (let [wrappers (wrapper-ops opts)]
    (concat
     (for [u (:var-usages analysis)
           :let [sym (symbol (str (:to u)) (str (:name u)))]
           :when (or (and (operation-nses (:to u)) (operation-macros (:name u)))
                     (contains? wrappers sym))]
       (assoc (select-keys u [:filename :row :name-row :name-col]) :op (get wrappers sym)))
     (for [d (:var-definitions analysis)
           :when (= 'clojure.core/def (:defined-by d))]
       {:filename (:filename d) :row (:row d) :name-row (:row d) :name-col (inc (:col d))
        :const (:name d)}))))

(defn- data?
  "True when x is data a reader gives back as written: no symbols or lists."
  [x]
  (cond
    (or (symbol? x) (seq? x)) false
    (map? x) (every? (fn [[k v]] (and (data? k) (data? v))) x)
    (coll? x) (every? data? x)
    :else (or (nil? x) (keyword? x) (string? x) (number? x) (boolean? x))))

(defn- read-forms [sites {:keys [form-at own-ns]} _opts]
  (let [{consts true ops false} (group-by #(contains? % :const) sites)
        consts (into {} (for [{:keys [const name-row name-col]} consts
                              :let [form (form-at name-row name-col)
                                    v (when (and (seq? form) (>= (count form) 3)) (last form))]
                              :when (and (coll? v) (data? v))]
                          [const v]))]
    (vec (for [{:keys [row name-row name-col op]} ops]
           (assoc (operation (form-at name-row name-col) {:own-ns own-ns :consts consts :op op})
                  :row row :kind kind)))))

(defn- checks
  "All :info:
   - :input-without-producer: a resolver needs a key that nothing produces,
     no resolver and no other entity's contract. The query can still give it
     as input (that is how a graph's entry points look); when nothing does,
     the resolver is unreachable. When some operation's output is written as
     code, :unread-outputs names them: any of them may produce the key;
   - :operation-option-not-read: an input, params or output written as code
     ((->> registry keys vec)), so its keys are unknown."
  [{:keys [entities loc flow]}]
  (let [produced (into #{} (mapcat #(:produces (flow (:entry (val %))))) entities)
        unread (vec (sort (for [[id e] entities :when (get-in e [:entry :pathom/not-read :output])] id)))]
    (concat
     (for [[id e] (sort-by key entities)
           :let [missing (vec (sort (remove produced (get-in e [:entry :pathom/input]))))]
           :when (seq missing)]
       (cond-> {:severity :info :problem :input-without-producer :dev-id id :keys missing :definition (loc e)}
         (seq unread) (assoc :unread-outputs unread)))
     (for [[id e] (sort-by key entities)
           :let [options (get-in e [:entry :pathom/not-read])]
           :when (seq options)]
       {:severity :info :problem :operation-option-not-read :dev-id id :options (vec (sort options)) :definition (loc e)}))))

(def pack
  {:pack/id :pathom
   :pack/config-keys #{:pathom}
   :pack/kinds #{kind}
   :pack/vocabulary #{"pathom"}
   :pack/data-keys? false
   :pack/dataflow dataflow
   :pack/find find-forms
   :pack/read read-forms
   :pack/checks checks
   :pack/runtime-fill runtime-fill
   :pack/runtime-diff runtime-diff
   :pack/describe (fn [entry] (when (:pathom/op entry) (summary-line entry)))})
