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

   `dataflow` is what this pack tells the index: which of these keys list
   data consumed and produced."
  (:require [clojure.string :as str]))

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
   value: {::pco/output concept-properties}, where (def concept-properties
   [...]) is written in the same file."
  [opts consts own-ns]
  (let [value (fn [s] (when (and (symbol? s) (or (nil? (namespace s)) (= own-ns (namespace s))))
                        (get consts (symbol (name s)))))]
    (into {} (for [[k v] opts]
               [k (cond
                    (value v) (value v)
                    (coll? v) (into (empty v) (map #(or (value %) %)) v)
                    :else v)]))))

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
