(ns atlas.store.canonical
  "Turn a registry into bytes that mean something.

  `pr-str` is not enough. It emits maps and sets in hash order, so two
  serialisations of the *same* registry differ, which defeats three things at
  once: change detection by content, delta compression in a git-backed store,
  and any hope of a reviewable diff. Everything here exists to make the bytes a
  function of the data and nothing else."
  (:require
   [clojure.edn :as edn]
   [clojure.string :as str])
  (:import
   [java.io PushbackReader StringReader]))


;; ---------------------------------------------------------------------------
;; Canonical printing
;; ---------------------------------------------------------------------------

(declare write-canon!)

(defn- write-coll! [^StringBuilder sb open xs close]
  (.append sb ^String open)
  (loop [xs xs first? true]
    (when (seq xs)
      (when-not first? (.append sb " "))
      (write-canon! (first xs) sb)
      (recur (rest xs) false)))
  (.append sb ^String close))

(defn write-canon!
  "Sorting by the *printed* form rather than by value is deliberate: it is total
  over arbitrary EDN, so a registry containing a set of mixed-type keys cannot
  throw a ClassCastException mid-serialisation."
  [x ^StringBuilder sb]
  (cond
    (map? x)    (write-coll! sb "{" (mapcat identity (sort-by (comp pr-str key) x)) "}")
    (set? x)    (write-coll! sb "#{" (sort-by pr-str x) "}")
    (vector? x) (write-coll! sb "[" x "]")
    (seq? x)    (write-coll! sb "(" x ")")
    :else       (.append sb ^String (pr-str x))))

(defn canon-str [x]
  (let [sb (StringBuilder.)] (write-canon! x sb) (.toString sb)))


;; ---------------------------------------------------------------------------
;; Non-deterministic props
;; ---------------------------------------------------------------------------

(def default-volatile-props
  "Props regenerated on every build rather than derived from source.

  yorba-clj's test-case fixtures embed `(java.util.Date.)` and a random uuid, so
  two builds of the *same commit* produced different bytes for five entities.
  The effect is disproportionate: every snapshot looks changed, no-op detection
  never fires, and the diff is noise. Excluding them is a workaround -- the real
  fix is a deterministic fixture -- but a store cannot depend on every consumer
  having done that."
  #{:test-case/fixture})

(defn strip-volatile
  ([registry] (strip-volatile registry default-volatile-props))
  ([registry props]
   (into {} (map (fn [[k v]] [k (apply dissoc v props)])) registry)))

(defn non-deterministic
  "Entities that differ between two builds of the same source, as
  `{compound-id #{prop-key ...}}`. Empty means the snapshot is reproducible.

  Worth running in CI once rather than trusting it: a consumer whose registry
  is not a pure function of its source gets silent churn forever, and the
  symptom (\"every commit changes everything\") does not point at the cause."
  [reg-a reg-b]
  (into {}
        (keep (fn [[cid props-a]]
                (let [props-b (get reg-b cid)
                      differing (into #{} (for [k (distinct (concat (keys props-a) (keys props-b)))
                                                :when (not= (get props-a k) (get props-b k))]
                                            k))]
                  (when (seq differing) [cid differing]))))
        reg-a))


;; ---------------------------------------------------------------------------
;; Layout: one file per entity type, one prop per line
;; ---------------------------------------------------------------------------
;;
;; Chosen by measurement over 8 consecutive real versions, not by taste:
;;
;;   single file        137.1 KB base   2.6 KB per change
;;   per type (pretty)  143.2 KB base   2.7 KB per change   <- this
;;   per entity         482.7 KB base  17.0 KB per change
;;
;; Per-entity files lose cross-entity redundancy -- 1000+ blobs each compress
;; alone -- which costs far more than the finer deltas gain. Pretty-printing
;; within a type file is free (marginally cheaper, in fact) and is what makes a
;; changed dep show up as a one-line diff.

(defn- slug [s]
  (-> (str s) (str/replace #"^:" "") (str/replace #"[^A-Za-z0-9._-]" "_")))

(defn- type-of [props] (slug (or (:atlas/type props) "untyped")))

(defn entity-str
  "One entity: compound id on the first line, then one prop per line."
  [[cid props]]
  (str "[" (canon-str cid) "\n {"
       (str/join "\n  " (map (fn [[k v]] (str (canon-str k) " " (canon-str v)))
                             (sort-by (comp pr-str key) props)))
       "}]\n"))

(defn registry->files
  "Registry -> {relative-path content}, deterministic for a given registry."
  [registry]
  (into {}
        (for [[t entities] (group-by (comp type-of val) registry)]
          [(str "entities/" t ".edn")
           (str/join (map entity-str (sort-by (comp canon-str key) entities)))])))

(defn- read-all [^String content]
  (let [rdr (PushbackReader. (StringReader. content))]
    (doall (take-while some? (repeatedly #(edn/read {:eof nil} rdr))))))

(defn files->registry
  "Inverse of `registry->files`. Only files under `entities/` are read, so a
  store may hold a README or metadata alongside without corrupting the parse."
  [files]
  (into {}
        (mapcat (fn [[path content]]
                  (when (re-find #"(^|/)entities/[^/]+\.edn$" path)
                    (read-all content)))
                files)))
