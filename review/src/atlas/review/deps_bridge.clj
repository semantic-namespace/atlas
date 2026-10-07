(ns atlas.review.deps-bridge
  "What sdiff's dependency graph cannot see in an atlas codebase: a form that
  names a registered keyword depends on the form that registers it. An
  `exec-fn` call by dev-id, an entity's deps and an integrant component key are
  all keywords, so each becomes a call to the registration or the
  `init-key` method that defines it."
  (:require [sdiff.deps :as deps]))

(defn- by-position [entries] (sort-by (juxt :row :col) entries))

(defn edges
  "Edges from keyword mentions to the forms that define those keywords. A form
  defines a keyword when a call matching `defining` (a regex over the called
  var's qualified name) is followed, as its first keyword, by that keyword."
  [defining {:keys [analysis form-at]}]
  (let [kws (update-vals (group-by :filename (filter :ns (:keywords analysis))) by-position)
        kw (fn [{:keys [ns name]}] (keyword (str ns) name))
        first-after (fn [file row col] (some #(when (or (> (:row %) row) (and (= (:row %) row) (> (:col %) col))) %) (kws file)))
        defines (into {} (for [{:keys [filename row col to name]} (:var-usages analysis)
                               :when (and to (re-find defining (str to "/" name)))
                               :let [k (first-after filename row col) f (form-at filename row)]
                               :when (and k f (= f (form-at filename (:row k))))]
                           [(kw k) f]))]
    (for [file (keys kws) e (kws file)
          :let [from (form-at file (:row e)) to (defines (kw e))]
          :when (and from to (not= from to))]
      [from to])))

(defn install!
  "Adds the bridge to sdiff's dependency graph."
  [defining]
  (deps/add-bridge! ::keywords (partial edges defining)))
