(ns atlas.source.hover
  "EXPERIMENTAL (see EXPERIMENTAL.md) — what an editor shows for a keyword
   at point, from atlas.source.index: a short line for eldoc, a longer text
   for a describe buffer, and the locations for jump-to-definition and
   find-references.

   A keyword is one of
   - an entity (a dev-id the sources define): its type, aspects, contract,
     where each source defines it, its implementing var, who uses it, and
     any problem `check` reports for it;
   - a data key (in some entity's context or response): who produces and
     consumes it, and where code mentions it;
   - an aspect (in some entity's aspects): the entities that carry it;
   - none of these.

   Locations are absolute paths, so an editor can open them directly."
  (:require [atlas.source.index :as index]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- abs-path [idx file] (str (io/file (:root idx) file)))

(defn- at [{:keys [file line end-line]}]
  (if file
    (str file ":" line (when (and end-line (not= end-line line)) (str "-" end-line)))
    "(no file)"))

(defn- source-line
  "One line saying where a source is and what it gave."
  [s]
  (str (at s) "    "
       (cond
         (:runtime-ns s) (str "REPL: registered by " (:runtime-ns s) ", snapshot " (:as-of s) "    ")
         (:store-commit s) (str "store: recorded at " (subs (:store-commit s) 0 7) ", " (:as-of s) "    ")
         (:register! s) "register!    ")
       (str/join " " (map name (:keys s)))
       (when (seq (:dropped s)) (str "    (as code: " (str/join " " (map name (:dropped s))) ")"))))

(defn- few
  "The first n items printed, and how many more there are."
  [xs n]
  (let [xs (vec xs)]
    (str (str/join " " (map pr-str (take n xs)))
         (when (> (count xs) n) (str " +" (- (count xs) n))))))

(defn- prop-ending [entry suffix]
  (some (fn [[k v]] (when (= suffix (name k)) v)) entry))

(defn- short-type [t] (when t (name t)))

;; =============================================================================
;; Entity
;; =============================================================================

(defn- entity [idx id]
  (let [w (index/where idx id)
        u (index/uses idx id)
        entry (:entry w)
        problems (filter #(= id (:dev-id %)) (index/check idx))
        context (prop-ending entry "context")
        response (prop-ending entry "response")
        deps (prop-ending entry "deps")
        fields (prop-ending entry "fields")
        n-used (+ (count (:referenced-by u)) (count (:called-from u)))
        contract (or (index/describe idx entry)
                     (cond
                       (or context response) (str "[" (few context 4) "] → [" (few response 4) "]")
                       fields (str "fields [" (few fields 5) "]")))
        line2 (str/join "  ·  " (remove nil? [contract
                                               (when (seq deps) (str "deps " (few deps 3)))
                                               (str "used by " n-used)
                                               (when (seq problems) (str "⚠ " (count problems)))
                                               (when-let [r (first (filter :runtime-ns (:sources w)))]
                                                 (str "REPL only (" (:runtime-ns r) ")"))
                                               (when-let [r (first (filter :store-commit (:sources w)))]
                                                 (str "store only (" (subs (:store-commit r) 0 7) ")"))]))
        others (sort-by key (remove (fn [[k _]] (#{"context" "response" "deps" "fields"} (name k))) entry))]
    {:kind "entity"
     :eldoc (str (pr-str id) "  " (short-type (:type w)) "  " (few (sort (:aspects w)) 6) "\n" line2)
     :describe
     (str/join
      "\n"
      (concat
       [(str (pr-str id) "    " (short-type (:type w)))
        ""
        (str "aspects    " (str/join " " (map pr-str (sort (:aspects w)))))]
       (when context [(str "context    " (str/join " " (map pr-str context)))])
       (when response [(str "response   " (str/join " " (map pr-str response)))])
       (when fields [(str "fields     " (str/join " " (map pr-str fields)))])
       (when (seq deps) [(str "deps       " (str/join " " (map pr-str (sort deps))))])
       (for [[k v] others] (str (pr-str k) "  " (pr-str v)))
       [""]
       (for [[i s] (map-indexed vector (:sources w))]
         (str (if (zero? i) "defined    " "           ") (source-line s)))
       [(let [impl (:impl w)]
          (cond
            (:var impl) (str "impl       " (:var impl) "    " (at impl))
            (:inline impl) (str "impl       inline in the register! call    " (at impl))
            :else "impl       none"))]
       [""]
       (if (seq (:referenced-by u))
         (cons "used by" (for [r (:referenced-by u)] (str "  " (pr-str (:dev-id r)) "  via " (pr-str (:via r)) "    " (at (:definition r)))))
         ["used by    nothing names it"])
       (when (seq (:called-from u))
         (cons "called from" (for [c (:called-from u)] (str "  " (:var c) (when (:dev-id c) (str " " (pr-str (:dev-id c)))) "    " (at c)))))
       (when-let [note (get-in u [:coverage :note])]
         [(str "  (" (get-in u [:coverage :linked-vars]) " of " (get-in u [:coverage :public-fns]) " public fns linked; " note ")")])
       (when (seq problems)
         (concat [""] (for [p problems]
                        (str "⚠ " (name (:severity p)) " " (name (:problem p))
                             (when (:keys p) (str " " (few (:keys p) 6)))
                             (when (:calls p) (str " calls " (pr-str (:calls p))))
                             (when-let [l (or (:impl p) (:definition p) (when (:file p) p))] (str "    " (at l)))))))))
     :definitions (vec (concat (for [s (:sources w) :when (:file s)]
                                 {:file (abs-path idx (:file s)) :line (:line s)
                                  :summary (str (cond (:runtime-ns s) "registered at runtime by " (:store-commit s) "recorded in the store from " (:register! s) "register!: " :else "defined: ")
                                                (if (:runtime-ns s) (:runtime-ns s) (str/join " " (map name (:keys s)))))})
                               (when-let [impl (when (:var (:impl w)) (:impl w))]
                                 [{:file (abs-path idx (:file impl)) :line (:line impl) :summary (str "impl: " (:var impl))}])))
     :references (vec (concat (for [r (:referenced-by u)]
                                {:file (abs-path idx (get-in r [:definition :file])) :line (get-in r [:definition :line])
                                 :summary (str (pr-str (:dev-id r)) " via " (pr-str (:via r)))})
                              (for [c (:called-from u)]
                                {:file (abs-path idx (:file c)) :line (:line c) :summary (str "called from " (:var c))})))}))

;; =============================================================================
;; Data key
;; =============================================================================

(defn- mentions
  "Where code mentions keyword `k`, by the var it appears in."
  [idx k]
  (for [[file {:keys [keywords]}] (sort-by key (:code idx))
        kw keywords
        :when (and (= (str (:ns kw)) (namespace k)) (= (str (:name kw)) (name k)))]
    {:file file :line (:row kw) :var (symbol (str (:from kw)) (str (:from-var kw)))}))

(defn- data-key [idx k]
  (let [{:keys [producers consumers shape props member-of]} (index/data-key idx k)
        code (mentions idx k)
        ids #(str/join ", " (map (comp pr-str :dev-id) %))
        problems (filter #(= k (:dev-id %)) (index/check idx))]
    {:kind "data-key"
     :eldoc (str (pr-str k) "  data key"
                 (str/join (for [s shape] (str "  " (name (:pack s)) " " (:form s))))
                 (str/join (for [[p v] props] (str "  " (pr-str p) " " (pr-str v))))
                 "\nproduced by " (if (seq producers) (ids producers) "nothing")
                 "  ·  consumed by " (if (seq consumers) (ids consumers) "nothing")
                 (when (seq member-of) (str "  ·  in " (ids member-of))))
     :describe (str/join "\n" (concat [(str (pr-str k) "    data key") ""]
                                      (for [s shape] (str (format "%-11s" (name (:pack s))) (:form s) "    " (at s)))
                                      (for [[p v] props] (str (pr-str p) "  " (pr-str v)))
                                      (when (seq member-of)
                                        (cons "member of" (for [m member-of] (str "  " (pr-str (:dev-id m)) "    " (at (:definition m))))))
                                      (when (or (seq shape) (seq props) (seq member-of)) [""])
                                      ["produced by"] (for [p producers] (str "  " (pr-str (:dev-id p)) "    " (at (:definition p))))
                                      [""] ["consumed by"] (for [c consumers] (str "  " (pr-str (:dev-id c)) "    " (at (:definition c))))
                                      [""] [(str "mentioned in code (" (count code) ")")]
                                      (for [m code] (str "  " (:var m) "    " (at {:file (#'index/relative (:root idx) (:file m)) :line (:line m)})))
                                      (when (seq problems)
                                        (concat [""] (for [p problems]
                                                       (str "⚠ " (name (:severity p)) " " (name (:problem p))
                                                            (when (:keys p) (str " " (few (:keys p) 6)))))))))
     :definitions (vec (concat (for [s shape :when (:file s)]
                                 {:file (abs-path idx (:file s)) :line (:line s) :summary (str (name (:pack s)) ": " (:form s))})
                               (for [p producers] {:file (abs-path idx (get-in p [:definition :file])) :line (get-in p [:definition :line]) :summary (str "produced by " (pr-str (:dev-id p)))})))
     :references (vec (concat (for [c consumers] {:file (abs-path idx (get-in c [:definition :file])) :line (get-in c [:definition :line]) :summary (str "consumed by " (pr-str (:dev-id c)))})
                              (for [m code] {:file (:file m) :line (:line m) :summary (str "in " (:var m))})))}))

;; =============================================================================
;; Aspect
;; =============================================================================

(defn- aspect [idx k carriers]
  {:kind "aspect"
   :eldoc (str (pr-str k) "  aspect of " (count carriers) " entities: " (few (map key carriers) 5))
   :describe (str/join "\n" (concat [(str (pr-str k) "    aspect, " (count carriers) " entities") ""]
                                    (for [[id e] carriers] (str "  " (pr-str id) "  " (short-type (get-in e [:entry :atlas/type])) "    " (at {:file (#'index/relative (:root idx) (:file e)) :line (:line e)})))))
   :definitions []
   :references (vec (for [[id e] carriers] {:file (:file e) :line (:line e) :summary (pr-str id)}))})

;; =============================================================================

(defn lookup
  "What to show for keyword `k`. {:kind \"none\"} when atlas doesn't know it."
  [idx k]
  (let [entities (:entities idx)
        carriers (sort-by key (filter #(contains? (set (get-in (val %) [:entry :atlas/aspects])) k) entities))]
    (cond
      (:shape (index/data-key idx k)) (data-key idx k)
      (contains? entities k) (entity idx k)
      (or (seq (:producers (index/data-key idx k))) (seq (:consumers (index/data-key idx k)))) (data-key idx k)
      (seq carriers) (aspect idx k carriers)
      :else {:kind "none"})))
