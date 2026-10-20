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
         (:runtime-as-of s) (str "running process: snapshot " (:runtime-as-of s) "    ")
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
        ;; its atlas type, or for what a pack gives (no type), the pack
        kind (or (short-type (:type w)) (some-> (get-in w [:definition :pack]) name))
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
     :eldoc (str (pr-str id) "  " kind (when (seq (:aspects w)) (str "  " (few (sort (:aspects w)) 6))) "\n" line2)
     :describe
     (str/join
      "\n"
      (concat
       [(str (pr-str id) "    " kind)
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
;; Diagnostics: check's problems, for one file
;; =============================================================================

(defn- locations
  "Every {:file :line :end-line} a problem points at."
  [p]
  (concat
   (when (and (:file p) (:line p)) [(select-keys p [:file :line])])
   (for [k [:definition :impl] :let [l (get p k)] :when (:file l)] l)
   (filter :file (:vars p))
   (filter :file (:definitions p))
   (for [v (:values p) s (:sources v) :when (:file s)] s)))

(defn- token-at
  "[line col end-col] for the first occurrence of `k` (a keyword, or a
   symbol) written in `lines` from line `from` (1-based) to `to`, or nil.
   ::name forms are not matched; :ns/name and :name are."
  [lines k from to]
  (let [text (pr-str k)
        pat (re-pattern (str "(?<![\\w:/.*+!?<>=-])" (java.util.regex.Pattern/quote text) "(?![\\w/.*+!?<>=-])"))]
    (some (fn [n]
            (when-let [l (nth lines (dec n) nil)]
              (let [m (re-matcher pat l)]
                (when (.find m) [n (inc (.start m)) (inc (.end m))]))))
          (range from (inc (min to (count lines)))))))

(def ^:private window
  "How many lines after a problem's line a key it names is looked for, at
   most: the search also stops where the file's next definition starts."
  40)

(defn- starts
  "The lines where something is defined in the file at canonical `path`:
   entries, forms a pack read, vars."
  [idx path]
  (into (sorted-set)
        (concat (for [[_ e] (:entities idx) d (:definitions e) :when (and (= path (:file d)) (:line d))] (:line d))
                (for [v (get-in idx [:code path :vars])] (:row v)))))

(defn- message
  "What the problem says, in a line, about `k` when it is about one key."
  [p k]
  (let [id (some-> (:dev-id p) pr-str)
        ks (or (some-> k pr-str) (str/join " " (map pr-str (:keys p))))]
    (str "atlas: "
         (case (:problem p)
           :keys-outside-contract (str ks " is used here but is not in " id "'s contract")
           :undeclared-call (str id " calls " (pr-str (:calls p)) ", which its entry doesn't name")
           :conflicting-values (str id " " (pr-str (:key p)) ": the sources give different values: "
                                    (str/join ", " (for [v (:values p) src (:sources v)] (str (:file src) ":" (:line src)))))
           :ref-to-undefined (str "no config defines " ks)
           :init-key-without-config (str "init-key for " id ", which no config defines")
           :data-key-without-schema (str "no schema shapes " ks)
           :schema-key-without-shape (str ks " has no spec of its own")
           :unqualified-schema-keys (str "unqualified key " ks ": dataflow can't follow it")
           :input-without-producer (str "nothing produces " ks)
           :operation-option-not-read (str id ": " (str/join ", " (map name (:options p))) " written as code, not read")
           :pack-form-not-read (str (some-> (:pack p) name) " form not read: " (:reason p))
           :register-call-not-read (str "register! call not read: " (:reason p))
           :id-in-several-packs (str id " is declared by several packs: " (str/join ", " (map name (:packs p))))
           :runtime-disagrees (str id " differs from the running process"
                                   (when (:changed-since-snapshot p) " (the file changed since the snapshot)"))
           :not-in-runtime (str id " is not registered in the running process")
           (str (name (:problem p)) (when id (str " " id)) (when (seq (:keys p)) (str " " ks))
                (when (:reason p) (str ": " (:reason p))) (when (:message p) (str ": " (:message p))))))))

(defn diagnostics
  "check's problems located in `file` (absolute, or relative to the root),
   for an editor: [{:line :col :end-col :severity :problem :message}], the
   columns 1-based and only when a token was found. A problem about keys
   is underlined at each key, after its line; otherwise at its dev-id on
   its line; otherwise the whole line."
  [idx file]
  (let [f (io/file file)
        path (str (.getCanonicalFile (if (.isAbsolute f) f (io/file (:root idx) file))))
        rel (#'index/relative (:root idx) path)
        lines (when (.isFile (io/file path)) (str/split-lines (slurp path)))
        starts (starts idx path)]
    (vec
     (distinct
      (for [p (index/check idx)
            l (locations p)
            :when (= rel (:file l))
            :let [line (:line l)
                  next-start (first (subseq starts > line))
                  end (or (:end-line l) (min (+ line window) (if next-start (dec next-start) Long/MAX_VALUE)))
                  severity (get {:error "error" :warning "warning"} (:severity p) "note")
                  ;; the key as written, or its name where it is destructured
                  ;; ({:user/keys [loyalty-tier]}), within the form
                  at-keys (keep (fn [k] (when-let [t (or (token-at lines k line end)
                                                         (when (qualified-keyword? k) (token-at lines (symbol (name k)) line end)))]
                                          [k t]))
                                (:keys p))
                  ;; the dev-id as written, or its name as the defining
                  ;; form's symbol ((pco/defresolver seed ...))
                  at-id (when-let [id (:dev-id p)]
                          (or (token-at lines id line line)
                              (when (qualified-keyword? id) (token-at lines (symbol (name id)) line line))))]
            d (cond
                (seq at-keys) (for [[k [n c e]] at-keys] {:line n :col c :end-col e :key k})
                at-id (let [[n c e] at-id] [{:line n :col c :end-col e}])
                :else [{:line line}])]
        (cond-> (assoc (dissoc d :key) :severity severity :problem (name (:problem p)) :message (message p (:key d)))
          (nil? (:col d)) (dissoc :col :end-col)))))))

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
