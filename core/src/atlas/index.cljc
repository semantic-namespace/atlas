(ns atlas.index
  "An index of an atlas project read from its files, without loading any of
   its code: the registry entries in its EDN files, with the line each starts
   on, and the vars that implement them (`^{:atlas.source/id ...}`), with what
   each var calls and which qualified keywords it mentions, as clj-kondo
   reads them.

   It answers where an entity is defined and implemented, what refers to it,
   who produces and consumes a data key, and where the code and the registry
   disagree. `build` reuses a cache of the code analysis and re-analyses only
   the source files whose modification time changed, so every call sees the
   working tree as it is, edits included.

   Runs under babashka (clj-kondo as a pod) and on the JVM (clj-kondo on the
   classpath)."
  (:require [atlas.source :as source]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            #?(:bb [edamame.core :as e]))
  #?(:bb (:import (java.io File))
     :clj (:import (java.io File StringReader)
                   (clojure.lang LineNumberingPushbackReader))))

(def cache-version 2)

;; =============================================================================
;; Files
;; =============================================================================

(defn- canonical [f] (.getCanonicalPath (io/file f)))

(defn- files-under [root dirs exts]
  (sort (for [d dirs
              ^File f (file-seq (io/file root d))
              :when (and (.isFile f) (some #(str/ends-with? (.getName f) %) exts))]
          (canonical f))))

(defn- relative [root path]
  (let [r (str (canonical root) "/")]
    (if (str/starts-with? path r) (subs path (count r)) path)))

;; =============================================================================
;; Registry files
;; =============================================================================

#?(:bb
   (defn- read-keyed-lines
     "[[k v line] ...] for the top-level map in `text`, `line` being where k
      is written. edamame puts :line on each value; the key is on that line
      or the last line before it that has the key as a whole token."
     [text]
     (let [m (e/parse-string text {:row-key :line :col-key :column})
           lines (str/split-lines text)
           key-line (fn [k value-line]
                      (let [token (re-pattern (str (java.util.regex.Pattern/quote (str k)) "(?=[\\s{}\\[\\]()]|$)"))]
                        (or (some #(when (re-find token (nth lines (dec %))) %) (range value-line 0 -1))
                            value-line)))]
       (when (map? m)
         (for [[k v] m] [k v (key-line k (or (:line (meta v)) 1))]))))

   :clj
   (defn- read-keyed-lines
     "[[k v line] ...] for the top-level map in `text`, `line` being where k
      is written. Clojure's reader keeps no lines on maps, so the map is read
      one key and value at a time, noting the line before each key."
     [text]
     (let [r (LineNumberingPushbackReader. (StringReader. text))
           skip (fn [] (loop []
                         (let [c (.read r)]
                           (cond
                             (= c -1) c
                             (or (Character/isWhitespace c) (= c (int \,))) (recur)
                             (= c (int \;)) (do (while (not (#{-1 (int \newline)} (.read r)))) (recur))
                             :else c))))]
       (binding [*read-eval* false]
         (when (= (int \{) (skip))
           (loop [acc []]
             (let [c (skip)]
               (if (or (= c -1) (= c (int \})))
                 acc
                 (do (.unread r c)
                     (let [line (.getLineNumber r)
                           k (read r)
                           v (read r)]
                       (recur (conj acc [k v line]))))))))))))

(defn read-registry-file
  "{dev-id {:entry entry :file path :line n}} for one EDN registry file."
  [path]
  (let [entries (read-keyed-lines (slurp path))]
    (when (nil? entries)
      (throw (ex-info "A registry file must hold a map of dev-id to entry" {:file path})))
    (into {} (for [[id entry line] entries]
               [id {:entry entry :file path :line line}]))))

;; =============================================================================
;; Code (clj-kondo)
;; =============================================================================

(def ^:private kondo
  (delay
    #?(:bb (do ((requiring-resolve 'babashka.pods/load-pod) 'clj-kondo/clj-kondo "2026.01.19")
               (requiring-resolve 'pod.borkdude.clj-kondo/run!))
       :clj (or (try (requiring-resolve 'clj-kondo.core/run!) (catch Exception _ nil))
                (throw (ex-info "atlas.index needs clj-kondo on the classpath" {}))))))

(defn- analyse-code
  "{file {:vars [..] :calls [..] :keywords [..]}} for `files`, the parts of
   clj-kondo's analysis the index uses."
  [files]
  (if (empty? files)
    {}
    (let [{:keys [analysis]} (@kondo {:lint (vec files)
                                      :config {:output {:canonical-paths true}
                                               :linters ^:replace {}
                                               :analysis {:var-definitions {:meta [:atlas.source/id]}
                                                          :keywords true}}})
          by-file (fn [k xf] (group-by :filename (into [] xf (get analysis k))))
          vars (by-file :var-definitions
                        (map (fn [{:keys [meta] :as d}]
                               (cond-> (select-keys d [:filename :ns :name :row :end-row :private :defined-by])
                                 (:atlas.source/id meta) (assoc :dev-id (:atlas.source/id meta))))))
          calls (by-file :var-usages
                         (comp (filter :from-var)
                               (map #(select-keys % [:filename :from :from-var :to :name :row]))))
          kws (by-file :keywords
                       (comp (filter #(and (:from-var %) (:ns %)))
                             (map #(select-keys % [:filename :from :from-var :ns :name :row]))))]
      (into {} (for [f files :let [f (canonical f)]]
                 [f {:vars (vec (get vars f)) :calls (vec (get calls f)) :keywords (vec (get kws f))}])))))

;; =============================================================================
;; Building
;; =============================================================================

(defn- mtime [path] (.lastModified (io/file path)))

(def ^:private transit
  "cognitect.transit's reader and writer fns when it is on the classpath (it
   is built into bb). The cache is ~10x faster to read as transit than EDN."
  (delay (try {:writer (requiring-resolve 'cognitect.transit/writer) :write (requiring-resolve 'cognitect.transit/write)
               :reader (requiring-resolve 'cognitect.transit/reader) :read (requiring-resolve 'cognitect.transit/read)}
              (catch Exception _ nil))))

(defn- write-cache [path c]
  (io/make-parents path)
  (if-let [{:keys [writer write]} @transit]
    (with-open [o (io/output-stream path)] (write (writer o :json) c))
    (spit path (pr-str c))))

(defn- read-cache [path config]
  (try
    (let [c (if-let [{:keys [reader read]} @transit]
              (with-open [i (io/input-stream path)] (read (reader i :json)))
              (edn/read-string (slurp path)))]
      (when (and (= cache-version (:version c)) (= config (:config c))) c))
    (catch Exception _ nil)))

(defn build
  "The index of the project at `root`. `:registry` and `:src` are directories
   under root holding the EDN registry files and the source. With `:cache`, a
   file path, the code analysis is reused for unchanged source files and the
   cache rewritten."
  [{:keys [root registry src cache] :or {root "."}}]
  (let [config {:root (canonical root) :registry (vec registry) :src (vec src)}
        edn-files (files-under root registry [".edn"])
        src-files (files-under root src [".clj" ".cljc"])
        cached (when cache (read-cache cache config))
        stamps (into {} (map (juxt identity mtime)) src-files)
        fresh? #(and (= (stamps %) (get-in cached [:stamps %])) (contains? (:code cached) %))
        stale (remove fresh? src-files)
        code (merge (select-keys (:code cached) (filter fresh? src-files))
                    (analyse-code stale))
        reads (for [f edn-files]
                (try {:file f :located (read-registry-file f)}
                     (catch Exception ex
                       {:problem {:problem :unreadable-registry-file :file (relative root f) :message (ex-message ex)}})))
        located (filter :located reads)
        line-of (into {} (for [{:keys [file located]} located [id {:keys [line]}] located] [[file id] line]))
        merged (source/merge-sources (for [{:keys [file located]} located]
                                       {:source file :entities (update-vals located :entry)}))
        definition (fn [id file]
                     {:file file :line (line-of [file id])
                      :keys (vec (sort (for [[k fs] (get-in merged [:provenance id]) :when (contains? fs file)] k)))})
        entities (into {} (for [[id entry] (:entities merged)
                                :let [defs (mapv #(definition id %) (source/sources-of (:provenance merged) id))
                                      primary (or (first (filter #(some #{:atlas/type} (:keys %)) defs)) (first defs))]]
                            [id (assoc primary :entry entry :definitions defs)]))]
    (when (and cache (or (seq stale) (not= (:stamps cached) stamps)))
      (write-cache cache {:version cache-version :config config :stamps stamps :code code}))
    {:root (canonical root)
     :entities entities
     :code code
     :analysed (count stale)
     :read-problems (concat (keep :problem reads)
                            (for [c (:conflicts merged)]
                              (update c :values (fn [vs] (mapv (fn [v] (update v :sources #(mapv (fn [f] {:file (relative root f) :line (line-of [f (:dev-id c)])}) %))) vs)))))}))

;; =============================================================================
;; Views
;; =============================================================================

(defn- all-vars [idx] (mapcat :vars (vals (:code idx))))

(defn linked-vars
  "{dev-id [var-definition ...]} for the vars that name a dev-id."
  [idx]
  (group-by :dev-id (filter :dev-id (all-vars idx))))

(defn- impl-of [idx id] (first (get (linked-vars idx) id)))

(defn- loc [idx {:keys [file line filename row end-row ns name]}]
  (cond
    file {:file (relative (:root idx) file) :line line}
    filename {:file (relative (:root idx) filename) :line row :end-line end-row :var (symbol (str ns) (str name))}))

(defn- references
  "[[prop dev-id] ...] for every registry entity an entry names in its props."
  [idx entry]
  (let [ids (:entities idx)]
    (distinct
     (for [[k v] entry
           :when (not (#{:atlas/type :atlas/aspects} k))
           x (tree-seq coll? seq v)
           :when (and (qualified-keyword? x) (contains? ids x))]
       [k x]))))

(defn- prop-named [entry n]
  (mapcat val (filter #(and (coll? (val %)) (= n (name (key %)))) entry)))

(defn- contract-keys [entry]
  {:consumes (set (prop-named entry "context"))
   :produces (set (prop-named entry "response"))})

;; =============================================================================
;; Questions
;; =============================================================================

(defn where
  "Where `id` is defined and implemented (its var), with its merged type,
   aspects and entry. :definition is the file giving its :atlas/type;
   :sources lists every file that gave it something, and which keys."
  [idx id]
  (when-let [e (get-in idx [:entities id])]
    (let [entry (:entry e)]
      {:dev-id id
       :type (:atlas/type entry)
       :aspects (:atlas/aspects entry)
       :definition (loc idx e)
       :impl (some->> (impl-of idx id) (loc idx))
       :sources (mapv (fn [d] (assoc (loc idx d) :keys (:keys d))) (:definitions e))
       :entry (dissoc entry :atlas/type :atlas/aspects)})))

(defn- coverage-note [idx]
  (let [linked (count (filter :dev-id (all-vars idx)))
        fns (count (filter #(and (not (:private %)) (= 'clojure.core/defn (:defined-by %))) (all-vars idx)))]
    {:linked-vars linked :public-fns fns
     :note (when (< linked fns) "only linked vars are seen as entities; calls from unlinked code are listed but not their entities")}))

(defn uses
  "What refers to `id`: entries naming it in their props, and code calling
   its var."
  [idx id]
  (let [impl (impl-of idx id)
        var->id (into {} (for [[k [v]] (linked-vars idx)] [[(:ns v) (:name v)] k]))]
    {:dev-id id
     :referenced-by (vec (for [[other e] (sort-by key (:entities idx))
                               [prop x] (references idx (:entry e))
                               :when (= x id)]
                           {:dev-id other :via prop :definition (loc idx e)}))
     :called-from (vec (when impl
                         (for [c (mapcat :calls (vals (:code idx)))
                               :when (and (= (:to c) (:ns impl)) (= (:name c) (:name impl)))]
                           (cond-> {:var (symbol (str (:from c)) (str (:from-var c)))
                                    :file (relative (:root idx) (:filename c)) :line (:row c)}
                             (var->id [(:from c) (:from-var c)]) (assoc :dev-id (var->id [(:from c) (:from-var c)]))))))
     :coverage (coverage-note idx)}))

(defn dependencies
  "The entities `id`'s entry names, with where each is defined."
  [idx id]
  (when-let [e (get-in idx [:entities id])]
    {:dev-id id
     :depends-on (vec (for [[prop x] (references idx (:entry e))]
                        {:dev-id x :via prop :definition (loc idx (get-in idx [:entities x]))}))}))

(defn data-key
  "The entities producing (response) and consuming (context) data key `k`."
  [idx k]
  (let [rows (for [[id e] (sort-by key (:entities idx))
                   :let [{:keys [consumes produces]} (contract-keys (:entry e))]]
               {:dev-id id :produces? (contains? produces k) :consumes? (contains? consumes k)
                :definition (loc idx e)})]
    {:data-key k
     :producers (vec (for [r rows :when (:produces? r)] (select-keys r [:dev-id :definition])))
     :consumers (vec (for [r rows :when (:consumes? r)] (select-keys r [:dev-id :definition])))}))

;; =============================================================================
;; Checks
;; =============================================================================

(defn- undeclared-calls
  "A linked var calling another linked var whose entity its entry doesn't name."
  [idx]
  (let [var->id (into {} (for [[k [v]] (linked-vars idx)] [[(:ns v) (:name v)] k]))]
    (for [c (mapcat :calls (vals (:code idx)))
          :let [from (var->id [(:from c) (:from-var c)])
                to (var->id [(:to c) (:name c)])]
          :when (and from to (not= from to)
                     (not (some #(= to (second %)) (references idx (get-in idx [:entities from :entry])))))]
      {:severity :warning :problem :undeclared-call :dev-id from :calls to
       :file (relative (:root idx) (:filename c)) :line (:row c)})))

(defn- keys-outside-contract
  "Data keys a linked var mentions that are neither in its own context or
   response nor in the response of an entity it names. Only keys that appear
   in some entity's contract count as data keys."
  [idx]
  (let [contracts (into {} (for [[id e] (:entities idx)] [id (contract-keys (:entry e))]))
        data-keys (into #{} (mapcat (fn [{:keys [consumes produces]}] (concat consumes produces))) (vals contracts))]
    (for [[id [v]] (sort-by key (linked-vars idx))
          :when (contains? (:entities idx) id)
          :let [own (contracts id)
                via-deps (into #{} (mapcat #(get-in contracts [(second %) :produces]))
                               (references idx (get-in idx [:entities id :entry])))
                allowed (into (into (:consumes own) (:produces own)) via-deps)
                outside (->> (get-in idx [:code (:filename v) :keywords])
                             (filter #(and (= (:from %) (:ns v)) (= (:from-var %) (:name v))))
                             (map #(keyword (str (:ns %)) (str (:name %))))
                             (filter data-keys)
                             (remove allowed)
                             distinct sort vec)]
          :when (seq outside)]
      {:severity :info :problem :keys-outside-contract :dev-id id :keys outside
       :impl (loc idx v)})))

(defn check
  "Where code and registry files disagree, most severe first."
  [idx]
  (let [linked (linked-vars idx)
        by-id (into {} (for [[k [v]] linked] [k v]))]
    (vec (concat
          (for [p (:read-problems idx)] (assoc p :severity :error))
          (for [[id vs] linked :when (> (count vs) 1)]
            {:severity :error :problem :dev-id-on-several-vars :dev-id id :vars (mapv #(loc idx %) vs)})
          (for [p (source/linkage-problems (into {} (for [[id e] (:entities idx)] [id (:entry e)])) by-id)]
            (cond-> (assoc (dissoc p :var) :severity :error)
              (:var p) (assoc :impl (loc idx (:var p)))
              (get-in idx [:entities (:dev-id p)]) (assoc :definition (loc idx (get-in idx [:entities (:dev-id p)])))))
          (undeclared-calls idx)
          (keys-outside-contract idx)))))

(defn summary [idx]
  {:entities (count (:entities idx))
   :source-files (count (:code idx))
   :analysed-now (:analysed idx)
   :coverage (dissoc (coverage-note idx) :note)})
