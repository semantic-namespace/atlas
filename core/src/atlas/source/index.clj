(ns atlas.source.index
  "EXPERIMENTAL (see EXPERIMENTAL.md) — the static view of a project's
   sources: what atlas.source would load, read from the files without
   loading any code. The registry entries in its EDN files, with the line
   each key is written on, merged by atlas.source's rules, and the vars
   that implement them (`^{:atlas.source/id ...}`), with what each var
   calls and which qualified keywords it mentions, as clj-kondo reads them.

   It answers where an entity is defined and implemented, what refers to it,
   who produces and consumes a data key, and where the code and the sources
   disagree. `build` reuses a cache of the code analysis and re-analyses only
   the source files whose modification time changed, so every call sees the
   working tree as it is, edits included.

   Runs under babashka (clj-kondo as a pod) and on the JVM (clj-kondo on the
   classpath)."
  (:require [atlas.registry :as registry]
            [atlas.source :as source]
            [atlas.source.pack.pathom :as pathom]
            [atlas.source.pack.schema :as schema]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str])
  (:import (java.io File StringReader)
           (clojure.lang LineNumberingPushbackReader)))

(def ^:private bb? (some? (System/getProperty "babashka.version")))

(def cache-version 8)

;; =============================================================================
;; Files
;; =============================================================================

(defn- canonical [f] (.getCanonicalPath (io/file f)))

(defn- under [root d]
  (let [f (io/file d)] (if (.isAbsolute f) f (io/file root d))))

(defn- hidden?
  "Dotfiles and dot-directories (.lsp, .clj-kondo, Emacs lock files .#x) and
   Emacs auto-save files (#x#): never sources."
  [^File f]
  (let [n (.getName f)] (or (str/starts-with? n ".") (str/starts-with? n "#"))))

(defn- files-under [root dirs exts]
  (sort (for [d dirs
              :let [top (under root d)]
              ^File f (tree-seq #(and (.isDirectory ^File %) (or (= % top) (not (hidden? %))))
                                #(seq (.listFiles ^File %))
                                top)
              :when (and (.isFile f) (not (hidden? f)) (some #(str/ends-with? (.getName f) %) exts))]
          (canonical f))))

(defn- relative [root path]
  (let [r (str (canonical root) "/")]
    (if (str/starts-with? path r) (subs path (count r)) path)))

;; =============================================================================
;; Registry files
;; =============================================================================

(defn- read-keyed-lines
  "[[k v line] ...] for the top-level map in `text`, `line` being where k is
   written. The reader keeps no lines on maps, so the map is read one key and
   value at a time, noting the line before each key."
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
                    (recur (conj acc [k v line])))))))))))

(defn read-registry-file
  "{dev-id {:entry entry :file path :line n}} for one EDN registry file."
  [path]
  (let [entries (read-keyed-lines (slurp path))]
    (when (nil? entries)
      (throw (ex-info "A registry file must hold a map of dev-id to entry" {:file path})))
    (into {} (for [[id entry line] entries]
               [id {:entry entry :file path :line line}]))))

;; =============================================================================
;; register! calls
;; =============================================================================

(defn- literal?
  "True when x is plain data: no symbols or lists (var references, calls,
   fns), no regexes or other host objects."
  [x]
  (cond
    (or (symbol? x) (seq? x)) false
    (map? x) (every? (fn [[k v]] (and (literal? k) (literal? v))) x)
    (coll? x) (every? literal? x)
    :else (or (nil? x) (keyword? x) (string? x) (number? x) (boolean? x) (char? x))))

(defn- resolve-keywords
  "`text` with auto-resolved keywords written out: ::alias/name with the
   namespace the file aliases, ::name with the file's own namespace."
  [text own-ns aliases]
  (str/replace text #"::([^\s/\[\](){}\"@^`~,;]+)(/)?"
               (fn [[_ a slash]]
                 (if slash
                   (str ":" (get aliases a a) "/")
                   (str ":" own-ns "/" a)))))

(defn- call-at
  "The form that starts with the `(` just before the symbol at `row`/`col`
   (1-based), :unreadable when it can't be read as data, nil when the symbol
   isn't at the head of a call. Auto-resolved keywords are resolved with the
   file's namespace and aliases first. `read-opts` go to read-string: the
   schema pack reads .cljc reader conditionals as :clj."
  ([lines row col own-ns aliases] (call-at lines row col own-ns aliases nil))
  ([lines row col own-ns aliases read-opts]
   (let [line (nth lines (dec row) nil)]
     (when (and line (>= col 2) (= \( (nth line (- col 2) nil)))
       (try (binding [*read-eval* false]
              (let [text (resolve-keywords (str/join "\n" (cons (subs line (- col 2)) (drop row lines)))
                                           own-ns aliases)]
                (if read-opts (read-string read-opts text) (read-string text))))
            (catch Exception _ :unreadable))))))

(defn- registration
  "What a register! call says, when its dev-id, type and aspects are written
   as data: {:row :dev-id :entry :dropped}, :dropped being the props whose
   values aren't data (an inline fn, a var reference). Otherwise {:row
   :skipped reason}. A 3-argument call gets the dev-id register! would
   generate."
  [form row]
  (let [args (when (seq? form) (rest form))
        [id type aspects props] (case (count args) 4 args 3 (cons ::generated args) nil)]
    (cond
      (= form :unreadable) {:row row :skipped "not readable as data"}
      (nil? type) {:row row :skipped "not a 3- or 4-argument call"}
      (not (and (literal? type) (literal? aspects) (or (= id ::generated) (literal? id))))
      {:row row :skipped "dev-id, type or aspects not written as data"}
      (not (and (qualified-keyword? type) (set? aspects) (every? qualified-keyword? aspects)
                (or (= id ::generated) (qualified-keyword? id))))
      {:row row :skipped "type, aspects or dev-id of the wrong shape"}
      (not (map? props)) {:row row :skipped "props not written as a map"}
      :else (let [kept (into {} (filter (fn [[k v]] (and (keyword? k) (literal? v)))) props)]
              {:row row
               :dev-id (if (= id ::generated) (registry/generate-dev-id (conj aspects type)) id)
               :entry (assoc (dissoc kept :atlas/dev-id) :atlas/type type :atlas/aspects aspects)
               :dropped (vec (sort (remove (set (keys kept)) (keys props))))}))))

(defn- registrations
  "Every call to atlas.registry/register! in `file`, from clj-kondo's usages,
   with the file's namespace and {alias ns} to resolve ::keywords."
  [file usages own-ns aliases]
  (let [lines (delay (str/split-lines (slurp file)))]
    ;; :row/:col is the call's opening paren; :name-row/:name-col the symbol
    (vec (for [{:keys [row name-row name-col]} (sort-by (juxt :row :col) usages)]
           (registration (call-at @lines name-row name-col own-ns aliases) row)))))

;; =============================================================================
;; Packs
;; =============================================================================
;;
;; A pack reads one library's own registry, in place: clojure.spec and malli
;; (atlas.source.pack.schema), Pathom (atlas.source.pack.pathom), or a
;; project's own, named by its qualified symbol under :packs. A pack is a map;
;; only :pack/find and :pack/read are required.
;;
;;   :pack/id               a keyword naming it
;;   :pack/config-keys      the :packs keys that enable a built-in pack
;;   :pack/kinds            the :kind of the sources it gives (one pack per kind)
;;   :pack/vocabulary       the key namespaces its entries use
;;   :pack/data-keys?       true when its entities are data keys (schemas)
;;   :pack/dataflow         {:consumes [k ...] :produces [k ...]}: entry keys
;;                          that list data keys
;;   :pack/find             (fn [analysis opts]) → sites, each {:filename :row
;;                          :name-row :name-col ...} found in clj-kondo's
;;                          analysis; name-row/name-col is the symbol right
;;                          after the form's opening paren
;;   :pack/read             (fn [sites file opts]) → [{:row :kind :dev-id
;;                          :entry} or {:row :kind :skipped reason}] for one
;;                          file's sites; file is {:form-at (fn [row col])
;;                          :own-ns :aliases}, form-at reading the form as data
;;   :pack/checks           (fn [view]) → problems; view is {:entities
;;                          :of-kinds :data-key? :loc :flow}
;;   :pack/inline-data-keys (fn [entities]) → data keys its entities define
;;                          inline, with no entity of their own
;;   :pack/shape            (fn [entry kind]) → a data key's shape, as text
;;   :pack/members          (fn [entry]) → the data keys a schema is made of
;;   :pack/describe         (fn [entry]) → one line for an editor, or nil
;;
;; `opts` is atlas-source.edn's :packs map. What :pack/read returns is
;; cached per file, so it must be data.

(def builtin-packs [schema/pack pathom/pack])

(defn enabled-packs
  "The packs `packs-config` (atlas-source.edn's :packs) enables: each
   built-in pack one of whose :pack/config-keys it has, and each qualified
   symbol in it, resolved to a project's own pack."
  [packs-config]
  (let [ks (set (keys packs-config))
        own (for [k ks :when (qualified-symbol? k)]
              (let [p (some-> (requiring-resolve k) deref)]
                (when-not (and (map? p) (:pack/find p) (:pack/read p))
                  (throw (ex-info "A pack is a map with :pack/find and :pack/read" {:pack k})))
                p))
        ps (vec (concat (filter #(some ks (:pack/config-keys %)) builtin-packs) (sort-by :pack/id own)))]
    (when-let [dup (seq (for [[k n] (frequencies (mapcat :pack/kinds ps)) :when (> n 1)] k))]
      (throw (ex-info "Two packs give sources of the same kind" {:kinds (vec dup)})))
    ps))

(defn- read-packs
  "{pack-id [result ...]} for `file`: each pack's :pack/read over its sites
   there, once per position (a .cljc file reports each form per language)."
  [file found packs-config own-ns aliases]
  (let [lines (delay (str/split-lines (slurp file)))
        f {:own-ns own-ns :aliases aliases
           :form-at (fn [row col] (call-at @lines row col own-ns aliases {:read-cond :allow :features #{:clj}}))}]
    (into {} (for [[pack sites] found]
               [(:pack/id pack)
                (vec ((:pack/read pack)
                      (sort-by :row (map first (vals (group-by (juxt :name-row :name-col) sites))))
                      f packs-config))]))))

(defn- dataflow
  "Which entry keys list consumed and produced data keys: atlas-source.edn's
   :dataflow, plus what each enabled pack declares."
  [config packs]
  (apply merge-with (comp vec distinct concat)
         {:consumes [] :produces []}
         (:dataflow config)
         (keep :pack/dataflow packs)))

(defn- pack-kinds [idx] (set (mapcat :pack/kinds (:packs idx))))

(defn- pack-of [idx kind] (first (filter #(contains? (:pack/kinds %) kind) (:packs idx))))

(defn describe
  "The line a pack gives an editor for `entry`, or nil."
  [idx entry]
  (some #(when-let [d (:pack/describe %)] (d entry)) (:packs idx)))

;; =============================================================================
;; Code (clj-kondo)
;; =============================================================================

(def ^:private kondo
  (delay
    (if bb?
      (do ((requiring-resolve 'babashka.pods/load-pod) 'clj-kondo/clj-kondo "2026.01.19")
          (requiring-resolve 'pod.borkdude.clj-kondo/run!))
      (or (try (requiring-resolve 'clj-kondo.core/run!) (catch Exception _ nil))
          (throw (ex-info "atlas.source.index needs clj-kondo on the classpath" {}))))))

(defn- analyse-code
  "{file {:vars [..] :calls [..] :keywords [..] :registrations [..]
   :pack-results {pack-id [..]}}} for `files`, the parts of clj-kondo's
   analysis the index uses, and what each enabled pack reads there."
  [files packs packs-config]
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
          own-ns (into {} (for [{:keys [filename name]} (:namespace-definitions analysis)] [filename (str name)]))
          aliases (update-vals (group-by :filename (filter :alias (:namespace-usages analysis)))
                               #(into {} (for [{:keys [alias to]} %] [(str alias) (str to)])))
          regs (by-file :var-usages
                        (filter #(and (= 'atlas.registry (:to %)) (= 'register! (:name %)))))
          found (vec (for [p packs] [p (group-by :filename ((:pack/find p) analysis packs-config))]))
          kws (by-file :keywords
                       (comp (filter #(and (:from-var %) (:ns %)))
                             (map #(select-keys % [:filename :from :from-var :ns :name :row]))))]
      (into {} (for [f files :let [f (canonical f)]]
                 [f {:vars (vec (get vars f)) :calls (vec (get calls f)) :keywords (vec (get kws f))
                     :registrations (registrations f (get regs f) (get own-ns f "user") (get aliases f {}))
                     :pack-results (let [here (for [[p by-file] found :let [sites (get by-file f)] :when (seq sites)] [p sites])]
                                     (if (seq here)
                                       (read-packs f here packs-config (get own-ns f "user") (get aliases f {}))
                                       {}))}])))))

;; =============================================================================
;; Store (the record CI keeps of the evaluated registry)
;; =============================================================================

(defn- git [dir & args]
  (let [{:keys [exit out]} (apply shell/sh "git" "-C" dir args)]
    (when (zero? exit) out)))

(defn- store-commit
  "The commit `ref` names in the store checkout `dir`, or nil."
  [{:keys [dir ref] :or {ref "origin/main"}}]
  (some-> (git dir "rev-parse" "--verify" "--quiet" (str ref "^{commit}")) str/trim not-empty))

(defn- read-store
  "{:commit :date :entities {dev-id entry}} for the registry the store
   recorded at `commit`: the entities/*.edn files under `prefix`, each a run
   of [compound-id props] forms."
  [{:keys [dir prefix] :or {prefix ""}} commit]
  (let [prefix (if (or (str/blank? prefix) (str/ends-with? prefix "/")) prefix (str prefix "/"))
        paths (filter #(re-find #"(?:^|/)entities/[^/]+\.edn$" %)
                      (str/split-lines (or (apply git dir "ls-tree" "-r" "--name-only" commit (when (seq prefix) ["--" prefix])) "")))
        forms (for [p paths
                    :let [r (java.io.PushbackReader. (StringReader. (or (git dir "show" (str commit ":" p)) "")))]
                    form (take-while some? (repeatedly #(edn/read {:eof nil :default (fn [_ v] v)} r)))]
                form)]
    {:commit commit
     :date (some-> (git dir "show" "-s" "--format=%cI" commit) str/trim)
     :entities (into {} (for [[cid props] forms
                              :let [cid (set cid) id (:atlas/dev-id props) type (:atlas/type props)]
                              :when id]
                          [id (assoc (dissoc props :atlas/dev-id) :atlas/type type :atlas/aspects (disj cid type))]))}))

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

(declare build*)

(defn build
  "The index of the project at `root`. `:registry` and `:src` are directories
   under root holding the EDN registry files and the source. With `:cache`, a
   file path, the code analysis is reused for unchanged source files and the
   cache rewritten. With `:previous`, an index this fn returned, that index
   is the cache instead: a long-lived process refreshes in memory, and the
   registry files are re-read only when one of them changed."
  [{:keys [root registry src cache previous runtime store packs dataflow] :or {root "." runtime ".atlas/runtime.edn"}}]
  (let [store (when (:dir store) (update store :dir #(canonical (under root %))))
        config {:root (canonical root) :registry (vec registry) :src (vec src) :runtime runtime :store store
                :packs (or packs {}) :dataflow dataflow}
        store-sha (when store (store-commit store))
        runtime-file (let [f (under root runtime)] (when (.isFile f) (canonical f)))
        edn-files (cond-> (vec (files-under root registry [".edn"])) runtime-file (conj runtime-file))
        src-files (files-under root src [".clj" ".cljc"])
        cached (cond
                 (and previous (= config (:config previous))) previous
                 cache (read-cache cache config))
        stamps (into {} (map (juxt identity mtime)) src-files)
        edn-stamps (into {} (map (juxt identity mtime)) edn-files)]
    (if (and previous (identical? cached previous) (= stamps (:stamps previous)) (= edn-stamps (:edn-stamps previous))
             (= store-sha (get-in previous [:store :commit])))
      (assoc previous :analysed 0)
      (build* config (enabled-packs (:packs config)) root cache cached stamps edn-stamps (remove #{runtime-file} edn-files) src-files runtime-file
              (when store-sha
                (if (= store-sha (get-in cached [:store :commit]))
                  (:store cached)
                  (read-store store store-sha)))))))

(defn- read-runtime
  "The REPL snapshot `atlas.source/watch!` writes: {:written :entities
   {dev-id entry} :registered-by {dev-id ns}}, or nil."
  [path]
  (when path
    (try (edn/read-string (slurp path)) (catch Exception _ nil))))

(defn- build* [config packs root cache cached stamps edn-stamps edn-files src-files runtime-file stored]
  (let [fresh? #(and (= (stamps %) (get-in cached [:stamps %])) (contains? (:code cached) %))
        stale (remove fresh? src-files)
        code (merge (select-keys (:code cached) (filter fresh? src-files))
                    (analyse-code stale packs (:packs config)))
        ;; registry files: re-read only the ones that changed
        edn (into {} (for [f edn-files]
                       [f (if (= (edn-stamps f) (get-in cached [:edn-stamps f]))
                            (get-in cached [:edn f])
                            (try {:located (read-registry-file f)}
                                 (catch Exception ex
                                   {:problem {:problem :unreadable-registry-file :file (relative root f) :message (ex-message ex)}})))]))
        ;; sources: each registry file, and each register! call written as data
        sources (concat
                 (for [[f {:keys [located]}] edn :when located]
                   {:label f :file f :kind :edn
                    :lines (update-vals located :line) :entities (update-vals located :entry)})
                 (for [[f {:keys [registrations]}] code
                       {:keys [row dev-id entry dropped]} registrations :when dev-id]
                   {:label (str f ":" row) :file f :kind :register :dropped dropped
                    :lines {dev-id row} :entities {dev-id entry}})
                 ;; packs: one source per entry a pack reads
                 (for [[f {:keys [pack-results]}] code
                       [_ results] (sort-by key pack-results)
                       {:keys [row kind dev-id entry]} results :when dev-id]
                   {:label (str f ":" row ":" dev-id) :file f :kind kind
                    :lines {dev-id row} :entities {dev-id entry}}))
        by-label (into {} (map (juxt :label identity)) sources)
        data-key-kinds (set (mapcat :pack/kinds (filter :pack/data-keys? packs)))
        merged (source/merge-sources (for [{:keys [label entities]} sources] {:source label :entities entities}))
        definition (fn [id label]
                     (let [{:keys [file kind lines dropped]} (by-label label)]
                       (cond-> {:file file :line (get lines id) :kind kind
                                :keys (vec (sort (for [[k ls] (get-in merged [:provenance id]) :when (contains? ls label)] k)))}
                         (seq dropped) (assoc :dropped dropped))))
        static (into {} (for [[id entry] (:entities merged)
                              :let [defs (mapv #(definition id %) (source/sources-of (:provenance merged) id))
                                    primary (or (first (filter #(some #{:atlas/type} (:keys %)) defs))
                                                ;; a data key: where its schema is written
                                                (first (filter #(data-key-kinds (:kind %)) defs))
                                                (first defs))]]
                          [id (assoc primary :entry entry :definitions defs)]))
        ;; the REPL snapshot fills gaps: only dev-ids no file defines
        snapshot (read-runtime runtime-file)
        ns-file (merge (:ns-files snapshot)
                       (into {} (for [[f {:keys [vars]}] code v vars] [(str (:ns v)) f])))
        runtime (into {} (for [[id entry] (:entities snapshot)
                               :when (not (contains? static id))
                               :let [n (get-in snapshot [:registered-by id])
                                     d {:file (ns-file n) :line 1 :kind :runtime :ns n :as-of (:written snapshot)
                                        :keys (vec (sort (keys entry)))}]]
                           [id (assoc d :entry entry :definitions [d])]))
        ;; the store fills what neither the files nor the REPL say
        recorded (into {} (for [[id entry] (:entities stored)
                                :when (not (or (contains? static id) (contains? runtime id)))
                                :let [f (some #(let [x (under root %)] (when (.isFile x) (canonical x))) (:atlas/source entry))
                                      d (cond-> {:kind :store :store-commit (:commit stored) :as-of (:date stored)
                                                 :keys (vec (sort (keys entry)))}
                                          f (assoc :file f :line 1))]]
                            [id (assoc d :entry entry :definitions [d])]))]
    (when (and cache (or (seq stale) (not= (:stamps cached) stamps)))
      (write-cache cache {:version cache-version :config config :stamps stamps :code code}))
    {:root (canonical root)
     :config config
     :packs packs
     :dataflow (dataflow config packs)
     :stamps stamps
     :edn-stamps edn-stamps
     :edn edn
     :entities (merge recorded runtime static)
     :store (when stored (assoc stored :filled (count recorded)))
     :code code
     :analysed (count stale)
     :runtime (when snapshot {:file runtime-file :written (:written snapshot) :filled (count runtime)})
     :read-problems (concat (keep :problem (vals edn))
                            (for [c (:conflicts merged)]
                              (update c :values (fn [vs] (mapv (fn [v] (update v :sources #(mapv (fn [l] (let [{:keys [file lines]} (by-label l)]
                                                                                                          {:file (relative root file) :line (get lines (:dev-id c))})) %))) vs)))))}))

;; =============================================================================
;; Views
;; =============================================================================

(defn- all-vars [idx] (mapcat :vars (vals (:code idx))))

(defn linked-vars
  "{dev-id [var-definition ...]} for the vars that name a dev-id."
  [idx]
  (group-by :dev-id (filter :dev-id (all-vars idx))))

(defn- impl-of [idx id] (first (get (linked-vars idx) id)))

(defn- loc [idx {:keys [file line filename row end-row ns name kind as-of] :as m}]
  (cond
    (= kind :runtime) (cond-> {:runtime-ns ns :as-of as-of}
                        file (assoc :file (relative (:root idx) file) :line line))
    (= kind :store) (cond-> {:store-commit (:store-commit m) :as-of as-of}
                      file (assoc :file (relative (:root idx) file) :line line))
    file (cond-> {:file (relative (:root idx) file) :line line}
           (= kind :register) (assoc :register! true)
           ((pack-kinds idx) kind) (assoc :pack kind))
    (contains? m :file) nil
    filename {:file (relative (:root idx) filename) :line row :end-line end-row :var (symbol (str ns) (str name))}))

(defn- data-key?
  "True when a pack whose entities are data keys gives the entity: it is a
   data key, whatever else the sources say about it."
  [idx e]
  (let [kinds (set (mapcat :pack/kinds (filter :pack/data-keys? (:packs idx))))]
    (boolean (some #(contains? kinds (:kind %)) (:definitions e)))))

(defn- references
  "[[prop dev-id] ...] for every registry entity an entry names in its props.
   Data keys are not references: `data-key` answers for them."
  [idx entry]
  (let [ids (:entities idx)]
    (distinct
     (for [[k v] entry
           :when (not (#{:atlas/type :atlas/aspects} k))
           x (tree-seq coll? seq v)
           :when (and (qualified-keyword? x) (contains? ids x) (not (data-key? idx (get ids x))))]
       [k x]))))

(defn- of-kinds
  "The entities some source of one of `kinds` gives."
  [idx kinds]
  (filter (fn [[_ e]] (some #(contains? kinds (:kind %)) (:definitions e))) (:entities idx)))

(defn- data-key-entities [idx]
  (filter #(data-key? idx (val %)) (:entities idx)))

(defn- inline-data-keys
  "Data keys the packs' entities define inline, with no entity of their own."
  [idx]
  (into #{} (mapcat #(when-let [f (:pack/inline-data-keys %)] (f (of-kinds idx (:pack/kinds %))))) (:packs idx)))

(defn- prop-named [entry n]
  (mapcat val (filter #(and (coll? (val %)) (= n (name (key %)))) entry)))

(defn- contract-keys
  "The data keys an entry consumes and produces: props named context and
   response (atlas's types), and the keys the index's :dataflow lists."
  [idx entry]
  (let [{:keys [consumes produces]} (:dataflow idx)
        declared (fn [ks] (mapcat #(let [v (get entry %)] (when (coll? v) v)) ks))]
    {:consumes (set (concat (prop-named entry "context") (declared consumes)))
     :produces (set (concat (prop-named entry "response") (declared produces)))}))

;; =============================================================================
;; Questions
;; =============================================================================

(defn where
  "Where `id` is defined and implemented, with its merged type, aspects and
   entry. :definition is the source giving its :atlas/type; :sources lists
   every source that gave it something (a registry file, a register! call,
   or the REPL snapshot) and which keys, with the props a register! call
   gave as code rather than data under :dropped. :impl is the annotated var,
   or the register! call when it gives :atlas/impl inline."
  [idx id]
  (when-let [e (get-in idx [:entities id])]
    (let [entry (:entry e)
          inline (first (filter #(some #{:atlas/impl :execution-function/impl} (:dropped %)) (:definitions e)))]
      {:dev-id id
       :type (:atlas/type entry)
       :aspects (:atlas/aspects entry)
       :definition (loc idx e)
       :impl (or (some->> (impl-of idx id) (loc idx))
                 (when inline (assoc (loc idx inline) :inline true)))
       :sources (mapv (fn [d] (cond-> (assoc (loc idx d) :keys (:keys d))
                                (:dropped d) (assoc :dropped (:dropped d))))
                      (:definitions e))
       :entry (dissoc entry :atlas/type :atlas/aspects)})))

(defn- coverage-note [idx]
  (let [linked (count (filter :dev-id (all-vars idx)))
        fns (count (filter #(and (not (:private %)) (= 'clojure.core/defn (:defined-by %))) (all-vars idx)))]
    {:linked-vars linked :public-fns fns
     :note (when (< 0 linked fns) "only linked vars are seen as entities; calls from unlinked code are listed but not their entities")}))

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
  "The entities producing (response) and consuming (context) data key `k`.
   When a pack shapes it: :shape, each pack's form and where it is written;
   :props, what sources other than the packs say about it (:data/privacy,
   say); :member-of, the schemas it is a key of."
  [idx k]
  (let [rows (for [[id e] (sort-by key (:entities idx))
                   :let [{:keys [consumes produces]} (contract-keys idx (:entry e))]]
               {:dev-id id :produces? (contains? produces k) :consumes? (contains? consumes k)
                :definition (loc idx e)})
        e (get-in idx [:entities k])
        own (when (and e (data-key? idx e)) (:entry e))
        vocabulary (into #{} (mapcat :pack/vocabulary) (:packs idx))
        member-of (vec (for [[id s] (sort-by key (data-key-entities idx))
                             :when (some (fn [p] (when-let [m (:pack/members p)] (some #{k} (m (:entry s)))))
                                         (:packs idx))]
                         {:dev-id id :definition (loc idx s)}))]
    (cond-> {:data-key k
             :producers (vec (for [r rows :when (:produces? r)] (select-keys r [:dev-id :definition])))
             :consumers (vec (for [r rows :when (:consumes? r)] (select-keys r [:dev-id :definition])))}
      own (assoc :shape (vec (for [d (:definitions e)
                                   :let [p (pack-of idx (:kind d))]
                                   :when (and p (:pack/data-keys? p))]
                               (assoc (loc idx d) :form (when-let [f (:pack/shape p)] (f own (:kind d))))))
                 :props (into (sorted-map) (remove #(vocabulary (namespace (key %)))) own))
      (seq member-of) (assoc :member-of member-of))))

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
   in some entity's contract, or that a pack shapes, count as data keys."
  [idx]
  (let [contracts (into {} (for [[id e] (:entities idx)] [id (contract-keys idx (:entry e))]))
        data-keys (-> #{}
                      (into (mapcat (fn [{:keys [consumes produces]}] (concat consumes produces))) (vals contracts))
                      (into (map key) (data-key-entities idx))
                      (into (inline-data-keys idx)))]
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

(defn- pack-problems
  "Each enabled pack's :pack/checks, and the forms a pack found but couldn't
   read (:pack-form-not-read, info)."
  [idx]
  (let [view {:entities (:entities idx)
              :of-kinds #(of-kinds idx %)
              :data-key? #(data-key? idx %)
              :loc #(loc idx %)
              :flow #(contract-keys idx %)}]
    (concat
     (mapcat #(when-let [c (:pack/checks %)] (c view)) (:packs idx))
     (for [[f {:keys [pack-results]}] (sort-by key (:code idx))
           [_ results] (sort-by key pack-results)
           {:keys [row kind skipped]} results :when skipped]
       {:severity :info :problem :pack-form-not-read :pack kind :reason skipped
        :file (relative (:root idx) f) :line row}))))

(defn check
  "Where code and registry files disagree, most severe first."
  [idx]
  (let [linked (linked-vars idx)
        by-id (into {} (for [[k [v]] linked] [k v]))]
    (vec (concat
          (for [p (:read-problems idx)] (assoc p :severity :error))
          (for [[id vs] linked :when (> (count vs) 1)]
            {:severity :error :problem :dev-id-on-several-vars :dev-id id :vars (mapv #(loc idx %) vs)})
          (for [p (source/linkage-problems (into {} (for [[id e] (:entities idx)] [id (:entry e)])) by-id)
                ;; a var is how an entity from a registry file gets its code; an
                ;; entity from register! or the REPL has it inline or at runtime
                :when (and (or (not= :entity-without-impl (:problem p))
                               (every? #(= :edn (:kind %)) (get-in idx [:entities (:dev-id p) :definitions])))
                           ;; what a pack gives is in the library's vocabulary: no :atlas/type
                           (not (and (= :entity-without-type (:problem p))
                                     (some #((pack-kinds idx) (:kind %)) (get-in idx [:entities (:dev-id p) :definitions])))))]
            (cond-> (assoc (dissoc p :var) :severity :error)
              (:var p) (assoc :impl (loc idx (:var p)))
              (get-in idx [:entities (:dev-id p)]) (assoc :definition (loc idx (get-in idx [:entities (:dev-id p)])))))
          (undeclared-calls idx)
          (keys-outside-contract idx)
          (pack-problems idx)
          (for [[f {:keys [registrations]}] (sort-by key (:code idx))
                {:keys [row skipped]} registrations :when skipped]
            {:severity :info :problem :register-call-not-read :reason skipped
             :file (relative (:root idx) f) :line row})))))

(defn summary [idx]
  (let [regs (mapcat :registrations (vals (:code idx)))
        kinds (frequencies (for [[_ e] (:entities idx)] (:kind e)))]
    {:entities (count (:entities idx))
     :entities-by-source (into (sorted-map) kinds)
     :register-calls {:read (count (filter :dev-id regs)) :not-read (count (filter :skipped regs))}
     :packs (let [rs (for [{:keys [pack-results]} (vals (:code idx)) [_ results] pack-results r results] r)]
              (into (sorted-map) (for [[kind rs] (group-by :kind rs)]
                                   [kind {:read (count (filter :dev-id rs)) :not-read (count (filter :skipped rs))}])))
     :runtime-snapshot (:runtime idx)
     :store (some-> (:store idx) (select-keys [:commit :date :filled]))
     :source-files (count (:code idx))
     :analysed-now (:analysed idx)
     :coverage (dissoc (coverage-note idx) :note)}))
