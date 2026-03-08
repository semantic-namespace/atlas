(ns atlas.source-tracker
  "Track source locations of register! calls.

   Scans Clojure source files using tools.reader to find register! forms,
   extracts dev-id and line range, and optionally enriches the registry
   with :atlas/source metadata.

   Combines with git to answer: which entities were touched in a commit?

   All public tools are registered as :atlas/execution-function entities
   and accessed via (atlas.registry.lookup/handle-tool {:tool/name ... :tool/args ...}).

   Dev-only — not needed in production."
  (:require [clojure.tools.reader :as reader]
            [clojure.tools.reader.reader-types :as rt]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.java.shell :as shell]
            [atlas.registry :as registry]))

;; =============================================================================
;; Source Scanning (internal helpers)
;; =============================================================================

(defn- read-all-forms
  "Read all top-level forms from a file using tools.reader with indexing.
   Returns a vector of forms with :line/:end-line metadata."
  [file]
  (let [content (slurp file)
        rdr (rt/indexing-push-back-reader content 1 (str file))]
    (binding [reader/*alias-map* (fn [_] nil)]
      (loop [forms []]
        (let [form (try
                     (reader/read {:eof ::eof :read-cond :allow
                                   :features #{:clj}} rdr)
                     (catch Exception e
                       (when-not (str/includes? (ex-message e) "EOF")
                         (println "Warning: reader error in" (str file) "-" (ex-message e)))
                       ::eof))]
          (if (= form ::eof)
            forms
            (recur (conj forms form))))))))

(defn- register-call?
  "Check if a form is a register! call (any namespace alias)."
  [form]
  (and (list? form)
       (symbol? (first form))
       (= "register!" (name (first form)))))

(defn- extract-dev-id
  "Extract the dev-id from a register! call.
   Returns the keyword if it's a literal, nil if it's a variable (loop case).
   Handles both 3-arity (type aspects value) and 4-arity (dev-id type aspects value)."
  [form]
  (let [args (rest form)
        first-arg (first args)]
    (when (keyword? first-arg)
      (if (and (namespace first-arg)
               (= "atlas" (namespace first-arg)))
        ;; 3-arity: first arg is :atlas/type, no literal dev-id
        nil
        ;; 4-arity: first arg is the dev-id
        first-arg))))

(defn- walk-find-register-calls
  "Walk a form tree and collect all register! calls with metadata.
   Uses manual recursion instead of postwalk to preserve reader metadata."
  [form file-path]
  (let [calls (atom [])]
    (letfn [(walk [x]
              (when (register-call? x)
                (let [m (meta x)
                      dev-id (extract-dev-id x)]
                  (when (and dev-id m (:line m))
                    (swap! calls conj
                           {:dev-id dev-id
                            :file (str file-path)
                            :line (:line m)
                            :end-line (:end-line m)
                            :column (:column m)
                            :end-column (:end-column m)}))))
              (when (sequential? x)
                (doseq [child x]
                  (walk child)))
              (when (map? x)
                (doseq [[k v] x]
                  (walk k)
                  (walk v)))
              (when (set? x)
                (doseq [child x]
                  (walk child))))]
      (walk form))
    @calls))

(defn- scan-file
  "Scan a single file for register! calls.
   Returns a sequence of {:dev-id :file :line :end-line ...} maps."
  [file-path]
  (let [file (io/file file-path)]
    (when (.exists file)
      (let [forms (read-all-forms file)]
        (mapcat #(walk-find-register-calls % file-path) forms)))))

(defn- read-all-forms-from-string
  "Read all forms from a string using tools.reader with indexing.
   Returns a vector of forms with :line/:end-line metadata."
  [content file-label]
  (let [rdr (rt/indexing-push-back-reader content 1 file-label)]
    (binding [reader/*alias-map* (fn [_] nil)]
      (loop [forms []]
        (let [form (try
                     (reader/read {:eof ::eof :read-cond :allow
                                   :features #{:clj}} rdr)
                     (catch Exception e
                       (when-not (str/includes? (ex-message e) "EOF")
                         (println "Warning: reader error in" file-label "-" (ex-message e)))
                       ::eof))]
          (if (= form ::eof)
            forms
            (recur (conj forms form))))))))

(defn- scan-string
  "Scan a string of Clojure source for register! calls.
   Returns a sequence of {:dev-id :file :line :end-line ...} maps."
  [content file-label]
  (let [forms (read-all-forms-from-string content file-label)]
    (mapcat #(walk-find-register-calls % file-label) forms)))

(defn- scan-dirs
  "Scan directories for all .clj/.cljc files containing register! calls.
   Returns a map of dev-id -> source location."
  [dirs]
  (let [clj-files (->> dirs
                       (map io/file)
                       (mapcat file-seq)
                       (filter #(and (.isFile %)
                                     (re-matches #".*\.cljc?$" (.getName %)))))]
    (->> clj-files
         (mapcat #(scan-file (.getPath %)))
         (reduce (fn [acc {:keys [dev-id] :as loc}]
                   (assoc acc dev-id (dissoc loc :dev-id)))
                 {}))))

;; =============================================================================
;; Git Integration (internal helpers)
;; =============================================================================

(defn- git-command
  "Run a git command and return stdout lines."
  [& args]
  (let [result (apply shell/sh "git" args)]
    (when (zero? (:exit result))
      (str/split-lines (str/trim (:out result))))))

(defn- source-for
  "Get the source location for a dev-id."
  [dev-id]
  (some (fn [[_ v]]
          (when (= dev-id (:atlas/dev-id v))
            (:atlas/source v)))
        @registry/registry))

(defn- parse-diff-ranges
  "Parse git diff output to extract changed line ranges per file.
   Returns {file-path [{:start N :end M} ...]}."
  [diff-output]
  (let [lines (str/split-lines diff-output)
        result (atom {})
        current-file (atom nil)]
    (doseq [line lines]
      (cond
        ;; New file header: +++ b/path/to/file.clj
        (str/starts-with? line "+++ b/")
        (reset! current-file (subs line 6))

        ;; Hunk header: @@ -old,len +new,len @@
        (and @current-file (str/starts-with? line "@@"))
        (when-let [[_ start len] (re-find #"\+(\d+)(?:,(\d+))?" line)]
          (let [s (parse-long start)
                l (if len (parse-long len) 1)]
            (swap! result update @current-file
                   (fnil conj []) {:start s :end (+ s l -1)})))))
    @result))

(defn- ranges-overlap?
  "Check if two line ranges overlap."
  [{s1 :start e1 :end} {s2 :start e2 :end}]
  (and (<= s1 e2) (<= s2 e1)))

(defn- source-dirs
  "Return default source directories to scan."
  []
  (filterv #(.isDirectory (io/file %))
           ["core/src" "test" "dev/src"]))

(defn- clj-files-in-commit
  "List .clj/.cljc files that exist at a given commit.
   Uses git ls-tree to find tracked files."
  [commit-ref]
  (let [lines (git-command "ls-tree" "-r" "--name-only" commit-ref)]
    (when lines
      (filterv #(re-matches #".*\.cljc?$" %) lines))))

(defn- file-content-at-commit
  "Get the content of a file as it existed at a specific commit."
  [commit-ref file-path]
  (let [result (shell/sh "git" "show" (str commit-ref ":" file-path))]
    (when (zero? (:exit result))
      (:out result))))

(defn- scan-at-commit
  "Scan source files as they existed at a specific commit.
   Returns a map of dev-id -> {:file :line :end-line ...}."
  [commit-ref]
  (let [files (clj-files-in-commit commit-ref)]
    (when files
      (->> files
           (mapcat (fn [file-path]
                     (when-let [content (file-content-at-commit commit-ref file-path)]
                       (try
                         (scan-string content file-path)
                         (catch Exception e
                           nil)))))
           (reduce (fn [acc {:keys [dev-id] :as loc}]
                     (assoc acc dev-id (dissoc loc :dev-id)))
                   {})))))

(defn- entities-in-commit
  "Find which entity definitions were touched in a commit.
   Scans files AT that commit for accurate line ranges.
   Returns a sequence of {:dev-id :file :line :end-line} maps."
  ([commit-ref]
   (let [diff-output (:out (shell/sh "git" "diff" (str commit-ref "~1.." commit-ref)))
         changed-ranges (parse-diff-ranges diff-output)
         locations (scan-at-commit commit-ref)]
     (->> locations
          (filter (fn [[dev-id {:keys [file line end-line]}]]
                    (let [file-ranges (get changed-ranges file)]
                      (when file-ranges
                        (let [entity-range {:start line :end (or end-line (+ line 10))}]
                          (some #(ranges-overlap? entity-range %) file-ranges))))))
          (map (fn [[dev-id loc]]
                 (assoc loc :dev-id dev-id)))))))

(defn- entities-staged
  "Find which entity definitions are touched in the currently staged changes.
   Returns a sequence of {:dev-id :file :line :end-line} maps."
  []
  (let [diff-output (:out (shell/sh "git" "diff" "--cached"))
        changed-ranges (parse-diff-ranges diff-output)
        locations (scan-dirs (source-dirs))]
    (->> locations
         (filter (fn [[dev-id {:keys [file line end-line]}]]
                   (let [rel-file (str/replace file #"^\./" "")
                         file-ranges (get changed-ranges rel-file)]
                     (when file-ranges
                       (let [entity-range {:start line :end (or end-line (+ line 10))}]
                         (some #(ranges-overlap? entity-range %) file-ranges))))))
         (map (fn [[dev-id loc]]
                (assoc loc :dev-id dev-id))))))

;; =============================================================================
;; Shared logic for tool impls
;; =============================================================================

(defn- recent-entities*
  "Core logic for recent-entities tool. Returns vec of entity maps."
  [n]
  (let [commits (git-command "log" (str "-" n) "--format=%H|%s|%ai")]
    (when commits
      (->> commits
           (mapcat
            (fn [commit-line]
              (let [[sha message date] (str/split commit-line #"\|" 3)]
                (try
                  (let [touched (entities-in-commit sha)]
                    (map (fn [loc]
                           {:dev-id (:dev-id loc)
                            :file (:file loc)
                            :commit sha
                            :message message
                            :date date})
                         touched))
                  (catch Exception _ nil)))))
           (reduce (fn [acc entry]
                     (if (some #(= (:dev-id %) (:dev-id entry)) acc)
                       acc
                       (conj acc entry)))
                   [])))))

(defn- recent-aspects*
  "Core logic for recent-aspects tool. Returns ranked aspect vec."
  [n]
  (let [entities (recent-entities* n)
        dev-ids (map :dev-id entities)]
    (->> dev-ids
         (mapcat (fn [dev-id]
                   (when-let [entry (some (fn [[cid v]]
                                            (when (= dev-id (:atlas/dev-id v))
                                              [cid v]))
                                          @registry/registry)]
                     (let [cid (first entry)
                           aspects (disj cid (registry/entity-type cid))]
                       (map (fn [aspect] {:aspect aspect :dev-id dev-id}) aspects)))))
         (group-by :aspect)
         (map (fn [[aspect entries]]
                {:aspect aspect
                 :count (count entries)
                 :dev-ids (mapv :dev-id entries)}))
         (sort-by :count >)
         vec)))

;; =============================================================================
;; Tool Registration
;; =============================================================================

(registry/register!
 :atlas.source-tracker/staged-entities
 :atlas/execution-function
 #{:tier/tooling :domain/llm-ide :intent/history :tool/staged-entities}
 {:execution-function/context [:git/staged]
  :execution-function/response [:entity/dev-ids :entity/by-file :entity/count]
  :atlas/docs "Show which entity definitions (register! calls) are affected by currently staged git changes. Compares staged index vs HEAD using git diff --cached, intersects changed line ranges with register! form locations."
  :atlas/impl (fn [_]
                (let [entities (entities-staged)
                      by-file (group-by :file entities)]
                  {:entity-count (count entities)
                   :entities (mapv :dev-id entities)
                   :by-file (into {}
                                  (map (fn [[file ents]]
                                         [file (mapv :dev-id ents)])
                                       by-file))}))})

(registry/register!
 :atlas.source-tracker/commit-entities
 :atlas/execution-function
 #{:tier/tooling :domain/llm-ide :intent/history :tool/commit-entities}
 {:execution-function/context [:git/commit-ref]
  :execution-function/response [:entity/dev-ids :entity/by-file :entity/count :git/message]
  :atlas/docs "Show which entity definitions were touched in a specific commit. Scans files as they existed at that commit via git show for accurate line ranges, then intersects with the commit's diff."
  :atlas/impl (fn [{:keys [git/commit-ref]}]
                (let [ref (or commit-ref "HEAD")
                      entities (entities-in-commit ref)
                      commit-msg (first (git-command "log" "-1" "--format=%s" ref))
                      by-file (group-by :file entities)]
                  {:commit ref
                   :message commit-msg
                   :entity-count (count entities)
                   :entities (mapv :dev-id entities)
                   :by-file (into {}
                                  (map (fn [[file ents]]
                                         [file (mapv :dev-id ents)])
                                       by-file))}))})

(registry/register!
 :atlas.source-tracker/commit-range
 :atlas/execution-function
 #{:tier/tooling :domain/llm-ide :intent/history :tool/commit-range}
 {:execution-function/context [:git/base-ref :git/head-ref]
  :execution-function/response [:entity/dev-ids :entity/by-entity :git/commit-count]
  :atlas/docs "Show all entity definitions touched across a range of commits (e.g. a PR branch vs main). Scans each commit individually for accuracy. Returns dev-ids with modification counts."
  :atlas/impl (fn [{:keys [git/base-ref git/head-ref]}]
                (let [commits (git-command "log" "--format=%H" (str base-ref ".." head-ref))]
                  (when commits
                    (let [all-entities (->> commits
                                            (mapcat #(entities-in-commit %))
                                            (group-by :dev-id))]
                      {:base base-ref
                       :head head-ref
                       :commit-count (count commits)
                       :entities-touched (count all-entities)
                       :dev-ids (vec (keys all-entities))
                       :by-entity (into {}
                                        (map (fn [[dev-id locs]]
                                               [dev-id {:file (:file (first locs))
                                                        :times-modified (count locs)}])
                                             all-entities))}))))})

(registry/register!
 :atlas.source-tracker/recent-entities
 :atlas/execution-function
 #{:tier/tooling :domain/llm-ide :intent/history :tool/recent-entities}
 {:execution-function/context [:query/count]
  :execution-function/response [:entity/dev-ids :git/commits :git/dates]
  :atlas/docs "Get dev-ids of entities touched in the last N commits, most recent first. Scans files at each commit for accurate detection."
  :atlas/impl (fn [{:keys [query/count]}]
                (recent-entities* (or count 10)))})

(registry/register!
 :atlas.source-tracker/recent-aspects
 :atlas/execution-function
 #{:tier/tooling :domain/llm-ide :intent/history :tool/recent-aspects}
 {:execution-function/context [:query/count]
  :execution-function/response [:entity/aspects :entity/dev-ids :query/ranked]
  :atlas/docs "Get aspects from recently touched entities, ranked by frequency. Shows which domains, tiers, and operations are active in recent work."
  :atlas/impl (fn [{:keys [query/count]}]
                (recent-aspects* (or count 10)))})

(registry/register!
 :atlas.source-tracker/recent-context
 :atlas/execution-function
 #{:tier/tooling :domain/llm-ide :intent/history :tool/recent-context}
 {:execution-function/context [:query/count]
  :execution-function/response [:entity/dev-ids :entity/aspects :entity/files]
  :atlas/docs "Combined view of recent work: entities, aspects, and files from last N commits. A 'continue where you left off' context snapshot."
  :atlas/impl (fn [{:keys [query/count]}]
                (let [n (or count 10)
                      entities (recent-entities* n)
                      aspects (recent-aspects* n)
                      files (->> entities (map :file) distinct vec)]
                  {:entities (mapv :dev-id entities)
                   :aspects (mapv :aspect (take 10 aspects))
                   :files files
                   :entity-count (count entities)
                   :aspect-count (count aspects)}))})

(registry/register!
 :atlas.source-tracker/attach-source-locations
 :atlas/execution-function
 #{:tier/tooling :domain/llm-ide :intent/history :tool/attach-source-locations}
 {:execution-function/context [:source/dirs]
  :execution-function/response [:source/total :source/tracked :source/untracked]
  :atlas/docs "Enrich registry entities with :atlas/source metadata (file, line, end-line) by scanning source directories with tools.reader. Run at dev startup."
  :atlas/impl (fn [{:keys [source/dirs]}]
                (let [dirs (or dirs (source-dirs))
                      locations (scan-dirs dirs)
                      attached (atom 0)]
                  (doseq [[compound-id value] @registry/registry
                          :let [dev-id (:atlas/dev-id value)
                                loc (get locations dev-id)]
                          :when loc]
                    (swap! registry/registry assoc-in [compound-id :atlas/source] loc)
                    (swap! attached inc))
                  {:total-entities (count @registry/registry)
                   :source-tracked @attached
                   :untracked (- (count @registry/registry) @attached)}))})

(registry/register!
 :atlas.source-tracker/entity-history
 :atlas/execution-function
 #{:tier/tooling :domain/llm-ide :intent/history :tool/entity-history}
 {:execution-function/context [:entity/dev-id]
  :execution-function/response [:git/commits :git/dates :git/authors :git/messages]
  :atlas/docs "Get git history for a specific entity's register! form using git log -L. Returns commits that touched the entity's line range."
  :atlas/impl (fn [{:keys [entity/dev-id]}]
                (when-let [{:keys [file line end-line]} (source-for dev-id)]
                  (let [range-str (format "%d,%d:%s" line (or end-line (+ line 10)) file)
                        lines (git-command "log" "--format=%H|%ai|%an|%s"
                                           (str "-L " range-str))]
                    (when lines
                      (->> lines
                           (filter #(str/includes? % "|"))
                           (map (fn [line]
                                  (let [[commit date author message] (str/split line #"\|" 4)]
                                    {:commit commit
                                     :date date
                                     :author author
                                     :message message}))))))))})
