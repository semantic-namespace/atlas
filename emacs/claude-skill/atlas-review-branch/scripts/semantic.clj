(ns atlas-review.semantic
  "A branch's diff, tied to the registry: which top-level forms each hunk touches,
  which entities and data keys those forms define or mention, and how the two
  registries (base and branch) differ in contract.

  Two REPLs are involved, one per registry. On each:
    (load-file \"…/semantic.clj\")
    (atlas-review.semantic/dump-registry! \"/tmp/registry-<side>.edn\")
  Then on the branch REPL:
    (def r (atlas-review.semantic/analyse \"<worktree>\" \"<base>\" \"/tmp/registry-base.edn\" \"/tmp/registry-branch.edn\"))
    (:cdiff r)                                          ; contracts that changed
    (atlas-review.semantic/append-index! r \"<notebook>.index.org\")   ; evidence per moved key, for the sidecar
    (atlas-review.semantic/write-extended! r \"<notebook>.groupings.org\") ; every hunk, grouped

  Forms are read with tools.reader in the file's own namespace and aliases, so
  ::alias/key resolves as the compiler would. Hunk owners come from sdiff
  (semantic-namespace/diff) when it is installed, else from line overlap, which
  is a lead to verify in the diff, not proof."
  (:require [clojure.edn :as edn]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [clojure.tools.reader :as r]
            [clojure.tools.reader.reader-types :as rt]
            [atlas.registry :as registry]
            [atlas.registry.lookup :as lookup]
            [atlas.ide :as ide]))

;;; Registry dump and contract diff

(defn dump-registry!
  "Write every dev-id's contract (type, identity, context, deps, response, MCP inputs) to PATH."
  [path]
  (let [summ (into {} (for [id (keys @registry/dev-id-index) :let [p (lookup/props-for id)]]
                        [id {:type (:atlas/type p) :aspects (set (lookup/identity-for id))
                             :context (vec (:execution-function/context p))
                             :deps (vec (sort (map str (:execution-function/deps p))))
                             :response (vec (:execution-function/response p))
                             :req (vec (:mcp-tool/req-input-args p)) :opt (vec (:mcp-tool/opt-input-args p))}]))]
    (spit path (pr-str summ))
    (count summ)))

(defn contract-diff
  "Per dev-id: [:new type], [:removed type], or [:changed {field {:- […] :+ […]}}]."
  [base branch]
  (into (sorted-map)
        (for [id (into (set (keys base)) (keys branch))
              :let [a (base id) b (branch id)] :when (not= a b)]
          [id (cond (nil? a) [:new (:type b)]
                    (nil? b) [:removed (:type a)]
                    :else [:changed (into {} (for [k [:type :aspects :context :deps :response :req :opt] :when (not= (k a) (k b))]
                                               [k {:- (vec (remove (set (k b)) (k a))) :+ (vec (remove (set (k a)) (k b)))}]))])])))

;;; Hunks and forms

(defn changed-files [wt base]
  (str/split-lines (:out (sh/sh "git" "diff" "--name-only" base :dir wt))))

(defn hunks
  "Hunks of FILE against BASE (-U0, so a hunk is exactly the changed lines), with
  the added and removed line texts."
  [wt base file]
  (->> (str/split-lines (:out (sh/sh "git" "diff" "-U0" base "--" file :dir wt)))
       (reduce (fn [acc l]
                 (if-let [[_ os oc ns nc] (re-find #"^@@ -(\d+)(?:,(\d+))? \+(\d+)(?:,(\d+))? @@" l)]
                   (let [oc (Long/parseLong (or oc "1")) nc (Long/parseLong (or nc "1"))
                         os (Long/parseLong os) ns (Long/parseLong ns)]
                     (conj acc {:old [os (+ os (max 0 (dec oc)))] :new [ns (+ ns (max 0 (dec nc)))]
                                :kind (cond (zero? oc) :add (zero? nc) :delete :else :modify)
                                :added [] :removed []}))
                   (cond (and (seq acc) (str/starts-with? l "+")) (update-in acc [(dec (count acc)) :added] conj (subs l 1))
                         (and (seq acc) (str/starts-with? l "-")) (update-in acc [(dec (count acc)) :removed] conj (subs l 1))
                         :else acc)))
               [])))

(defn- form-owner
  "What a top-level FORM defines: a registered dev-id, an ig/init-key dispatch entity,
  a spec/doc key, a var, or nil."
  [form]
  (when (seq? form)
    (let [head (first form) hn (when (symbol? head) (name head)) arg (second form)]
      (cond
        (and (= "defmethod" hn) (keyword? (nth form 2 nil))) {:kind :entity :id (nth form 2) :via (str arg)}
        (and (#{"register!" "bind"} hn) (keyword? arg)) {:kind :entity :id arg}
        (and (= "def" hn) (keyword? arg)) {:kind :spec :id arg}
        (and hn (re-matches #"def.*" hn) (symbol? arg)) {:kind :var :id (symbol (name arg))}
        :else nil))))

(defn- ns-info
  "The file's namespace and its :require aliases, from the ns form."
  [src]
  (let [rdr (rt/indexing-push-back-reader (rt/string-push-back-reader src))
        ns-form (try (r/read {:eof nil :read-cond :allow :features #{:clj}} rdr) (catch Exception _ nil))]
    (when (and (seq? ns-form) (= 'ns (first ns-form)))
      {:ns (second ns-form)
       :aliases (into {} (for [clause (rest ns-form) :when (and (seq? clause) (= :require (first clause)))
                               spec (rest clause) :when (vector? spec)
                               :let [[lib & {:keys [as]}] spec] :when as]
                           [as lib]))})))

(defn forms
  "Top-level forms of FILE with their line span and owner, read in the file's ns."
  [wt file]
  (let [src (slurp (java.io.File. wt file))
        {:keys [ns aliases]} (ns-info src)
        rdr (rt/indexing-push-back-reader (rt/string-push-back-reader src))]
    (binding [*ns* (if ns (create-ns ns) *ns*)
              r/*alias-map* (fn [a] (or (get aliases a) (symbol (str "?" a))))
              r/*read-eval* false]
      (loop [acc []]
        (let [f (try (r/read {:eof ::eof :read-cond :allow :features #{:clj}} rdr)
                     (catch Exception e {::err (ex-message e)}))]
          (cond (= ::eof f) acc
                (and (map? f) (::err f)) (recur (conj acc {:err (::err f)}))
                :else (let [m (meta f)]
                        (recur (conj acc {:line (:line m) :end-line (:end-line m)
                                          :owner (form-owner f) :head (when (seq? f) (first f))})))))))))

(def ^:private kw-re #"(::?)([\w.\-]+/)?([\w.\-!?*+]+)")

(defn mentions
  "Qualified keywords in LINES, alias-resolved like the reader would for the file's ns."
  [{:keys [ns aliases]} lines]
  (set (for [l lines
             :let [l (str/replace l #";.*$" "")]
             [_ colons alias-part nm] (re-seq kw-re l)
             :let [alias (some-> alias-part (subs 0 (dec (count alias-part))) symbol)]
             :when (or (= "::" colons) alias-part)]
         (cond (and (= "::" colons) alias) (keyword (str (or (get aliases alias) (str "?" alias))) nm)
               (= "::" colons) (keyword (str ns) nm)
               :else (keyword (str alias) nm)))))

(defn- overlaps? [[a b] [c d]] (and a c (<= a d) (<= c b)))

(def ^:dynamic *sdiff-home*
  (or (System/getenv "SDIFF_HOME") (str (System/getProperty "user.home") "/git/semantic-namespace/diff")))

(defn structural
  "sdiff file reports of WT against BASE by path; nil when sdiff is not installed."
  [wt base]
  (let [{:keys [exit out err]} (try (sh/sh "bb" "sdiff" "edn" wt base "." :dir *sdiff-home*)
                                    (catch Exception e {:exit -1 :err (ex-message e)}))]
    (if (zero? exit)
      (into {} (for [f (:clj (edn/read-string out))] [(:path f) f]))
      (binding [*out* *err*]
        (println "atlas-review: sdiff unavailable, owners come from line overlap:" (str/trim (str err)))
        nil))))

(defn- read-owner [{:keys [ns aliases]} src]
  (when src
    (binding [*ns* (if ns (create-ns ns) *ns*)
              r/*alias-map* (fn [a] (or (get aliases a) (symbol (str "?" a))))
              r/*read-eval* false]
      (try (form-owner (r/read-string {:read-cond :allow :features #{:clj}} src))
           (catch Exception _ nil)))))

(defn- rows [node] (when (:row node) [(:row node) (:end-row node)]))

(defn- path-str [form c]
  (str (str/join " " (remove nil? (:id form)))
       (when (seq (:path c))
         (str " › " (str/join " › " (map #(if (vector? %) (str/join " " %) (str %)) (:path c)))))))

(defn- structural-touch [forms h]
  (for [f forms
        :let [new-hit (and (not= :delete (:kind h)) (overlaps? (:new h) (rows (:new f))))
              old-hit (and (not= :add (:kind h)) (overlaps? (:old h) (rows (:old f))))]
        :when (or new-hit old-hit)
        :let [paths (distinct (for [c (:changes f)
                                    :when (or (and new-hit (overlaps? (:new h) (rows (:new c))))
                                              (and old-hit (overlaps? (:old h) (rows (:old c)))))]
                                (path-str f c)))]]
    {:form f :paths (if (seq paths) (vec paths) [(path-str f nil)])}))

(defn file-map
  "Each hunk of FILE with the forms it changed, their owners, change paths and the keywords it mentions."
  ([wt base file] (file-map wt base file nil))
  ([wt base file sforms]
   (let [exists? (.exists (java.io.File. wt file))
         fs (if (and exists? (nil? sforms)) (try (forms wt file) (catch Exception _ [])) [])
         info (when exists? (ns-info (slurp (java.io.File. wt file))))
         base-src (let [x (sh/sh "git" "show" (str base ":" file) :dir wt)] (when (zero? (:exit x)) (:out x)))
         base-info (when base-src (ns-info base-src))]
     (for [h (hunks wt base file)]
       (let [st (when sforms (structural-touch sforms h))
             owners (if sforms
                      (mapcat (fn [{:keys [form]}] (keep identity [(read-owner info (get-in form [:new :src]))
                                                                  (read-owner base-info (get-in form [:old :src]))]))
                              st)
                      (keep :owner (filter #(overlaps? (:new h) [(:line %) (:end-line %)]) fs)))]
         (cond-> (assoc h :file file
                        :owners (vec (distinct owners))
                        :mentions (into (mentions info (:added h)) (mentions base-info (:removed h))))
           sforms (assoc :paths (vec (mapcat :paths st))
                         :cosmetic (every? #(= :comments (:op %)) (mapcat (comp :changes :form) st)))))))))

;;; Analysis

(defn analyse
  "Everything the notebook needs: hunks with owners and known mentions, both
  registries, the contract diff, and the data keys the diff moved."
  [wt base base-dump branch-dump]
  (let [base-reg (read-string (slurp base-dump)) br-reg (read-string (slurp branch-dump))
        data-keys (fn [reg] (set (mapcat (fn [[_ p]] (concat (:context p) (:response p) (:req p) (:opt p))) reg)))
        known (into (into (set (keys br-reg)) (keys base-reg)) (into (data-keys br-reg) (data-keys base-reg)))
        deps (set (map #(keyword (subs % 1)) (mapcat (fn [[_ p]] (:deps p)) br-reg)))
        components (set (filter #(= :atlas/structure-component (:type (br-reg %))) (keys br-reg)))
        smap (structural wt base)
        hs (vec (for [f (changed-files wt base) h (file-map wt base f (some-> smap (get f) :forms))]
                  (let [k (set (filter known (:mentions h)))]
                    (assoc h :known k
                           :entities (vec (filter #(or (br-reg %) (base-reg %)) (concat (map :id (filter #(= :entity (:kind %)) (:owners h))) k)))
                           :data-keys (vec (remove #(or (br-reg %) (deps %) (components %)) k))))))
        cdiff (contract-diff base-reg br-reg)
        moved (->> cdiff
                   (mapcat (fn [[id [st d]]]
                             (case st
                               :changed (concat (get-in d [:context :-]) (get-in d [:context :+]) (get-in d [:response :-]) (get-in d [:response :+]))
                               :new (concat (:context (br-reg id)) (:response (br-reg id)))
                               :removed (concat (:context (base-reg id)) (:response (base-reg id))))))
                   (remove #(or (br-reg %) (base-reg %) (deps %) (components %)))
                   distinct vec)]
    {:wt wt :base base :hunks hs :base-reg base-reg :br-reg br-reg :cdiff cdiff :moved moved}))

(defn- hunk-name [h] (str (subs (:file h) (inc (.lastIndexOf ^String (:file h) "/"))) ":" (first (:new h))))
(defn- link [wt h] (format "[[file:%s/%s::%d][%s]]" wt (:file h) (first (:new h)) (hunk-name h)))
(defn- owner-entities [h] (map :id (filter #(= :entity (:kind %)) (:owners h))))

(defn- provider
  "A coarse facet for the counts: the services/* aspects of the entities the hunk
  involves, else those of the entities its file registers, else the file's directory."
  [br-reg file-facets h]
  (let [svc (fn [ids] (set (filter #(= "services" (namespace %)) (mapcat #(:aspects (br-reg %)) ids))))
        fs (let [own (svc (:entities h))] (if (seq own) own (get file-facets (:file h) #{})))]
    (cond (> (count fs) 1) "shared" (seq fs) (name (first fs))
          :else (let [d (str/split (:file h) #"/")] (if (> (count d) 1) (nth d (- (count d) 2)) "top-level")))))

(defn- file-facets [br-reg hunks]
  (into {} (for [[f hs] (group-by :file hunks)]
             [f (set (filter #(= "services" (namespace %)) (mapcat #(:aspects (br-reg %)) (mapcat owner-entities hs))))])))

(defn evidence
  "For each data key the contract diff moved, evidence lines a point can be backed
  with: producers, consumers, hunk counts, defining hunks, call sites. One org
  heading per key, named `key/<name>`, ready for the index sidecar."
  [{:keys [wt hunks br-reg base-reg moved]}]
  (let [ff (file-facets br-reg hunks)]
   (str/join
   (for [k moved]
     (let [p (try (ide/producers-of k) (catch Throwable _ [])) c (try (ide/consumers-of k) (catch Throwable _ []))
           in? (fn [reg] (some (fn [[_ e]] (some #{k} (concat (:context e) (:response e)))) reg))
           hs (filter #(contains? (:known %) k) hunks)
           test? (fn [h] (re-find #"(^|/)test/" (:file h)))
           spec-def? (fn [h] (some #(= :spec (:kind %)) (:owners h)))
           defs (remove test? (filter (fn [h] (or (seq (owner-entities h)) (spec-def? h))) hs))
           calls (remove (fn [h] (or (seq (owner-entities h)) (spec-def? h) (test? h))) hs)
           fmt (fn [ids] (if (seq ids) (str/join ", " (map #(str "=" % "=") ids)) "nobody in the registry"))]
       (str "* key/" (name k) (cond (and (in? br-reg) (not (in? base-reg))) "  — new" (and (in? base-reg) (not (in? br-reg))) "  — removed" :else "") "\n"
            (if (and (in? base-reg) (not (in? br-reg)))
              (str "- Gone from every contract. ‹atlas› " (count hs) " hunks remove its last mentions (" (count (filter test? hs)) " in tests). ‹code›\n")
              (str "- Produced by " (fmt p) "; consumed by " (fmt c) ". ‹atlas›\n"
                   "- " (count hs) " hunks mention it: " (str/join ", " (for [[pr n] (sort-by key (frequencies (map #(provider br-reg ff %) hs)))] (str n " " pr)))
                   "; " (count (filter test? hs)) " in tests. ‹code›\n"))
            (when (seq defs) (str "- Defined or handled in: " (str/join " · " (map #(link wt %) (take 4 defs))) " ‹code›\n"))
            (when (seq calls) (str "- Supplied at " (count calls) " call sites: " (str/join " · " (map #(link wt %) calls)) " ‹code›\n"))))))))

(defn append-index!
  "Append the generated evidence (one `key/<name>` heading per moved data key) and
  the contract diff to the index sidecar at PATH. The points' own headings
  (\"2.2\") are written by the reviewer, who links or copies these lines."
  [{:keys [cdiff] :as r} path]
  (spit path (str "* contract-diff\n" (str/join "\n" (for [[id d] cdiff] (str "- =" id "= " (pr-str d)))) "\n" (evidence r)) :append true)
  path)

(defn write-extended!
  "The ledger: every hunk once per group it belongs to, under four groupings, plus the residue."
  [{:keys [wt hunks br-reg cdiff moved]} path]
  (let [row (fn [h] (str "  - " (link wt h) " " (name (:kind h))
                         (let [os (remove #(= :entity (:kind %)) (:owners h))] (when (seq os) (str " ~" (str/join ", " (map (comp str :id) os)) "~")))
                         (when (seq (:entities h)) (str " → " (str/join ", " (map str (distinct (:entities h))))))
                         (when (seq (:paths h)) (str " · " (str/join "; " (map #(str "=" % "=") (take 3 (:paths h))))))
                         (when (:cosmetic h) " · formatting or comments only")))
        by (fn [f] (->> hunks (mapcat (fn [h] (map (fn [g] [g h]) (f h)))) (group-by first)
                        (map (fn [[g xs]] [g (map second xs)])) (sort-by (comp - count second))))
        section (fn [title groups] (str "* " title "\n" (str/join (for [[g hs] groups] (str "** " g "  (" (count hs) ")\n" (str/join "\n" (map row hs)) "\n")))))
        facets (fn [h] (set (filter #(#{"services" "entity" "action"} (namespace %)) (mapcat #(:aspects (br-reg %)) (:entities h)))))]
    (spit path
          (str "#+TITLE: Extended data — every hunk, grouped\n#+STARTUP: overview\n"
               "~var~ = the non-registry form the hunk touches; → = registry entities it defines or mentions; =…= = where in the form it changed.\n\n"
               "* Contract diff\n" (str/join "\n" (for [[id d] cdiff] (str "- =" id "= " (pr-str d)))) "\n"
               (section "By data key mentioned" (by (fn [h] (or (seq (:data-keys h)) [:none]))))
               (section "By compound-id facet" (by (fn [h] (or (seq (facets h)) [:none]))))
               (section "By entity type" (by (fn [h] (or (seq (distinct (map #(:type (br-reg %)) (:entities h)))) [:none]))))
               "* Data flow of the moved keys\n"
               (str/join (for [k moved :let [p (try (ide/producers-of k) (catch Throwable _ [])) c (try (ide/consumers-of k) (catch Throwable _ []))]]
                           (str "** " k "\n   produced by: " (if (seq p) (str/join ", " (map str p)) "nobody") "\n   consumed by: " (if (seq c) (str/join ", " (map str c)) "nobody") "\n")))
               "* Residue: no entity, no data key\n" (str/join "\n" (map row (remove (fn [h] (or (seq (:entities h)) (seq (:data-keys h)))) hunks))) "\n"))
    path))

;;; Stale evidence: does the code a point links to still say what it said?

(def ^:private link-re #"\[\[(file|diff):([^\]]+?)(?:::(\d+))?\]")

(defn- index-points
  "The index sidecar as [{:id \"2.2\" :start :end :links [[kind target line] …]}], in order."
  [index-text]
  (let [lines (vec (str/split-lines index-text))
        heads (keep-indexed (fn [i l] (when-let [[_ id] (re-matches #"\* ([0-9][0-9.]*|key/.*|contract-diff)\s*" l)] [i id])) lines)]
    (for [[[i id] [j _]] (partition 2 1 [[(count lines) nil]] heads)]
      {:id id :start i :end j
       :links (vec (for [l (subvec lines (inc i) j) [_ kind target line] (re-seq link-re l)]
                     [kind target (some-> line Long/parseLong)]))})))

(defn- location-text
  "The line a link points at, in the worktree; nil when the file or line is gone."
  [wt [kind target line]]
  (let [f (java.io.File. (if (= kind "file") target (str wt "/" target)))]
    (when (and line (.exists f))
      (nth (str/split-lines (slurp f)) (dec line) nil))))

(defn- fingerprint [wt links]
  (when (seq links)
    (str (hash (mapv #(some-> (location-text wt %) str/trim) links)))))

(defn stamp-index!
  "Record, under every point of the index at PATH, a fingerprint of the lines its
  links point at (an org property :STAMP:). Idempotent: an existing stamp is replaced."
  [wt path]
  (let [text (slurp path)
        lines (vec (str/split-lines text))
        pts (index-points text)
        out (loop [ls lines pts (reverse pts)]
              (if-let [{:keys [start links]} (first pts)]
                (let [fp (fingerprint wt links)
                      body-start (inc start)
                      has-drawer? (= ":PROPERTIES:" (str/trim (get ls body-start "")))
                      ls (if has-drawer?
                           (let [end (loop [k body-start] (if (or (>= k (count ls)) (= ":END:" (str/trim (ls k)))) k (recur (inc k))))]
                             (vec (concat (subvec ls 0 body-start) (subvec ls (min (count ls) (inc end))))))
                           ls)]
                  (recur (if fp (vec (concat (subvec ls 0 body-start) [":PROPERTIES:" (str ":STAMP: " fp) ":END:"] (subvec ls body-start))) ls)
                         (rest pts)))
                ls))]
    (spit path (str (str/join "\n" out) "\n"))
    (count (filter #(fingerprint wt (:links %)) pts))))

(defn stale-points
  "Points of the index at PATH whose linked lines no longer match their :STAMP:,
  with the links whose text is gone. Empty when nothing moved."
  [wt path]
  (let [text (slurp path) lines (vec (str/split-lines text))]
    (for [{:keys [id start end links]} (index-points text)
          :let [stamp (some #(second (re-matches #"\s*:STAMP:\s*(\S+)" %)) (subvec lines (inc start) end))
                now (fingerprint wt links)]
          :when (and stamp (not= stamp now))]
      {:id id :gone (vec (remove #(location-text wt %) links))})))
