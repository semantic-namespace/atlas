(ns atlas.review.cli
  "Registry questions answered from a git store checkout: version diffs and
  timelines from `atlas.history.facts`, closures, dataflow and gaps from
  `atlas.datalog.graph`. Runs under babashka; see the `registry` task in the
  root bb.edn."
  (:require [atlas.datalog.graph :as graph]
            [atlas.history.facts :as hf]
            [atlas.review.registry :as store]
            [clojure.java.shell :refer [sh]]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [clojure.walk :as walk]))

(def usage
  "usage: <command> <store-dir> [args] [--prefix p] [--json]
  diff <old-ref> <new-ref>
  timeline <from-ref> <to-ref> [dev-id]
  blast-radius <ref> <dev-id>... [--hops n]
  trace-causes <ref> <dev-id>... [--hops n]
  producers-of <ref> <data-key>
  consumers-of <ref> <data-key>
  dependents <ref> <dev-id>
  dependencies <ref> <dev-id>
  gaps <ref> [--min-similarity x] [--max n]")

(defn- fail [msg & [data]] (throw (ex-info msg (or data {}))))

(defn- misuse [msg] (throw (ex-info msg {::usage? true})))

(defn- kw [s]
  (if (keyword? s) s (keyword (cond-> s (str/starts-with? s ":") (subs 1)))))

(def ^:private flags #{"--prefix" "--hops" "--min-similarity" "--max"})

(defn parse-args [args]
  (loop [[a & more :as args] args, pos [], opts {}]
    (cond
      (empty? args) (assoc opts :args pos)
      (= "--json" a) (recur more pos (assoc opts :json true))
      (flags a) (if (seq more)
                  (recur (rest more) pos (assoc opts (keyword (subs a 2)) (first more)))
                  (misuse (str a " needs a value")))
      (str/starts-with? a "--") (misuse (str "unknown option " a))
      :else (recur more (conj pos a) opts))))

(defn- git [dir & args]
  (let [{:keys [exit out err]} (apply sh "git" "-C" dir args)]
    (if (zero? exit) (str/trim out) (fail (str "store " dir ": " (str/trim err))))))

(defn- commit-of [dir ref]
  (let [{:keys [exit out]} (sh "git" "-C" dir "rev-parse" "--verify" "--quiet" (str ref "^{commit}"))]
    (if (zero? exit) (str/trim out) (fail (str "store " dir ": no commit named " ref)))))

(defn- registry-at [commit]
  (try
    (store/registry-at commit)
    (catch Exception e
      (when (:args (ex-data e)) (fail (ex-message e)))
      (fail (str "commit " (subs commit 0 7) " holds a registry that cannot be read as EDN"
                 " (" (ex-message e) "); it was probably recorded before the store"
                 " sanitised unreadable values such as #object[...]")
            {:commit commit}))))

(defn- label [i commit] (format "%04d-%s" i (subs commit 0 7)))

(defn history-db
  "History facts for `commits` in order, labelled so that sorting the labels
  keeps commit order. With `skip-unreadable?` a commit whose registry cannot be
  read is left out and listed under :unreadable instead of failing."
  ([commits] (:db (history-db commits false)))
  ([commits skip-unreadable?]
   (dorun (pmap #(try (store/registry-at %) (catch Exception _)) commits))
   (dissoc (reduce (fn [{:keys [db prev] :as acc} [i commit]]
                     (if-let [reg (try (registry-at commit)
                                       (catch clojure.lang.ExceptionInfo e
                                         (when-not (and skip-unreadable? (:commit (ex-data e))) (throw e))))]
                       (-> acc
                           (assoc :db (hf/snapshot db reg (label i commit) prev nil) :prev reg)
                           (update :versions conj (label i commit)))
                       (update acc :unreadable conj commit)))
                   {:db hf/empty-db :versions [] :unreadable []}
                   (map-indexed vector commits))
           :prev)))

(defn renames
  "Entities deleted in `v-new` whose type and aspects reappear, unchanged,
  under a new dev-id; as `atlas.history/detect-renames`."
  [db v-old v-new]
  (let [aspects-at (reduce (fn [m [_ s]] (update m (:snap/dev-id s) (fnil into #{}) (:snap/aspect s)))
                           {} (get-in db [:snaps v-old]))
        fresh (filter #(and (:snap/added %) (not (:snap/removed %))) (hf/version-diff db v-new))]
    (vec (for [{:snap/keys [dev-id type]} (hf/version-deleted db v-new)
               :let [aspects (aspects-at dev-id #{})]
               :when (seq aspects)
               {to :snap/dev-id t :snap/type added :snap/added} fresh
               :when (and (= type t) (= aspects (set added)))]
           {:from dev-id :to to :type type :aspects aspects}))))

(defn version-report [db v-old v-new]
  {:changed (vec (hf/version-diff db v-new))
   :deleted (vec (hf/version-deleted db v-new))
   :renames (renames db v-old v-new)
   :summary (vec (hf/version-summary db v-new))
   :vocabulary (hf/vocabulary-diff db v-old v-new)
   :properties (hf/property-diff db v-old v-new)})

(defn- ref-info [dir ref commit]
  {:ref ref :commit commit :subject (git dir "log" "-1" "--format=%s" commit)})

(defn diff [dir old new]
  (let [a (commit-of dir old) b (commit-of dir new)
        db (history-db [a b])]
    (merge {:from (ref-info dir old a) :to (ref-info dir new b)}
           (version-report db (label 0 a) (label 1 b)))))

(defn range-commits
  "`from` and then every store version after it up to `to`, oldest first."
  [dir from to]
  (let [a (commit-of dir from) b (commit-of dir to)
        between (set (str/split-lines (git dir "rev-list" (str a ".." b))))]
    (into [a] (comp (map :commit) (filter between)) (reverse (store/versions b)))))

(defn timeline
  ([dir from to] (timeline dir from to nil))
  ([dir from to dev-id]
   (let [commits (range-commits dir from to)
         {:keys [db versions unreadable]} (history-db commits true)]
     {:from (ref-info dir from (first commits))
      :to (ref-info dir to (peek commits))
      :versions versions
      :unreadable unreadable
      :timeline (if dev-id
                  (vec (hf/full-timeline db (kw dev-id)))
                  (vec (for [[v-old v-new] (partition 2 1 versions)
                             :let [r (version-report db v-old v-new)]]
                         {:version v-new
                          :summary (:summary r)
                          :changed (count (:changed r))
                          :deleted (mapv :snap/dev-id (:deleted r))
                          :renames (mapv #(select-keys % [:from :to]) (:renames r))})))})))

(defn graph-at [dir ref]
  (graph/from-registry (registry-at (commit-of dir ref))))

(defn- by-hops [closure] (vec (sort-by (juxt :hops (comp str :entity)) closure)))

(defn- hops [opts] (some-> (:hops opts) parse-long))

(defn- sorted [xs] (vec (sort-by str xs)))

(defn- ids [xs] (vec (sort xs)))

(defn run [cmd dir args opts]
  (case cmd
    "diff" (let [[a b] args] (diff dir a b))
    "timeline" (let [[a b id] args] (timeline dir a b id))
    "blast-radius" (by-hops (graph/downstream-closure (graph-at dir (first args)) (set (map kw (rest args))) (hops opts)))
    "trace-causes" (by-hops (graph/upstream-closure (graph-at dir (first args)) (set (map kw (rest args))) (hops opts)))
    "producers-of" (ids (graph/producers-of (graph-at dir (first args)) (kw (second args))))
    "consumers-of" (ids (graph/consumers-of (graph-at dir (first args)) (kw (second args))))
    "dependents" (ids (graph/effective-dependents (graph-at dir (first args)) (kw (second args))))
    "dependencies" (ids (graph/effective-dependencies (graph-at dir (first args)) (kw (second args))))
    "gaps" (graph/structural-gaps (graph-at dir (first args))
                                  (or (some-> (:min-similarity opts) parse-double) 0.5)
                                  (or (some-> (:max opts) parse-long) 20))
    (misuse (str "unknown command " cmd))))

(def ^:private arity
  {"diff" 2 "timeline" 2 "blast-radius" 2 "trace-causes" 2 "producers-of" 2
   "consumers-of" 2 "dependents" 2 "dependencies" 2 "gaps" 1})

(defn- jsonable [x]
  (walk/postwalk #(cond (keyword? %) (subs (str %) 1)
                        (set? %) (sorted %)
                        :else %)
                 x))

(defn- emit [x {:keys [json]}]
  (if json
    (println ((requiring-resolve 'clojure.data.json/write-str) (jsonable x)))
    (binding [pp/*print-right-margin* 120] (pp/pprint x))))

(defn main [& argv]
  (let [{[cmd dir & args] :args :as opts} (parse-args argv)]
    (when-not (and cmd dir (arity cmd) (>= (count args) (arity cmd)))
      (misuse (if (and cmd (not (arity cmd))) (str "unknown command " cmd) "missing arguments")))
    (when-not (.isDirectory (java.io.File. ^String dir))
      (fail (str "no store checkout at " dir)))
    (store/configure! dir (or (:prefix opts) "") nil)
    (emit (run cmd dir args opts) opts)))

(defn -main [& argv]
  (try
    (apply main argv)
    (shutdown-agents)
    (catch clojure.lang.ExceptionInfo e
      (binding [*out* *err*]
        (println "error:" (ex-message e))
        (when (::usage? (ex-data e)) (println usage)))
      (System/exit 1))))
