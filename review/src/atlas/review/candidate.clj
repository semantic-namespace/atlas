(ns atlas.review.candidate
  (:require [atlas.review.registry :as reg]
            [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]]
            [clojure.string :as str]))

(def ^:dynamic *workflow* (or (System/getenv "SDIFF_REGISTRY_WORKFLOW") "atlas-registry"))
(def ^:dynamic *artifact* (or (System/getenv "SDIFF_REGISTRY_ARTIFACT") "registry"))
(def ^:dynamic *dir* (io/file (System/getProperty "java.io.tmpdir") "sdiff-candidates"))

(def meta-file
  "The artifact's optional sidecar naming the commits CI built from:
  `{:base-sha <full sha> :head-sha <full sha>}`, the base being the first
  parent of the merge commit it built."
  "atlas-registry.meta.edn")

(defn- gh [& args]
  (let [{:keys [exit out err]} (apply sh "gh" args)]
    (if (zero? exit) out (throw (ex-info (str/trim (str err out)) {:args args})))))

(defn ci-run [repo head]
  (first (json/parse-string (gh "run" "list" "--repo" repo "--workflow" *workflow* "--commit" head
                                "--limit" "1" "--json" "databaseId,status,conclusion,url,createdAt")
                            true)))

(defn- built-on [f]
  (when (.exists f)
    (let [m (try (edn/read-string (slurp f)) (catch Exception _ nil))]
      (when (and (map? m) (string? (:base-sha m)) (re-matches #"[0-9a-f]{7,40}" (:base-sha m)))
        (select-keys m [:base-sha :head-sha])))))

(defn fetch
  "The registry CI built for the PR head, as `{:registry m :label l :run-at t
  :built-on {:base-sha :head-sha}}` (`:built-on` only when the artifact has
  the sidecar), or `{:reason why-not}`."
  [repo num head]
  (let [run (try (ci-run repo head) (catch Exception e {:error (ex-message e)}))
        short (subs head 0 9)]
    (cond
      (:error run) {:reason (str "gh: " (:error run))}
      (nil? run) {:reason (str "CI has no " *workflow* " run for " short " (the workflow skips PRs that touch no source)")}
      (not= "completed" (:status run)) {:reason (str "CI is still building the registry for " short " (" (:url run) ")")}
      (not= "success" (:conclusion run)) {:reason (str "the " *workflow* " run for " short " " (:conclusion run) " (" (:url run) ")")}
      :else
      (let [dir (io/file *dir* (str num "-" (subs head 0 7)))
            f (io/file dir "atlas-registry.edn")]
        (when-not (.exists f)
          (.mkdirs dir)
          (gh "run" "download" (str (:databaseId run)) "--repo" repo "-n" *artifact* "-D" (str dir)))
        (if (.exists f)
          (let [on (built-on (io/file dir meta-file))]
            (cond-> {:registry (reg/from-file f) :label (str "pr" num "-" (subs head 0 7) " (run " (:databaseId run) ")")
                     :run-at (:createdAt run)}
              on (assoc :built-on on)))
          {:reason (str "the run's artifact has no atlas-registry.edn (" (:url run) ")")})))))

(defonce ^:private known (atom {}))

(defn- remembered [k f]
  (if (contains? @known k)
    (@known k)
    (let [v (f)]
      (when (some? v) (swap! known assoc k v))
      v)))

(defn ancestor?
  "Whether project commit `a` is `b` or one of its ancestors, from GitHub's
  compare; nil when GitHub cannot tell."
  [repo a b]
  (remembered [repo :ancestor a b]
              #(try (case (str/trim (gh "api" (str "repos/" repo "/compare/" a "..." b) "--jq" ".status"))
                      ("identical" "ahead") true
                      ("behind" "diverged") false
                      nil)
                    (catch Exception _ nil))))

(defn committed-at
  "The committer time of project commit `sha`, or nil."
  [repo sha]
  (remembered [repo :committed sha]
              #(try (not-empty (str/trim (gh "api" (str "repos/" repo "/commits/" sha) "--jq" ".commit.committer.date")))
                    (catch Exception _ nil))))

(defn pulls-with
  "The pull requests of `repo` whose commits include project commit `sha`, as
  `{:number :state :merged? :merge-sha :title}`, merged ones first; nil when
  GitHub cannot tell."
  [repo sha]
  (remembered [repo :pulls sha]
              #(try (->> (json/parse-string (gh "api" (str "repos/" repo "/commits/" sha "/pulls")) true)
                         (map (fn [p] {:number (:number p) :state (:state p) :merged? (some? (:merged_at p))
                                       :merge-sha (when (:merged_at p) (:merge_commit_sha p)) :title (:title p)}))
                         (sort-by (comp not :merged?))
                         vec)
                    (catch Exception _ nil))))

(defn ancestry
  "What `reg/base-for` asks about the commits of `repo`."
  [repo]
  {:ancestor? (fn [a b] (when repo (ancestor? repo a b)))
   :committed-at (fn [sha] (when repo (committed-at repo sha)))})
