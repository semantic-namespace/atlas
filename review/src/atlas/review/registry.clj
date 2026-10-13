(ns atlas.review.registry
  (:require [atlas.registry :as registry]
            [atlas.store.canonical :as canon]
            [clojure.edn :as edn]
            [clojure.java.shell :refer [sh]]
            [clojure.string :as str]))

(defonce config (atom {:store nil :prefix nil :branch "origin/main" :fetch-ttl-ms 60000}))

(defn configure! [store prefix branch]
  (swap! config assoc :store store :prefix (str (str/replace prefix #"/$" "") "/") :branch (or branch "origin/main")))

(defonce ^:private cache (atom {}))

(defn- git [& args]
  (let [{:keys [exit out err]} (apply sh "git" "-C" (:store @config) args)]
    (if (zero? exit) out (throw (ex-info (str "registry store: " (str/trim err)) {:args args})))))

(defn- normalise [reg] (into {} (map (fn [[k e]] [(if (vector? k) (set k) k) e])) reg))

(defn- parse-version [line]
  (let [[commit date subject trailer] (str/split line #"\u001f" -1)
        [_ source env from] (re-matches #"registry: (\S+)(?: deployed to (\S+) from (\S+))?.*" (or subject ""))]
    {:commit commit :date date :subject subject :source source :env env :from from
     :source-sha (not-empty (str/trim (or trailer "")))}))

(defn versions
  "Recorded versions on `branch` (the configured one by default), newest first:
  `{:commit :date :subject :source :source-sha :env :from}`. Source is the
  project sha from the subject, source-sha the `Source-Sha` trailer, env and
  from the environment and git branch of a `deployed to <env> from <branch>` subject."
  ([] (versions (:branch @config)))
  ([branch]
   (for [l (str/split (git "log" "--format=%H%x1f%cI%x1f%s%x1f%(trailers:key=Source-Sha,valueonly,separator=%x20)%x1e" branch "--" (:prefix @config)) #"\u001e")
         :let [l (str/trim l)] :when (seq l)]
     (parse-version l))))

(defn- built-from? [sha {:keys [source source-sha]}]
  (boolean (or (and source-sha (= sha source-sha)) (and source (str/starts-with? sha source)))))

(defn version-for
  "The version on `branch` built from `sha`, or nil."
  ([sha] (version-for (:branch @config) sha))
  ([branch sha] (when sha (some #(when (built-from? sha %) %) (versions branch)))))

(defn latest
  ([] (latest (:branch @config)))
  ([branch] (first (versions branch))))

(defonce ^:private fetched (atom {}))

(defn refresh!
  "Fetches the store, at most once per `:fetch-ttl-ms` for `k`; true when it
  did. A failed fetch leaves the checkout as it was."
  [k]
  (let [now (System/currentTimeMillis)
        k [(:store @config) k]
        due? (fn [m] (<= (+ (get m k 0) (:fetch-ttl-ms @config)) now))
        [before] (swap-vals! fetched #(if (due? %) (assoc % k now) %))]
    (when (due? before)
      (try (git "fetch" "--quiet") true (catch Exception _ false)))))

(defn branches
  "Every local and remote-tracking branch of the store."
  []
  (set (remove #(or (str/blank? %) (str/ends-with? % "/HEAD"))
               (str/split-lines (git "for-each-ref" "--format=%(refname:short)" "refs/heads" "refs/remotes")))))

(defn- allowed
  "The branch among `names` that `asked` names, the configured branch first,
  so `main` means `origin/main` when that is the one configured."
  [names asked]
  (let [spellings (cons asked (map #(str % "/" asked) (remove str/blank? (str/split-lines (git "remote")))))]
    (or (some #{(:branch @config)} spellings) (some names spellings))))

(defn resolve-against
  "The store branch `asked` names, or nil when the store has no such branch.
  `asked` is matched against the store's own branch names and never reaches
  git otherwise; `deployed/dev` finds `origin/deployed/dev` when only the
  remote has it. Fetches the store first, at most once per TTL per branch."
  [asked]
  (when (and (string? asked) (seq asked))
    (refresh! (or (allowed (branches) asked) ::unknown))
    (allowed (branches) asked)))

(defn label [{:keys [commit source]}] (str (subs commit 0 7) (when source (str " (" source ")"))))

(defn registry-at
  "The registry recorded at a store commit, keys normalised to sets."
  [commit]
  (or (@cache commit)
      (let [prefix (:prefix @config)
            paths (filter #(re-find #"/entities/[^/]+\.edn$" %) (str/split-lines (git "ls-tree" "-r" "--name-only" commit "--" prefix)))
            files (into {} (for [p paths] [(subs p (count prefix)) (git "show" (str commit ":" p))]))
            reg (normalise (canon/files->registry files))]
        (swap! cache assoc commit reg)
        reg)))

(defn from-file
  "A registry map from an EDN file, keys normalised to sets."
  [f]
  (normalise (edn/read-string {:default (fn [_ v] v)} (slurp f))))

(defmacro with-version [reg & body]
  `(binding [registry/*registry-override* ~reg] ~@body))
