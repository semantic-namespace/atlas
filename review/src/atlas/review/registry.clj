(ns atlas.review.registry
  (:require [atlas.registry :as registry]
            [atlas.store.canonical :as canon]
            [clojure.edn :as edn]
            [clojure.java.shell :refer [sh]]
            [clojure.string :as str]))

(defonce config (atom {:store nil :prefix nil :branch "origin/main" :fetch-ttl-ms 60000}))

(defn configure! [store prefix branch]
  (swap! config assoc :store store
         :prefix (if (str/blank? prefix) "" (str (str/replace prefix #"/$" "") "/"))
         :branch (or branch "origin/main")))

(defonce ^:private cache (atom {}))

(defn- git [& args]
  (let [{:keys [exit out err]} (apply sh "git" "-C" (:store @config) args)]
    (if (zero? exit) out (throw (ex-info (str "registry store: " (str/trim err)) {:args args})))))

(defn- normalise [reg] (into {} (map (fn [[k e]] [(if (vector? k) (set k) k) e])) reg))

(defn- parse-version [line]
  (let [[commit date subject trailer] (str/split line #"\u001f" -1)
        [_ source env from] (re-matches #"registry: (\S+)(?: deployed to (\S+) from (\S+))?.*" (or subject ""))
        title (when-not env (some-> (re-matches #"registry: \S+ (.+)" (or subject "")) second str/trim not-empty))]
    {:commit commit :date date :subject subject :source source :env env :from from :title title
     :source-sha (not-empty (str/trim (or trailer "")))}))

(defn versions
  "Recorded versions on `branch` (the configured one by default), newest first:
  `{:commit :date :subject :source :source-sha :env :from :title}`. Source is
  the project sha from the subject, source-sha the `Source-Sha` trailer, env
  and from the environment and git branch of a `deployed to <env> from
  <branch>` subject, title the subject of the project commit a line version
  records, when the store's subject carries it. A version is a commit that
  changes the registry under the prefix, or an empty `registry: ` commit: the
  store records a merge or a deploy that leaves the registry as it was
  without changing a file."
  ([] (versions (:branch @config)))
  ([branch]
   (let [prefix (:prefix @config)]
     (for [chunk (str/split (git "log" "--name-only" "--format=%x1e%H%x1f%cI%x1f%s%x1f%(trailers:key=Source-Sha,valueonly,separator=%x20)" branch) #"\u001e")
           :let [[head & files] (remove str/blank? (str/split-lines chunk))]
           :when head
           :let [v (parse-version (str/trim head))]
           :when (if (seq files)
                   (some #(str/starts-with? % prefix) files)
                   (str/starts-with? (or (:subject v) "") "registry: "))]
       v))))

(defn- built-from? [sha {:keys [source source-sha]}]
  (boolean (or (and source-sha (= sha source-sha)) (and source (str/starts-with? sha source)))))

(defn version-for
  "The version on `branch` built from `sha`, or nil."
  ([sha] (version-for (:branch @config) sha))
  ([branch sha] (when sha (some #(when (built-from? sha %) %) (versions branch)))))

(defn latest
  ([] (latest (:branch @config)))
  ([branch] (first (versions branch))))

(defn deployed
  "The newest deploy record on an environment branch: a version whose subject
  says `deployed to <env>`, or nil. Versions without it, such as the line
  commits a new branch is seeded with, are not deploys."
  [branch]
  (some #(when (:env %) %) (versions branch)))

(defn oldest [branch] (last (versions branch)))

(defn- instant [t] (.toInstant (java.time.OffsetDateTime/parse t)))

(defn recorded-by
  "The newest version on `branch` the store recorded at or before `t`, an
  ISO-8601 time, or nil."
  [branch t]
  (let [t (instant t)]
    (some #(when-not (.isAfter (instant (:date %)) t) %) (versions branch))))

(defn since
  "The versions on `branch` recorded after `v`, newest first."
  [branch v]
  (take-while #(not= (:commit v) (:commit %)) (versions branch)))

(defn- newest-within
  "The newest version on `branch` built from `sha` or an ancestor of it, asking
  `ancestor?` about each version newest first; `:none` when no version is that
  old, nil as soon as `ancestor?` cannot tell."
  [branch sha ancestor?]
  (reduce (fn [none v]
            (let [built (or (:source-sha v) (:source v))]
              (case (when built (ancestor? built sha)) true (reduced v) false none (reduced nil))))
          :none (versions branch)))

(defn base-for
  "The version on `branch` to compare a registry built on project commit `sha`
  with, as `{:version v :how how}`. How is `:exact` when the store recorded
  `sha` itself; `:ancestor` for the newest version built from an ancestor of
  it, by `ancestor?` (true, false, or nil when it cannot tell); `:committed`
  for the newest recorded by `sha`'s commit time, from `committed-at`, when
  ancestry is unknown; `:oldest` when the store has no version that old;
  `:latest` when nothing places `sha`."
  [branch sha {:keys [ancestor? committed-at]}]
  (if-let [v (version-for branch sha)]
    {:version v :how :exact}
    (let [found (when sha (newest-within branch sha ancestor?))
          t (when (and sha (nil? found)) (committed-at sha))]
      (cond
        (map? found) {:version found :how :ancestor}
        (= :none found) {:version (oldest branch) :how :oldest}
        t (if-let [v (recorded-by branch t)] {:version v :how :committed} {:version (oldest branch) :how :oldest})
        :else {:version (latest branch) :how :latest}))))

(defn- position [branch sha]
  (first (keep-indexed (fn [i v] (when (built-from? sha v) i)) (versions branch))))

(defn relation
  "How version `to` of `line` stands to version `from` of another branch:
  `:descends` when `from` was built from `to`'s commit or an ancestor of it,
  `:behind` when `to`'s commit is an ancestor of `from`'s, `:diverged` when
  neither, nil when that cannot be told. Versions both recorded on `line` are
  ordered by the store; others are asked of `ancestor?`."
  [line from to ancestor?]
  (let [a (or (:source-sha from) (:source from))
        b (or (:source-sha to) (:source to))]
    (when (and a b)
      (if (or (str/starts-with? a b) (str/starts-with? b a))
        :descends
        (let [i (position line a) j (position line b)]
          (if (and i j)
            (if (>= i j) :descends :behind)
            (case (ancestor? a b)
              true :descends
              false (case (ancestor? b a) true :behind false :diverged nil)
              nil)))))))

(defn base-recorded-by
  "The version on `branch` to compare a registry built at time `t` with, when
  the commit it was built on is unknown: the newest recorded by then."
  [branch t]
  (if-let [v (recorded-by branch t)]
    {:version v :how :run-time}
    {:version (oldest branch) :how :oldest}))

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

(defn shown
  "Store branch `ref` as a reader names it, without the remote it is tracked
  from: `origin/main` is shown as `main`."
  [ref]
  (let [[remote branch] (str/split (str ref) #"/" 2)
        remotes (try (set (str/split-lines (git "remote"))) (catch Exception _ #{}))]
    (if (and branch (remotes remote)) branch ref)))

(defn named
  "The store branch `asked` names, or nil, without fetching; see `resolve-against`."
  [asked]
  (when (and (string? asked) (seq asked))
    (allowed (branches) asked)))

(defn resolve-against
  "The store branch `asked` names, or nil when the store has no such branch.
  `asked` is matched against the store's own branch names and never reaches
  git otherwise; `deployed/dev` finds `origin/deployed/dev` when only the
  remote has it. Fetches the store first, at most once per TTL per branch."
  [asked]
  (when (and (string? asked) (seq asked))
    (refresh! (or (named asked) ::unknown))
    (named asked)))

(defn label [{:keys [commit source]}] (str (subs commit 0 7) (when source (str " (" source ")"))))

(defn registry-at
  "The registry recorded at a store commit, keys normalised to sets."
  [commit]
  (or (@cache commit)
      (let [prefix (:prefix @config)
            paths (filter #(re-find #"(?:^|/)entities/[^/]+\.edn$" %)
                         (str/split-lines (apply git "ls-tree" "-r" "--name-only" commit (when (seq prefix) ["--" prefix]))))
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
