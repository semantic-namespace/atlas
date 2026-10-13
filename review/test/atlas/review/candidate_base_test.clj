(ns atlas.review.candidate-base-test
  (:require [atlas.review.candidate :as candidate]
            [atlas.review.decorate :as decorate]
            [atlas.review.registry :as reg]
            [atlas.store.canonical :as canon]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]]
            [clojure.string :as str]
            [clojure.test :refer [deftest is use-fixtures]]
            [sdiff.decorate :as d]))

(defn- ent [id] [#{:atlas/execution-function (keyword "domain" (name id))}
                 {:atlas/dev-id id :atlas/type :atlas/execution-function :execution-function/context [:k/in]}])

(defn- registry [& ids] (into {} (map ent ids)))

(def before-merge (registry :fn/x :fn/gone))
(def after-merge (registry :fn/x :fn/y))
(def pr-head (registry :fn/x :fn/gone :fn/z))

(defn- git! [dir date & args]
  (let [env (cond-> (into {} (System/getenv))
              date (assoc "GIT_AUTHOR_DATE" date "GIT_COMMITTER_DATE" date))
        {:keys [exit out err]} (apply sh "git" "-c" "user.name=test" "-c" "user.email=test@example.com" "-c" "commit.gpgsign=false"
                                      (concat args [:dir (str dir) :env env]))]
    (if (zero? exit) (str/trim out) (throw (ex-info err {:args args})))))

(defn- commit! [dir date msg]
  (git! dir date "commit" "-q" "--allow-empty" "-m" msg)
  (git! dir nil "rev-parse" "HEAD"))

(defn- record! [dir date reg src & [subject]]
  (let [root (io/file dir "registries" "app")]
    (doseq [f (reverse (file-seq root))] (io/delete-file f true))
    (doseq [[p content] (canon/registry->files reg)]
      (let [f (io/file root p)] (io/make-parents f) (spit f content))))
  (git! dir nil "add" "-A")
  (git! dir date "commit" "-q" "-m" (or subject (str "registry: " (subs src 0 7))) "-m" (str "Source-Sha: " src)))

(def ^:dynamic *project* nil)
(def ^:dynamic *store* nil)
(def ^:dynamic *shas* nil)

(defn- tmp-dir [prefix] (.toFile (java.nio.file.Files/createTempDirectory prefix (make-array java.nio.file.attribute.FileAttribute 0))))

(defn- fixture
  "A project whose main gets p0..p3, a pull request branched at p2, and a
  store that recorded the registry at p1 (08:00) and at p3 (12:27): p2 left
  the registry as p1 did, p3 merged a change to it after CI built the PR."
  [t]
  (let [tmp (tmp-dir "candidate-base")
        project (io/file tmp "project") store (io/file tmp "store")]
    (.mkdirs project) (.mkdirs store)
    (git! project nil "init" "-q") (git! project nil "checkout" "-q" "-b" "main")
    (let [p0 (commit! project "2026-10-13T07:00:00Z" "p0")
          p1 (commit! project "2026-10-13T07:30:00Z" "p1")
          p2 (commit! project "2026-10-13T08:45:00Z" "p2")
          _ (git! project nil "checkout" "-q" "-b" "pr")
          head (commit! project "2026-10-13T08:50:00Z" "pr head")
          _ (git! project nil "checkout" "-q" "main")
          p3 (commit! project "2026-10-13T12:00:00Z" "p3")]
      (git! store nil "init" "-q") (git! store nil "checkout" "-q" "-b" "main")
      (record! store "2026-10-13T08:00:00Z" before-merge p1)
      (record! store "2026-10-13T12:27:00Z" after-merge p3)
      (let [saved @reg/config]
        (reg/configure! (str store) "registries/app" "main")
        (try (binding [*project* project
                       *store* store
                       *shas* {:p0 p0 :p1 p1 :p2 p2 :p3 p3 :head head}
                       candidate/*dir* (io/file tmp "candidates")]
               (t))
             (finally (reset! reg/config saved)
                      (doseq [f (reverse (file-seq tmp))] (io/delete-file f true))))))))

(use-fixtures :each fixture)

(defn- s7 [k] (subs (*shas* k) 0 7))

(defn- fake-gh
  "Answers the `gh` calls the review makes from the fixture: one successful
  run started at `created-at` whose artifact holds `pr-head` and, with
  `meta`, the sidecar; compare and commit lookups from the project's git."
  [{:keys [created-at meta compare-fails?]}]
  (fn [& [a b :as args]]
    (cond
      (= ["run" "list"] [a b])
      (json/generate-string [{:databaseId 42 :status "completed" :conclusion "success" :url "https://ci/42" :createdAt created-at}])

      (= ["run" "download"] [a b])
      (let [dir (io/file (last args))]
        (.mkdirs dir)
        (spit (io/file dir "atlas-registry.edn") (pr-str pr-head))
        (when meta (spit (io/file dir candidate/meta-file) (pr-str meta)))
        "")

      (and (= "api" a) (str/includes? b "/compare/"))
      (if compare-fails?
        (throw (ex-info "HTTP 502" {}))
        (let [[_ x y] (re-find #"/compare/(\w+)\.\.\.(\w+)" b)
              ancestor? (zero? (:exit (sh "git" "merge-base" "--is-ancestor" x y :dir (str *project*))))]
          (cond (= x y) "identical\n" ancestor? "ahead\n" :else "diverged\n")))

      (and (= "api" a) (str/includes? b "/commits/"))
      (git! *project* nil "show" "-s" "--format=%cI" (last (str/split b #"/")))

      :else (throw (ex-info (str "unexpected gh " (str/join " " args)) {})))))

(defn- page
  "The context and header text of the PR's page, with `gh` answered by `fake-gh`."
  [{:keys [against] :as opts}]
  (let [head (*shas* :head)
        report {:base (*shas* :p2) :head head :pr {:repo "owner/repo" :num 9 :head head} :clj []}]
    (reset! @#'candidate/known {})
    (decorate/forget-attempt! 9 head)
    (with-redefs [candidate/gh (fake-gh opts)]
      (binding [d/*params* (if against {"against" against} {})]
        (let [ctx (decorate/context report)]
          [ctx (str/join "\n" (map d/hiccup->text (decorate/registry-header ctx report)))])))))

(defn- version-of [k] (reg/version-for "main" (*shas* k)))

(deftest a-candidate-built-before-a-later-merge-is-compared-with-the-version-it-was-built-on
  (let [[ctx h] (page {:created-at "2026-10-13T09:01:00Z" :meta {:base-sha (*shas* :p2) :head-sha (*shas* :head)}})]
    (is (= before-merge (:base ctx)) "not the newest version, which has the later merge")
    (is (str/includes? h (str "registry unchanged from " (s7 :p1) " to the PR's base " (s7 :p2))))
    (is (str/includes? h "Registry: 1 new, 0 changed, 0 deleted"))
    (is (not (str/includes? h ":fn/gone")))
    (is (= [(version-of :p3)] (:stale ctx)))
    (is (str/includes? h (str "stale candidate Built against " (s7 :p2) "; 1 registry change merged since (" (s7 :p3) ")")))
    (is (str/includes? h "Re-run CI to see this PR against current main."))
    (is (not (str/includes? h "newest recorded")))))

(deftest without-the-sidecar-the-base-is-inferred-from-when-the-run-started
  (let [[ctx h] (page {:created-at "2026-10-13T09:01:00Z"})]
    (is (= before-merge (:base ctx)))
    (is (str/includes? h (str "registry unchanged from " (s7 :p1) " to the PR's base (inferred from the CI run's start 2026-10-13T09:01:00Z)")))
    (is (str/includes? h "Built against main as of 2026-10-13T09:01:00Z; 1 registry change merged since"))))

(deftest a-run-after-the-merge-is-compared-with-the-merge
  (let [[ctx h] (page {:created-at "2026-10-13T12:30:00Z"})]
    (is (= after-merge (:base ctx)))
    (is (empty? (:stale ctx)))
    (is (not (str/includes? h "stale candidate")))))

(deftest a-base-older-than-the-store-is-compared-with-the-oldest-version-and-warned
  (let [[ctx h] (page {:created-at "2026-10-13T09:01:00Z" :meta {:base-sha (*shas* :p0)}})]
    (is (= before-merge (:base ctx)) "the oldest, not the newest")
    (is (str/includes? h "the store has no version as old as this PR's base; comparing with the oldest one"))
    (is (= 1 (count (:stale ctx))))))

(deftest without-ancestry-the-base-is-placed-by-commit-time
  (let [[ctx h] (page {:created-at "2026-10-13T09:01:00Z" :meta {:base-sha (*shas* :p2)} :compare-fails? true})]
    (is (= before-merge (:base ctx)))
    (is (str/includes? h (str "registry unchanged from " (s7 :p1) " to the PR's base " (s7 :p2) ", by commit time")))))

(deftest a-recorded-base-is-matched-exactly
  (let [[ctx h] (page {:created-at "2026-10-13T12:30:00Z" :meta {:base-sha (*shas* :p3)}})]
    (is (= after-merge (:base ctx)))
    (is (str/includes? h (str "@ " (reg/label (version-of :p3)) " · the PR's base")))
    (is (empty? (:stale ctx)))))

(deftest the-sidecar-is-read-from-the-artifact
  (let [head (*shas* :head) meta {:base-sha (*shas* :p2) :head-sha head}]
    (with-redefs [candidate/gh (fake-gh {:created-at "2026-10-13T09:01:00Z" :meta meta})]
      (let [c (candidate/fetch "owner/repo" 9 head)]
        (is (= meta (:built-on c)))
        (is (= "2026-10-13T09:01:00Z" (:run-at c)))
        (is (= pr-head (:registry c)))))))

(deftest versions-are-placed-by-the-time-the-store-recorded-them
  (is (= (version-of :p3) (reg/recorded-by "main" "2026-10-13T12:27:00Z")))
  (is (= (version-of :p1) (reg/recorded-by "main" "2026-10-13T14:26:59+02:00")))
  (is (nil? (reg/recorded-by "main" "2026-10-13T07:59:59Z")))
  (is (= [(version-of :p3)] (reg/since "main" (version-of :p1)))))

(defn- deploy!
  "Commits `branch` in the project off commit `from` (main itself when `branch`
  is main) and records `reg` as what dev runs, deployed from that commit."
  [branch from reg]
  (let [src (if (= "main" branch)
              (*shas* from)
              (do (git! *project* nil "checkout" "-q" "-b" branch (*shas* from))
                  (let [c (commit! *project* "2026-10-13T13:00:00Z" branch)]
                    (git! *project* nil "checkout" "-q" "main")
                    c)))]
    (git! *store* nil "checkout" "-q" "--orphan" "deployed/dev")
    (record! *store* "2026-10-13T13:05:00Z" reg src (str "registry: " (subs src 0 7) " deployed to dev from " branch))
    (git! *store* nil "checkout" "-q" "-f" "main")
    src))

(def deployed-ahead (registry :fn/x :fn/y :fn/w))

(defn- risk-rows [h] (filter #(re-find #"^- (\S+ risk|context) " %) (str/split-lines h)))

(defn- against-dev [opts] (page (merge {:created-at "2026-10-13T09:01:00Z" :meta {:base-sha (*shas* :p2)} :against "deployed/dev"} opts)))

(deftest an-environment-older-than-the-base-shows-what-was-merged-since-its-deploy
  (deploy! "main" :p0 (registry :fn/x))
  (let [[ctx h] (against-dev {})]
    (is (= :descends (get-in ctx [:against :step])))
    (is (= before-merge (:merge ctx)))
    (is (str/includes? h "Merged since dev's deploy, not there yet: 1 new, 0 changed, 0 deleted"))
    (is (str/includes? h "merged, not yet on dev"))
    (is (re-find #":fn/gone — .* · merged since dev's deploy, not this PR" h))
    (is (not (str/includes? h "Differs between")))))

(deftest an-environment-deployed-from-a-branch-ahead-of-the-base-is-not-merged-since
  (let [f1 (deploy! "feature" :p3 deployed-ahead)
        [ctx h] (against-dev {})]
    (is (= :behind (get-in ctx [:against :step])))
    (is (= f1 (get-in ctx [:against :source-sha])))
    (is (= before-merge (:merge ctx)) "the PR's base, not the newer main the environment holds")
    (is (str/includes? h "Differs between dev and this PR's base: dev runs feature; this PR is built on an older main: 1 new, 0 changed, 2 deleted"))
    (is (str/includes? h ": older than what dev runs"))
    (is (not (str/includes? h "Merged since")))
    (is (not (str/includes? h "merged since dev's deploy")))
    (is (not (str/includes? h "merged, not yet on dev")))
    (is (str/includes? h "This PR: 1 new, 0 changed, 0 deleted"))
    (doseq [id [":fn/y" ":fn/w" ":fn/gone"]]
      (is (re-find (re-pattern (str "context " id " — differs between dev and this PR's base, not this PR")) h) id))
    (is (re-find #"^- \S+ risk :fn/z " (first (risk-rows h))) "this PR's own entity first, the environment's differences after it")
    (is (every? #(str/starts-with? % "- context ") (rest (risk-rows h))))
    (is (str/includes? (d/hiccup->text (decorate/declares ctx :fn/y)) "in what dev runs, not in this PR's base; not this PR"))
    (is (str/includes? (d/hiccup->text (decorate/declares ctx :fn/z)) "new in this PR"))))

(deftest an-environment-deployed-from-a-branch-that-diverged-from-the-base-is-not-merged-since
  (deploy! "side" :p0 deployed-ahead)
  (let [[ctx h] (against-dev {})]
    (is (= :diverged (get-in ctx [:against :step])))
    (is (str/includes? h "Differs between dev and this PR's base: dev runs side; this PR is built on a main that diverged from it: 1 new, 0 changed, 2 deleted"))
    (is (str/includes? h ": diverged from what dev runs"))
    (is (not (re-find #"\ba (origin|older)\b" h)))
    (is (not (str/includes? h "merged since dev's deploy")))
    (is (re-find #"context :fn/y — differs between dev and this PR's base, not this PR" h))))

(deftest when-ancestry-cannot-be-told-the-environment-step-is-only-a-difference
  (deploy! "feature" :p3 deployed-ahead)
  (let [[ctx h] (against-dev {:compare-fails? true})]
    (is (nil? (get-in ctx [:against :step])))
    (is (= before-merge (:merge ctx)))
    (is (str/includes? h "Differs between dev and this PR's base: 1 new, 0 changed, 2 deleted"))
    (is (not (str/includes? h "dev runs feature")))
    (is (str/includes? h ": not known to be older or newer than what dev runs"))
    (is (not (str/includes? h "merged since dev's deploy")))
    (is (re-find #"context :fn/w — differs between dev and this PR's base, not this PR" h))))
