(ns atlas.review.candidate-base-test
  (:require [atlas.review.candidate :as candidate]
            [atlas.review.decorate :as decorate]
            [atlas.review.env :as env]
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
      (let [f (io/file root p)] (io/make-parents f) (spit f content)))
    (spit (io/file root "source") src))
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
  `meta`, the sidecar; compare and commit lookups from the project's git; the
  pull requests holding a commit from `pulls`, `{sha [pr]}`, failing for a
  commit it does not name."
  [{:keys [created-at meta compare-fails? pulls]}]
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

      (and (= "api" a) (str/ends-with? b "/pulls"))
      (let [sha (second (re-find #"/commits/(\w+)/pulls" b))]
        (if (contains? pulls sha) (json/generate-string (pulls sha)) (throw (ex-info "HTTP 422" {}))))

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

(defn- deploy-to!
  "Commits `branch` in the project off commit `from` (main itself when `branch`
  is main) and records `reg` as what `env` runs, deployed from that commit."
  [env branch from reg]
  (let [src (if (= "main" branch)
              (*shas* from)
              (do (git! *project* nil "checkout" "-q" "-b" branch (*shas* from))
                  (let [c (commit! *project* "2026-10-13T13:00:00Z" branch)]
                    (git! *project* nil "checkout" "-q" "main")
                    c)))]
    (git! *store* nil "checkout" "-q" "--orphan" (str "deployed/" env))
    (record! *store* "2026-10-13T13:05:00Z" reg src (str "registry: " (subs src 0 7) " deployed to " env " from " branch))
    (git! *store* nil "checkout" "-q" "-f" "main")
    src))

(defn- deploy! [branch from reg] (deploy-to! "dev" branch from reg))

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

(def ^:private now (java.time.ZonedDateTime/parse "2026-10-14T09:00:00Z"))

(defn- env-hiccup
  ([against] (env-hiccup against {}))
  ([against gh-opts]
   (reset! @#'candidate/known {})
   (swap! reg/config assoc :repo "owner/repo")
   (with-redefs [candidate/gh (fake-gh gh-opts)]
     (binding [env/*now* (constantly now)]
       (env/page {:params (if against {"against" against} {})})))))

(defn- env-page
  ([against] (env-page against {}))
  ([against gh-opts] (d/hiccup->text (env-hiccup against gh-opts))))

(defn- links [h] (set (keep #(when (map? %) (:href %)) (tree-seq coll? seq h))))

(def side-registry (registry :fn/x :fn/gone :fn/w))

(defn- row [text env] (some #(when (str/starts-with? % (str "- " env " ")) (subs % 2)) (str/split-lines text)))

(deftest the-environment-overview-lists-recorded-and-expected-environments
  (deploy-to! "dev" "main" :p1 before-merge)
  (let [side (deploy-to! "staging" "side" :p2 side-registry)
        saved @env/settings]
    (reset! env/settings {:envs ["dev" "staging" "production"]})
    (try
      (let [h (env-page nil)]
        (is (= ["dev" "staging"] (keys (env/recorded))))
        (is (= (str "dev runs main at " (s7 :p1) " · 1 merge with registry changes waiting: " (s7 :p3) " · deployed 19h ago") (row h "dev")))
        (is (= (str "staging runs side at " (subs side 0 7) ", diverged from main · 1 merge with registry changes waiting: " (s7 :p3)
                    " · 1 entity only on its branch · deployed 19h ago")
               (row h "staging")))
        (is (= "production not recorded yet" (row h "production")))
        (is (not (str/includes? h "origin/")))
        (is (contains? (links (env-hiccup nil)) "/env?against=deployed%2Fstaging")))
      (finally (reset! env/settings saved)))))

(defn- sections [h]
  (let [[top on-line] (str/split h #"(?m)^On main, not deployed" 2)
        [on-line only-env] (if on-line (str/split on-line #"(?m)^Only on " 2) [nil (second (str/split top #"(?m)^Only on " 2))])]
    {:top top :on-line on-line :only-env only-env}))

(deftest an-environment-deployed-from-a-diverged-branch-lists-both-sides
  (let [side (deploy-to! "staging" "side" :p2 side-registry)
        h (env-page "deployed/staging")
        {:keys [top on-line only-env]} (sections h)]
    (is (str/includes? top "staging was deployed from side, which diverged from main after " ))
    (is (str/includes? top (s7 :p1)) "where they parted: the newest main version the side branch holds")
    (is (contains? (links (env-hiccup "deployed/staging")) (str "https://github.com/owner/repo/compare/" side "..." (*shas* :p3))) "the compare link")
    (is (contains? (links (env-hiccup "deployed/staging")) (str "https://github.com/owner/repo/commit/" side)))
    (is (str/includes? on-line " · 1 new, 0 changed, 1 deleted"))
    (is (re-find #"new :fn/y" on-line))
    (is (re-find #"deleted :fn/gone" on-line))
    (is (not (str/includes? on-line ":fn/w")))
    (is (str/starts-with? only-env "staging's branch · 1 new, 0 changed, 0 deleted"))
    (is (re-find #"new :fn/w" only-env))
    (is (not (re-find #":fn/(y|gone)" only-env)))
    (is (not (str/includes? h "#")) "GitHub names no pull request for the side commit")))

(deftest an-environment-at-an-older-line-commit-only-lacks-what-the-line-merged
  (deploy-to! "dev" "main" :p1 before-merge)
  (let [h (env-page "deployed/dev")
        {:keys [top on-line only-env]} (sections h)]
    (is (str/includes? top "dev runs an older main: what follows is on main and not deployed to dev yet."))
    (is (str/includes? on-line " · 1 new, 0 changed, 1 deleted"))
    (is (re-find #"new :fn/y" on-line))
    (is (re-find #"deleted :fn/gone" on-line))
    (is (nil? only-env))
    (is (not (str/includes? h "Only on")))))

(deftest an-environment-at-the-line-has-nothing-waiting
  (deploy-to! "dev" "main" :p3 after-merge)
  (is (str/includes? (env-page "deployed/dev") "dev runs main's newest registry; nothing on main waits to be deployed.")))

(deftest the-environment-page-names-only-store-branches
  (deploy-to! "dev" "main" :p1 before-merge)
  (let [h (env-page "--upload-pack=touch /tmp/x")]
    (is (str/includes? h "against=--upload-pack=touch /tmp/x: the registry store has no such branch"))
    (is (str/starts-with? (row h "dev") "dev runs main at") "the overview is shown under the problem"))
  (is (str/includes? (env-page "deployed/production") "against=deployed/production: the registry store has no such branch"))
  (is (str/includes? (env-page "main") "against=main: that is main itself")))

(deftest an-environment-ahead-of-the-line-only-has-its-branch
  (deploy-to! "dev" "feature" :p3 deployed-ahead)
  (let [h (env-page "deployed/dev")
        {:keys [top on-line only-env]} (sections h)]
    (is (str/includes? top "dev was deployed from feature, which already holds main's newest commit"))
    (is (nil? on-line))
    (is (str/starts-with? only-env "dev's branch · 1 new, 0 changed, 0 deleted"))
    (is (re-find #"new :fn/w" only-env))))

(defn- data-key [id] [#{:atlas/data-key (keyword "dk" (name id))} {:atlas/dev-id id :atlas/type :atlas/data-key}])

(defn- merge-on-main!
  "Commits `subject` on the project's main and records `reg` on the store's
  main as that merge, the way the store's subjects carry it."
  [subject reg]
  (git! *project* nil "checkout" "-q" "main")
  (let [c (commit! *project* "2026-10-14T07:00:00Z" subject)]
    (record! *store* "2026-10-14T07:10:00Z" reg c (str "registry: " (subs c 0 7) " " subject))
    c))

(def keys-added (into after-merge (map data-key [:k.app/a :k.app/b :k.app/c :k.app/d :k.app/e])))

(defn- before? [text a b] (< (str/index-of text a) (str/index-of text b)))

(deftest the-line-s-undeployed-entities-are-listed-under-the-merge-that-changed-them
  (deploy-to! "dev" "main" :p1 before-merge)
  (let [m14 (merge-on-main! "Add the app keys (#14)" keys-added)
        m15 (merge-on-main! "Tidy the docs (#15)" keys-added)
        h (env-page "deployed/dev")
        {:keys [on-line]} (sections h)
        lines (str/split-lines on-line)]
    (is (str/starts-with? on-line " · 6 new, 0 changed, 1 deleted · 3 merges since dev's commit"))
    (is (some #{(str "Tidy the docs (#15) · " (subs m15 0 7) " · no registry changes")} lines) "a merge without registry changes is still listed")
    (is (some #{(str "Add the app keys (#14) · " (subs m14 0 7) " · 5 entities · 1h ago")} lines))
    (is (some #{"new 5 data keys in :k.app/: a, b, c, d, e"} lines) "a long group of alike rows is one line")
    (is (some #{(str (s7 :p3) " · 2 entities · 20h ago")} lines))
    (is (before? on-line "(#15)" "(#14)") "newest first")
    (is (before? on-line "(#14)" (str (s7 :p3) " · 2 entities")))
    (is (before? on-line (s7 :p3) ":fn/y") ":fn/y under the merge that added it")
    (is (not (str/includes? (subs on-line 0 (str/index-of on-line (s7 :p3))) ":fn/")))
    (is (some #{"2 exec-fns"} lines))
    (let [ls (links (env-hiccup "deployed/dev"))]
      (is (contains? ls "https://github.com/owner/repo/pull/14"))
      (is (contains? ls (str "https://github.com/owner/repo/commit/" m15))))
    (is (not (str/includes? h "contract changed")) "a new entity has no contract to have changed")
    (is (= 1 (count (re-seq #"test case" h))) "a reason every row shares is said once")
    (is (str/includes? h "None of these has a test case covering it or what depends on it."))
    (is (some #{"- new :fn/y"} lines) "a row keeps only the reasons that tell it apart")
    (is (= (str "dev runs main at " (s7 :p1) " · 2 merges with registry changes waiting: #14, " (s7 :p3) " · deployed 19h ago")
           (row (env-page nil) "dev")))))

(defn- deploy-pr-head!
  "Deploys to staging the head of a pull request branched off p3, then merges
  it to main as a new commit, as a squash merge does; the head and the merge."
  []
  (let [head (deploy-to! "staging" "pr-12" :p3 deployed-ahead)
        merged (merge-on-main! "Add w (#12)" deployed-ahead)]
    [head merged]))

(deftest an-environment-running-a-pull-request-before-its-merge-is-not-called-diverged
  (let [[head merged] (deploy-pr-head!)
        gh {:pulls {head [{:number 12 :state "closed" :merged_at "2026-10-14T07:00:00Z" :merge_commit_sha merged :title "Add w"}]}}
        h (env-page "deployed/staging" gh)]
    (is (str/includes? h (str "staging runs pr-12 — #12 before it was merged (as " (subs merged 0 7) "), registry-wise identical to #12 as merged; nothing else on main waits to be deployed.")))
    (is (not (str/includes? h "diverged")))
    (is (str/includes? h "On main, not deployed: nothing."))
    (is (str/includes? h "Only on staging's branch: nothing."))
    (is (= (str "staging runs pr-12 at " (subs head 0 7) " — #12 before it was merged · nothing waiting · deployed 19h ago")
           (row (env-page nil gh) "staging")))))

(deftest without-github-an-environment-running-a-pull-request-head-falls-back-to-diverged
  (deploy-pr-head!)
  (let [h (env-page "deployed/staging")]
    (is (str/includes? h "staging was deployed from pr-12, which diverged from main after "))
    (is (not (str/includes? h "#12 before")))))

(deftest an-environment-running-an-open-pull-request-says-so
  (let [f (deploy! "feature" :p3 deployed-ahead)
        gh {:pulls {f [{:number 21 :state "open" :merged_at nil :title "Add w"}]}}
        h (env-page "deployed/dev" gh)]
    (is (str/includes? h "dev runs feature — #21, not merged yet. dev was deployed from feature, which already holds main's newest commit"))
    (is (= (str "dev runs feature at " (subs f 0 7) " — #21, not merged yet · 1 entity only on its branch · deployed 19h ago")
           (row (env-page nil gh) "dev")))))

(deftest environments-on-one-commit-say-so
  (deploy-to! "dev" "main" :p1 before-merge)
  (deploy-to! "production" "main" :p1 before-merge)
  (let [h (env-page nil)]
    (is (not (str/includes? (row h "dev") "same commit")))
    (is (str/starts-with? (row h "production") (str "production runs main at " (s7 :p1) " (same commit as dev) · ")))))

(deftest times-are-told-relative-to-now
  (binding [env/*now* (constantly now)]
    (is (= "just now" (env/ago "2026-10-14T08:59:30Z")))
    (is (= "12 min ago" (env/ago "2026-10-14T08:48:00Z")))
    (is (= "20h ago" (env/ago "2026-10-13T15:00:00+02:00")))
    (is (= "yesterday 08:38" (env/ago "2026-10-13T08:38:00Z")))
    (is (= "Sun 13:38" (env/ago "2026-10-11T13:38:00Z")))
    (is (= "2026-10-01" (env/ago "2026-10-01T10:00:00Z"))))
  (deploy-to! "dev" "main" :p1 before-merge)
  (is (some #(and (map? %) (= "2026-10-13T13:05:00+00:00" (:title %))) (tree-seq coll? seq (env-hiccup "deployed/dev")))
      "the exact time stays on hover"))
