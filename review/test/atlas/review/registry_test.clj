(ns atlas.review.registry-test
  (:require [atlas.review.decorate :as decorate]
            [atlas.review.reach-review :as reach-review]
            [atlas.review.registry :as reg]
            [atlas.store.canonical :as canon]
            [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sdiff.decorate :as d]))

(defn- sha [c] (apply str (repeat 40 c)))

(def src1 (sha "1"))
(def src2 (sha "2"))
(def src3 (sha "3"))

(defn- ent [id & aspects] [(into #{:atlas/execution-function} aspects) {:atlas/dev-id id :atlas/type :atlas/execution-function :execution-function/context [:k/in]}])

(def deployed {(first (ent :fn/x :domain/x)) (second (ent :fn/x :domain/x)) (first (ent :fn/gone :domain/gone)) (second (ent :fn/gone :domain/gone))})
(def merged {(first (ent :fn/x :domain/x)) (second (ent :fn/x :domain/x)) (first (ent :fn/y :domain/y)) (second (ent :fn/y :domain/y))})
(def head {(first (ent :fn/x :domain/x)) (assoc (second (ent :fn/x :domain/x)) :execution-function/context [:k/in :k/more])
           (first (ent :fn/y :domain/y)) (second (ent :fn/y :domain/y))
           (first (ent :fn/z :domain/z)) (second (ent :fn/z :domain/z))})

(defn- git! [dir & args]
  (let [{:keys [exit out err]} (apply sh "git" "-c" "user.name=test" "-c" "user.email=test@example.com" "-c" "commit.gpgsign=false"
                                      (concat args [:dir (str dir)]))]
    (if (zero? exit) out (throw (ex-info err {:args args})))))

(defn- record! [dir registry subject src]
  (let [root (io/file dir "registries" "app")]
    (doseq [f (reverse (file-seq root))] (io/delete-file f true))
    (doseq [[p content] (canon/registry->files registry)]
      (let [f (io/file root p)] (io/make-parents f) (spit f content))))
  (git! dir "add" "-A")
  (git! dir "commit" "-q" "-m" subject "-m" (str "Source-Sha: " src)))

(def ^:dynamic *origin* nil)

(defn- store-fixture [t]
  (let [tmp (.toFile (java.nio.file.Files/createTempDirectory "registry-store" (make-array java.nio.file.attribute.FileAttribute 0)))
        origin (io/file tmp "origin") store (io/file tmp "store")]
    (.mkdirs origin)
    (git! origin "init" "-q")
    (git! origin "checkout" "-q" "-b" "main")
    (record! origin deployed (str "registry: " (subs src1 0 7)) src1)
    (record! origin merged (str "registry: " (subs src2 0 7)) src2)
    (git! origin "checkout" "-q" "--orphan" "deployed/dev")
    (record! origin deployed (str "registry: " (subs src1 0 7) " deployed to dev from main") src1)
    (git! origin "checkout" "-q" "--orphan" "deployed/staging")
    (record! origin deployed (str "registry: " (subs src1 0 7) " deployed to staging from hotfix") src1)
    (git! origin "checkout" "-q" "main")
    (git! tmp "clone" "-q" (str origin) (str store))
    (let [saved @reg/config]
      (reg/configure! (str store) "registries/app" "origin/main")
      (try (binding [*origin* origin] (t))
           (finally (reset! reg/config saved)
                    (doseq [f (reverse (file-seq tmp))] (io/delete-file f true)))))))

(use-fixtures :each store-fixture)

(deftest versions-are-read-per-branch
  (let [[newest oldest :as main] (reg/versions "origin/main")]
    (is (= 2 (count main)))
    (is (= [(subs src2 0 7) src2 nil] ((juxt :source :source-sha :env) newest)))
    (is (= (subs src1 0 7) (:source oldest)))
    (is (= main (reg/versions)) "the configured branch by default"))
  (let [[v :as dev] (reg/versions "origin/deployed/dev")]
    (is (= 1 (count dev)))
    (is (= ["dev" "main" src1] ((juxt :env :from :source-sha) v)))
    (is (re-matches #"\d{4}-\d\d-\d\dT.*" (:date v)))))

(deftest a-version-is-found-by-the-sha-it-was-built-from
  (is (= (subs src2 0 7) (:source (reg/version-for "origin/main" src2))))
  (is (= (subs src1 0 7) (:source (reg/version-for src1))))
  (is (nil? (reg/version-for "origin/main" src3)))
  (is (= (subs src1 0 7) (:source (reg/latest "origin/deployed/dev")))))

(deftest against-names-only-branches-of-the-store
  (is (= "origin/deployed/dev" (reg/resolve-against "deployed/dev")) "the remote branch when only the remote has it")
  (is (= "origin/deployed/dev" (reg/resolve-against "origin/deployed/dev")))
  (is (= "origin/main" (reg/resolve-against "main")) "the configured branch first")
  (is (nil? (reg/resolve-against "--upload-pack=touch /tmp/x")))
  (is (nil? (reg/resolve-against "-c")))
  (is (nil? (reg/resolve-against "deployed/prod")))
  (is (nil? (reg/resolve-against "")))
  (is (nil? (reg/resolve-against nil))))

(deftest the-store-is-fetched-at-most-once-per-ttl
  (swap! reg/config assoc :fetch-ttl-ms 600000)
  (reg/resolve-against "deployed/dev")
  (git! *origin* "checkout" "-q" "deployed/dev")
  (record! *origin* merged (str "registry: " (subs src2 0 7) " deployed to dev from main") src2)
  (reg/resolve-against "deployed/dev")
  (is (= src1 (:source-sha (reg/latest "origin/deployed/dev"))) "fetched less than a TTL ago")
  (swap! reg/config assoc :fetch-ttl-ms 0)
  (reg/resolve-against "deployed/dev")
  (is (= src2 (:source-sha (reg/latest "origin/deployed/dev")))))

(def report {:base src2 :head src3 :pr {:repo "owner/repo" :num 7 :head src3} :clj []})

(defn- with-candidate [f]
  (let [attempts @#'decorate/attempts]
    (swap! attempts assoc [7 src3] {:registry head :label "run 42"})
    (try (f) (finally (swap! attempts dissoc [7 src3])))))

(defn- context [params] (binding [d/*params* params] (decorate/context report)))

(defn- header [ctx] (str/join "\n" (map d/hiccup->text (decorate/registry-header ctx report))))

(defn- state [ctx id] (d/hiccup->text (decorate/declares ctx id)))

(deftest without-against-the-base-is-the-merge-base-on-the-configured-branch
  (with-candidate
    (fn []
      (doseq [ctx [(context {}) (context {"against" "main"}) (context {"against" "origin/main"})]]
        (is (nil? (:against ctx)))
        (is (= (reg/registry-at (:commit (reg/version-for src2))) (:base ctx)))
        (let [h (header ctx)]
          (is (str/includes? h "base main @ "))
          (is (not (str/includes? h "origin/")) "the store's remote is not shown")
          (is (str/includes? h "this PR's merge base"))
          (is (str/includes? h "Registry: 1 new, 1 changed, 0 deleted"))
          (is (not (str/includes? h "deploy"))))
        (is (str/includes? (state ctx :fn/z) "new in this PR"))
        (is (str/includes? (state ctx :fn/x) "contract changed"))))))

(deftest against-an-environment-the-base-is-what-it-runs
  (with-candidate
    (fn []
      (let [ctx (context {"against" "deployed/dev"})
            h (header ctx)]
        (is (= "origin/deployed/dev" (get-in ctx [:against :ref])))
        (is (= deployed (:base ctx)) "the newest on the branch, not a fallback")
        (is (= merged (:merge ctx)))
        (is (str/includes? h "base deployed/dev @ "))
        (is (str/includes? h "what dev runs, deployed from main"))
        (is (str/includes? h (str "built from " (subs src1 0 12))))
        (is (str/includes? h "→ main @ "))
        (is (not (str/includes? h "origin/")) "the store's remote is not shown")
        (is (str/includes? h "merged, not yet on dev"))
        (is (str/includes? h "→ this PR · CI run 42"))
        (is (str/includes? h "Merged since dev's deploy, not there yet: 1 new, 0 changed, 1 deleted"))
        (is (str/includes? h "This PR: 1 new, 1 changed, 0 deleted"))
        (is (re-find #":fn/y — .* · merged since dev's deploy, not this PR" h))
        (is (not (re-find #":fn/z — .*merged since" h)))
        (is (str/includes? (state ctx :fn/z) "new in this PR"))
        (is (str/includes? (state ctx :fn/y) "new since dev's deploy"))
        (is (str/includes? (state ctx :fn/gone) "deleted since dev's deploy"))
        (is (str/includes? (state ctx :fn/x) "contract changed by this PR"))
        (is (not (str/includes? (state ctx :fn/x) "since")))))))

(deftest an-unknown-against-falls-back-and-says-so
  (with-candidate
    (fn []
      (let [ctx (context {"against" "--upload-pack=x"})
            h (header ctx)]
        (is (nil? (:against ctx)))
        (is (str/includes? h "against=--upload-pack=x: the registry store has no such branch; compared against main instead"))
        (is (str/includes? h "Registry: 1 new, 1 changed, 0 deleted"))))))

(deftest the-reach-context-keeps-against
  (with-candidate
    (fn []
      (binding [d/*params* {"against" "deployed/dev"}]
        (is (= "origin/deployed/dev" (get-in (#'reach-review/context report) [:against :ref])))))))

(deftest store-branches-are-named-without-their-remote
  (is (= "main" (reg/shown "origin/main")))
  (is (= "deployed/dev" (reg/shown "origin/deployed/dev")))
  (is (= "deployed/dev" (reg/shown "deployed/dev")) "a local branch keeps its name")
  (is (= "elsewhere/main" (reg/shown "elsewhere/main")) "only a remote of the store is dropped"))

(deftest a-diverged-environment-names-the-line-without-its-remote
  (git! *origin* "checkout" "-q" "--orphan" "deployed/qa")
  (record! *origin* merged (str "registry: " (subs src3 0 7) " deployed to qa from side") src3)
  (git! *origin* "checkout" "-q" "main")
  (swap! reg/config assoc :fetch-ttl-ms 0)
  (with-redefs [atlas.review.candidate/ancestor? (fn [_ _ _] false)]
    (with-candidate
      (fn []
        (let [h (header (context {"against" "deployed/qa"}))]
          (is (str/includes? h "qa runs side; this PR is built on a main that diverged from it"))
          (is (not (re-find #"\ba (origin|older)\b" h))))))))

(deftest the-branch-an-environment-was-deployed-from-is-named-once
  (with-candidate
    (fn []
      (let [on-line (header (context {"against" "deployed/dev"}))
            off-line (header (context {"against" "deployed/staging"}))]
        (is (str/includes? on-line "what dev runs, deployed from main"))
        (is (not (str/includes? on-line ", not main")))
        (is (re-find #"what staging runs, recorded \S+ deployed from hotfix, not main" off-line))
        (is (not (str/includes? off-line ", deployed from hotfix")))))))
