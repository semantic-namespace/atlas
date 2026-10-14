(ns store-cli-smoke
  (:require [atlas.registry :as registry]
            [atlas.review.cli :as cli]
            [atlas.review.registry :as store]
            [atlas.store.canonical :as canon]
            [babashka.classpath :as cp]
            [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.edn :as edn]
            [clojure.string :as str]))

(def prefix "demo/calendar/")

(def failures (atom 0))

(defn- check [what ok?]
  (println (if ok? "ok  " "FAIL") what)
  (when-not ok? (swap! failures inc)))

(defn- git [dir & args]
  (apply p/shell {:dir (str dir) :out :string :err :string} "git" args))

(defn- commit! [dir reg msg]
  (doseq [f (fs/glob (fs/path dir prefix "entities") "*.edn")] (fs/delete f))
  (doseq [[path content] (canon/registry->files reg)]
    (let [f (fs/path dir prefix path)]
      (fs/create-dirs (fs/parent f))
      (spit (str f) content)))
  (git dir "add" "-A")
  (git dir "commit" "-q" "-m" msg)
  (str/trim (:out (git dir "rev-parse" "HEAD"))))

(defn- entry [reg dev-id] (first (filter #(= dev-id (:atlas/dev-id (val %))) reg)))

(defn- example-registry []
  (cp/add-classpath "test")
  (require 'atlas.ontology.execution-function 'atlas.ontology.interface-endpoint 'app.calendar-availability)
  ((resolve 'app.calendar-availability/init-registry!))
  (-> @registry/registry canon/sanitize canon/strip-volatile))

(defn- next-version [reg]
  (let [[dropped-k] (entry reg :endpoint/admin-delete-user)
        [renamed-k renamed] (entry reg :fn/list-user-calendar-events)
        [grown-k grown] (entry reg :fn/check-user-availability)]
    (-> reg
        (dissoc dropped-k)
        (assoc renamed-k (assoc renamed :atlas/dev-id :fn/list-calendar-events))
        (dissoc grown-k)
        (assoc (conj grown-k :tier/cached) grown))))

(defn- run [dir & args]
  (store/configure! (str dir) prefix nil)
  (let [{[cmd & more] :args :as opts} (cli/parse-args args)]
    (cli/run cmd (str dir) more opts)))

(defn- bb-registry [dir & args]
  (apply p/shell {:continue true :dir (str dir) :out :string :err :string}
         "bb" "--config" (str (fs/absolutize "bb.edn")) "registry"
         (concat args ["--prefix" prefix])))

(defn -main [& _]
  (let [dir (fs/create-temp-dir {:prefix "atlas-store-smoke"})]
    (try
      (git dir "init" "-q")
      (git dir "config" "user.email" "smoke@example.invalid")
      (git dir "config" "user.name" "smoke")
      (let [v1 (example-registry)
            v2 (next-version v1)
            a (commit! dir v1 "registry: v1")
            b (commit! dir v2 "registry: v2")
            _ (spit (str (fs/path dir prefix "entities" "atlas_broken.edn"))
                    "[#{:atlas/broken} {:atlas/dev-id :broken/x :broken/v #object[java.lang.Object 0x1 \"x\"]}]\n")
            _ (git dir "add" "-A")
            _ (git dir "commit" "-q" "-m" "registry: v3")
            d (run dir "diff" a b)]
        (check "diff names the deleted and the renamed-away entity"
               (= #{:endpoint/admin-delete-user :fn/list-user-calendar-events} (set (map :snap/dev-id (:deleted d)))))
        (check "diff detects the rename"
               (= [{:from :fn/list-user-calendar-events :to :fn/list-calendar-events}]
                  (mapv #(select-keys % [:from :to]) (:renames d))))
        (check "diff shows the added aspect"
               (some #(and (= :fn/check-user-availability (:snap/dev-id %)) (= [:tier/cached] (:snap/added %)))
                     (:changed d)))
        (check "diff reports the vocabulary change" (contains? (:added (:vocabulary d)) :tier/cached))
        (let [t (run dir "timeline" a "HEAD")
              e (run dir "timeline" a b ":fn/check-user-availability")]
          (check "timeline skips and lists the unreadable commit" (= 1 (count (:unreadable t))))
          (check "timeline has one step per readable version" (= 1 (count (:timeline t))))
          (check "entity timeline covers both versions" (= 2 (count (:timeline e)))))
        (let [deps (run dir "dependencies" b ":endpoint/query-availability")
              dents (run dir "dependents" b (str (first deps)))
              blast (run dir "blast-radius" b (str (first deps)))
              causes (run dir "trace-causes" b ":endpoint/query-availability" "--hops" "1")]
          (check "dependencies of an endpoint are found" (seq deps))
          (check "the endpoint is among its dependency's dependents" (some #{:endpoint/query-availability} dents))
          (check "blast radius reaches the endpoint"
                 (some #(= :endpoint/query-availability (:entity %)) blast))
          (check "one-hop trace-causes equals the dependencies"
                 (= (set deps) (set (map :entity causes))))
          (check "every one-hop cause is at hop 1" (every? #(= 1 (:hops %)) causes)))
        (let [k (first (keep #(first (:execution-function/context (val %))) v2))
              producers (run dir "producers-of" b (str k))
              consumers (run dir "consumers-of" b (str k))]
          (check (str "consumers of " k " are found") (seq consumers))
          (check (str "producers of " k " is a vector") (vector? producers)))
        (let [gaps (run dir "gaps" b "--min-similarity" "0.6" "--max" "3")]
          (check "gaps honours --max and --min-similarity"
                 (and (<= 1 (count gaps) 3) (every? #(> (:similarity %) 0.6) gaps))))
        (let [ok (bb-registry dir "diff" "." (subs a 0 7) (subs b 0 7))
              broken (bb-registry dir "diff" "." b "HEAD")
              misuse (bb-registry dir "nonsense" ".")]
          (check "bb --config registry diff runs from another directory"
                 (and (zero? (:exit ok)) (= (:deleted d) (:deleted (edn/read-string (:out ok))))))
          (check "an unreadable commit is a one-line error, not a stack trace"
                 (and (= 1 (:exit broken))
                      (str/includes? (:err broken) "cannot be read as EDN")
                      (not (str/includes? (:err broken) "Stack trace"))))
          (check "an unknown command prints usage" (and (= 1 (:exit misuse)) (str/includes? (:err misuse) "usage:")))))
      (finally (fs/delete-tree dir)))
    (if (pos? @failures)
      (do (println @failures "failed") (System/exit 1))
      (println "OK store CLI smoke"))))
