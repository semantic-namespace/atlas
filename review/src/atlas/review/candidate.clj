(ns atlas.review.candidate
  (:require [atlas.review.registry :as reg]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]]
            [clojure.string :as str]))

(def ^:dynamic *workflow* (or (System/getenv "SDIFF_REGISTRY_WORKFLOW") "atlas-registry"))
(def ^:dynamic *artifact* (or (System/getenv "SDIFF_REGISTRY_ARTIFACT") "registry"))

(defn- gh [& args]
  (let [{:keys [exit out err]} (apply sh "gh" args)]
    (if (zero? exit) out (throw (ex-info (str/trim (str err out)) {:args args})))))

(defn ci-run [repo head]
  (first (json/parse-string (gh "run" "list" "--repo" repo "--workflow" *workflow* "--commit" head
                                "--limit" "1" "--json" "databaseId,status,conclusion,url")
                            true)))

(defn fetch
  "The registry CI built for the PR head, as `{:registry m :label l}`, or
  `{:reason why-not}`."
  [repo num head]
  (let [run (try (ci-run repo head) (catch Exception e {:error (ex-message e)}))
        short (subs head 0 9)]
    (cond
      (:error run) {:reason (str "gh: " (:error run))}
      (nil? run) {:reason (str "CI has no " *workflow* " run for " short " (the workflow skips PRs that touch no source)")}
      (not= "completed" (:status run)) {:reason (str "CI is still building the registry for " short " (" (:url run) ")")}
      (not= "success" (:conclusion run)) {:reason (str "the " *workflow* " run for " short " " (:conclusion run) " (" (:url run) ")")}
      :else
      (let [dir (io/file (System/getProperty "java.io.tmpdir") "sdiff-candidates" (str num "-" (subs head 0 7)))
            f (io/file dir "atlas-registry.edn")]
        (when-not (.exists f)
          (.mkdirs dir)
          (gh "run" "download" (str (:databaseId run)) "--repo" repo "-n" *artifact* "-D" (str dir)))
        (if (.exists f)
          {:registry (reg/from-file f) :label (str "pr" num "-" (subs head 0 7) " (run " (:databaseId run) ")")}
          {:reason (str "the run's artifact has no atlas-registry.edn (" (:url run) ")")})))))
