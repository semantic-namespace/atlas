(ns atlas.review.candidate
  (:require [atlas.review.registry :as reg]
            [babashka.http-client :as http]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]]
            [clojure.string :as str]))

(def ^:dynamic *cloud-key* (System/getenv "ATLAS_CLOUD_KEY"))

(defn label [num head] (str "pr" num "-" (subs head 0 7)))

(defn- gh [& args]
  (let [{:keys [exit out err]} (apply sh "gh" args)]
    (if (zero? exit) out (throw (ex-info (str/trim (str err out)) {:args args})))))

(defn ci-run [repo head]
  (first (json/parse-string (gh "run" "list" "--repo" repo "--workflow" "atlas-registry" "--commit" head
                                "--limit" "1" "--json" "databaseId,status,conclusion,url")
                            true)))

(defn- put! [org project version file]
  (let [{:keys [status body]} (http/put (str reg/*cloud-url* "/" org "/" project "/" version)
                                        {:headers {"authorization" (str "Bearer " *cloud-key*) "content-type" "application/edn"}
                                         :body (slurp file) :throw false :timeout 120000})]
    (when-not (#{200 201} status) (throw (ex-info (str "atlas-cloud refused the candidate: " status " " body) {})))
    (reg/forget! (str "/" org "/" project "/versions"))
    version))

(defn stage!
  "Stages the PR head's registry, built by CI, as `pr<N>-<head>` on the cloud.
  Returns `{:version v}` or `{:reason why-not}`."
  [org project repo num head]
  (let [run (try (ci-run repo head) (catch Exception e {:error (ex-message e)}))]
    (cond
      (nil? *cloud-key*) {:reason "set ATLAS_CLOUD_KEY so the server can stage the PR's registry"}
      (:error run) {:reason (str "gh: " (:error run))}
      (nil? run) {:reason (str "CI has no atlas-registry run for " (subs head 0 9) " (the workflow skips PRs that touch no source)")}
      (not= "completed" (:status run)) {:reason (str "CI is still building the registry for " (subs head 0 9) " (" (:url run) ")")}
      (not= "success" (:conclusion run)) {:reason (str "the atlas-registry run for " (subs head 0 9) " " (:conclusion run) " (" (:url run) ")")}
      :else
      (let [dir (io/file (System/getProperty "java.io.tmpdir") "sdiff-candidates" (str num "-" (subs head 0 7)))]
        (.mkdirs dir)
        (gh "run" "download" (str (:databaseId run)) "--repo" repo "-n" "registry" "-D" (str dir))
        (let [f (io/file dir "atlas-registry.edn")]
          (if (.exists f)
            {:version (put! org project (label num head) f)}
            {:reason (str "the run's artifact has no atlas-registry.edn (" (:url run) ")")}))))))
