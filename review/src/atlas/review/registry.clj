(ns atlas.review.registry
  (:require [babashka.http-client :as http]
            [clojure.edn :as edn]
            [atlas.registry :as registry]))

(def ^:dynamic *cloud-url* (or (System/getenv "ATLAS_CLOUD_URL") "http://localhost:8090"))

(defonce ^:private cache (atom {}))

(defn- fetch-edn [path]
  (let [url (str *cloud-url* path)]
    (or (@cache url)
        (let [{:keys [status body]} (http/get url {:throw false :timeout 120000})]
          (when (not= 200 status) (throw (ex-info (str "atlas-cloud " status " for " path) {:status status})))
          (let [v (edn/read-string {:default (fn [_ v] v)} body)]
            (swap! cache assoc url v)
            v)))))

(defn forget! [path] (swap! cache dissoc (str *cloud-url* path)))

(defn versions [org project] (:versions (fetch-edn (str "/" org "/" project "/versions"))))

(defn latest-main [org project]
  (->> (versions org project)
       (keep #(when-let [[_ a b c] (re-matches #"v(\d+)\.(\d+)\.(\d+)(?:-.*)?" %)] [(mapv parse-long [a b c]) %]))
       (sort-by first) last second))

(defn version
  "A stored version as a registry map, keys normalised to sets so it can be
  bound as `atlas.registry/*registry-override*`."
  [org project v]
  (into {} (map (fn [[k e]] [(if (vector? k) (set k) k) e])) (fetch-edn (str "/" org "/" project "/" v))))

(defn diff [org project from to] (fetch-edn (str "/" org "/" project "/diff?from=" from "&to=" to)))

(defmacro with-version [reg & body]
  `(binding [registry/*registry-override* ~reg] ~@body))
