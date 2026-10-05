(ns atlas.review.registry
  (:require [atlas.registry :as registry]
            [atlas.store.canonical :as canon]
            [clojure.edn :as edn]
            [clojure.java.shell :refer [sh]]
            [clojure.string :as str]))

(defonce config (atom {:store nil :prefix nil :branch "origin/main"}))

(defn configure! [store prefix branch]
  (reset! config {:store store :prefix (str (str/replace prefix #"/$" "") "/") :branch (or branch "origin/main")}))

(defonce ^:private cache (atom {}))

(defn- git [& args]
  (let [{:keys [exit out err]} (apply sh "git" "-C" (:store @config) args)]
    (if (zero? exit) out (throw (ex-info (str "registry store: " (str/trim err)) {:args args})))))

(defn- normalise [reg] (into {} (map (fn [[k e]] [(if (vector? k) (set k) k) e])) reg))

(defn versions
  "Recorded versions, newest first: `{:commit :source :subject}`, where source
  is the project sha the version was built from."
  []
  (let [{:keys [prefix branch]} @config]
    (for [l (str/split-lines (git "log" "--format=%H %s" branch "--" prefix)) :when (seq l)
          :let [[commit subject] (str/split l #" " 2)]]
      {:commit commit :subject subject :source (second (re-matches #"registry: (\S+).*" (or subject "")))})))

(defn version-for
  "The recorded version built from `sha`, or nil."
  [sha]
  (some #(when (and (:source %) (str/starts-with? sha (:source %))) %) (versions)))

(defn latest [] (first (versions)))

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
