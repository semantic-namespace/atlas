(ns atlas.store.file
  "Filesystem Store: a directory of canonical EDN. The zero-infrastructure
  default, and the one that keeps a registry inside a repository the team
  already controls."
  (:require
   [atlas.store.protocol :as p]
   [clojure.java.io :as io]
   [clojure.string :as str])
  (:import [java.io File]))


(defn- all-files [^File root]
  (into {} (for [^File f (file-seq root)
                 :when (.isFile f)]
             [(str/replace (.getPath f) (str (.getPath root) "/") "") (slurp f)])))


(defrecord FileStore [^String dir]
  p/Store
  (head [_]
    (let [f (io/file dir)]
      (when (and (.isDirectory f) (seq (.listFiles f))) dir)))

  (read-at [_ _ref]
    ;; A directory has one state, so `ref` is meaningless here. Callers wanting
    ;; history from a FileStore should put the directory in git and use
    ;; GitHubStore (or a local git store) instead of inventing versions here.
    (let [f (io/file dir)]
      (when (.isDirectory f) (all-files f))))

  (write! [_ _opts files]
    (let [root (io/file dir)]
      ;; Replace, don't merge: stale files from a previous write would otherwise
      ;; read back as entities that no longer exist.
      (when (.isDirectory root)
        (doseq [^File f (reverse (file-seq root)) :when (not= f root)] (.delete f)))
      (doseq [[path content] files]
        (let [out (io/file root path)]
          (.mkdirs (.getParentFile out))
          (spit out content)))
      {:ref dir})))


(defn file-store [dir] (->FileStore dir))
