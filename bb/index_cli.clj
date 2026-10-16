(ns index-cli
  "bb index <command> [args] [--root dir] [--registry dir]... [--src dir]... [--json] [--no-cache]

   Answers from the project's files: the EDN registry and the source, read by
   clj-kondo. Nothing is loaded or run. Directories come from the flags, or
   from <root>/atlas-index.edn ({:registry [...] :src [...]}). The code
   analysis is cached in <root>/.atlas/index-cache.transit and refreshed for the
   files that changed since the last call."
  (:require [atlas.index :as index]
            [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.string :as str]))

(def usage
  "Commands:
  where <dev-id>      where it is defined (EDN entry) and implemented (var), with its entry
  uses <dev-id>       entries that name it, and code that calls its var
  deps <dev-id>       entities its entry names
  data <data-key>     entities producing and consuming the key
  check               where code and registry files disagree
  summary             counts, coverage, files re-analysed by this call")

(defn- parse [args]
  (loop [[a & more :as args] args opts {:registry [] :src [] :positional []}]
    (cond
      (empty? args) opts
      (= a "--root") (recur (rest more) (assoc opts :root (first more)))
      (= a "--registry") (recur (rest more) (update opts :registry conj (first more)))
      (= a "--src") (recur (rest more) (update opts :src conj (first more)))
      (= a "--json") (recur more (assoc opts :json true))
      (= a "--no-cache") (recur more (assoc opts :no-cache true))
      :else (recur more (update opts :positional conj a)))))

(defn- config [{:keys [root registry src no-cache]}]
  (let [root (or root ".")
        f (io/file root "atlas-index.edn")
        from-file (when (.exists f) (edn/read-string (slurp f)))]
    {:root root
     :registry (if (seq registry) registry (:registry from-file ["registry"]))
     :src (if (seq src) src (:src from-file ["src"]))
     :cache (when-not no-cache (str (io/file root ".atlas" "index-cache.transit")))}))

(defn- kw [s] (keyword (str/replace s #"^:" "")))

(defn -main [& args]
  (let [{[cmd arg] :positional :as opts} (parse args)
        idx (delay (index/build (config opts)))
        out (case cmd
              "where" (or (index/where @idx (kw arg)) {:not-found (kw arg)})
              "uses" (index/uses @idx (kw arg))
              "deps" (or (index/dependencies @idx (kw arg)) {:not-found (kw arg)})
              "data" (index/data-key @idx (kw arg))
              "check" (index/check @idx)
              "summary" (index/summary @idx)
              (do (println usage) (System/exit (if cmd 1 0))))]
    (if (:json opts)
      (println (json/generate-string out {:pretty true}))
      (pp/pprint out))))
