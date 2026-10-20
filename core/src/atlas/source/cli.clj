(ns atlas.source.cli
  "EXPERIMENTAL (see EXPERIMENTAL.md). Runs under babashka (cheshire and
   clj-kondo as a pod); the JVM never needs it.

   Installed with bbin (core/bb.edn) as the atlas-source command; in an
   atlas checkout it is also `bb source`.

   atlas-source <command> [args] [--root dir] [--registry dir]... [--src dir]...
             [--store dir [--store-prefix p] [--store-ref ref]] [--json] [--no-cache]

   Answers from the project's files, nothing loaded or run: the EDN registry
   files and the register! calls written as data (clj-kondo reads the code);
   then, for what no file says, the REPL's snapshot (.atlas/runtime.edn, kept
   by atlas.source/watch!) and the registry store CI records (a git checkout,
   at --store-ref, default origin/main). Settings come from the flags, or from
   <root>/atlas-source.edn ({:registry [...] :src [...] :store {:dir :prefix
   :ref} :packs {:spec {} :malli {:registries [ns/var ...]}}}). The code analysis is cached in <root>/.atlas/source-index.transit
   and refreshed for the files that changed since the last call."
  (:require [atlas.source.hover :as hover]
            [atlas.source.index :as index]
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
  summary             counts, coverage, files re-analysed by this call
  hover <keyword>     what an editor shows for a keyword: eldoc line, describe text, locations
  diagnostics <file>  check's problems in one file, with the line and columns to underline
  serve               long-lived: one JSON request per line on stdin, one JSON reply per line
                      {\"id\":1,\"op\":\"hover\",\"keyword\":\":fn/x\"} -> {\"id\":1,\"result\":{...}}
                      the index stays in memory and is refreshed from disk before each reply")

(defn- parse [args]
  (loop [[a & more :as args] args opts {:registry [] :src [] :positional []}]
    (cond
      (empty? args) opts
      (= a "--root") (recur (rest more) (assoc opts :root (first more)))
      (= a "--registry") (recur (rest more) (update opts :registry conj (first more)))
      (= a "--src") (recur (rest more) (update opts :src conj (first more)))
      (= a "--store") (recur (rest more) (assoc-in opts [:store :dir] (first more)))
      (= a "--store-prefix") (recur (rest more) (assoc-in opts [:store :prefix] (first more)))
      (= a "--store-ref") (recur (rest more) (assoc-in opts [:store :ref] (first more)))
      (= a "--json") (recur more (assoc opts :json true))
      (= a "--no-cache") (recur more (assoc opts :no-cache true))
      :else (recur more (update opts :positional conj a)))))

(defn- config [{:keys [root registry src no-cache store]}]
  (let [root (or root ".")
        f (io/file root "atlas-source.edn")
        from-file (when (.exists f) (edn/read-string (slurp f)))]
    {:root root
     :registry (if (seq registry) registry (:registry from-file ["registry"]))
     :src (if (seq src) src (:src from-file ["src"]))
     :store (or store (:store from-file))
     :packs (:packs from-file {})
     :dataflow (:dataflow from-file)
     :cache (when-not no-cache (str (io/file root ".atlas" "source-index.transit")))}))

(defn- kw [s] (keyword (str/replace s #"^:" "")))

(defn- serve
  "Answers one JSON request per line until stdin closes. Before each reply
   the index is refreshed from what changed on disk, in memory."
  [cfg]
  (let [idx (atom (index/build (dissoc cfg :cache)))]
    (loop []
      (when-let [line (read-line)]
        (let [{:keys [id op keyword file]} (try (json/parse-string line true) (catch Exception _ nil))
              reply (try
                      (swap! idx #(index/build (assoc (dissoc cfg :cache) :previous %)))
                      {:id id
                       :result (case op
                                 "hover" (hover/lookup @idx (kw keyword))
                                 "check" (index/check @idx)
                                 "diagnostics" (hover/diagnostics @idx file)
                                 "summary" (index/summary @idx)
                                 {:error (str "unknown op " op)})}
                      (catch Exception e {:id id :error (ex-message e)}))]
          (println (json/generate-string reply))
          (flush)
          (recur))))))

(defn -main [& args]
  (let [{[cmd arg] :positional :as opts} (parse args)
        idx (delay (index/build (config opts)))
        out (case cmd
              "where" (or (index/where @idx (kw arg)) {:not-found (kw arg)})
              "uses" (index/uses @idx (kw arg))
              "deps" (or (index/dependencies @idx (kw arg)) {:not-found (kw arg)})
              "data" (index/data-key @idx (kw arg))
              "check" (index/check @idx)
              "diagnostics" (hover/diagnostics @idx arg)
              "summary" (index/summary @idx)
              "hover" (hover/lookup @idx (kw arg))
              "serve" (do (serve (config opts)) (System/exit 0))
              (do (println usage) (System/exit (if cmd 1 0))))]
    (if (:json opts)
      (println (json/generate-string out {:pretty true}))
      (pp/pprint out))))
