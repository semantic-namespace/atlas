;; One side of `bb upgrade-check`, run in a fresh JVM inside the project:
;;
;;   clojure -Sdeps '<atlas version>' -M bb/upgrade_runner.clj '<options EDN>'
;;
;; Options: {:build {:fn qualified-symbol} | {:file path}
;;           :out dir
;;           :probes path | nil}   ; nil: choose the probe arguments from this registry
;;
;; Builds the project's registry, writes it to <out>/registry.edn (sanitised:
;; no fns), then calls the read-only atlas.llm-ide tools and writes
;; <out>/probes.edn ({tool {:args :result :ms}}) and <out>/probe-args.edn.
;;
;; Only APIs that atlas has had since 0.1.13 are used, so the same script runs
;; against the version being upgraded from.

(require '[clojure.edn :as edn]
         '[clojure.java.io :as io]
         '[clojure.walk :as walk]
         '[atlas.registry :as registry]
         '[atlas.registry.serialise :as serialise])

(def opts (edn/read-string (first *command-line-args*)))
(def out (io/file (:out opts)))
(.mkdirs out)

(defn- by-print [a b] (compare (pr-str a) (pr-str b)))

(defn- data?
  "True for values EDN can hold."
  [v]
  (or (nil? v) (boolean? v) (number? v) (string? v) (keyword? v) (symbol? v)
      (char? v) (uuid? v) (inst? v) (coll? v)))

(defn canon
  "x as data that prints the same whenever it is equal: every map and set
   sorted by how its members print, and values that aren't data named
   instead of printed. A fn prints with a class name and hash code made fresh
   on every run (fn__57048@4b426ae9), so it becomes `fn`; a var becomes its
   name; any other object `object:<class>`."
  [x]
  (walk/postwalk (fn [v]
                   (cond
                     (fn? v) 'fn
                     (var? v) (symbol (subs (str v) 2))
                     (not (data? v)) (symbol (str "object:" (.getName (class v))))
                     (record? v) v
                     (map? v) (into (sorted-map-by by-print) v)
                     (set? v) (into (sorted-set-by by-print) v)
                     :else v))
                 x))

(defn- write! [file x]
  (spit (io/file out file)
        (binding [*print-namespace-maps* false *print-length* nil *print-level* nil]
          (pr-str x))))

;; --- build -------------------------------------------------------------------

(let [{build-fn :fn file :file} (:build opts)]
  (cond
    build-fn ((requiring-resolve build-fn))
    file (load-file file)
    :else (throw (ex-info "options need :build {:fn sym} or {:file path}" opts))))

;; written before atlas.llm-ide is loaded for the probes, so the registry is
;; the project's own, with or without the tools
(def reg (canon (serialise/sanitize @registry/registry)))
(write! "registry.edn" reg)
(println "registry:" (count reg) "entities")

;; --- probes ------------------------------------------------------------------

(require '[atlas.registry.lookup :as lookup] 'atlas.llm-ide)

(defn- most
  "The n most frequent of xs, ties broken by how they print."
  [n xs]
  (map key (take n (sort-by (juxt (comp - val) (comp pr-str key)) (frequencies xs)))))

(defn choose-probes
  "Arguments for each read-only atlas.llm-ide tool, chosen from the registry
   so any project gets meaningful ones: its best-connected functions, its most
   used data keys and aspects."
  [reg]
  (let [ents (for [[cid p] reg] (assoc p ::cid (set cid)))
        efs (filter #(= :atlas/execution-function (:atlas/type %)) ents)
        fns (vec (or (seq (map :atlas/dev-id (take 3 (sort-by (juxt (comp - count :execution-function/deps) (comp pr-str :atlas/dev-id)) efs))))
                     (map :atlas/dev-id (take 3 (sort-by (comp pr-str :atlas/dev-id) ents)))))
        f1 (first fns)
        consumed (or (first (most 1 (mapcat :execution-function/context efs))) :entity/dev-id)
        produced (or (first (most 1 (mapcat :execution-function/response efs))) consumed)
        [aspect aspect2] (concat (most 2 (remove #(= "atlas" (namespace %)) (mapcat ::cid ents))) [:tier/service :tier/foundation])
        type (if (seq efs) :atlas/execution-function (or (first (most 1 (keep :atlas/type ents))) :atlas/execution-function))
        fn-set (set fns)
        risk {:entity/dev-id-set fn-set :risk/centrality-threshold 5 :aspect/domain-namespace "domain"
              :aspect/high-risk aspect :aspect/external-integration :integration/external}]
    [[:aggregate-by-aspect {:query/aspect aspect}]
     [:aspect-anomalies {}]
     [:batch-blast-radius {:entity/dev-id-set fn-set :query/max-hops 3}]
     [:batch-change-risk risk]
     [:batch-entity-detail {:entity/dev-id-set fn-set}]
     [:blast-radius {:entity/dev-id-or-set f1 :query/max-hops 3}]
     [:bottlenecks {}]
     [:by-aspect {:query/aspect aspect2}]
     [:change-risk risk]
     [:compare-entities {:entity/dev-id-set fn-set}]
     [:consumers-of {:data/key consumed}]
     [:data-flow {:entity/dev-id f1}]
     [:entities-by-all-types {}]
     [:entities-by-type {:entity/type type}]
     [:entity-detail {:entity/dev-id f1}]
     [:entity-snapshot-fast {:entity/dev-id f1}]
     [:explain-area {:query/focus aspect :query/depth 2 :aspect/entry-point :tier/api :aspect/key-component :tier/foundation}]
     [:islands {}]
     [:list-entity-types {}]
     [:ontology-info {:entity/type type}]
     [:orphans {:aspect/exclude-from-orphan-check #{} :aspect/type-namespace "atlas"}]
     [:producers-of {:data/key produced}]
     [:structural-gaps {}]
     [:suggest-placement {:entity/intended-aspects #{aspect aspect2} :dataflow/consumes [consumed] :dataflow/produces [produced]}]
     [:trace-causes {:symptom/dev-id f1 :query/max-hops 3}]
     [:trace-data-key {:data/key consumed}]]))

(def probes
  (if-let [p (:probes opts)]
    (edn/read-string (slurp p))
    (choose-probes reg)))
(write! "probe-args.edn" probes)

(defn call [tool args]
  (try (lookup/handle-tool {:tool/name (keyword "atlas.llm-ide" (name tool)) :tool/args args})
       (catch Throwable t {:thrown (.getName (class t)) :message (.getMessage t)})))

(write! "probes.edn"
        (into (sorted-map)
              (for [[tool args] probes]
                (do (call tool args) ;; warm
                    (let [t0 (System/nanoTime)
                          r (call tool args)]
                      [tool {:args args :result (canon r) :ms (/ (- (System/nanoTime) t0) 1e6)}])))))
(println "probes:" (count probes))
(shutdown-agents)
(System/exit 0)
