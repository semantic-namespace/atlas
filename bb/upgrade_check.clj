(ns upgrade-check
  "bb upgrade-check --build <fn-or-file> [--project dir] [--from v] [--to v]
                    [--paths p,...] [--probes file] [--out dir]

   Whether moving a project to another atlas version changes what atlas does
   for it. Builds the project's registry with each version, in a fresh JVM,
   and calls the read-only atlas.llm-ide tools on it; then compares the two
   registries entity by entity and the two sets of answers tool by tool.

   --build    how the project builds its registry: a fully qualified fn of no
              args (app.registry/build!), or a .clj file to load
   --project  the project directory (default .)
   --from/--to  atlas versions, default `current` (the project's own deps):
              current | mvn:<version> | local:<path to atlas core> | git:<sha>
   --paths    the project :paths to use, comma separated (as its registry
              build needs them; replaces the deps.edn :paths)
   --probes   a file of [[tool args] ...] to use instead of probes chosen from
              the --from registry
   --out      where the two sides and the report are written (default a temp dir)

   Exit 0 when registry and answers are identical, 1 when anything differs,
   3 when a side fails to run."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.data :as data]
            [clojure.edn :as edn]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.walk :as walk]))

(def atlas-git "https://github.com/semantic-namespace/atlas")

(defn- parse [args]
  (loop [[a v & more :as args] args opts {}]
    (if (empty? args)
      opts
      (recur more (assoc opts (keyword (str/replace a #"^--" "")) v)))))

(defn coordinate
  "The deps.edn coordinate for an atlas version spec, nil for `current`."
  [spec]
  (let [[kind v] (str/split (or spec "current") #":" 2)]
    (case kind
      "current" nil
      "mvn" {:mvn/version v}
      "local" {:local/root (str (fs/absolutize v))}
      "git" {:git/url atlas-git :git/sha v :deps/root "core"}
      (throw (ex-info (str "unknown atlas version " spec ": use current, mvn:<v>, local:<path> or git:<sha>") {})))))

(defn- sdeps [{:keys [paths]} spec]
  (cond-> {}
    paths (assoc :paths (str/split paths #","))
    (coordinate spec) (assoc :deps {'io.github.semantic-namespace/atlas (coordinate spec)})))

(defn- runner []
  (str (fs/path (fs/parent (System/getProperty "babashka.config")) "bb" "upgrade_runner.clj")))

(defn- build-spec [b]
  (if (str/ends-with? b ".clj") {:file (str (fs/absolutize b))} {:fn (symbol b)}))

(defn- run-side!
  "Runs one side; true when it wrote its results."
  [{:keys [project build] :as opts} spec dir probes]
  (fs/create-dirs dir)
  (let [log (str (fs/path dir "run.log"))
        args (pr-str (cond-> {:build (build-spec build) :out (str dir)} probes (assoc :probes (str probes))))
        cmd ["clojure" "-Sdeps" (pr-str (sdeps opts spec)) "-M" (runner) args]
        t0 (System/currentTimeMillis)
        {:keys [exit]} @(apply p/process {:dir project :out log :err log} cmd)]
    (println (format "  %-28s %s  (%.0fs)" (or spec "current") (if (zero? exit) "ok" (str "FAILED, see " log))
                     (/ (- (System/currentTimeMillis) t0) 1000.0)))
    (and (zero? exit) (fs/exists? (fs/path dir "probes.edn")))))

(defn- rd [f] (edn/read-string {:default (fn [_ v] v)} (slurp (str f))))

;; --- comparing ---------------------------------------------------------------

(defn- stable
  "x with the values a build makes fresh each time set aside: UUIDs, instants,
   and runs of 9 or more digits inside strings and symbols (timestamps and
   counters glued onto ids, like \"grant-793828266765548\")."
  [x]
  (let [blank #(str/replace % #"\d{9,}" "#")]
    (walk/postwalk #(cond
                      (uuid? %) ::uuid
                      (inst? %) ::inst
                      (string? %) (blank %)
                      (symbol? %) (symbol (blank (str %)))
                      :else %)
                   x)))

(defn registry-delta
  "Two registries compared by dev-id."
  [a b]
  (let [by-id (fn [reg] (into {} (for [[cid p] reg] [(or (:atlas/dev-id p) cid) {:cid (set cid) :props (dissoc p :atlas/dev-id)}])))
        a (by-id a) b (by-id b)
        both (filter (set (keys b)) (keys a))]
    {:entities [(count a) (count b)]
     :only-from (vec (sort-by pr-str (remove (set (keys b)) (keys a))))
     :only-to (vec (sort-by pr-str (remove (set (keys a)) (keys b))))
     :identity-changed (vec (for [id (sort-by pr-str both) :when (not= (:cid (a id)) (:cid (b id)))]
                              {:dev-id id
                               :aspects-removed (sort-by pr-str (set/difference (:cid (a id)) (:cid (b id))))
                               :aspects-added (sort-by pr-str (set/difference (:cid (b id)) (:cid (a id))))}))
     ;; a prop that differs only in UUIDs and instants is made fresh by the
     ;; build itself, not changed by atlas: reported apart, not a difference
     :props-changed (vec (for [id (sort-by pr-str both)
                               :let [pa (:props (a id)) pb (:props (b id))
                                     ks (for [k (set/union (set (keys pa)) (set (keys pb)))
                                              :when (not= (stable (get pa k)) (stable (get pb k)))] k)]
                               :when (seq ks)]
                           {:dev-id id :keys (vec (sort-by pr-str ks))}))
     :volatile (vec (for [id (sort-by pr-str both)
                          :let [pa (:props (a id)) pb (:props (b id))
                                ks (for [k (set/union (set (keys pa)) (set (keys pb)))
                                         :when (and (not= (get pa k) (get pb k)) (= (stable (get pa k)) (stable (get pb k))))] k)]
                          :when (seq ks)]
                      {:dev-id id :keys (vec (sort-by pr-str ks))}))}))

(defn- paths
  "Where one side of clojure.data/diff has something, to depth 3."
  [x prefix]
  (cond
    (and (map? x) (< (count prefix) 3)) (mapcat (fn [[k v]] (paths v (conj prefix k))) x)
    (and (sequential? x) (< (count prefix) 3) (some some? x)) (mapcat (fn [i v] (when (some? v) (paths v (conj prefix i)))) (range) x)
    (some? x) [prefix]
    :else []))

(defn probe-delta
  "Two sets of tool answers compared tool by tool."
  [a b]
  (vec (for [t (sort (into (set (keys a)) (keys b)))
             :let [ra (get-in a [t :result]) rb (get-in b [t :result])
                   same? (= (stable ra) (stable rb))
                   [only-a only-b] (data/diff (stable ra) (stable rb))]]
         (cond-> {:tool t :same? same? :ms [(get-in a [t :ms]) (get-in b [t :ms])]}
           (and same? (not= ra rb)) (assoc :volatile? true)
           (not same?) (assoc :differs-at (vec (take 6 (distinct (concat (paths only-a []) (paths only-b []))))))
           (or (:thrown ra) (:thrown rb)) (assoc :thrown [(:message ra) (:message rb)])))))

(defn- print-report [{:keys [from to registry probes]}]
  (let [{:keys [entities only-from only-to identity-changed props-changed]} registry
        same-reg? (every? empty? [only-from only-to identity-changed props-changed])]
    (println)
    (println (format "registry  %s: %d entities  %s: %d entities  -> %s" from (first entities) to (second entities)
                     (if same-reg? "identical" "DIFFERENT")))
    (when (seq only-from) (println "  only with" from ":" (count only-from) (take 10 only-from)))
    (when (seq only-to) (println "  only with" to ":" (count only-to) (take 10 only-to)))
    (when (seq identity-changed) (println "  compound identity changed:" (count identity-changed) (take 5 identity-changed)))
    (when (seq props-changed)
      (println "  props changed:" (count props-changed) " by key:" (frequencies (mapcat :keys props-changed))))
    (when (seq (:volatile registry))
      (println "  (not a difference) props made fresh by each build, differing only in UUIDs/instants:"
               (count (:volatile registry)) "entities, keys" (vec (distinct (mapcat :keys (:volatile registry))))))
    (println)
    (println (format "%-24s %-6s %9s %9s  %s" "tool" "same?" "ms from" "ms to" "differs at"))
    (doseq [{:keys [tool same? volatile? ms differs-at thrown]} probes]
      (println (format "%-24s %-6s %9.1f %9.1f  %s" (name tool) (if same? "yes" "NO") (double (or (first ms) 0)) (double (or (second ms) 0))
                       (str (when volatile? "(only fresh values differ)")
                            (when differs-at (pr-str (take 3 differs-at))) (when thrown (str " thrown: " (pr-str thrown)))))))
    (println)
    (println (format "answers: %d of %d identical; probe time %.0f ms -> %.0f ms"
                     (count (filter :same? probes)) (count probes)
                     (reduce + (keep (comp first :ms) probes)) (reduce + (keep (comp second :ms) probes))))
    (and same-reg? (every? :same? probes))))

(defn -main [& args]
  (let [opts (merge {:project "." :from "current" :to "current"} (parse args))
        _ (when-not (:build opts) (println "--build is required: a qualified fn or a .clj file") (System/exit 3))
        opts (update opts :project #(str (fs/absolutize %)))
        out (fs/path (or (:out opts) (fs/create-temp-dir {:prefix "atlas-upgrade-check"})))
        a (fs/path out "from") b (fs/path out "to")]
    (println "atlas upgrade check for" (:project opts) "->" (str out))
    (when-not (run-side! opts (:from opts) a (:probes opts)) (System/exit 3))
    (when-not (run-side! opts (:to opts) b (or (:probes opts) (fs/path a "probe-args.edn"))) (System/exit 3))
    (let [report {:from (:from opts) :to (:to opts)
                  :registry (registry-delta (rd (fs/path a "registry.edn")) (rd (fs/path b "registry.edn")))
                  :probes (probe-delta (rd (fs/path a "probes.edn")) (rd (fs/path b "probes.edn")))}]
      (spit (str (fs/path out "report.edn")) (pr-str report))
      (System/exit (if (print-report report) 0 1)))))
