(ns upgrade-check-smoke
  "Runs bb upgrade-check on the cart example (test/app/cart.clj): the same
   atlas on both sides is identical; an atlas whose register! adds a prop is
   caught and named; a --probes file replaces the chosen probes. Needs the
   clojure CLI: each side is a JVM."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.edn :as edn]
            [clojure.string :as str]))

(def failures (atom 0))

(defn- check [what ok?]
  (println (if ok? "ok  " "FAIL") what)
  (when-not ok? (swap! failures inc)))

(defn- upgrade-check [out & args]
  (let [{:keys [exit]} @(apply p/process {:out :inherit :err :inherit} "bb" "upgrade-check"
                               "--build" "app.cart/init-registry!" "--paths" "test" "--out" (str out) args)]
    {:exit exit
     :report (let [f (fs/path out "report.edn")] (when (fs/exists? f) (edn/read-string (slurp (str f)))))}))

(defn -main [& _]
  (let [dir (fs/create-temp-dir {:prefix "atlas-upgrade-check-smoke"})
        patched (fs/path dir "patched-core")]
    (try
      (let [{:keys [exit report]} (upgrade-check (fs/path dir "same") "--from" "local:core" "--to" "local:core")]
        (check "the same atlas on both sides: exit 0" (zero? exit))
        (check "the same atlas on both sides: every answer identical" (every? :same? (:probes report)))
        (check "probes were chosen and run" (< 20 (count (:probes report)))))

      ;; an atlas whose register! stamps every entity with an extra prop
      (fs/copy-tree "core" patched)
      (let [f (str (fs/path patched "src" "atlas" "registry.cljc"))
            s (slurp f)
            from "value-with-meta (assoc value :atlas/dev-id dev-id :atlas/type type)"]
        (when-not (str/includes? s from) (throw (ex-info "register! changed; update the smoke patch" {:file f})))
        (spit f (str/replace s from "value-with-meta (assoc value :atlas/dev-id dev-id :atlas/type type :smoke/patched true)")))
      (let [{:keys [exit report]} (upgrade-check (fs/path dir "patched") "--from" "local:core" "--to" (str "local:" patched))]
        (check "a changed atlas: exit 1" (= 1 exit))
        (check "the report names the prop it added"
               (= #{:smoke/patched} (set (mapcat :keys (get-in report [:registry :props-changed])))))
        (check "every entity is counted as changed"
               (= (first (get-in report [:registry :entities])) (count (get-in report [:registry :props-changed])))))

      ;; an atlas whose register! stamps the time: fresh on each build, not a change
      (let [stamped (fs/path dir "stamped-core")]
        (fs/copy-tree "core" stamped)
        (let [f (str (fs/path stamped "src" "atlas" "registry.cljc"))]
          (spit f (str/replace (slurp f) "value-with-meta (assoc value :atlas/dev-id dev-id :atlas/type type)"
                               "value-with-meta (assoc value :atlas/dev-id dev-id :atlas/type type :smoke/at (java.util.Date.) :smoke/id (str \"run-\" (System/nanoTime)))")))
        (let [{:keys [exit report]} (upgrade-check (fs/path dir "stamped") "--from" (str "local:" stamped) "--to" (str "local:" stamped))]
          (check "values a build makes fresh (instants, nano ids) are volatile, not changes: exit 0"
                 (and (zero? exit) (empty? (get-in report [:registry :props-changed]))
                      (= #{:smoke/at :smoke/id} (set (mapcat :keys (get-in report [:registry :volatile]))))))))

      (let [probes (fs/path dir "probes.edn")]
        (spit (str probes) "[[:bottlenecks {}] [:list-entity-types {}]]")
        (let [{:keys [exit report]} (upgrade-check (fs/path dir "probes") "--from" "local:core" "--to" "local:core" "--probes" (str probes))]
          (check "a --probes file: only those tools are called"
                 (and (zero? exit) (= [:bottlenecks :list-entity-types] (mapv :tool (:probes report)))))))
      (finally (fs/delete-tree dir)))
    (if (pos? @failures)
      (do (println @failures "failed") (System/exit 1))
      (println "OK"))))
