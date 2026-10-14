(ns bb-compat
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [edamame.core :as e]))

(def source-roots ["core/src" "store/src" "review/src"])

(defn- ns-name-of [file]
  (let [form (e/parse-string (slurp (str file)) {:all true :read-cond :allow :features #{:clj}})]
    (when (and (seq? form) (= 'ns (first form)))
      (second form))))

(defn namespaces []
  (->> source-roots
       (mapcat #(fs/glob % "**.{clj,cljc}"))
       (keep ns-name-of)
       (into (sorted-set))))

(defn- load-ns [ns-sym]
  (let [{:keys [exit err]} (p/shell {:continue true :out :string :err :string}
                                    "bb" "-e" (str "(require '" ns-sym ")"))]
    {:ns ns-sym
     :loads? (zero? exit)
     :message (some #(when (str/starts-with? % "Message:") (str/trim (subs % 8)))
                    (str/split-lines (or err "")))}))

(defn -main [& _]
  (let [jvm-only (edn/read-string (slurp "bb/jvm-only.edn"))
        found (namespaces)
        results (pmap load-ns found)
        broken (remove #(or (:loads? %) (contains? jvm-only (:ns %))) results)
        stale (filter #(and (:loads? %) (contains? jvm-only (:ns %))) results)
        missing (remove found (keys jvm-only))]
    (println (format "bb %s: %d namespaces under %s, %d listed JVM-only"
                     (System/getProperty "babashka.version") (count found)
                     (str/join ", " source-roots) (count jvm-only)))
    (doseq [{:keys [ns message]} broken]
      (println "FAIL does not load in bb:" ns "-" message))
    (doseq [{:keys [ns]} stale]
      (println "FAIL listed in bb/jvm-only.edn but loads in bb, remove it:" ns))
    (doseq [ns missing]
      (println "FAIL listed in bb/jvm-only.edn but not found, remove it:" ns))
    (if (or (seq broken) (seq stale) (seq missing))
      (System/exit 1)
      (println "OK" (- (count found) (count jvm-only)) "namespaces load in bb"))))
