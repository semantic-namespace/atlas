(ns pathom-snapshot-check
  "Rebuilds test/fixtures/source-packs/snapshots/pathom.edn from real Pathom 3
   and Pathom 2 environments (make_pathom_snapshot.clj, on the JVM) and checks
   that the operations it records are the ones committed. A change in
   Pathom's index shape, or in atlas.source.pack.pathom/snapshot!, fails it.
   Needs the clojure CLI."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.edn :as edn]
            [clojure.string :as str]))

(def fixture "test/fixtures/source-packs")

(defn -main [& _]
  (let [committed (str fixture "/snapshots/pathom.edn")
        before (slurp committed)]
    (try
      (p/shell {:out :string :err :string}
               "clojure" "-Sdeps"
               (pr-str {:paths ["core/src" (str fixture "/src")]
                        :deps {'com.wsscode/pathom3 {:mvn/version "2023.08.22-alpha"}
                               'com.wsscode/pathom {:mvn/version "2.4.0"}}})
               "-M" (str fixture "/make_pathom_snapshot.clj"))
      (let [expected (:entities (edn/read-string before))
            rebuilt (:entities (edn/read-string (slurp committed)))]
        (if (= expected rebuilt)
          (println "pathom-snapshot-check: real Pathom gives the committed" (count rebuilt) "operations")
          (do (println "pathom-snapshot-check: real Pathom gives different operations")
              (println " only committed:" (sort (remove (set (keys rebuilt)) (keys expected))))
              (println " only rebuilt:  " (sort (remove (set (keys expected)) (keys rebuilt))))
              (doseq [[k v] (sort-by key rebuilt) :when (and (contains? expected k) (not= v (expected k)))]
                (println " " k "committed" (expected k) "rebuilt" v))
              (System/exit 1))))
      (finally (spit committed before)))))
