(ns build
  "Build script for compound-identity library"
  (:require [clojure.tools.build.api :as b]))

(def lib 'com.github.semantic-namespace/atlas)
(def version "0.0.1-SNAPSHOT")
(def class-dir "target/classes")
(def basis (b/create-basis {:project "deps.edn"}))
(def jar-file (format "target/%s-%s.jar" (name lib) version))

(defn clean [_]
  (b/delete {:path "target"}))

(defn jar [_]
  (clean nil)
  (b/write-pom {:class-dir class-dir
                :lib lib
                :version version
                :basis basis
                :src-dirs ["src"]
                :scm {:url "https://github.com/semantic-namespace/atlas"
                      :connection "scm:git:git://github.com/semantic-namespace/atlas.git"
                      :developerConnection "scm:git:ssh://git@github.com/semantic-namespace/atlas.git"
                      :tag (str "v" version)}
                :pom-data [[:description "Atlas: semantic compound identity registry and invariant checking"]
                           [:url "https://github.com/semantic-namespace/atlas"]
                           [:licenses
                            [:license
                             [:name "MIT License"]
                             [:url "https://opensource.org/licenses/MIT"]]]]})
  (b/copy-dir {:src-dirs ["src" "resources"]
               :target-dir class-dir})
  (b/jar {:class-dir class-dir
          :jar-file jar-file})
  (println (str "Built JAR: " jar-file)))

(defn install [_]
  (jar nil)
  (b/install {:basis basis
              :lib lib
              :version version
              :jar-file jar-file
              :class-dir class-dir})
  (println "Installed to local Maven repo"))
