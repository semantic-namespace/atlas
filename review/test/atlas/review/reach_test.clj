(ns atlas.review.reach-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [atlas.review.reach :as reach]))

(def files
  {"src/app/port.clj" "(ns app.port)\n(defprotocol Store (save! [s x]))\n(defprotocol Mail (send! [m x]))\n"
   "src/app/pg.clj" "(ns app.pg (:require [app.port :as port] [next.jdbc :as jdbc]))\n(defrecord Pg [ds]\n  port/Store\n  (save! [_ x] (jdbc/execute! ds [x])))\n"
   "src/app/mail.clj" "(ns app.mail (:require [app.port :as port] [clj-http.client :as http]))\n(defrecord A [] port/Mail (send! [_ x] (http/post \"a\" x)))\n(defrecord B [] port/Mail (send! [_ x] (http/post \"b\" x)))\n"
   "src/app/q.clj" "(ns app.q (:require [hugsql.core :as hugsql]))\n(hugsql/def-db-fns \"app/q.sql\")\n(defn lookup [db id] (db-find-by-id db {:id id}))\n"
   "src/app/fns.clj" "(ns app.fns (:require [app.port :as port] [app.q] [atlas.registry :as registry]))\n(defn persist [s x] (port/save! s x))\n(registry/register! :fn/save :atlas/execution-function #{:domain/x}\n  {:atlas/impl (fn [{:keys [s x]}] (persist s x))})\n(registry/register! :fn/caller :atlas/execution-function #{:domain/y}\n  {:atlas/impl (fn [arg] (exec :fn/save arg))})\n(registry/register! :fn/find :atlas/execution-function #{:domain/z}\n  {:atlas/impl (fn [{:keys [db id]}] (app.q/lookup db id))})\n(registry/register! :fn/notify :atlas/execution-function #{:services/postgres}\n  {:atlas/impl (fn [{:keys [m x]}] (port/send! m x))})\n"})

(defn- project []
  (let [root (.toFile (java.nio.file.Files/createTempDirectory "reach" (make-array java.nio.file.attribute.FileAttribute 0)))]
    (doseq [[p s] files] (let [f (io/file root p)] (io/make-parents f) (spit f s)))
    (str root)))

(def config {:registration-heads #"register!$" :component-heads #"init-key$"
             :sinks {:postgres #"^next\.jdbc" :http #"^clj-http"} :services {} :java-sinks {}
             :generators {:postgres #"^hugsql\.core/def-db-fns"} :opaque #"^$"})

(deftest own-io-follows-calls-single-impl-protocols-and-keywords
  (let [rows (into {} (map (juxt :id identity)) (reach/compare-aspects (reach/graph (project) ["src"] config)
                                                                       {:postgres #{:services/postgres}}
                                                                       some?))]
    (is (contains? (:reached (rows :fn/save)) :postgres) "through a helper and a protocol with one implementation")
    (is (= [:postgres] (:undeclared (rows :fn/save))))
    (is (not (contains? (:reached (rows :fn/caller)) :postgres)) "another entity's I/O is not its own")
    (is (contains? (:transitive (rows :fn/caller)) :postgres) "but it reaches it through that entity")
    (is (not (contains? (:reached (rows :fn/notify)) :http)) "two implementations: only may, not does")
    (is (contains? (:may (rows :fn/notify)) :http))
    (is (= [:postgres] (:unreached (rows :fn/notify))) "declared but not done")
    (is (contains? (:reached (rows :fn/find)) :postgres) "a call to a function hugsql defines at load time")))
