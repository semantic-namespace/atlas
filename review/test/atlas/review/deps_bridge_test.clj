(ns atlas.review.deps-bridge-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [atlas.review.deps-bridge :as bridge]
            [sdiff.deps :as deps]))

(def files
  {"src/app/fns.clj" "(ns app.fns (:require [atlas.registry :as registry] [next.jdbc :as jdbc]))\n(registry/register! :fn/save :atlas/execution-function #{:x}\n  {:execution-function/deps #{:app/db}\n   :atlas/impl (fn [{:keys [ds]}] (jdbc/execute! ds [\"x\"]))})\n(defn caller [arg] (exec :fn/save arg))\n"
   "src/app/sys.clj" "(ns app.sys (:require [integrant.core :as ig] [taoensso.carmine :as car]))\n(defmethod ig/init-key :app/db [_ opts] (car/wcar opts (car/ping)))\n"})

(defn- project []
  (let [root (.toFile (java.nio.file.Files/createTempDirectory "bridge" (make-array java.nio.file.attribute.FileAttribute 0)))]
    (doseq [[p s] files] (let [f (io/file root p)] (io/make-parents f) (spit f s)))
    (str root)))

(deftest keywords-become-calls-to-what-defines-them
  (bridge/install! #"(^|/)(register!|init-key)$")
  (let [g (deps/graph (project) deps/defaults)
        node (fn [pred] (some (fn [[k v]] (when (pred v) k)) (:forms g)))
        caller (node #(= "app.fns/caller" (:var %)))
        reg (node #(re-find #"register! :fn/save" (:name %)))
        init (node #(re-find #"defmethod ig/init-key :app/db" (:name %)))]
    (is (contains? (get-in g [:calls caller]) reg) "an exec-fn call by dev-id")
    (is (contains? (get-in g [:calls reg]) init) "an entity's deps name an integrant component")
    (is (contains? (get-in g [:io init]) :redis))))
