(ns atlas.datalog-graph-test
  (:require [clojure.test :refer [deftest is testing]]
            [atlas.registry :as registry]
            [atlas.datalog :as datalog]
            [atlas.datalog.graph :as graph]
            [atlas.ide.trace :as trace]
            [app.pet-shop :as pet-shop]
            [app.cart :as cart]
            [app.calendar-availability :as calendar]
            [app.grain-counter :as grain]))

(def ^:private ontology-nses
  '[atlas.ontology
    atlas.ontology.execution-function
    atlas.ontology.structure-component
    atlas.ontology.interface-endpoint
    atlas.ontology.data-schema
    atlas.ontology.grain])

(defn- load-app! [init-fn]
  (registry/reset-all!)
  (datalog/reset-db-cache!)
  (doseq [n ontology-nses] (require n :reload))
  (init-fn)
  (registry/current-registry))

(def ^:private apps
  {:pet-shop pet-shop/init-registry!
   :cart cart/init-registry!
   :calendar-availability calendar/init-registry!
   :grain-counter grain/init-registry!})

(defn- closure-set [rows] (set rows))

(defn- assert-equivalent [db g]
  (let [ids (sort-by str (datalog/query-all-entities db))]
    (is (= (set ids) (set (graph/entity-ids g))))
    (doseq [id ids]
      (is (= (datalog/query-effective-dependencies db id) (graph/effective-dependencies g id)) (str id " deps"))
      (is (= (datalog/query-effective-dependents db id) (graph/effective-dependents g id)) (str id " dependents"))
      (is (= (datalog/query-entity-aspects db id) (graph/entity-aspects g id)) (str id " aspects"))
      (doseq [hops [nil 1 3]]
        (is (= (closure-set (datalog/query-upstream-closure db id hops))
               (closure-set (graph/upstream-closure g id hops)))
            (str id " upstream " hops))
        (is (= (closure-set (datalog/query-downstream-closure db id hops))
               (closure-set (graph/downstream-closure g id hops)))
            (str id " downstream " hops))))
    (is (= (closure-set (datalog/query-downstream-closure db (set (take 5 ids)) 2))
           (closure-set (graph/downstream-closure g (set (take 5 ids)) 2))))
    (doseq [[min-sim max-r] [[0.5 20] [0.0 100000] [0.8 5]]]
      (is (= (datalog/query-structural-gaps db min-sim max-r)
             (graph/structural-gaps g min-sim max-r))
          (str "structural-gaps " min-sim " " max-r)))
    (is (= (datalog/query-structural-gaps db) (graph/structural-gaps g)))
    ids))

(defn- without-extractor-fns [reg]
  (update-vals reg #(dissoc % :datalog-extractor/fn)))

(deftest graph-matches-datalog-on-live-registry
  (doseq [[app init-fn] apps]
    (testing app
      (let [reg (load-app! init-fn)
            ids (assert-equivalent (datalog/create-db) (graph/for-registry reg))]
        (is (seq ids))
        (is (some #(seq (graph/upstream-closure (graph/for-registry reg) % nil)) ids)
            "registry has edges to traverse")))))

(deftest graph-matches-datalog-on-snapshot
  (doseq [[app init-fn] apps]
    (testing app
      (let [reg (without-extractor-fns (load-app! init-fn))]
        (assert-equivalent (datalog/create-db reg) (graph/from-registry reg))))))

(defn- unique-ids [tree]
  (set (map :dep/dev-id (remove :dep/already-seen? tree))))

(deftest trace-matches-datalog
  (doseq [[app init-fn] apps]
    (testing app
      (load-app! init-fn)
      (let [db (datalog/create-db)]
        (doseq [id (datalog/query-all-entities db)]
          (is (= (datalog/query-effective-dependencies db id) (set (trace/dependencies-of id))) (str id " deps"))
          (is (= (datalog/query-effective-dependents db id) (set (trace/dependents-of id))) (str id " dependents"))
          (is (not (contains? (set (trace/dependencies-of id)) id)) (str id " self-loop"))
          (is (not (contains? (set (trace/dependents-of id)) id)) (str id " self-loop"))
          (is (= (set (map :entity (datalog/query-upstream-closure db id nil))) (unique-ids (trace/recursive-dependencies-of id)))
              (str id " recursive deps"))
          (is (= (set (map :entity (datalog/query-downstream-closure db id nil))) (unique-ids (trace/recursive-dependents-of id)))
              (str id " recursive dependents")))))))
