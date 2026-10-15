(ns atlas.history-facts-test
  (:require [clojure.test :refer [deftest testing is]]
            [atlas.registry :as registry]
            [atlas.datalog :as datalog]
            [atlas.history :as history]
            [atlas.history.facts :as facts]
            [app.pet-shop :as pet-shop]
            [app.cart :as cart]
            [app.calendar-availability :as calendar]))

(defn- registry-of
  [& init-fns]
  (registry/reset-all!)
  (datalog/reset-db-cache!)
  (require 'atlas.ontology :reload)
  (require 'atlas.ontology.execution-function :reload)
  (require 'atlas.ontology.structure-component :reload)
  (require 'atlas.ontology.interface-endpoint :reload)
  (require 'atlas.ontology.data-schema :reload)
  (doseq [f init-fns] (f))
  (let [reg @registry/registry]
    (registry/reset-all!)
    (datalog/reset-db-cache!)
    reg))

(defn- by-type
  [reg etype]
  (->> reg
       (filter (fn [[_ p]] (= etype (:atlas/type p))))
       (sort-by (comp str :atlas/dev-id val))))

(defn- rename
  [reg [cid props] new-dev-id]
  (-> reg (dissoc cid) (assoc cid (assoc props :atlas/dev-id new-dev-id))))

(defn- retype
  [reg [cid props] new-type]
  (-> reg
      (dissoc cid)
      (assoc (-> cid (disj (:atlas/type props)) (conj new-type))
             (assoc props :atlas/type new-type))))

(defn- reaspect
  [reg [cid props] drop-aspect add-aspect]
  (-> reg (dissoc cid) (assoc (-> cid (disj drop-aspect) (conj add-aspect)) props)))

(defn- versions-fixture
  []
  (let [v1       (registry-of pet-shop/init-registry!)
        both     (registry-of pet-shop/init-registry! cart/init-registry!)
        fns      (by-type both :atlas/execution-function)
        schemas  (by-type both :atlas/data-schema)
        [f1 f2]  fns
        [s1 s2]  schemas
        renamed  (keyword "renamed" (name (:atlas/dev-id (val f2))))
        v2       (-> both
                     (dissoc (key s2))
                     (rename f2 renamed)
                     (retype s1 :atlas/structure-component)
                     (reaspect f1 (first (disj (key f1) :atlas/execution-function)) :tier/test-only))
        v3       (registry-of calendar/init-registry! cart/init-registry!)]
    [["v1" v1 nil nil]
     ["v2" v2 v1 {(:atlas/dev-id (val f2)) renamed}]
     ["v3" v3 v2 nil]]))

(defn- build-both
  [versions]
  (let [conn (history/create-conn)
        db   (reduce (fn [db [v reg prev renames]]
                       (history/snapshot-version! conn reg v prev renames)
                       (facts/snapshot db reg v prev renames))
                     facts/empty-db
                     versions)]
    [@conn db]))

(deftest version-facts-feed-snapshot-version
  (let [[[_ v1] [_ v2 _ renames]] (versions-fixture)
        conn   (history/create-conn)
        result (history/snapshot-version! conn v2 "v2" v1 renames)
        {:keys [snaps props]} (facts/version-facts v2 "v2" v1 renames)]
    (is (= (:changed result) (count (filter :snap/added snaps))))
    (is (= (:deleted result) (count (filter :snap/deleted snaps))))
    (is (= (:properties result) (count props)))
    (is (pos? (count props)))))

(deftest facts-queries-match-datascript
  (let [versions    (versions-fixture)
        [ds db]     (build-both versions)
        labels      (map first versions)
        pairs       (partition 2 1 labels)
        dev-ids     (->> versions (mapcat (comp vals second)) (map :atlas/dev-id) distinct)]
    (testing "fixture exercises every fact kind"
      (is (seq (history/version-diff ds "v2")))
      (is (seq (history/version-deleted ds "v2")))
      (is (seq (history/version-renames ds "v2")))
      (is (some :snap/prev-type (history/version-diff ds "v2")))
      (is (seq (:added (history/property-diff ds "v1" "v2"))))
      (is (seq (:removed (history/property-diff ds "v2" "v3")))))
    (is (= (history/versions ds) (facts/versions db)))
    (doseq [v labels]
      (testing v
        (is (= (history/version-diff ds v) (facts/version-diff db v)))
        (is (= (history/version-deleted ds v) (facts/version-deleted db v)))
        (is (= (history/version-renames ds v) (facts/version-renames db v)))
        (is (= (history/version-summary ds v) (facts/version-summary db v)))
        (is (= (history/vocabulary-at-version ds v) (facts/vocabulary-at-version db v)))))
    (doseq [[a b] pairs]
      (testing (str a ".." b)
        (is (= (history/vocabulary-diff ds a b) (facts/vocabulary-diff db a b)))
        (is (= (history/property-diff ds a b) (facts/property-diff db a b)))))
    (testing "timelines"
      (is (= (map #(history/entity-timeline ds %) dev-ids)
             (map #(facts/entity-timeline db %) dev-ids)))
      (is (= (map #(history/full-timeline ds %) dev-ids)
             (map #(facts/full-timeline db %) dev-ids)))
      (is (< 2 (count (facts/full-timeline db (-> (facts/version-renames db "v2") first :snap/dev-id))))))))
