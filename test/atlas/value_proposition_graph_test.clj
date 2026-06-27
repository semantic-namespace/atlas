(ns atlas.value-proposition-graph-test
  "Tests that value-proposition and business-pattern entities are properly
   connected via :entity/depends edges in the datascript graph, enabling
   blast-radius and trace-causes traversal."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [atlas.registry :as registry]
            [atlas.registry.lookup :as lookup]
            [atlas.registry.graph :as graph]
            [atlas.ontology :as ontology]
            [atlas.datalog :as datalog]
            [atlas.ontology.value-proposition]
            [atlas.ontology.business-pattern]
            [atlas.ontology.execution-function]
            [datascript.core :as d]))

;; ---------------------------------------------------------------------------
;; Fixtures
;; ---------------------------------------------------------------------------

(use-fixtures :each
  (fn [f]
    (registry/reset-all!)
    (datalog/reset-db-cache!)
    (require 'atlas.ontology.value-proposition :reload)
    (require 'atlas.ontology.business-pattern :reload)
    (require 'atlas.ontology.execution-function :reload)
    (f)
    (registry/reset-all!)
    (datalog/reset-db-cache!)))

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defn- seed-bp! []
  (registry/register!
   :business-pattern/inbox-scanning
   :atlas/business-pattern
   #{:atlas/business-pattern :domain/logins :domain/mailing-lists}
   {:business-pattern/principle "Inbox is ground truth for digital services"
    :business-pattern/business-value "Comprehensive discovery"}))

(defn- seed-endpoints! []
  (registry/register!
   :endpoint/logins-search
   :atlas/yorba-endpoint
   #{:atlas/yorba-endpoint :domain/logins}
   {:endpoint/method :get
    :endpoint/deps []
    :endpoint/input []
    :endpoint/output []
    :endpoint/serialisation :serial/logins})

  (registry/register!
   :endpoint/inbox-provider
   :atlas/yorba-endpoint
   #{:atlas/yorba-endpoint :domain/inbox}
   {:endpoint/method :get
    :endpoint/deps []
    :endpoint/input []
    :endpoint/output []
    :endpoint/serialisation :serial/inbox-provider}))

(defn- seed-vp! []
  (registry/register!
   :value-proposition/login-detection
   :atlas/value-proposition
   #{:atlas/value-proposition :domain/logins :scope/inbox}
   {:value-proposition/business-problem "Users don't know their accounts"
    :value-proposition/solution "Inbox scan"
    :value-proposition/implements-pattern :business-pattern/inbox-scanning
    :value-proposition/external-boundaries [:endpoint/logins-search
                                            :endpoint/inbox-provider]}))

;; ---------------------------------------------------------------------------
;; Tests: generic-extract-facts
;; ---------------------------------------------------------------------------

(deftest generic-extract-facts-uses-atlas-type
  (testing ":atlas/type from props is used — not compound-id scanning"
    (seed-bp!)
    (seed-vp!)
    (registry/compile!)
    (let [reg (registry/current-registry)
          ;; Get the compiled VP entity
          vp-compound-id (lookup/identity-for :value-proposition/login-detection)
          vp-props (lookup/props-for :value-proposition/login-detection)]
      ;; The compiled compound-id inherits :atlas/business-pattern from the BP
      (is (contains? vp-compound-id :atlas/business-pattern)
          "VP compound-id should contain :atlas/business-pattern via inheritance")
      (is (= :atlas/value-proposition (:atlas/type vp-props))
          ":atlas/type in props is :atlas/value-proposition, not :atlas/business-pattern")
      ;; generic-extract-facts should find VP type-refs, not BP type-refs
      (let [facts (datalog/generic-extract-facts reg vp-compound-id vp-props)
            depends-facts (filter #(= :entity/depends (nth % 2)) facts)]
        (is (seq depends-facts)
            "generic-extract-facts should produce :entity/depends facts for VP")
        (is (some #(= :business-pattern/inbox-scanning (nth % 3)) depends-facts)
            "VP implements-pattern should produce :entity/depends -> BP")
        (is (some #(= :endpoint/logins-search (nth % 3)) depends-facts)
            "VP external-boundaries should produce :entity/depends -> endpoint")))))

;; ---------------------------------------------------------------------------
;; Tests: datascript DB via create-db (snapshot path)
;; ---------------------------------------------------------------------------

(deftest snapshot-db-has-vp-depends-edges
  (testing "create-db with a snapshot map produces :entity/depends edges for VPs"
    (seed-bp!)
    (seed-endpoints!)
    (seed-vp!)
    (registry/compile!)
    ;; Simulate what cloud/pull returns: the compiled+stripped registry map
    (let [snapshot (registry/current-registry)
          db (datalog/create-db snapshot)]
      (testing "VP depends on its business pattern"
        (is (= #{:value-proposition/login-detection}
               (set (d/q '[:find [?dev-id ...]
                           :where [?e :atlas/dev-id ?dev-id]
                           [?e :entity/depends :business-pattern/inbox-scanning]]
                         db)))
            "blast-radius on BP should find implementing VPs"))
      (testing "VP depends on its external-boundary endpoints"
        (is (= #{:value-proposition/login-detection}
               (set (d/q '[:find [?dev-id ...]
                           :where [?e :atlas/dev-id ?dev-id]
                           [?e :entity/depends :endpoint/logins-search]]
                         db)))
            "blast-radius on endpoint should find VPs that surface it"))
      (testing "downstream-closure via get-db with registry-override"
        (binding [registry/*registry-override* snapshot]
          (datalog/reset-db-cache!)
          (let [db2 (datalog/get-db)
                affected (datalog/query-downstream-closure
                          db2
                          #{:business-pattern/inbox-scanning}
                          3)]
            (is (some #(= :value-proposition/login-detection (:entity %)) affected)
                "query-downstream-closure must find VP as dependent of BP")))))))

;; ---------------------------------------------------------------------------
;; Tests: dataflow/deps-key on VP ontology
;; ---------------------------------------------------------------------------

(deftest vp-ontology-has-deps-key
  (testing ":atlas/value-proposition ontology declares :dataflow/deps-key"
    (let [ont-props (lookup/props-for :atlas/value-proposition)]
      (is (= [:value-proposition/implements-pattern
              :value-proposition/external-boundaries]
             (:dataflow/deps-key ont-props))
          "VP ontology must declare both props as deps-keys")))

  (testing "ontology/deps-for returns BP + endpoints for a VP"
    (seed-bp!)
    (seed-endpoints!)
    (seed-vp!)
    (let [deps (ontology/deps-for :value-proposition/login-detection)]
      (is (contains? deps :business-pattern/inbox-scanning)
          "deps-for must include the implementing business pattern")
      (is (contains? deps :endpoint/logins-search)
          "deps-for must include external-boundary endpoints"))))

;; ---------------------------------------------------------------------------
;; Tests: type-ref datalog-verbs
;; ---------------------------------------------------------------------------

(deftest vp-type-refs-use-entity-depends
  (testing ":type-ref/value-proposition-pattern uses :entity/depends as datalog-verb"
    (let [tr (lookup/props-for :type-ref/value-proposition-pattern)]
      (is (= :entity/depends (:type-ref/datalog-verb tr))
          "implements-pattern type-ref must use :entity/depends verb")))

  (testing ":type-ref/value-proposition-external-boundaries uses :entity/depends"
    (let [tr (lookup/props-for :type-ref/value-proposition-external-boundaries)]
      (is (= :entity/depends (:type-ref/datalog-verb tr))
          "external-boundaries type-ref must use :entity/depends verb"))))

;; ---------------------------------------------------------------------------
;; Tests: compound-id collision detector
;; ---------------------------------------------------------------------------

(deftest no-compound-id-collision-on-invariants
  (testing "invariants added by value-proposition and business-pattern ontologies each have unique compound-ids"
    (seed-bp!)
    (seed-vp!)
    (let [regs @registry/registrations
          by-compound-id (group-by (fn [r] (conj (:aspects r) (:type r))) regs)
          collisions (for [[_cid entries] by-compound-id
                           :let [dev-ids (distinct (map :dev-id entries))]
                           :when (> (count dev-ids) 1)]
                       dev-ids)]
      (is (empty? collisions)
          (str "Duplicate compound-ids found — one dev-id will be silently lost: " collisions)))))

(deftest invariant-collision-detector-fires
  (testing "invariant-no-compound-id-collision detects when two dev-ids share a compound-id"
    ;; Register two entities with the same compound-id deliberately
    (registry/register! :test/entity-a :atlas/invariant #{:meta/collision-test-aspect} {:x 1})
    (registry/register! :test/entity-b :atlas/invariant #{:meta/collision-test-aspect} {:x 2})
    (let [result (atlas.registry.graph/invariant-no-compound-id-collision)]
      (is (some? result) "must detect the collision")
      (is (= :error (:severity result)))
      (is (some #(and (contains? (set %) :test/entity-a)
                      (contains? (set %) :test/entity-b))
                (map :dev-ids (:details result)))
          "must name both colliding dev-ids"))))
