(ns atlas.ide-test
  "Verification suite for the IDE integration layer. Ensures IDE-facing APIs
   return stable, editor-friendly data (sorted vectors, namespaced keys)
   without mutating shared state."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [atlas.registry :as cid]
            [atlas.registry.lookup :as entity]
            [atlas.ontology :as ot]
            [atlas.ontology.execution-function :as ef]
            [atlas.ontology.structure-component :as sc]
            [atlas.ontology.interface-endpoint :as ie]
            [atlas.ide :as ide]))

;; ---------------------------------------------------------------------------
;; ⚙️ Test Fixture — reset registry and caches for isolation
;; ---------------------------------------------------------------------------

(use-fixtures :each
  (fn [f]
    (reset! cid/registry {})
    (require 'atlas.ontology :reload)
    (require 'atlas.ontology.execution-function :reload)
    (require 'atlas.ontology.structure-component :reload)
    (require 'atlas.ontology.interface-endpoint :reload)
    (ot/register-entity-types!)
    (reset! @#'ide/reverse-deps-cache {:time 0 :data {}})
    (reset! @#'ide/data-key-cache {:time 0 :entity/produces {} :entity/consumes {}})
    (f)
    (reset! cid/registry {})
    (reset! @#'ide/reverse-deps-cache {:time 0 :data {}})
    (reset! @#'ide/data-key-cache {:time 0 :entity/produces {} :entity/consumes {}})))

(defn- seed-registry! []
  ;; Components
  (cid/register!

   :component/storage

   :atlas/structure-component

   #{ :domain/storage}
   {:execution-function/deps #{}})

  (cid/register!
   :component/cache
   :atlas/structure-component
   #{ :domain/cache}
   {:execution-function/deps #{}})

  (cid/register!
   :component/audit
   :atlas/structure-component
   #{ :domain/support}
   {:execution-function/deps #{}})

  ;; Functions
  (cid/register!

   :fn/alpha

   :atlas/execution-function

   #{ :tier/service :domain/alpha}
   {:interface-endpoint/context [:alpha/input :alpha/flag]
    :interface-endpoint/response [:alpha/output]
    :execution-function/deps #{:component/storage}})

  (cid/register!
   :fn/beta
   :atlas/execution-function
   #{ :tier/service :domain/beta}
   {:interface-endpoint/context [:alpha/output]
    :interface-endpoint/response [:beta/result]
    :execution-function/deps #{:fn/alpha :component/cache}})

  (cid/register!
   :fn/gamma
   :atlas/execution-function
   #{ :tier/service :domain/gamma}
   {:interface-endpoint/context [:alpha/output]
    :interface-endpoint/response [:gamma/summary]
    :execution-function/deps #{:fn/alpha}})

  (cid/register!
   :fn/profile
   :atlas/execution-function
   #{ :tier/service :domain/profile :compliance/pii :compliance/audited}
   {:interface-endpoint/context [:user/id]
    :interface-endpoint/response [:profile/payload]
    :execution-function/deps #{:component/audit :component/storage}})

  (cid/register!
   :endpoint/order
   :atlas/interface-endpoint
   #{ :protocol/http :tier/api :authorization/required :capacity/rate-limited :domain/orders}
   {:interface-endpoint/context [:order/request]
    :interface-endpoint/response [:order/response]
    :execution-function/deps #{:fn/beta}})

  (cid/register!
   :fn/http-handler
   :atlas/execution-function
   #{ :semantic-namespace/error-handler :protocol/http :temporal/timeout :domain/support}
   {:interface-endpoint/context []
    :interface-endpoint/response []
    :execution-function/deps #{}}))

;; ---------------------------------------------------------------------------
;; ✅ Entity information stays namespaced and sorted
;; ---------------------------------------------------------------------------

(deftest entity-info-returns-sorted-vectors
  (seed-registry!)
  (let [info (ide/entity-info :fn/alpha)]
    (is (= {:entity/dev-id :fn/alpha
            :entity/identity [:atlas/execution-function :domain/alpha :tier/service]
            :entity/aspects [:domain/alpha :tier/service]
            :entity/definition-keys [:atlas/dev-id
                                     :execution-function/context
                                     :execution-function/response
                                     :execution-function/deps]
            :entity/definition-values {:atlas/dev-id :fn/alpha
                                       :execution-function/deps [:component/storage]}
            :interface-endpoint/context [:alpha/flag :alpha/input]
            :interface-endpoint/response [:alpha/output]
            :execution-function/deps [:component/storage]
            :atlas/fields []}
           info)
        "entity-info preserves namespaced keys and sorted vectors")))

;; ---------------------------------------------------------------------------
;; 🔗 Dependency navigation is vectorized and ordered
;; ---------------------------------------------------------------------------

(deftest dependency-navigation
  (seed-registry!)
  (is (= [:component/cache :fn/alpha]
         (ide/dependencies-of :fn/beta))
      "dependencies-of returns sorted vector")
  (is (= [:fn/beta :fn/gamma]
         (ide/dependents-of :fn/alpha))
      "dependents-of returns sorted vector via reverse cache"))

;; ---------------------------------------------------------------------------
;; 🎛️ IDE listings expose namespaced aspects and stable ordering
;; ---------------------------------------------------------------------------

(deftest ide-listings-are-namespaced-and-sorted
  (seed-registry!)
  (let [entities (ide/list-all-entities)
        aspects (ide/list-aspects)
        ;; Filter out ontology definitions and meta-entities (extractors, invariants) from entity list
        meta-entity? (fn [entity]
                       (let [dev-id (:entity/dev-id entity)]
                         (and (keyword? dev-id)
                              (or (= "atlas" (namespace dev-id))
                                  (= "ontology" (namespace dev-id))
                                  (= "datalog-extractor" (namespace dev-id))
                                  (= "invariant" (namespace dev-id))
                                  (= "type-ref" (namespace dev-id))))))
        non-ontology-entities (remove meta-entity? entities)
        ;; Filter out ontology, entity type, and meta aspects from aspects list
        entity-type-aspect? (fn [aspect-map]
                              (let [aspect (:aspect/aspect aspect-map)
                                    ns (when (keyword? aspect) (namespace aspect))]
                                (or (= "atlas" ns)
                                    (= "meta" ns))))
        non-ontology-aspects (remove entity-type-aspect? aspects)]
    (is (= [:component/audit :component/cache :component/storage :endpoint/order :fn/alpha :fn/beta :fn/gamma :fn/http-handler :fn/profile]
           (map :entity/dev-id non-ontology-entities))
        "list-all-entities returns deterministic order for dev-ids (excluding ontology)")
    (is (= [{:aspect/aspect :authorization/required :aspect/count 1}
            {:aspect/aspect :capacity/rate-limited :aspect/count 1}
            {:aspect/aspect :compliance/audited :aspect/count 1}
            {:aspect/aspect :compliance/pii :aspect/count 1}
            {:aspect/aspect :domain/alpha :aspect/count 1}
            {:aspect/aspect :domain/beta :aspect/count 1}
            {:aspect/aspect :domain/cache :aspect/count 1}
            {:aspect/aspect :domain/gamma :aspect/count 1}
            {:aspect/aspect :domain/orders :aspect/count 1}
            {:aspect/aspect :domain/profile :aspect/count 1}
            {:aspect/aspect :domain/storage :aspect/count 1}
            {:aspect/aspect :domain/support :aspect/count 2}
            {:aspect/aspect :protocol/http :aspect/count 2}
            {:aspect/aspect :semantic-namespace/error-handler :aspect/count 1}
            {:aspect/aspect :tier/api :aspect/count 1}
            {:aspect/aspect :tier/service :aspect/count 4}
            {:aspect/aspect :temporal/timeout :aspect/count 1}]
           non-ontology-aspects)
        "list-aspects returns namespaced aspect maps with counts (excluding ontology)")))

;; ---------------------------------------------------------------------------
;; 🔤 Completion helpers return editor-friendly strings
;; ---------------------------------------------------------------------------

(deftest completion-helpers
  (seed-registry!)
  (is (= ["fn/alpha" "fn/beta" "fn/gamma" "fn/http-handler" "fn/profile"]
         (ide/complete-dev-id "fn/"))
      "complete-dev-id omits leading colon and stays ordered")
  (is (= ["domain/alpha" "domain/beta" "domain/cache" "domain/gamma" "domain/orders" "domain/profile" "domain/storage" "domain/support"]
         (ide/complete-aspect "domain/"))
      "complete-aspect surfaces namespaced aspect strings")
  (is (= ["alpha/flag" "alpha/input" "alpha/output"]
         (ide/complete-data-key "alpha/"))
      "complete-data-key returns stringified, sorted data keys"))

;; ---------------------------------------------------------------------------
;; 🧭 Producers/consumers expose sorted, namespaced dev-ids
;; ---------------------------------------------------------------------------

(deftest producers-and-consumers-are-namespaced
  (seed-registry!)
  (is (= [:fn/alpha]
         (ide/producers-of :alpha/output))
      "producers-of returns sorted vector of namespaced dev-ids")
  (is (= [:fn/beta :fn/gamma]
         (ide/consumers-of :alpha/output))
      "consumers-of returns sorted vector of namespaced dev-ids"))

;; ---------------------------------------------------------------------------
;; 🛰️ Advanced queries stay namespaced and vectorized
;; ---------------------------------------------------------------------------

(deftest advanced-queries-return-vectors
  (seed-registry!)

  (is (= [{:dataflow/needs :alpha/output
           :dataflow/produced-by [:fn/alpha]
           :dataflow/satisfied? true}]
         (ide/data-flow :fn/beta))
      "data-flow vectorizes produced-by and keeps keywords")

  (let [order (ide/execution-order)
        positions (zipmap order (range))]
    (is (< (positions :fn/alpha) (positions :fn/beta)) "alpha executes before beta")
    (is (< (positions :fn/alpha) (positions :fn/gamma)) "alpha executes before gamma"))

  (is (= {:dataflow/data-key :alpha/output
          :dataflow/produced-by [:fn/alpha]
          :dataflow/consumed-by [:fn/beta :fn/gamma]
          :dataflow/connected? true}
         (ide/trace-data-flow :alpha/output))
      "trace-data-flow uses namespaced keywords and sorted vectors")

  (is (= {:impact/entity :fn/alpha
          :impact/produces [:alpha/output]
          :impact/direct-dependents [:fn/beta :fn/gamma]}
         (ide/impact-of-change :fn/alpha))
      "impact-of-change sorts dependents and keeps namespaces")

  (let [coupling (ide/domain-coupling)
        gamma-coupling (first (filter #(= :domain/gamma (:coupling/domain %)) coupling))
        orders-coupling (first (filter #(= :domain/orders (:coupling/domain %)) coupling))]
    (is (= [:domain/alpha] (:coupling/depends-on gamma-coupling)) "gamma depends on alpha domain")
    (is (= [:domain/beta] (:coupling/depends-on orders-coupling)) "orders depend on beta domain"))

  (is (= [{:pii/id :fn/profile
           :pii/audited? true
           :pii/context [:user/id]
           :pii/response [:profile/payload]}]
         (ide/pii-surface))
      "pii-surface sorts context/response and preserves keywords")

  (let [opts (ide/error-handler-coverage)
        coverage-by-concern (group-by :error-handler/concern (:error-handler/coverage opts))]
    (is (= [{:error-handler/id :fn/http-handler
             :error-handler/handles [:protocol/http :temporal/timeout]}]
           (:error-handler/handlers opts))
        "handlers sorted and namespaced")
    (is (every? true?
                (map :error-handler/has-handler? (or (coverage-by-concern :protocol/http) [])))
        "protocol/http coverage reports handler present")
    (is (every? false?
                (map :error-handler/has-handler? (or (coverage-by-concern :authorization/required) [])))
        "authorization coverage reports missing handler"))

  (let [suggestions (ide/suggest-aspects :fn/beta)]
    (is (vector? (:ontology/similar-entries suggestions)) "similar entries returned as vector")
    (is (every? (fn [entry]
                  (and (vector? entry) (every? keyword? entry)))
                (:ontology/similar-entries suggestions))
        "similar entries are vectors of namespaced keywords")
    (is (every? keyword? (:ontology/suggested-aspects suggestions))
        "suggested aspects remain keywords"))

  (let [inspection (ide/inspect-entity :fn/beta)]
    (is (= [:atlas/execution-function :domain/beta :tier/service]
           (:inspection/semantic-identity inspection))
        "inspect-entity returns sorted identity vector")
    (is (= [:component/cache :fn/alpha]
           (get-in inspection [:inspection/value :execution-function/deps]))
        "inspect-entity vectorizes deps inside value map"))

  (let [catalog (ide/aspect-catalog)]
    (is (every? (fn [opts]
                  (every? (fn [opts2]
                            (vector? (:aspect-catalog/examples opts2)))
                          (:aspect-catalog/items opts)))
                catalog)
        "aspect catalog example lists are vectors")))

;; ---------------------------------------------------------------------------
;; 🛡️ Validation, docs, and listings remain namespaced
;; ---------------------------------------------------------------------------

(deftest validation-and-docs-apis-are-namespaced
  (seed-registry!)

  ;; Filter out ontology entity from structure-component query
  (is (= [:component/audit :component/cache :component/storage]
         (vec (remove #{:atlas/structure-component} (ide/entities-with-aspect :atlas/structure-component))))
      "entities-with-aspect returns sorted vector (excluding ontology)")

  (let [result (with-redefs [atlas.invariant/check-all
                             (fn [] {:valid? true :errors [] :warnings []})]
                 (ide/check-invariants))]
    (is (= {:invariant/valid? true
            :invariant/error-count 0
            :invariant/warning-count 0
            :invariant/errors []
            :invariant/warnings []}
           result)
        "check-invariants summarizes counts and uses namespaced keys"))

  (let [result (with-redefs [atlas.invariant/check-all
                             (fn [] {:errors [{:message "fn/alpha is bad" :details "fn/alpha"}
                                              {:message "fn/beta is fine" :details "fn/beta"}]
                                     :warnings [{:message "fn/alpha warning" :details "fn/alpha"}]})]
                 (ide/validate-entity :fn/alpha))]
    (is (= {:validation/dev-id :fn/alpha
            :validation/errors [{:message "fn/alpha is bad" :details "fn/alpha"}]
            :validation/warnings [{:message "fn/alpha warning" :details "fn/alpha"}]}
           result)
        "validate-entity filters to namespaced dev-id occurrences"))

  (let [doc-result (with-redefs [atlas.docs/enrich-with-descriptions
                                 (fn [dev-id] {:doc/id dev-id :doc/title "example"})]
                     (ide/entity-doc :fn/alpha))
        summary-result (with-redefs [atlas.docs/system-overview
                                     (fn [] {:docs/system "ok"})]
                         (ide/system-summary))]
    (is (= {:doc/id :fn/alpha :doc/title "example"} doc-result)
        "entity-doc returns the doc payload")
    (is (= {:docs/system "ok"} summary-result)
        "system-summary passes through doc overview"))

  (let [called (atom nil)
        response (with-redefs [atlas.ontology/validate-before-register
                               (fn [ids] (reset! called ids) :validated)]
                   (ide/validate-identity [:foo/bar :baz/qux]))]
    (is (= :validated response) "validate-identity returns delegated result")
    (is (= #{:foo/bar :baz/qux} @called) "validate-identity sends keyword set"))

  (is (every? map? (ide/list-templates)) "list-templates remains usable"))
