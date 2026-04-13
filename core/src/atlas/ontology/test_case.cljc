(ns atlas.ontology.test-case
  "Test-case ontology module.

   Defines the `:atlas/test-case` entity type. Auto-registers on require.

   A test-case binds:
   - A target entity (endpoint or execution-function) to test
   - A fixture (input data map)
   - Optional mocks (component overrides)
   - Expectations (predicates over the result, trace, or post-call state)

   Usage:
     (require '[atlas.ontology.test-case])"
  (:require [atlas.registry :as registry]
            [atlas.registry.lookup :as entity]
            [atlas.ontology :as ontology]
            [atlas.ontology.type-ref :as type-ref]
            [clojure.spec.alpha :as s]))

;; =============================================================================
;; SPECS
;; =============================================================================

(s/def :expectation/kind #{:result :trace :invariant})
(s/def :expectation/docs string?)
(s/def :expectation/spec qualified-keyword?)
(s/def :expectation/pred fn?)
(s/def :expectation/must-contain qualified-keyword?)

(s/def :test-case/expectation
  (s/keys :req [:expectation/kind]
          :opt [:expectation/docs :expectation/spec :expectation/pred
                :expectation/must-contain]))

(s/def :test-case/target qualified-keyword?)
(s/def :test-case/fixture map?)
(s/def :test-case/mocks (s/nilable map?))
(s/def :test-case/expectations (s/coll-of :test-case/expectation))

;; =============================================================================
;; ONTOLOGY
;; =============================================================================

(registry/register!
 :atlas/test-case
 :atlas/ontology
 #{:atlas/test-case}
 {:ontology/for :atlas/test-case
  :ontology/keys [:test-case/target
                  :test-case/fixture
                  :test-case/mocks
                  :test-case/expectations]
  :ontology/not-serialisable-keys [:test-case/mocks :test-case/expectations]
  :dataflow/deps-key :test-case/target})

;; Type-ref: test-case → target entity (the entity being tested)
(registry/register!
 :type-ref/test-case-target
 :atlas/type-ref
 #{:meta/ref-test-case-target}
 {:type-ref/source :atlas/test-case
  :type-ref/property :test-case/target
  :type-ref/datalog-verb :test/tests
  :type-ref/cardinality :db.cardinality/one})

;; =============================================================================
;; DATALOG EXTRACTOR
;; =============================================================================

(registry/register!
 :datalog-extractor/test-case
 :atlas/datalog-extractor
 #{:meta/test-case-extractor}
 {:datalog-extractor/fn
  (fn [compound-id props]
    (when (contains? compound-id :atlas/test-case)
      ;; Automatic reference extraction via type-ref
      ;; Emits :test/tests fact from :test-case/target
      (type-ref/extract-reference-facts
       :atlas/test-case
       compound-id
       props)))

  :datalog-extractor/schema {:test/tests {:db/cardinality :db.cardinality/one}}})

;; =============================================================================
;; INVARIANTS
;; =============================================================================

(defn- user-test-cases
  "All test-case dev-ids, excluding the ontology meta-entity."
  []
  (->> (entity/all-with-aspect :atlas/test-case)
       (remove #(entity/has-aspect? % :atlas/ontology))))

(registry/register!
 :invariant/test-case-target-exists
 :atlas/invariant
 #{:meta/test-case-target-exists}
 {:invariant/fn
  (fn []
    (let [test-cases (user-test-cases)
          violations (for [tc-id test-cases
                          :let [props  (entity/props-for tc-id)
                                target (:test-case/target props)]
                          :when target
                          :when (nil? (entity/identity-for target))]
                      {:test-case tc-id :target target})]
      (when (seq violations)
        {:invariant :test-case-target-exists
         :violation :target-not-found
         :details (vec violations)
         :severity :error
         :message (str "Test-case targets reference non-existent entities: "
                       (mapv :target violations))})))})

(registry/register!
 :invariant/test-case-has-expectations
 :atlas/invariant
 #{:meta/test-case-has-expectations}
 {:invariant/fn
  (fn []
    (let [test-cases (user-test-cases)
          violations (for [tc-id test-cases
                          :let [props (entity/props-for tc-id)
                                expectations (:test-case/expectations props)]
                          :when (empty? expectations)]
                      tc-id)]
      (when (seq violations)
        {:invariant :test-case-has-expectations
         :violation :empty-expectations
         :details (vec violations)
         :severity :warning
         :message (str "Test-cases with no expectations: " (vec violations))})))})

(registry/register!
 :invariant/test-case-pred-has-docs
 :atlas/invariant
 #{:meta/test-case-pred-has-docs}
 {:invariant/fn
  (fn []
    (let [test-cases (user-test-cases)
          violations (for [tc-id test-cases
                          :let [expectations (:test-case/expectations
                                              (entity/props-for tc-id))]
                          exp expectations
                          :when (:expectation/pred exp)
                          :when (not (:expectation/docs exp))]
                      {:test-case tc-id
                       :expectation exp})]
      (when (seq violations)
        {:invariant :test-case-pred-has-docs
         :violation :pred-without-docs
         :details (vec violations)
         :severity :warning
         :message (str "Predicate expectations should have :expectation/docs for display: "
                       (count violations) " missing")})))})
