(ns atlas.ontology.test-case
  "Test-case ontology module.

   Defines the `:atlas/test-case` entity type. Auto-registers on require.

   A test-case binds:
   - A target entity (endpoint or execution-function) to test
   - A fixture (input data map)
   - Optional mocks (component overrides)
   - Expectations (predicates over the result, trace, or post-call state)

   From registry files (atlas.source), the parts that aren't data (host
   objects in the fixture, mock fns, expectation predicates) come from a var
   of no args carrying :atlas.source/id: load! puts it under :test-case/impl
   and `case-of` merges what it returns into the case.

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
(s/def :test-case/impl ifn?)

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
                  :test-case/expectations
                  :test-case/impl]
  :ontology/impl-key :test-case/impl
  :ontology/not-serialisable-keys [:test-case/mocks :test-case/expectations :test-case/impl]
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
  "All test-case INSTANCE dev-ids, excluding the ontology/type meta-entities.

   Discriminates by props :atlas/type — instances are :atlas/test-case, the
   descriptors are :atlas/ontology / :atlas/type. Filtering on props is robust
   where an :atlas/ontology aspect-check is not: the :atlas/test-case meta dev-id
   carries two compound-ids (#{:atlas/test-case :atlas/ontology} and
   #{:atlas/test-case :atlas/type}), and has-aspect? may resolve to the latter —
   which has no :atlas/ontology aspect, so it would slip through."
  []
  (->> (entity/all-with-aspect :atlas/test-case)
       (filter #(= :atlas/test-case (:atlas/type (entity/props-for %))))))

(defn case-of
  "The test case `dev-id` as the runner sees it: its registered props, with
   what its :test-case/impl returns merged in. The two fixtures merge by key;
   any other key, or a fixture key, given by both with different values
   throws, as a conflict between atlas.source sources does."
  [dev-id]
  (let [props (entity/props-for dev-id)
        impl (:test-case/impl props)]
    (if-not impl
      props
      (let [given (impl)
            _ (when-not (map? given)
                (throw (ex-info "A test case's :test-case/impl must return a map"
                                {:test-case dev-id :returned given})))
            fixture-clash (for [[k v] (:test-case/fixture given)
                                :when (contains? (:test-case/fixture props) k)
                                :when (not= v (get-in props [:test-case/fixture k]))]
                            k)
            clash (for [[k v] (dissoc given :test-case/fixture)
                        :when (contains? props k)
                        :when (not= v (get props k))]
                    k)]
        (when (or (seq clash) (seq fixture-clash))
          (throw (ex-info "A test case's props and its :test-case/impl disagree"
                          {:test-case dev-id
                           :keys (vec clash)
                           :fixture-keys (vec fixture-clash)})))
        (cond-> (merge props (dissoc given :test-case/fixture))
          (or (:test-case/fixture props) (:test-case/fixture given))
          (assoc :test-case/fixture (merge (:test-case/fixture props)
                                           (:test-case/fixture given))))))))

(registry/register!
 :invariant/test-case-target-exists
 :atlas/invariant
 #{:meta/test-case-target-exists}
 {:invariant/rule {:doc "A test case's :test-case/target exists"
                   :severity :error
                   :when {:= {:atlas/type :atlas/test-case}}
                   :require [{:resolves :test-case/target}]}
  :invariant/fn
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
                          :let [expectations (:test-case/expectations (case-of tc-id))]
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
                          :let [expectations (:test-case/expectations (case-of tc-id))]
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
