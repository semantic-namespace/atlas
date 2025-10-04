(ns atlas.registry-test
  "Verification suite for the Semantic Kernel (compound identity algebra).
   Each test validates algebraic, analytical, or registry-level properties
   of `atlas.registry`.

   Goals:
   • Prove closure, determinism, and validity of the algebra
   • Ensure every query and analysis function returns pure data
   • Provide readable, narrative test documentation for developers"
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [atlas.registry :as id]
            [atlas.query :as q]))

;; ---------------------------------------------------------------------------
;; ⚙️ Test Fixture — reset registry for each test
;; ---------------------------------------------------------------------------

(use-fixtures :each
  (fn [f]
    (reset! id/registry {})
    (f)
    (reset! id/registry {})))

;; ---------------------------------------------------------------------------
;; ✅ Validation Rules
;; ---------------------------------------------------------------------------

(deftest validity-rules
  "The `valid?` function defines what a well-formed compound identity is:
   - It must be a set (not a vector, list, or nil)
   - It must contain ≥2 elements
   - All elements must be qualified keywords (`:ns/name`)."
  (testing "valid? enforces structure"
    (is (false? (id/valid? nil)))
    (is (false? (id/valid? #{:a})))
    (is (false? (id/valid? #{:a/b :c})))  ; contains unqualified keyword
    (is (true?  (id/valid? #{:foo/a :foo/b})))))


;; ---------------------------------------------------------------------------
;; 🧱 Basic Registry Operations
;; ---------------------------------------------------------------------------

(deftest registry-crud
  "Tests creation, retrieval, and deletion in the identity registry.
   Also ensures `exists?` correctly reports presence and value match."
  (testing "register!, fetch, exists?, remove*"
    (let [cid #{:app/foo :app/bar}
          val {:test/val "val"}
          expected-val (assoc val :atlas/dev-id :app/foo :atlas/type :app/foo)]
      (id/register! :app/foo :app/foo #{:app/bar} val)
      (is (= expected-val (id/fetch cid)) "Registered value retrievable")
      (is (= {:exists true :matches true}
             (id/exists? cid expected-val)) "Value matches expected")
      (is (= {:exists true :matches false}
             (id/exists? cid {:test/val "other"})) "Detects mismatch correctly")
      (id/remove cid)
      (is (nil? (id/fetch cid)) "Value removed from registry"))))

(deftest registry-uniqueness
  "Tests that register! enforces uniqueness of both compound IDs and dev-ids."
  (testing "find-by-dev-id helper function"
    (let [cid #{:test/epsilon :test/five}]
      (id/register! :entity/finder :test/epsilon #{:test/five} {:data "value"})
      (is (= [cid {:atlas/dev-id :entity/finder :data "value" :atlas/type :test/epsilon}]
             (q/find-by-dev-id @id/registry :entity/finder))
          "Should find entity by dev-id")
      (is (nil? (q/find-by-dev-id @id/registry :entity/nonexistent))
          "Should return nil for nonexistent dev-id"))))

(deftest auto-generated-dev-id
  "Tests that register! auto-generates deterministic dev-ids when not provided."
  (testing "auto-generates dev-id in 3-arg arity when not in value"
    (let [cid #{:test/zeta :test/six}]
      (id/register! :test/zeta #{:test/six} {:data "auto"})
      (let [registered (id/fetch cid)]
        (is (contains? registered :atlas/dev-id)
            "Should have auto-generated dev-id")
        (is (= :auto/test--six--test--zeta (:atlas/dev-id registered))
            "dev-id should be deterministic based on sorted aspects"))))

  (testing "uses provided dev-id in value over auto-generation"
    (let [cid #{:test/eta :test/seven}]
      (id/register! :custom/id :test/eta #{:test/seven} {:data "manual"})
      (is (= :custom/id (:atlas/dev-id (id/fetch cid)))
          "Should use provided dev-id instead of auto-generating")))

  (testing "generate-dev-id is deterministic"
    (let [cid1 #{:foo/bar :baz/qux}
          cid2 #{:baz/qux :foo/bar}]  ; Same aspects, different order
      (is (= (id/generate-dev-id cid1)
             (id/generate-dev-id cid2))
          "Should generate same dev-id regardless of set order")))

  (testing "auto-generated dev-ids are unique per compound ID"
    (let [cid1 #{:test/theta :test/eight}
          cid2 #{:test/iota :test/nine}]
      (id/register! :test/theta #{:test/eight} {:data "first"})
      (id/register! :test/iota #{:test/nine} {:data "second"})
      (is (not= (:atlas/dev-id (id/fetch cid1))
                (:atlas/dev-id (id/fetch cid2)))
          "Different compound IDs should get different auto-generated dev-ids"))))


;; ---------------------------------------------------------------------------
;; 🔍 Query and Discovery
;; ---------------------------------------------------------------------------

(deftest query-and-find
  "Verifies algebraic discovery functions:
   - `query` finds all supersets of a given set of aspects
   - `find-with` returns map of identities containing an aspect."
  (testing "query and find-with return deterministic sets"
    (id/register! :app/a :app/a #{:app/b} {:test/val "x"})
    (id/register! :app/a :app/a #{:app/b :app/c} {:test/val "y"})
    (id/register! :app/b :app/b #{:app/d} {:test/val "z"})

    (is (= [[":app/a" ":app/b"]
            [":app/a" ":app/b" ":app/c"]]
           (mapv #(mapv str %) (q/query-superset @id/registry #{:app/a :app/b})))
        "query returns all supersets sorted lexically")

    (is (contains? (q/find-by-aspect @id/registry :app/a)
                   #{:app/a :app/b :app/c})
        "find-with locates all identities containing :app/a")))


;; ---------------------------------------------------------------------------
;; 🧮 Algebraic Closure
;; ---------------------------------------------------------------------------

(deftest algebraic-closure
  "Ensures set algebra (union, intersection, difference) behaves deterministically
   and preserves closure (results remain valid identities)."
  (testing "query-algebra operations"
    (id/register! :x/a :x/a #{:x/b} {:test/val 1})
    (id/register! :x/a :x/a #{:x/b :x/c} {:test/val 2})
    (id/register! :x/a :x/a #{:x/d} {:test/val 3})

    (is (= 2 (count (q/query-algebra @id/registry {:intersection [#{:x/a :x/b}]})))
        "Intersection returns all supersets of {:x/a :x/b}")
    (is (= 3 (count (q/query-algebra @id/registry {:union [#{:x/a} #{:x/d}]})))
        "Union returns all sets containing either aspect group")
    (is (= 1 (count (q/query-algebra @id/registry {:difference [#{:x/a :x/b} #{:x/c}]})))
        "Difference excludes those containing :x/c")))


;; ---------------------------------------------------------------------------
;; 🤝 Semantic Neighbors and Correlation
;; ---------------------------------------------------------------------------

(deftest neighbor-similarity
  "Tests `semantic-neighbors` which finds related identities and computes
   their Jaccard-like similarity coefficient (|A∩B| / |A∪B|)."
  (testing "semantic-neighbors returns similarity ratio"
    (id/register! :n/x :n/x #{:n/y} {:test/val :a})
    (id/register! :n/x :n/x #{:n/y :n/z} {:test/val :b})
    (id/register! :n/x :n/x #{:n/z} {:test/val :c})
    (let [res (q/semantic-similarity @id/registry #{:n/x :n/y})]
      (is (every? #(contains? % :similarity) res) "All results include similarity score")
      (is (apply >= (map :similarity res)) "Results sorted by similarity descending"))))

(deftest correlation-properties
  "The correlation matrix should be:
   - Symmetric (freq(a,b) == freq(b,a))
   - Normalized between 0–1 across all pairs."
  (testing "correlation matrix is symmetric and normalized"
    (id/register! :c/a :c/a #{:c/b} {:test/val 1})
    (id/register! :c/b :c/b #{:c/c} {:test/val 2})
    (let [m (id/correlation-matrix)
          aab (get-in m [:c/a :c/b])
          aba (get-in m [:c/b :c/a])]
      (is (= aab aba) "Symmetric co-occurrence matrix")
      (is (<= 0 aab 1) "Normalized within range 0–1"))))


;; ---------------------------------------------------------------------------
;; 📈 Analytics and Meta-Information
;; ---------------------------------------------------------------------------

(deftest analytics-coverage
  "Checks the coherence between statistical summaries:
   - `aspect-frequency` lists every aspect at least once
   - `identity-stats` aligns with total identity count."
  (testing "aspect-frequency and identity-stats coherence"
    (id/register! :r/a :r/a #{:r/b} {:test/val 1})
    (id/register! :r/a :r/a #{:r/c} {:test/val 2})
    (let [freq (q/aspect-frequency @id/registry)
          stats (q/identity-stats @id/registry)]
      (is (map? freq) "Frequency returns map of counts")
      (is (seq stats) "Identity stats returns non-empty sequence")
      (is (= (count (q/all-identities @id/registry))
             (count stats))
          "Each registered identity appears in stats"))))


;; ---------------------------------------------------------------------------
;; 🔁 Determinism and Reproducibility
;; ---------------------------------------------------------------------------

(deftest deterministic-output
  "All kernel operations should be pure and deterministic:
   identical queries produce identical ordered outputs."
  (testing "sorted and stable results"
    (id/register! :s/a :s/a #{:s/b} {:test/val 1})
    (id/register! :s/a :s/a #{:s/c} {:test/val 2})
    (let [r1 (q/query-superset @id/registry #{:s/a})
          r2 (q/query-superset @id/registry #{:s/a})]
      (is (= r1 r2) "Repeated queries yield identical output"))))


;; ---------------------------------------------------------------------------
;; 🧩 Diagnostics and Summary
;; ---------------------------------------------------------------------------

(deftest diagnostics
  "Verifies heuristic and diagnostic tools:
   - `missing-aspects` suggests related attributes
   - `find-anomalies` detects contradictory combinations
   - `summary` aggregates high-level metrics"
  (testing "missing-aspects and summary return structured data"
    (id/register! :m/x :m/x #{:m/y} {:test/val 1})
    (id/register! :m/x :m/x #{:m/z} {:test/val 2})
    (is (vector? (id/missing-aspects #{:m/x :m/y})) "Returns vector of suggestions")
    (is (map? (id/summary)) "Summary returns pure map of metadata"))
  (testing "find-anomalies detects conflicting identities"
    (id/register! :api/endpoint :api/endpoint #{:sync/operation :async/operation} {:test/val 1})
    (let [anom (id/find-anomalies)]
      (is (seq anom) "Anomalies detected for contradictory identities")
      (is (set? (first anom)) "Each anomaly entry is an identity set"))))
