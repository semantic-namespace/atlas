(ns atlas.store.canonical-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [atlas.store.canonical :as canon]))


(def reg
  {#{:fn/alpha :atlas/execution-function}
   {:atlas/dev-id :fn/alpha :atlas/type :atlas/execution-function
    :execution-function/deps #{:component/db :component/cache}}
   #{:fn/beta :atlas/execution-function}
   {:atlas/dev-id :fn/beta :atlas/type :atlas/execution-function
    :execution-function/context [:user/id :user/email]}
   #{:data/thing :atlas/yorba-data}
   {:atlas/dev-id :data/thing :atlas/type :atlas/yorba-data}})


(deftest canonical-output-is-stable-under-reordering
  ;; The whole point. Two maps that are `=` but built in different orders must
  ;; serialise identically, or a git-backed store churns on every write and no-op
  ;; detection never fires.
  (let [a {:b 2 :a 1 :c #{:z :y :x}}
        b (into {} (shuffle (seq {:c #{:x :y :z} :a 1 :b 2})))]
    (is (= a b))
    (is (= (canon/canon-str a) (canon/canon-str b)))))


(deftest canonical-sorting-is-total-over-mixed-types
  ;; Sorting by value would throw here; sorting by printed form must not.
  (is (string? (canon/canon-str #{:kw "str" 42 'sym [1 2] nil}))))


(deftest round-trips-exactly
  (let [files (canon/registry->files reg)]
    (is (= 2 (count files)) "one file per entity type")
    (is (= reg (canon/files->registry files)))))


(deftest layout-is-deterministic
  (is (= (canon/registry->files reg)
         (canon/registry->files (into {} (shuffle (seq reg)))))))


(deftest non-entity-files-are-ignored-when-parsing
  ;; A store may hold a README next to the data; parsing must not choke on it.
  (let [files (assoc (canon/registry->files reg) "README.md" "# not edn {{{")]
    (is (= reg (canon/files->registry files)))))


(deftest volatile-props-are-excluded
  (let [with-ts (assoc-in reg [#{:fn/alpha :atlas/execution-function} :test-case/fixture]
                          {:now (java.util.Date.)})]
    (is (= (canon/registry->files reg)
           (canon/registry->files (canon/strip-volatile with-ts))))))


(deftest non-determinism-is-reported-with-the-guilty-prop
  (let [a (assoc-in reg [#{:fn/alpha :atlas/execution-function} :some/stamp] 1)
        b (assoc-in reg [#{:fn/alpha :atlas/execution-function} :some/stamp] 2)]
    (testing "detected"
      (is (= {#{:fn/alpha :atlas/execution-function} #{:some/stamp}}
             (canon/non-deterministic a b))))
    (testing "and identical registries report nothing"
      (is (empty? (canon/non-deterministic a a))))))


(deftest one-prop-per-line-so-diffs-are-readable
  (let [content (get (canon/registry->files reg) "entities/atlas_execution-function.edn")]
    (is (< 1 (count (clojure.string/split-lines content)))
        "a single long line makes every change look like a rewrite")))


;; ---------------------------------------------------------------------------
;; Live values
;; ---------------------------------------------------------------------------
;;
;; The bug these exist for: a registry holds live values (`:atlas/impl` is a
;; function), `pr-str` prints them as `#object[...]` without complaint, and the
;; write succeeds. The failure surfaces on the NEXT run, reading its own output
;; with "No reader function for tag object" -- 205 of them, in production.
;;
;; It survived every earlier test because the fixtures were registries pulled
;; back from atlas-cloud, i.e. already sanitised by the exact step this library
;; was missing. Fixtures must therefore be built here, not fetched.

(defrecord SomeRecord [a])

(def live-reg
  {#{:fn/impl :atlas/execution-function}
   {:atlas/dev-id :fn/impl
    :atlas/type   :atlas/execution-function
    :atlas/impl   (fn [_] :result)                    ; the real offender
    :execution-function/deps #{:component/db}}
   #{:fn/nested :atlas/execution-function}
   {:atlas/dev-id :fn/nested
    :atlas/type   :atlas/execution-function
    :nested/map   {:ok 1 :bad (fn [] nil)}            ; buried one level down
    :nested/coll  [1 2 (atom 3)]}
   #{:fn/exotic :atlas/execution-function}
   {:atlas/dev-id :fn/exotic
    :atlas/type   :atlas/execution-function
    :a/record     (->SomeRecord 1)                    ; a map that prints tagged
    :a/date       #inst "2026-01-01T00:00:00.000-00:00"
    :a/uuid       #uuid "00000000-0000-0000-0000-000000000001"}})


(deftest live-values-are-detected
  (testing "functions, atoms and records are not storable"
    (is (not (canon/storable? (fn [] nil))))
    (is (not (canon/storable? (atom 1))))
    (is (not (canon/storable? (->SomeRecord 1))))
    (is (not (canon/storable? {:ok 1 :bad (fn [] nil)})) "including nested")
    (is (not (canon/storable? [1 2 (atom 3)]))))
  (testing "ordinary EDN, and the tagged literals that do read back, are"
    (is (canon/storable? #inst "2026-01-01T00:00:00.000-00:00"))
    (is (canon/storable? #uuid "00000000-0000-0000-0000-000000000001"))
    (is (canon/storable? {:a [1 "two" :three #{:x}] :b nil}))))


(deftest sanitised-registry-round-trips
  ;; The assertion that was missing. Without sanitize this throws
  ;; "No reader function for tag object" -- exactly the production failure.
  (let [clean (canon/sanitize live-reg)]
    (is (= clean (canon/files->registry (canon/registry->files clean))))))


(deftest unsanitised-registry-fails-to-round-trip
  ;; Proves the test above is testing something.
  (is (thrown? Exception
               (canon/files->registry (canon/registry->files live-reg)))))


(deftest sanitize-drops-the-prop-not-the-entity
  (let [clean (canon/sanitize live-reg)]
    (is (= 3 (count clean)) "every entity survives")
    (is (nil? (get-in clean [#{:fn/impl :atlas/execution-function} :atlas/impl])))
    (is (= #{:component/db}
           (get-in clean [#{:fn/impl :atlas/execution-function} :execution-function/deps]))
        "storable props on the same entity are untouched")
    (is (= #inst "2026-01-01T00:00:00.000-00:00"
           (get-in clean [#{:fn/exotic :atlas/execution-function} :a/date])))))


(deftest unstorable-props-are-reportable
  ;; Expected to be non-empty for any real registry, so it is reported rather
  ;; than raised -- but a consumer should be able to see what it lost.
  (is (= {:atlas/impl 1 :nested/map 1 :nested/coll 1 :a/record 1}
         (canon/unstorable-props live-reg))))
