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
