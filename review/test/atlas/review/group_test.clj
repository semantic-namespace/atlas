(ns atlas.review.group-test
  (:require [clojure.test :refer [deftest is]]
            [atlas.review.decorate :as d]))

(def attach #'d/attach)
(def keep-together #'d/keep-test-files-together)

(deftest the-nearest-group-wins-and-a-tie-is-shared
  (let [owner (attach {:a #{0} :z #{1}} #{#{:a :b} #{:b :c} #{:z :y} #{:y :c} #{:a :t} #{:z :t}} #{} #{0 1})]
    (is (= #{0} (owner :b)))
    (is (= #{1} (owner :y)))
    (is (= #{0 1} (owner :t)) "reached by both groups at the same distance")
    (is (= #{0 1} (owner :c)))))

(deftest a-hub-is-reached-but-not-walked-through
  (let [owner (attach {:a #{0}} #{#{:a :hub} #{:hub :far}} #{:hub} #{0})]
    (is (= #{0} (owner :hub)))
    (is (nil? (owner :far)))))

(deftest only-active-groups-expand
  (is (nil? ((attach {:a #{0} :m #{1}} #{#{:m :x}} #{} #{0}) :x))))

(deftest a-test-file-follows-the-source-it-is-named-after
  (let [src ["src/a/policy.clj" "defn decide"]
        t1 ["test/a/policy_test.clj" "deftest one"]
        t2 ["test/a/policy_test.clj" "deftest two"]
        owner (keep-together {src #{5} t1 #{5} t2 #{7}} [src t1 t2] {} #{})]
    (is (= #{5} (owner t2)))))
