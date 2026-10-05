(ns atlas.review.diff-test
  (:require [clojure.test :refer [deftest is]]
            [atlas.review.diff :as diff]))

(def base {#{:a :t} {:atlas/dev-id :x :atlas/type :t :execution-function/context [:k1 :k2] :execution-function/deps #{:d1}}
           #{:b :t} {:atlas/dev-id :y :atlas/type :t}})
(def cand {#{:a :c :t} {:atlas/dev-id :x :atlas/type :t :execution-function/context [:k1 :k3] :execution-function/deps #{:d1}}
           #{:z :t} {:atlas/dev-id :z :atlas/type :t}})

(deftest entity-delta-names-what-moved
  (let [d (diff/entity-delta base cand :x)]
    (is (= #{:c} (:aspects-added d)))
    (is (= #{} (:aspects-removed d)))
    (is (= #{[:x :execution-function/context :k3]} (:props-added d)))
    (is (= #{[:x :execution-function/context :k2]} (:props-removed d)))
    (is (not (:new? d)))))

(deftest summary-sorts-entities-into-new-changed-deleted
  (is (= {:new [:z] :changed [:x] :deleted [:y]} (diff/summary base cand)))
  (is (= {:new [] :changed [] :deleted []} (diff/summary base base))))
