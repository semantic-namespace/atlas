(ns atlas.review.ownership-test
  (:require [clojure.test :refer [deftest is]]
            [atlas.review.ownership :as o]))

(def snap
  {:forms {["a.clj" 1] {:name "defn helper"}
           ["a.clj" 5] {:name "defn impl"}
           ["a.clj" 9] {:name "registry/register! :fn.x/add"}
           ["b.clj" 1] {:name "registry/register! :endpoint/add"}
           ["b_test.clj" 1] {:name "registry/register! :test/add" :test true}}
   :callers {["a.clj" 1] #{["a.clj" 5]}
             ["a.clj" 5] #{["a.clj" 9]}
             ["a.clj" 9] #{["b.clj" 1] ["b_test.clj" 1]}}})

(deftest owners-are-the-first-registrations-met-walking-callers-back
  (is (= [["a.clj" 9]] (#'o/walk-back snap ["a.clj" 1] true)))
  (is (= [["a.clj" 9] ["b.clj" 1]] (#'o/walk-back snap ["a.clj" 1] false)) "past the owner, test registrations left out"))

(deftest a-verb-is-a-write-unless-it-is-a-known-read
  (is (#'o/writes? #{:action/unsubscribe :domain/mailing-lists}))
  (is (with-redefs [o/settings (atom (update @o/settings :verb-namespaces conj "grant"))]
        (#'o/writes? #{:grant/modify}))
      "a host adds its own verb namespaces")
  (is (not (#'o/writes? #{:action/query :domain/logins})))
  (is (not (#'o/writes? #{:domain/logins}))))

(deftest exposure-alone-does-not-lift
  (is (nil? (o/weight {:owners [{:aspects #{:action/query}}] :exposed [{:type ":atlas/endpoint"}]})))
  (is (= [:writes] (o/weight {:owners [{:aspects #{:action/add}}]})))
  (is (= [:new-io] (o/weight {:owners [{:aspects #{} :reaches-added [:http]}]}))))
