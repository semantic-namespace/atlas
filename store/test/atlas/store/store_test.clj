(ns atlas.store.store-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [atlas.store :as store]
   [atlas.store.file :as file]
   [atlas.store.protocol :as p]))


(defn- tmp-dir []
  (let [d (java.io.File/createTempFile "atlas-store" "")]
    (.delete d) (.mkdirs d) (.getAbsolutePath d)))

(defn- reg-of [n]
  (into {} (for [i (range n)]
             [#{(keyword "fn" (str "f" i)) :atlas/execution-function}
              {:atlas/dev-id (keyword "fn" (str "f" i))
               :atlas/type :atlas/execution-function}])))


(deftest writes-then-detects-no-change
  (let [s (file/file-store (tmp-dir))
        r (reg-of 50)]
    (is (:changed? (store/write-if-changed! s r {:message "first"})))
    (testing "the same registry a second time is free -- no write, no diff engine"
      (is (false? (:changed? (store/write-if-changed! s r {:message "second"})))))
    (testing "and a real change is written"
      (is (:changed? (store/write-if-changed! s (assoc-in r [#{:fn/f0 :atlas/execution-function}
                                                             :execution-function/deps]
                                                          #{:component/db})
                                              {:message "third"}))))))


(deftest deleted-entities-disappear
  ;; Replacement, not merge: a stale file would read back as a live entity.
  (let [s (file/file-store (tmp-dir))]
    (store/write-if-changed! s (reg-of 50) {:message "a"})
    (store/write-if-changed! s (reg-of 46) {:message "b" :max-shrink 0.2})
    (is (= 46 (count (atlas.store.canonical/files->registry (p/read-at s nil)))))))


(deftest a-collapsed-build-is-refused
  ;; The failure this guard exists for: an incomplete registry build is
  ;; well-formed and silent, and writing it corrupts the history.
  (let [s (file/file-store (tmp-dir))]
    (store/write-if-changed! s (reg-of 1000) {:message "full"})
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Refusing to write"
                          (store/write-if-changed! s (reg-of 600) {:message "partial"})))
    (testing "a small change is still allowed"
      (is (:changed? (store/write-if-changed! s (reg-of 960) {:message "ok"}))))
    (testing "and a genuine mass deletion can be forced"
      (is (:changed? (store/write-if-changed! s (reg-of 10) {:message "real" :force? true}))))))


(deftest reproducibility-assertion
  (let [r (reg-of 10)]
    (is (true? (store/assert-reproducible! r r)))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not reproducible"
                          (store/assert-reproducible!
                           r (assoc-in r [#{:fn/f0 :atlas/execution-function} :x/stamp] 1))))))
