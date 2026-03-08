(ns atlas.source-tracker-test
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [atlas.source-tracker :as tracker]
            [atlas.registry :as registry]
            [clojure.java.io :as io]))

;; ---------------------------------------------------------------------------
;; Fixture — reset registry
;; ---------------------------------------------------------------------------

(use-fixtures :each
  (fn [f]
    (reset! registry/registry {})
    (f)
    (reset! registry/registry {})))

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defn- write-temp-clj
  "Write content to a temp .clj file, return its path."
  [content]
  (let [f (java.io.File/createTempFile "atlas-test" ".clj")]
    (.deleteOnExit f)
    (spit f content)
    (.getPath f)))

;; ---------------------------------------------------------------------------
;; scan-file
;; ---------------------------------------------------------------------------

(deftest scan-file-finds-register-calls
  (let [src "(ns test.app
  (:require [atlas.registry :as registry]))

(registry/register!
 :fn/validate-token
 :atlas/execution-function
 #{:domain/auth :tier/service}
 {:execution-function/context [:auth/token]
  :execution-function/response [:auth/valid?]})

(registry/register!
 :fn/refresh-token
 :atlas/execution-function
 #{:domain/auth :protocol/oauth}
 {:execution-function/context [:auth/refresh-token]
  :execution-function/response [:auth/token-pair]})
"
        file (write-temp-clj src)
        results (tracker/scan-file file)]

    (testing "finds both register! calls"
      (is (= 2 (count results))))

    (testing "extracts dev-ids"
      (is (= #{:fn/validate-token :fn/refresh-token}
             (set (map :dev-id results)))))

    (testing "captures line numbers"
      (let [by-id (zipmap (map :dev-id results) results)]
        (is (= 4 (:line (get by-id :fn/validate-token))))
        (is (number? (:end-line (get by-id :fn/validate-token))))
        (is (< (:line (get by-id :fn/validate-token))
               (:end-line (get by-id :fn/validate-token))))))))

(deftest scan-file-skips-loop-registrations
  (let [src "(ns test.ontology
  (:require [atlas.registry :as registry]))

(doseq [t [:atlas/execution-function :atlas/interface-endpoint]]
  (registry/register!
   t
   :atlas/type
   #{t}
   {:registry-definition/keys [:atlas/dev-id]}))
"
        file (write-temp-clj src)
        results (tracker/scan-file file)]

    (testing "skips register! calls with variable dev-ids"
      (is (= 0 (count results))))))

(deftest scan-string-works-like-scan-file
  (let [src "(ns test.str
  (:require [atlas.registry :as r]))

(r/register!
 :fn/from-string
 :atlas/execution-function
 #{:domain/test}
 {:execution-function/context [:test/in]
  :execution-function/response [:test/out]})
"
        results (tracker/scan-string src "virtual.clj")]

    (testing "finds register! calls from a string"
      (is (= 1 (count results))))

    (testing "extracts dev-id and line range"
      (let [r (first results)]
        (is (= :fn/from-string (:dev-id r)))
        (is (= "virtual.clj" (:file r)))
        (is (= 4 (:line r)))
        (is (number? (:end-line r)))
        (is (< (:line r) (:end-line r)))))))

(deftest scan-file-handles-3-arity
  (let [src "(ns test.app
  (:require [atlas.registry :as registry]))

(registry/register!
 :atlas/execution-function
 #{:domain/cart :tier/service}
 {:execution-function/context [:cart/items]})
"
        file (write-temp-clj src)
        results (tracker/scan-file file)]

    (testing "skips 3-arity (no literal dev-id)"
      (is (= 0 (count results))))))

;; ---------------------------------------------------------------------------
;; scan-dirs
;; ---------------------------------------------------------------------------

(deftest scan-dirs-builds-dev-id-map
  (let [dir (System/getProperty "java.io.tmpdir")
        sub (io/file dir "atlas-test-scan")
        _ (.mkdirs sub)
        src "(ns test.scan
  (:require [atlas.registry :as r]))

(r/register!
 :ep/list-users
 :atlas/interface-endpoint
 #{:domain/users :tier/api}
 {:interface-endpoint/context [:query/page]
  :interface-endpoint/response [:user/list]})
"
        file (io/file sub "test_scan.clj")]
    (try
      (spit file src)
      (let [result (tracker/scan-dirs [(.getPath sub)])]

        (testing "returns map keyed by dev-id"
          (is (contains? result :ep/list-users)))

        (testing "value has file and line info"
          (let [loc (get result :ep/list-users)]
            (is (string? (:file loc)))
            (is (number? (:line loc)))
            (is (number? (:end-line loc))))))
      (finally
        (.delete file)
        (.delete sub)))))

;; ---------------------------------------------------------------------------
;; parse-diff-ranges (via private access)
;; ---------------------------------------------------------------------------

(deftest parse-diff-ranges-extracts-hunks
  (let [parse #'tracker/parse-diff-ranges
        diff-output "diff --git a/src/app.clj b/src/app.clj
index abc123..def456 100644
--- a/src/app.clj
+++ b/src/app.clj
@@ -10,3 +10,5 @@
 unchanged
+new line 1
+new line 2
 unchanged
@@ -30,2 +32,4 @@
 unchanged
+another change
+more changes
 unchanged
"
        result (parse diff-output)]

    (testing "extracts file path"
      (is (contains? result "src/app.clj")))

    (testing "extracts both hunks"
      (is (= 2 (count (get result "src/app.clj")))))

    (testing "hunk ranges are correct"
      (let [hunks (get result "src/app.clj")]
        (is (= {:start 10 :end 14} (first hunks)))
        (is (= {:start 32 :end 35} (second hunks)))))))

;; ---------------------------------------------------------------------------
;; ranges-overlap?
;; ---------------------------------------------------------------------------

(deftest ranges-overlap-detects-overlaps
  (let [overlap? #'tracker/ranges-overlap?]

    (testing "overlapping ranges"
      (is (overlap? {:start 5 :end 10} {:start 8 :end 15})))

    (testing "contained range"
      (is (overlap? {:start 5 :end 15} {:start 8 :end 10})))

    (testing "adjacent ranges don't overlap"
      (is (not (overlap? {:start 5 :end 10} {:start 11 :end 15}))))

    (testing "non-overlapping"
      (is (not (overlap? {:start 1 :end 5} {:start 10 :end 20}))))))

;; ---------------------------------------------------------------------------
;; attach-source-locations!
;; ---------------------------------------------------------------------------

(deftest attach-source-locations-enriches-registry
  (let [dir (System/getProperty "java.io.tmpdir")
        sub (io/file dir "atlas-test-attach")
        _ (.mkdirs sub)
        src "(ns test.attach
  (:require [atlas.registry :as r]))

(r/register!
 :fn/do-thing
 :atlas/execution-function
 #{:domain/test}
 {:execution-function/context [:test/input]})
"
        file (io/file sub "test_attach.clj")]
    (try
      (spit file src)
      ;; Register the entity in the real registry
      (registry/register!
       :fn/do-thing
       :atlas/execution-function
       #{:domain/test}
       {:execution-function/context [:test/input]})

      (let [result (tracker/attach-source-locations! [(.getPath sub)])]

        (testing "reports tracking stats"
          (is (= 1 (:source-tracked result))))

        (testing "entity now has :atlas/source"
          (let [entity (registry/fetch #{:atlas/execution-function :domain/test})]
            (is (some? (:atlas/source entity)))
            (is (number? (get-in entity [:atlas/source :line])))
            (is (number? (get-in entity [:atlas/source :end-line]))))))
      (finally
        (.delete file)
        (.delete sub)))))
