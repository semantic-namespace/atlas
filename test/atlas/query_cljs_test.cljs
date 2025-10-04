(ns atlas.query-cljs-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [atlas.query :as q]))

(deftest match-score-basics
  (testing "empty selection yields 0.0 score"
    (is (= 0.0 (q/match-score #{:tier/service} #{} :count))))

  (testing ":and mode"
    (is (= 1.0 (q/match-score #{:tier/service :domain/auth} #{:tier/service} :and)))
    (is (= 0.0 (q/match-score #{:tier/service} #{:tier/service :domain/auth} :and))))

  (testing ":or mode"
    (is (= 1.0 (q/match-score #{:tier/service :domain/auth} #{:tier/service} :or)))
    (is (= 0.0 (q/match-score #{:tier/service} #{:domain/auth} :or))))

  (testing ":count mode"
    (is (= 1.0 (q/match-score #{:tier/service :domain/auth} #{:tier/service} :count)))
    (is (= 0.5 (q/match-score #{:tier/service :domain/auth} #{:tier/service :domain/users} :count)))))

(deftest query-match-with-negations
  (let [identity #{:tier/service :domain/auth}]
    (testing "negations exclude matching identities"
      (is (true? (q/query-matches? identity {::q/selected #{:tier/service}
                                            ::q/negated #{}
                                            ::q/mode :and})))
      (is (false? (q/query-matches? identity {::q/selected #{:tier/service}
                                             ::q/negated #{:domain/auth}
                                             ::q/mode :and}))))

    (testing "negation-only query is supported"
      (is (true? (q/query-matches? identity {::q/selected #{}
                                            ::q/negated #{:domain/users}
                                            ::q/mode :or})))
      (is (false? (q/query-matches? identity {::q/selected #{}
                                             ::q/negated #{:domain/auth}
                                             ::q/mode :or}))))

    (testing "min-score threshold only applies when selections exist"
      (is (true? (q/query-matches? identity {::q/selected #{}
                                            ::q/negated #{}
                                            ::q/mode :count
                                            ::q/min-score 0.9})))
      (is (false? (q/query-matches? identity {::q/selected #{:tier/service :domain/users}
                                             ::q/negated #{}
                                             ::q/mode :count
                                             ::q/min-score 0.9})))
      (is (true? (q/query-matches? identity {::q/selected #{:tier/service :domain/users}
                                            ::q/negated #{}
                                            ::q/mode :count
                                            ::q/min-score 0.5}))))))

(deftest find-by-aspect-in-cljs
  (let [reg {#{:tier/service :domain/auth} {:atlas/dev-id :fn/a}
             #{:tier/service :domain/users} {:atlas/dev-id :fn/b}
             #{:tier/api :domain/users} {:atlas/dev-id :endpoint/c}}
        res (q/find-by-aspect reg :tier/service)]
    (is (= #{:fn/a :fn/b} (set (map (comp :atlas/dev-id val) res))))))
