(ns atlas.adapter.tilakone-test
  (:require
   [clojure.test :refer [deftest is use-fixtures testing]]
   [atlas.adapter.tilakone :as tk-adapter]
   [atlas.registry :as registry]
   [atlas.registry.lookup :as entity]
   [atlas.datalog :as datalog]))

(use-fixtures :each
  (fn [f]
    (registry/reset-all!)
    (datalog/reset-all!)
    (require 'atlas.ontology.workflow-producer :reload)
    (require 'atlas.ontology.workflow :reload)
    (f)
    (registry/reset-all!)
    (datalog/reset-all!)))

;; =============================================================================
;; TEST DATA — gmail-message workflow (simplified from yorba)
;; =============================================================================

(def gmail-message-fsm
  {:tilakone.core/states
   [{:tilakone.core/name        :lbi.msg/init
     :tilakone.core/transitions [{:tilakone.core/on :wf/success :tilakone.core/to :gmail/bearer-token}
                                 {:tilakone.core/on :wf/error   :tilakone.core/to :lbi.msg/error}
                                 {:tilakone.core/on :lbi/exited :tilakone.core/to :wf/exit}]}
    {:tilakone.core/name        :gmail/bearer-token
     :tilakone.core/transitions [{:tilakone.core/on :wf/success :tilakone.core/to :gmail/downloaded-and-scored}
                                 {:tilakone.core/on :wf/error   :tilakone.core/to :lbi.msg/error}]}
    {:tilakone.core/name        :gmail/downloaded-and-scored
     :tilakone.core/transitions [{:tilakone.core/on :wf/success    :tilakone.core/to :lbi.msg/enriched}
                                 {:tilakone.core/on :wf/not-scored :tilakone.core/to :lbi.msg/next-enqueued}
                                 {:tilakone.core/on :gmail/retry   :tilakone.core/to :gmail/downloaded-and-scored}
                                 {:tilakone.core/on :wf/error      :tilakone.core/to :lbi.msg/error}]}
    {:tilakone.core/name        :lbi.msg/enriched
     :tilakone.core/transitions [{:tilakone.core/on :wf/success :tilakone.core/to :lbi.msg/next-enqueued}
                                 {:tilakone.core/on :wf/error   :tilakone.core/to :lbi.msg/error}]}
    {:tilakone.core/name        :lbi.msg/next-enqueued
     :tilakone.core/transitions [{:tilakone.core/on :wf/success :tilakone.core/to :wf/exit}
                                 {:tilakone.core/on :wf/error   :tilakone.core/to :lbi.msg/error-enqueued}]}
    {:tilakone.core/name        :lbi.msg/error
     :wf/log-level              :info
     :tilakone.core/transitions [{:tilakone.core/on '_ :tilakone.core/to :lbi.msg/next-enqueued}]}
    {:tilakone.core/name        :lbi.msg/error-enqueued
     :wf/log-level              :warn
     :tilakone.core/transitions [{:tilakone.core/on '_}]}
    {:tilakone.core/name        :wf/exit
     :tilakone.core/transitions [{:tilakone.core/on '_}]}]
   :tilakone.core/state :lbi.msg/init
   :workflow-id :gmail/message
   :queue-type :lbi-message-wf-2})

;; =============================================================================
;; TILAKONE → ATLAS
;; =============================================================================

(deftest tilakone->atlas-producers
  (let [atlas-data (tk-adapter/tilakone->atlas gmail-message-fsm)
        producers (:producers atlas-data)
        by-id (into {} (map (juxt :dev-id identity) producers))]

    (testing "extracts all states as producers"
      (is (= 8 (count producers)))
      (is (contains? by-id :lbi.msg/init))
      (is (contains? by-id :gmail/bearer-token))
      (is (contains? by-id :wf/exit)))

    (testing "extracts signals per producer"
      (is (= #{:wf/success :wf/error :lbi/exited}
             (:signals (by-id :lbi.msg/init))))
      (is (= #{:wf/success :wf/not-scored :gmail/retry :wf/error}
             (:signals (by-id :gmail/downloaded-and-scored)))))

    (testing "catch-all becomes :signal/any"
      (is (= #{:signal/any}
             (:signals (by-id :lbi.msg/error)))))

    (testing "preserves state metadata"
      (is (= {:wf/log-level :info}
             (:state-meta (by-id :lbi.msg/error))))
      (is (= {:wf/log-level :warn}
             (:state-meta (by-id :lbi.msg/error-enqueued))))
      (is (nil? (:state-meta (by-id :lbi.msg/init)))))))

(deftest tilakone->atlas-workflow
  (let [atlas-data (tk-adapter/tilakone->atlas gmail-message-fsm)
        workflow (:workflow atlas-data)]

    (testing "initial producer"
      (is (= :lbi.msg/init (:initial-producer workflow))))

    (testing "all producers listed"
      (is (= 8 (count (:producers workflow)))))

    (testing "per-producer transitions"
      (is (= {:wf/success :gmail/bearer-token
              :wf/error :lbi.msg/error
              :lbi/exited :wf/exit}
             (get-in workflow [:transitions :lbi.msg/init]))))

    (testing "self-transition (retry)"
      (is (= :gmail/downloaded-and-scored
             (get-in workflow [:transitions :gmail/downloaded-and-scored :gmail/retry]))))

    (testing "terminal transition maps to :workflow/exit"
      (is (= :workflow/exit
             (get-in workflow [:transitions :lbi.msg/error-enqueued :signal/any]))))

    (testing "runtime metadata preserved"
      (is (= {:workflow-id :gmail/message
              :queue-type :lbi-message-wf-2}
             (:runtime-meta workflow))))))

;; =============================================================================
;; ROUNDTRIP
;; =============================================================================

(deftest roundtrip-tilakone->atlas->tilakone
  (let [atlas-data (tk-adapter/tilakone->atlas gmail-message-fsm)]
    ;; Register into atlas
    (tk-adapter/register-atlas! atlas-data
      {:workflow-dev-id :wf/gmail-message
       :workflow-aspects #{:domain/logins :services/gmail}})

    ;; Convert back
    (let [roundtrip (tk-adapter/atlas->tilakone :wf/gmail-message)
          original-norm (tk-adapter/normalize-tilakone gmail-message-fsm)
          result-norm (tk-adapter/normalize-tilakone roundtrip)]

      (testing "roundtrip produces identical normalized FSM"
        (is (= original-norm result-norm)))

      (testing "initial state preserved"
        (is (= (:tilakone.core/state original-norm)
               (:tilakone.core/state result-norm))))

      (testing "runtime metadata preserved"
        (is (= (:workflow-id original-norm) (:workflow-id result-norm)))
        (is (= (:queue-type original-norm) (:queue-type result-norm))))

      (testing "state count preserved"
        (is (= (count (:tilakone.core/states original-norm))
               (count (:tilakone.core/states result-norm))))))))

(deftest roundtrip-with-name-fn
  (let [;; Use a name-fn that prefixes with wp/
        name-fn (fn [state-name]
                  (keyword "wp" (str (namespace state-name) "." (name state-name))))
        reverse-fn (fn [producer-id]
                     (let [n (name producer-id)
                           ;; wp/lbi.msg.init -> :lbi.msg/init
                           dot-idx (.lastIndexOf n ".")]
                       (keyword (subs n 0 dot-idx) (subs n (inc dot-idx)))))
        atlas-data (tk-adapter/tilakone->atlas gmail-message-fsm {:name-fn name-fn})]

    (tk-adapter/register-atlas! atlas-data
      {:workflow-dev-id :wf/gmail-renamed
       :workflow-aspects #{:domain/logins}})

    (let [roundtrip (tk-adapter/atlas->tilakone :wf/gmail-renamed
                      {:reverse-name-fn reverse-fn})
          original-norm (tk-adapter/normalize-tilakone gmail-message-fsm)
          result-norm (tk-adapter/normalize-tilakone roundtrip)]

      (testing "roundtrip with name mapping produces identical FSM"
        (is (= original-norm result-norm))))))

;; =============================================================================
;; REGISTRATION VALIDATION
;; =============================================================================

(deftest registered-producers-have-correct-props
  (let [atlas-data (tk-adapter/tilakone->atlas gmail-message-fsm)]
    (tk-adapter/register-atlas! atlas-data
      {:workflow-dev-id :wf/gmail-message
       :workflow-aspects #{:domain/logins}})

    (testing "each producer is registered with correct signals"
      (let [init-props (entity/props-for :lbi.msg/init)]
        (is (= #{:wf/success :wf/error :lbi/exited}
               (:workflow-producer/signals init-props)))))

    (testing "error state has state-meta with log-level"
      (let [error-props (entity/props-for :lbi.msg/error)]
        (is (= {:wf/log-level :info}
               (:tilakone/state-meta error-props)))))

    (testing "workflow has correct transitions structure"
      (let [wf-props (entity/props-for :wf/gmail-message)]
        (is (= :lbi.msg/init (:workflow/initial-producer wf-props)))
        (is (= 8 (count (:workflow/producers wf-props))))
        (is (map? (get-in wf-props [:workflow/transitions :lbi.msg/init])))))))

(deftest registered-workflow-has-runtime-meta
  (let [atlas-data (tk-adapter/tilakone->atlas gmail-message-fsm)]
    (tk-adapter/register-atlas! atlas-data
      {:workflow-dev-id :wf/gmail-message
       :workflow-aspects #{}})

    (testing "runtime metadata stored in workflow props"
      (let [wf-props (entity/props-for :wf/gmail-message)]
        (is (= {:workflow-id :gmail/message
                :queue-type :lbi-message-wf-2}
               (:tilakone/runtime-meta wf-props)))))))
