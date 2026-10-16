(ns atlas.ontology.allium-rule-test
  (:require
   [clojure.test :refer [deftest is use-fixtures testing]]
   [clojure.string :as str]
   [atlas.registry :as registry]
   [atlas.datalog :as datalog]
   [atlas.ide :as ide]
   [atlas.invariant :as inv]
   [atlas.adapter.allium :as allium]))

(use-fixtures :each
  (fn [f]
    (registry/reset-all!)
    (datalog/reset-all!)
    (require 'atlas.ontology.execution-function :reload)
    (require 'atlas.ontology.allium-rule :reload)
    (f)
    (registry/reset-all!)
    (datalog/reset-all!)))

(defn- seed! []
  (registry/register!
   :fn/start-trial
   :atlas/execution-function
   #{:domain/subscription :operation/create :tier/service}
   {:execution-function/context [:member/id]
    :execution-function/response [:trial/created-on]})
  (registry/register!
   :rule/first-scan-starts-trial
   :atlas/allium-rule
   #{:domain/subscription :operation/create}
   {:rule/name "FirstBankScanStartsTrial"
    :rule/when "BankScanRequested(bank_item)"
    :rule/requires ["not exists Trial{member: bank_item.member}"]
    :rule/ensures ["Trial.created(member: bank_item.member, created_on: now)"]
    :rule/implemented-by :fn/start-trial}))

(defn- violations-of [inv-key]
  (->> (:violations (inv/check-all))
       (filter #(= inv-key (:invariant %)))))

(deftest rule-is-a-dependent-of-its-implementor
  (seed!)
  (testing "a change to the implementing fn reaches the rule"
    (is (contains? (set (ide/dependents-of :fn/start-trial))
                   :rule/first-scan-starts-trial))))

(deftest implemented-by-must-resolve
  (seed!)
  (testing "a rule pointing at a registered entity passes"
    (is (empty? (violations-of :allium-rule-implemented-by-exists))))

  (testing "a rule pointing at a missing entity is an error"
    (registry/register!
     :rule/trial-deleted
     :atlas/allium-rule
     #{:domain/subscription :operation/delete}
     {:rule/name "TrialDeleted"
      :rule/when "TrialDeletionRequested(member)"
      :rule/ensures ["not exists Trial{member: member}"]
      :rule/implemented-by :endpoint/delete-trial})
    (let [[v] (violations-of :allium-rule-implemented-by-exists)]
      (is (= :error (:severity v)))
      (is (= [{:entity :rule/trial-deleted :missing-implementor :endpoint/delete-trial}]
             (:details v)))))

  (testing "a rule with no implementor is not flagged: it may state intent the code lacks"
    (registry/register!
     :rule/one-institution
     :atlas/allium-rule
     #{:domain/subscription :operation/limit}
     {:rule/name "OneInstitutionDuringTrial"
      :rule/when "BankConnectRequested(member)"
      :rule/ensures ["BankConnectRejected(member: member)"]})
    (is (not-any? #(= :rule/one-institution (:entity %))
                  (mapcat :details (violations-of :allium-rule-implemented-by-exists))))))

(deftest rule-names-are-unique
  (seed!)
  (is (empty? (violations-of :allium-rule-names-unique)))
  (registry/register!
   :rule/first-scan-starts-trial-v2
   :atlas/allium-rule
   #{:domain/subscription :operation/start}
   {:rule/name "FirstBankScanStartsTrial"
    :rule/when "BankScanRequested(bank_item)"
    :rule/ensures ["Trial.created(member: bank_item.member, created_on: now)"]})
  (let [[v] (violations-of :allium-rule-names-unique)]
    (is (= [{:rule-name "FirstBankScanStartsTrial"
             :entities [:rule/first-scan-starts-trial :rule/first-scan-starts-trial-v2]}]
           (:details v)))))

(deftest emitter-projects-registered-rules
  (seed!)
  (let [{:keys [allium mode]} (allium/registry->allium
                               @registry/registry
                               {:select (fn [cid _] (contains? cid :domain/subscription))
                                :module-name "subscription"})]
    (is (= :project mode))
    (is (str/includes? allium "rule FirstBankScanStartsTrial {"))
    (is (str/includes? allium "-- atlas implemented-by: fn/start-trial"))
    (is (not (str/includes? allium "rule StartTrial"))
        "in :project mode the implementing fn is not scaffolded as a rule of its own")))
