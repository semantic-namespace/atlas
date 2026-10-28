(ns atlas.meta-source-types-from-test
  "atlas's meta-source pack: an entity atlas's facts describe takes its
   type from the ontology covering another library's facts about it, and
   the merged entities are atlas's registry."
  (:require [clojure.test :refer [deftest is testing]]
            [atlas.meta-source :as ms]))

(def ^:private derive-types #'atlas.meta-source/derive-types)
(def ^:private checks #'atlas.meta-source/checks)

(def ^:private command-ontology
  {:atlas/type :atlas/ontology :atlas/aspects #{:ddd/command}
   :ontology/for :ddd/command :ontology/keys [:command/targets]
   :ontology/from {:overarch/el :command}})

(defn- entities [m] (update-vals m (fn [entry] {:entry entry :kinds #{:edn}})))

(deftest an-ontology-from-another-vocabulary-types-what-atlas-describes
  (let [es (entities {:ddd/command command-ontology
                      :bank/open {:overarch/el :command :atlas/aspects #{:domain/accounts}}
                      :bank/close {:overarch/el :command :atlas/aspects #{:domain/accounts :operation/close}
                                   :atlas/type :app/special}
                      :bank/unannotated {:overarch/el :command}
                      :bank/view {:overarch/el :use-case :atlas/aspects #{:domain/accounts}}})]
    (testing "an entity with aspects and no type takes the type of the ontology covering it"
      (is (= {:bank/open {:atlas/type :ddd/command}} (derive-types es {}))))
    (testing "a type a source gives is kept; an entity no atlas fact describes, or no ontology covers, gets none"
      (is (not-any? #{:bank/close :bank/unannotated :bank/view} (keys (derive-types es {})))))))

(deftest two-ontologies-covering-an-entity-give-it-no-type-and-say-so
  (let [es (entities {:ddd/command command-ontology
                      :cqrs/command (assoc command-ontology :ontology/for :cqrs/command :atlas/aspects #{:cqrs/command})
                      :bank/open {:overarch/el :command :atlas/aspects #{:domain/accounts}}})]
    (is (empty? (derive-types es {})))
    (is (= [[:bank/open [:cqrs/command :ddd/command]]]
           (for [p (checks {:entities es :data-key? (constantly false) :loc (constantly nil)
                            :impl (constantly nil) :linked-by (constantly nil)})
                 :when (= :type-ambiguous (:problem p))]
             [(:dev-id p) (:types p)])))))

(deftest the-merged-entities-are-atlas-s-registry
  (let [reg (ms/registry-of {:bank/open {:atlas/type :ddd/command :atlas/aspects #{:domain/accounts}
                                         :overarch/el :command :command/targets :bank/account}
                             :bank/drawn-only {:overarch/el :component}})]
    (testing "keyed by compound identity, with every source's facts and the dev-id"
      (is (= {#{:ddd/command :domain/accounts}
              {:atlas/type :ddd/command :atlas/aspects #{:domain/accounts} :overarch/el :command
               :command/targets :bank/account :atlas/dev-id :bank/open}}
             reg)))))
