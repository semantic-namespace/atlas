(ns atlas.ontology.shapeup
  "Shape Up process ontology module. Auto-registers on require.

   Defines entity types for Shape Up methodology artifacts:
   - :atlas/shapeup-pitch  — shaped problem + solution, not yet bet
   - :atlas/shapeup-bet    — selected pitch for a cycle
   - :atlas/shapeup-scope  — chunk of work within a bet (has deps)

   Usage:
     (require '[atlas.ontology.shapeup])"
  (:require [atlas.registry :as registry]
            [atlas.registry.lookup :as entity]
            [atlas.ontology.type-ref :as type-ref]
            [atlas.ontology.llm-prompt]
            [clojure.spec.alpha :as s]))

;; ---------------------------------------------------------------------------
;; Specs
;; ---------------------------------------------------------------------------

(s/def :shapeup/summary string?)
(s/def :shapeup/cycle string?)
(s/def :shapeup/cut-reason string?)
(s/def :shapeup/shipped-summary string?)
(s/def :shapeup/parent-pitch :atlas/dev-id)
(s/def :shapeup/parent-bet :atlas/dev-id)
(s/def :shapeup/deps (s/coll-of :atlas/dev-id))

;; ---------------------------------------------------------------------------
;; :atlas/shapeup-pitch
;; ---------------------------------------------------------------------------

(registry/register!
 :atlas/shapeup-pitch
 :atlas/ontology
 #{:atlas/shapeup-pitch}
 {:ontology/for  :atlas/shapeup-pitch
  :ontology/keys [:shapeup/summary
                  :shapeup/cycle
                  :shapeup/cut-reason
                  :shapeup/shipped-summary]})

;; ---------------------------------------------------------------------------
;; :atlas/shapeup-bet
;; ---------------------------------------------------------------------------

(registry/register!
 :atlas/shapeup-bet
 :atlas/ontology
 #{:atlas/shapeup-bet}
 {:ontology/for  :atlas/shapeup-bet
  :ontology/keys [:shapeup/summary
                  :shapeup/cycle
                  :shapeup/parent-pitch
                  :shapeup/cut-reason
                  :shapeup/shipped-summary]})

(registry/register!
 :type-ref/shapeup-bet-pitch
 :atlas/type-ref
 #{:meta/ref-shapeup-bet-pitch}
 {:type-ref/source       :atlas/shapeup-bet
  :type-ref/property     :shapeup/parent-pitch
  :type-ref/datalog-verb :shapeup/parent-pitch
  :type-ref/cardinality  :db.cardinality/one})

;; Datalog extractor for bets
(registry/register!
 :datalog-extractor/shapeup-bet
 :atlas/datalog-extractor
 #{:meta/shapeup-bet-extractor}
 {:datalog-extractor/fn
  (fn [compound-id props]
    (when (contains? compound-id :atlas/shapeup-bet)
      (type-ref/extract-reference-facts :atlas/shapeup-bet compound-id props)))
  :datalog-extractor/schema
  {:shapeup/parent-pitch {:db/cardinality :db.cardinality/one}}})

;; ---------------------------------------------------------------------------
;; :atlas/shapeup-scope
;; ---------------------------------------------------------------------------

(registry/register!
 :atlas/shapeup-scope
 :atlas/ontology
 #{:atlas/shapeup-scope}
 {:ontology/for  :atlas/shapeup-scope
  :ontology/keys [:shapeup/summary
                  :shapeup/cycle
                  :shapeup/parent-bet
                  :shapeup/deps
                  :shapeup/cut-reason
                  :shapeup/shipped-summary]})

(registry/register!
 :type-ref/shapeup-scope-bet
 :atlas/type-ref
 #{:meta/ref-shapeup-scope-bet}
 {:type-ref/source       :atlas/shapeup-scope
  :type-ref/property     :shapeup/parent-bet
  :type-ref/datalog-verb :shapeup/parent-bet
  :type-ref/cardinality  :db.cardinality/one})

(registry/register!
 :type-ref/shapeup-scope-deps
 :atlas/type-ref
 #{:meta/ref-shapeup-scope-deps}
 {:type-ref/source       :atlas/shapeup-scope
  :type-ref/property     :shapeup/deps
  :type-ref/datalog-verb :entity/depends
  :type-ref/cardinality  :db.cardinality/many})

;; Datalog extractor for scopes
(registry/register!
 :datalog-extractor/shapeup-scope
 :atlas/datalog-extractor
 #{:meta/shapeup-scope-extractor}
 {:datalog-extractor/fn
  (fn [compound-id props]
    (when (contains? compound-id :atlas/shapeup-scope)
      (type-ref/extract-reference-facts :atlas/shapeup-scope compound-id props)))
  :datalog-extractor/schema
  {:shapeup/parent-bet {:db/cardinality :db.cardinality/one}
   :entity/depends     {:db/cardinality :db.cardinality/many}}})

;; ---------------------------------------------------------------------------
;; Invariants
;; ---------------------------------------------------------------------------

(registry/register!
 :invariant/shapeup-cut-has-reason
 :atlas/invariant
 #{:meta/shapeup-cut-reason-check}
 {:invariant/fn
  (fn []
    (let [all-shapeup (concat (entity/all-with-aspect :atlas/shapeup-pitch)
                              (entity/all-with-aspect :atlas/shapeup-bet)
                              (entity/all-with-aspect :atlas/shapeup-scope))
          violations  (for [id all-shapeup
                            :when (entity/has-aspect? id :status/cut)
                            :when (not (:shapeup/cut-reason (entity/props-for id)))]
                        {:entity id})]
      (when (seq violations)
        {:invariant :shapeup-cut-has-reason
         :violation :cut-without-reason
         :details   (vec violations)
         :severity  :error
         :message   "Cut entities must have :shapeup/cut-reason — institutional memory depends on it"})))})

(registry/register!
 :invariant/shapeup-bet-pitch-exists
 :atlas/invariant
 #{:meta/shapeup-bet-pitch-check}
 {:invariant/fn
  (fn []
    (let [violations (for [bet-id (entity/all-with-aspect :atlas/shapeup-bet)
                           :let   [pitch-id (:shapeup/parent-pitch (entity/props-for bet-id))]
                           :when  (and pitch-id (nil? (entity/identity-for pitch-id)))]
                       {:bet bet-id :missing-pitch pitch-id})]
      (when (seq violations)
        {:invariant :shapeup-bet-pitch-exists
         :violation :bet-references-missing-pitch
         :details   (vec violations)
         :severity  :warning
         :message   "Bets reference pitches not found in registry"})))})

(registry/register!
 :invariant/shapeup-scope-bet-exists
 :atlas/invariant
 #{:meta/shapeup-scope-bet-check}
 {:invariant/fn
  (fn []
    (let [violations (for [scope-id (entity/all-with-aspect :atlas/shapeup-scope)
                           :let     [bet-id (:shapeup/parent-bet (entity/props-for scope-id))]
                           :when    (and bet-id (nil? (entity/identity-for bet-id)))]
                       {:scope scope-id :missing-bet bet-id})]
      (when (seq violations)
        {:invariant :shapeup-scope-bet-exists
         :violation :scope-references-missing-bet
         :details   (vec violations)
         :severity  :warning
         :message   "Scopes reference bets not found in registry"})))})

(registry/register!
 :invariant/shapeup-deferred-has-reason
 :atlas/invariant
 #{:meta/shapeup-deferred-reason-check}
 {:invariant/fn
  (fn []
    (let [all-shapeup (concat (entity/all-with-aspect :atlas/shapeup-pitch)
                              (entity/all-with-aspect :atlas/shapeup-bet)
                              (entity/all-with-aspect :atlas/shapeup-scope))
          violations  (for [id all-shapeup
                            :when (entity/has-aspect? id :status/deferred)
                            :when (not (:shapeup/cut-reason (entity/props-for id)))]
                        {:entity id})]
      (when (seq violations)
        {:invariant :shapeup-deferred-has-reason
         :violation :deferred-without-reason
         :details   (vec violations)
         :severity  :error
         :message   "Deferred entities must have :shapeup/cut-reason recording why and what would unblock it"})))})

(registry/register!
 :invariant/shapeup-scope-deps-same-bet
 :atlas/invariant
 #{:meta/shapeup-scope-deps-check}
 {:invariant/fn
  (fn []
    (let [violations (for [scope-id (entity/all-with-aspect :atlas/shapeup-scope)
                           :let     [props    (entity/props-for scope-id)
                                     bet-id   (:shapeup/parent-bet props)
                                     dep-ids  (:shapeup/deps props)]
                           dep-id   dep-ids
                           :let     [dep-bet (:shapeup/parent-bet (entity/props-for dep-id))]
                           :when    (and dep-bet bet-id (not= dep-bet bet-id))]
                       {:scope scope-id :dep dep-id :scope-bet bet-id :dep-bet dep-bet})]
      (when (seq violations)
        {:invariant :shapeup-scope-deps-same-bet
         :violation :dep-crosses-bet-boundary
         :details   (vec violations)
         :severity  :warning
         :message   "Scope deps should reference scopes within the same bet"})))})

;; ---------------------------------------------------------------------------
;; Skills (llm-prompt entities for Shape Up workflows)
;; ---------------------------------------------------------------------------

(registry/register!
 :shapeup.prompts.write/pitch
 :atlas/llm-prompt
 #{:atlas/llm-prompt :domain/shapeup :intent/write :role/pm :operation/pitch}
 {:llm-prompt/summary  "Guide a PM through writing a Shape Up pitch — elicit problem, appetite, and solution, then register as :atlas/shapeup-pitch"
  :llm-prompt/file     ".claude/commands/shape-pitch.md"
  :llm-prompt/mcp-deps [:atlas.llm-ide/cloud-propose
                        :atlas.llm-ide/suggest-placement]
  :llm-prompt/produces :atlas/shapeup-pitch})

(registry/register!
 :shapeup.prompts.write/bet
 :atlas/llm-prompt
 #{:atlas/llm-prompt :domain/shapeup :intent/write :role/pm :operation/bet}
 {:llm-prompt/summary  "Guide a PM through placing a bet on a pitch for the upcoming cycle — registers as :atlas/shapeup-bet"
  :llm-prompt/file     ".claude/commands/shape-bet.md"
  :llm-prompt/mcp-deps [:atlas.llm-ide/cloud-propose
                        :atlas.llm-ide/entity-detail]
  :llm-prompt/produces :atlas/shapeup-bet})

(registry/register!
 :shapeup.prompts.write/scope
 :atlas/llm-prompt
 #{:atlas/llm-prompt :domain/shapeup :intent/write :role/pm :operation/scope}
 {:llm-prompt/summary  "Break a bet into scopes — elicit chunks, dependencies, and hill status, register all as :atlas/shapeup-scope"
  :llm-prompt/file     ".claude/commands/shape-scope.md"
  :llm-prompt/mcp-deps [:atlas.llm-ide/cloud-propose
                        :atlas.llm-ide/entity-detail]
  :llm-prompt/produces :atlas/shapeup-scope})

(registry/register!
 :shapeup.prompts.write/cut
 :atlas/llm-prompt
 #{:atlas/llm-prompt :domain/shapeup :intent/write :role/pm :operation/cut}
 {:llm-prompt/summary  "Mark a scope or bet as cut or shipped — enforces cut-reason, warns on deps, updates status via cloud-propose"
  :llm-prompt/file     ".claude/commands/shape-cut.md"
  :llm-prompt/mcp-deps [:atlas.llm-ide/cloud-propose
                        :atlas.llm-ide/entity-detail
                        :atlas.llm-ide/blast-radius]})
