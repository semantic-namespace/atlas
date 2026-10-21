(ns fixtures.invariant-rules.entities
  "Entities that pass and that fail each invariant carrying an
   :invariant/rule. atlas.invariant-rules-test runs the invariants' fns on
   them; bb atlas-pack-smoke runs their rules. Both must fail the same ones.
   Each has a :case/* aspect of its own: two entities with one compound
   identity would be one entity in the registry."
  (:require [atlas.registry :as registry]))

(defn register-all! []
  (registry/register! :fixture/endpoint-api :atlas/interface-endpoint #{:domain/fixture :tier/api :case/endpoint-api} {})
  (registry/register! :fixture/endpoint-service :atlas/interface-endpoint #{:domain/fixture :tier/service :case/endpoint-service} {})

  (registry/register! :fixture/component-foundation :atlas/structure-component #{:domain/fixture :tier/foundation :case/component-foundation} {})
  (registry/register! :fixture/component-service :atlas/structure-component #{:domain/fixture :tier/service :case/component-service} {})

  (registry/register! :fixture/fn-helper :atlas/execution-function #{:domain/fixture :tier/service :case/fn-helper} {})
  (registry/register! :fixture/fn-pure-ok :atlas/execution-function #{:domain/fixture :effect/pure :case/fn-pure-ok}
                      {:execution-function/deps #{:fixture/fn-helper}})
  (registry/register! :fixture/fn-pure-with-component :atlas/execution-function #{:domain/fixture :effect/pure :case/fn-pure-with-component}
                      {:execution-function/deps #{:fixture/component-foundation}})

  (registry/register! :fixture/decision-retired-with-reason :atlas/decision #{:domain/fixture :status/retired :case/decision-retired-with-reason}
                      {:decision/retirement-reason "replaced by the v2 flow"})
  (registry/register! :fixture/decision-retired-bare :atlas/decision #{:domain/fixture :status/retired :case/decision-retired-bare} {})
  (registry/register! :fixture/decision-active-realized :atlas/decision #{:domain/fixture :status/active :case/decision-active-realized}
                      {:decision/realized-by [:engine/fixture]})
  (registry/register! :fixture/decision-active-bare :atlas/decision #{:domain/fixture :status/active :case/decision-active-bare} {})

  (registry/register! :fixture/authority-revoked-with-reason :atlas/decision-authority #{:domain/fixture :status/revoked :case/authority-revoked-with-reason}
                      {:authority/revocation-reason "policy ended"})
  (registry/register! :fixture/authority-revoked-bare :atlas/decision-authority #{:domain/fixture :status/revoked :case/authority-revoked-bare} {})

  (registry/register! :fixture/pattern-used :atlas/business-pattern #{:domain/fixture :case/pattern-used} {})
  (registry/register! :fixture/pattern-unused :atlas/business-pattern #{:domain/fixture :case/pattern-unused} {})

  (registry/register! :fixture/vp-bounded :atlas/value-proposition #{:domain/fixture :case/vp-bounded}
                      {:value-proposition/external-boundaries [:boundary/fixture]
                       :value-proposition/implements-pattern :fixture/pattern-used})
  (registry/register! :fixture/vp-dangling-pattern :atlas/value-proposition #{:domain/fixture :case/vp-dangling-pattern}
                      {:value-proposition/external-boundaries [:boundary/fixture]
                       :value-proposition/implements-pattern :fixture/pattern-gone})
  (registry/register! :fixture/vp-bare :atlas/value-proposition #{:domain/fixture :case/vp-bare} {})

  (registry/register! :fixture/system-ok :atlas/system #{:domain/fixture :case/system-ok}
                      {:system/delivers [:fixture/vp-bounded :fixture/vp-dangling-pattern]})
  (registry/register! :fixture/system-dangling :atlas/system #{:domain/fixture :case/system-dangling}
                      {:system/delivers [:fixture/vp-gone]})

  (registry/register! :fixture/deploy-ok :atlas/deployment #{:domain/fixture :case/deploy-ok}
                      {:deployment/system :fixture/system-ok :deployment/git-sha "abc123"})
  (registry/register! :fixture/deploy-dangling :atlas/deployment #{:domain/fixture :case/deploy-dangling}
                      {:deployment/system :fixture/system-gone :deployment/git-sha "abc123"})

  (registry/register! :fixture/rule-ok :atlas/allium-rule #{:domain/fixture :case/rule-ok}
                      {:rule/name "FixtureOk" :rule/implemented-by :fixture/fn-helper})
  (registry/register! :fixture/rule-dangling :atlas/allium-rule #{:domain/fixture :case/rule-dangling}
                      {:rule/name "FixtureDangling" :rule/implemented-by :fn/gone})

  (registry/register! :fixture/tc-ok :atlas/test-case #{:domain/fixture :case/tc-ok} {:test-case/target :fixture/fn-helper})
  (registry/register! :fixture/tc-dangling :atlas/test-case #{:domain/fixture :case/tc-dangling} {:test-case/target :fn/gone})

  (registry/register! :fixture/pitch :atlas/shapeup-pitch #{:domain/fixture :case/pitch} {})
  (registry/register! :fixture/bet-ok :atlas/shapeup-bet #{:domain/fixture :case/bet-ok} {:shapeup/parent-pitch :fixture/pitch})
  (registry/register! :fixture/bet-dangling :atlas/shapeup-bet #{:domain/fixture :case/bet-dangling} {:shapeup/parent-pitch :pitch/gone})
  (registry/register! :fixture/scope-ok :atlas/shapeup-scope #{:domain/fixture :case/scope-ok} {:shapeup/parent-bet :fixture/bet-ok})
  (registry/register! :fixture/scope-dangling :atlas/shapeup-scope #{:domain/fixture :case/scope-dangling} {:shapeup/parent-bet :bet/gone})

  (registry/register! :fixture/prompt-ok :atlas/llm-prompt #{:domain/fixture :intent/read :case/prompt-ok}
                      {:llm-prompt/mcp-deps [:fixture/fn-helper]})
  (registry/register! :fixture/prompt-dangling :atlas/llm-prompt #{:domain/fixture :intent/read :case/prompt-dangling}
                      {:llm-prompt/mcp-deps [:tool/gone]})

  (registry/register! :fixture/wp-ok :atlas/workflow-producer #{:domain/fixture :case/wp-ok}
                      {:workflow-producer/execution-function :fixture/fn-helper})
  (registry/register! :fixture/wp-dangling :atlas/workflow-producer #{:domain/fixture :case/wp-dangling}
                      {:workflow-producer/execution-function :fn/gone})
  (registry/register! :fixture/wf-ok :atlas/workflow #{:domain/fixture :case/wf-ok} {:workflow/producers [:fixture/wp-ok]})
  (registry/register! :fixture/wf-dangling :atlas/workflow #{:domain/fixture :case/wf-dangling} {:workflow/producers [:wp/gone]}))

(def failing
  "Each invariant's failing fixtures."
  {:invariant/endpoints-are-api-tier #{:fixture/endpoint-service}
   :invariant/components-are-foundation #{:fixture/component-service}
   :invariant/pure-has-no-deps #{:fixture/fn-pure-with-component}
   :invariant/decision-terminal-requires-reason #{:fixture/decision-retired-bare}
   :invariant/decision-active-has-realization #{:fixture/decision-active-bare}
   :invariant/authority-terminal-requires-reason #{:fixture/authority-revoked-bare}
   :invariant/value-proposition-has-external-boundaries #{:fixture/vp-bare}
   :invariant/value-proposition-has-pattern #{:fixture/vp-bare :fixture/vp-dangling-pattern}
   :invariant/value-proposition-wired-to-system #{:fixture/vp-bare}
   :invariant/business-pattern-has-implementors #{:fixture/pattern-unused}
   :invariant/system-delivers-valid-vps #{:fixture/system-dangling}
   :invariant/deployment-references-valid-system #{:fixture/deploy-dangling}
   :invariant/allium-rule-implemented-by-exists #{:fixture/rule-dangling}
   :invariant/test-case-target-exists #{:fixture/tc-dangling}
   :invariant/shapeup-bet-pitch-exists #{:fixture/bet-dangling}
   :invariant/shapeup-scope-bet-exists #{:fixture/scope-dangling}
   :invariant/llm-prompt-deps-exist #{:fixture/prompt-dangling}
   :invariant/workflow-producer-ef-exists #{:fixture/wp-dangling}
   :invariant/workflow-producers-exist #{:fixture/wf-dangling}})
