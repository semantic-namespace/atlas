(ns fixtures.invariant-rules.entities
  "One entity that passes and one that fails each invariant carrying an
   :invariant/rule. atlas.invariant-rules-test runs the invariants' fns on
   them; bb atlas-pack-smoke runs their rules. Both must fail the same ones."
  (:require [atlas.registry :as registry]))

(defn register-all! []
  (registry/register! :fixture/endpoint-api :atlas/interface-endpoint #{:domain/fixture :tier/api} {})
  (registry/register! :fixture/endpoint-service :atlas/interface-endpoint #{:domain/fixture :tier/service} {})

  (registry/register! :fixture/component-foundation :atlas/structure-component #{:domain/fixture :tier/foundation} {})
  (registry/register! :fixture/component-service :atlas/structure-component #{:domain/fixture :tier/service} {})

  (registry/register! :fixture/decision-retired-with-reason :atlas/decision #{:domain/fixture :status/retired}
                      {:decision/retirement-reason "replaced by the v2 flow"})
  (registry/register! :fixture/decision-retired-bare :atlas/decision #{:domain/fixture :status/retired} {})
  (registry/register! :fixture/decision-active-realized :atlas/decision #{:domain/fixture :status/active}
                      {:decision/realized-by [:engine/fixture]})
  (registry/register! :fixture/decision-active-bare :atlas/decision #{:domain/fixture :status/active} {})

  (registry/register! :fixture/authority-revoked-with-reason :atlas/decision-authority #{:domain/fixture :status/revoked}
                      {:authority/revocation-reason "policy ended"})
  (registry/register! :fixture/authority-revoked-bare :atlas/decision-authority #{:domain/fixture :status/revoked} {})

  (registry/register! :fixture/vp-bounded :atlas/value-proposition #{:domain/fixture}
                      {:value-proposition/external-boundaries [:boundary/fixture]})
  (registry/register! :fixture/vp-bare :atlas/value-proposition #{:domain/fixture} {}))

(def failing
  "Each invariant's failing fixtures."
  {:invariant/endpoints-are-api-tier #{:fixture/endpoint-service}
   :invariant/components-are-foundation #{:fixture/component-service}
   :invariant/decision-terminal-requires-reason #{:fixture/decision-retired-bare}
   :invariant/decision-active-has-realization #{:fixture/decision-active-bare}
   :invariant/authority-terminal-requires-reason #{:fixture/authority-revoked-bare}
   :invariant/value-proposition-has-external-boundaries #{:fixture/vp-bare}})
