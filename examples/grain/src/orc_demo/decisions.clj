(ns orc-demo.decisions
  "The decision↔ORC BRIDGE MODULE — decisions whose realizations are ORC
   behaviour-tree nodes.

   WHY THIS MODULE EXISTS (module placement, 2026-07-15):
   `atlas.ontology.decision` is a SEAM: engine-neutral by construction, it
   requires only registry/lookup/type-ref and knows nothing about ORC. That
   neutrality is the whole DMN-derived point — a decision is separate from
   whichever dispatcher consumes its verdict.

   `atlas.ontology.behavior-tree` is a SPECIALIZATION: the BT/ORC ontology, living
   downstream in this module because `examples/grain/deps.edn` states the rule
   plainly — \"Atlas core stays grain-free.\"

   A decision REALIZED BY a BT node is a specialization of BOTH, so it belongs
   downstream of both — which is here. It cannot live in core: core has no
   behaviour-tree code (`grep behavior-tree core/src/` is empty), so a
   `:behavior-tree/*`-carrying entity in the core registry is vocabulary
   leaking across a boundary the code respects. (That leak really happened:
   `atlas/core@snap-005` acquired `:behavior-tree/llm` via
   `:bt.review/judge-verdict`; snap-006 backs it out.)

   THE PAYOFF: co-location makes references resolve. A decision in atlas/core
   pointing at a BT node in grain/orc dangles by construction — orgs are auth
   boundaries (an atlas key cannot even read grain/orc). Putting the decision
   and its realization in ONE module means one registry, and every
   `:decision/realized-by` resolves. The earlier dangling refs were not a
   missing cross-registry feature; they were a misplaced module."
  (:require [atlas.registry :as registry]
            [atlas.ontology :as ontology]
            [atlas.ontology.decision]
            [atlas.ontology.behavior-tree]))

(defn init-decisions!
  []
  ;; ==========================================================================
  ;; RESILIENCE — the retry/circuit-breaker demo
  ;;
  ;; Proven live through ORC Path A (build-workflow! + execute): built once to
  ;; a deterministic sheet-id, ticked twice with different inputs, fallback
  ;; short-circuiting correctly. See docs/adapter-orc-execution-bridge-path-a.md.
  ;; ==========================================================================

  ;; --- knowledge ---
  (registry/register!
   :failure.resilience/cascading-failure :atlas/risk-failure-mode
   #{:domain/resilience :failure/availability}
   {:risk-failure-mode/detection
    "repeated calls to a failing downstream service amplify load instead of backing off"
    :risk-failure-mode/prevention-strategy
    "trip the breaker after sustained failure; serve a fallback while open"})

  (registry/register!
   :authority.resilience/circuit-breaker-pattern :atlas/decision-authority
   #{:domain/resilience :status/active}
   {:authority/kind :heuristic
    :authority/statement
    "Fail fast to a fallback rather than repeatedly hammering a failing dependency; trip the breaker after sustained failure."
    :authority/source "docs/adapter-orc-getting-started.md"})

  ;; --- structure: the BT that realizes the decision ---
  (registry/register!
   :retry/root :atlas/execution-function
   #{:behavior-tree/fallback :domain/resilience :workflow/retry-with-breaker}
   {:behavior-tree/inputs #{:bb/request}
    :behavior-tree/children [:retry/call-sequence :retry/open-circuit]})

  (registry/register!
   :retry/call-sequence :atlas/execution-function
   #{:behavior-tree/sequence :domain/resilience :workflow/retry-with-breaker}
   {:behavior-tree/children [:retry/attempt-call :retry/check-success]})

  (registry/register!
   :retry/attempt-call :atlas/execution-function
   #{:behavior-tree/leaf :behavior-tree/code :domain/resilience
     :workflow/retry-with-breaker :operation/call}
   {:execution-function/context [:bb/request]
    :execution-function/response [:bb/response :bb/call-error]})

  ;; the condition node — the decision's realization. Its :behavior-tree/check
  ;; IS the decision's :decision/logic-body, lifted verbatim.
  (registry/register!
   :retry/check-success :atlas/execution-function
   #{:behavior-tree/condition :domain/resilience :workflow/retry-with-breaker}
   {:execution-function/context [:bb/call-error]
    :execution-function/response []
    :behavior-tree/check {:key :call-error :op :equals :value nil}})

  (registry/register!
   :retry/open-circuit :atlas/execution-function
   #{:behavior-tree/leaf :behavior-tree/code :domain/resilience
     :workflow/retry-with-breaker :operation/fallback}
   {:execution-function/context [:bb/call-error]
    :execution-function/response [:bb/fallback-response :bb/circuit-state]
    :node/mitigates #{:failure.resilience/cascading-failure}})

  ;; --- the decision: engine-neutral question, realized by the condition node ---
  (registry/register!
   :decision.resilience/call-succeeded :atlas/decision
   #{:domain/resilience :decision/routing :status/active}
   {:decision/question    "Did the downstream call succeed?"
    :decision/inputs      [:bb/call-error]
    :decision/outcomes    #{:outcome/proceed :outcome/fall-back}
    :decision/logic-style :predicate
    :decision/logic-body  {:key :call-error :op :equals :value nil}
    :decision/realized-by #{:retry/check-success}
    :decision/mandated-by #{:authority.resilience/circuit-breaker-pattern}
    :decision/mitigates   #{:failure.resilience/cascading-failure}})

  ;; ==========================================================================
  ;; REVIEW CI-GATE — atlas-review's autonomous half, realized on ORC
  ;;
  ;; NOT the rich review. /atlas-review's value is the DOCUMENT (named
  ;; shared/missing aspects, aspect-signature fits, dependency map, claim
  ;; buckets); its verdict is a one-word summary of that work. This gate
  ;; answers a THINNER question from THINNER evidence — diff statistics only —
  ;; and says so. Different inputs mean a different question, and a decision's
  ;; identity is question + alphabet + inputs; so this is its own decision, not
  ;; a second realization of the rich review. `:decision.review/atlas-review-verdict`
  ;; stays in atlas/core, realized only by its prose prompt.
  ;;
  ;; Proven live: real claude-haiku-4-5 call, ~438 tokens / ~$0.0005 / ~1s.
  ;; ==========================================================================

  ;; The projection. This is where ALL the epistemic weight sits: the judge can
  ;; only ever know what these six fields admit. It is a COMPUTATION (open-valued
  ;; — returns a map, not a closed alphabet), hence an execution-function, not a
  ;; decision. It is also where the false-approve bug lived: a 404 silently
  ;; became {:from-count nil :changed-count 0 :deleted-count 0} and the judge
  ;; dutifully approved a version that did not exist. A leaf fetching external
  ;; state MUST fail loudly rather than degrade to a plausible empty result —
  ;; bt-contract-complete checks that reads are AVAILABLE, not MEANINGFUL.
  (registry/register!
   :bt.review/fetch-diff :atlas/execution-function
   #{:behavior-tree/leaf :behavior-tree/code :domain/review
     :workflow/atlas-review-ai :operation/fetch}
   {:execution-function/context [:bb/baseline :bb/candidate]
    :execution-function/response [:bb/diff-stats]})

  (registry/register!
   :bt.review/judge-verdict :atlas/execution-function
   #{:behavior-tree/leaf :behavior-tree/llm :domain/review
     :workflow/atlas-review-ai :operation/judge}
   {:execution-function/context [:bb/diff-stats]
    :execution-function/response [:bb/verdict]})

  (registry/register!
   :bt.review/root :atlas/execution-function
   #{:behavior-tree/sequence :domain/review :workflow/atlas-review-ai}
   {:behavior-tree/inputs #{:bb/baseline :bb/candidate}
    :behavior-tree/children [:bt.review/fetch-diff :bt.review/judge-verdict]})

  (registry/register!
   :decision.review/ci-gate :atlas/decision
   #{:domain/review :decision/gate :status/active}
   {:decision/question    "Do these diff statistics warrant blocking the build?"
    :decision/inputs      [:bb/diff-stats]
    :decision/outcomes    #{:verdict/pass :verdict/block}
    :decision/logic-style :llm
    :decision/logic-body
    "Given created/changed/deleted counts, block if the diff mutates or deletes existing entities; pass if purely additive."
    :decision/realized-by #{:bt.review/judge-verdict}})

  (ontology/register-entity-types!)
  :done)
