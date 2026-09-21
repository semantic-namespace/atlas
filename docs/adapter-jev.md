# Jev Adapter (Design)

> **Status: Design — not implemented, and not yet evaluated.** Everything
> below about Jev is taken from TypeSafe AI's announcement post
> ([introducing-system-one-models-and-jev](https://typesafe.ai/blog/introducing-system-one-models-and-jev),
> 2026) plus the existence of `docs.typesafe.ai` and the
> [`system-one-adapter-python`](https://github.com/typesafe-ai/system-one-adapter-python)
> wrapper. The API surface has **not** been exercised; the latency,
> calibration and cost figures are the vendor's claims, not measurements.
> Read this as "what the integration would be if the claims hold", and treat
> the worked example in [Second realization](#worked-example-a-second-realization-of-the-ci-gate)
> as the experiment that would decide it.
>
> Companion: [decision-semantics.md](decision-semantics.md),
> [adapter-orc-execution-bridge.md](adapter-orc-execution-bridge.md).

Jev is a "System One Model": it takes program state and returns a **typed
value from a schema fixed in advance**, with a calibrated probability
attached. It cannot emit free text or code. Its stated shape is a closed
output alphabet of at most 255 choices, 70–500ms end to end, $0.042/MTok in
with output tokens free.

That is not a language model with a JSON mode bolted on. It is an engine
for exactly the artifact `atlas.ontology.decision` already describes.

## Why an adapter, when we already have `:llm`

`:decision/logic-style` currently names four traditions: `:predicate`,
`:table`, `:llm`, `:code`. The operative guidance (from the
skill-as-decision-tree work) has been: keep discriminators `:table` or
`:code` — cheap, auditable, and structurally unable to invent an outcome
outside the alphabet — and spend `:llm` only where no table can work.

That guidance has a hole in it. A `:table` cannot read a paragraph. An
`:llm` can, but it pays for that with latency, cost, and the standing
possibility of an answer off the alphabet. So every decision whose *inputs*
are unstructured but whose *question* is closed has been forced onto the
expensive side of the line.

A System One model sits in that hole:

| | structured input | unstructured input |
|---|---|---|
| **closed question** | `:predicate` / `:table` — auditable, free | **the hole** → Jev |
| **open question** | `:code` | `:llm` |

The trade is explicit and worth naming: a table tells you *why* it decided;
Jev gives you a **confidence number instead of a reason**. It buys alphabet
safety and speed at the cost of the audit trail a table hands over for
free. So the rule stays: table where the evidence is already structured;
Jev only where it genuinely is not.

## The shape match

The ontology and the model line up field for field — not by design on
either side, which is the interesting part:

| `:atlas/decision` | Jev |
|---|---|
| `:decision/outcomes` — closed set, **≥ 2** (one outcome is a computation) | the output schema's choices, **≤ 255** |
| `:decision/inputs` — declared data-keys | "structured program state" as input |
| `:decision/question` | the query |
| `:decision/logic-body` | the instruction / trained policy |
| `:decision/realized-by` | the call site |
| `:decision/mandated-by` — the authority | *nothing — Jev has no equivalent* |
| *nothing — atlas has no home for this* | the calibrated probability |

Both ends of the cardinality constraint are already enforced somewhere:
`:invariant/decision-outcomes-closed` rejects an alphabet below 2, and Jev
caps at 255. Together they are a single checkable range, and the adapter
should check it at emit time rather than discovering it in a 4xx.

The two blank rows are the whole substance of the collaboration. Atlas has
governance Jev lacks; Jev has calibration atlas has nowhere to put.

## Direction: emit only

Pure projection, like [Overarch](adapter-overarch.md) and
[YAML-LD](adapter-yaml-ld.md), not an ingest-and-scaffold arrangement like
[Allium](adapter-allium.md). Atlas owns the decision; the adapter renders it
as a Jev request; nothing flows back into the registry as vocabulary.

```clojure
(jev/emit-decision :decision.review/ci-gate)
;; =>
;; {:query   "Do these diff statistics warrant blocking the build?"
;;  :schema  {:type   :choice
;;            :values [:verdict/pass :verdict/block]}   ; ← :decision/outcomes, verbatim
;;  :state   {:diff-stats ...}                          ; ← :decision/inputs, projected
;;  :instruction "Given created/changed/deleted counts, block if ..."}  ; ← :decision/logic-body
```

The point is that **the schema stops being hand-maintained**. Today an
outcome alphabet lives in the registry and again, by hand, in whatever
engine executes it; they drift, and nothing notices. Emitting from
`:decision/outcomes` makes drift impossible in the same way the ORC bridge
made it impossible for behaviour trees.

**Honest gap in the emit.** Atlas declares data-key *identity*, not data-key
*shape* — schemas are malli's job, one layer down (see the artifact table in
[adapter-allium.md](adapter-allium.md)). So the **output** side of the
request is fully derivable from the registry, and the **input** side is only
partially: atlas knows which keys are in scope and what produces them, not
what they contain. Either the projection function is supplied per decision,
or the adapter reads `:atlas/data-schema` entities where they exist. Do not
pretend the input side is free.

## The vocabulary question — do NOT add `:jev` to core

The obvious move is to widen the `:decision/logic-style` spec set to
`#{:predicate :table :llm :code :jev}`. That is the snap-005 leak again, in
a new coat: `atlas.ontology.decision` is a **seam** — engine-neutral by
construction, requiring only registry/lookup/type-ref — and a vendor's name
in a core spec is downstream vocabulary walking across a boundary the code
respects. The module rule is already written down
([ontology-extensibility.md](ontology-extensibility.md)): seams in core,
specializations downstream.

Recommended instead:

- **Core** gains at most one *engine-neutral* style — `:model` (a trained
  classifier decides; the logic body is an instruction, not an inspectable
  mapping). That is a genuine new tradition alongside decision tables and
  production rules, and it would have been worth adding even if Jev did not
  exist.
- **The adapter module** owns `:jev/*` aspects on the **realization**
  entity, never on the decision. The decision says "a model decides this";
  the realization says "specifically, Jev does."

This also forces the limitation already logged against the ontology: a
decision with a `:table` realization *and* a `:model` realization has two
logic-styles, and `:decision/logic-style` is single-valued. Adding a fast
model makes that bite immediately, because running both engines against one
question is the *good* configuration, not a transitional state — see below.
Per-realization logic-style stops being a "candidate future fix".

## The governance half: where atlas earns its place

Jev reports how confident it is. It has no idea what the decision is
*permitted to do*. Atlas does — `:decision/mandated-by` an authority,
`:decision/mitigates` a failure mode, invariants over the graph.

Two additions carry that:

```clojure
;; on the decision, not the realization — the threshold is a property of the
;; question and its authority, not of whichever engine answers it
{:decision/confidence-threshold 0.9
 :decision/below-threshold      :verdict/escalate}   ; must be IN the alphabet
```

and the invariant that makes it mean something:

> **`:invariant/model-decision-guarding-writes-needs-threshold`** (error) —
> a decision whose logic-style is `:model` must declare
> `:decision/confidence-threshold` and a `:decision/below-threshold` outcome
> if any consumer of its verdict carries `:effect/write`.

That is the question "where in this architecture are probabilistic
decisions allowed to act unsupervised" — squarely atlas's job, and nobody
else's. A fast, cheap, structurally-safe model makes it *more* urgent, not
less: the cost of putting one in a write path drops to nearly zero, so the
only remaining brake is a declared one.

### The invariant that already exists is the important one

`:invariant/decision-realization-covers-inputs` was written after the
false-approve bug: a leaf read `[:bb/diff-summary]` while the decision
declared three richer inputs, a 404 degraded to
`{:from-count nil :changed-count 0 :deleted-count 0}`, and the judge
confidently approved a version that did not exist. The lesson recorded then
was that **an LLM judge cannot rescue a broken deterministic upstream**.

A System One model does not change that and makes it cheaper to commit. At
200ms and effectively free, the temptation to point a decision at a thin
projection is much stronger than at 1s and $0.0005. The warning that fires
on it is already written and already negative-tested. This is the concrete
safety argument for putting a registry in front of a fast model, and it
costs nothing — it is running today.

## Worked example: a second realization of the CI gate

`:decision.review/ci-gate` (in `examples/grain/src/orc_demo/decisions.clj`)
is the right first experiment, because it is small, already built, already
run against real data, and deliberately scoped to a thin question:

```clojure
{:decision/question    "Do these diff statistics warrant blocking the build?"
 :decision/inputs      [:bb/diff-stats]
 :decision/outcomes    #{:verdict/pass :verdict/block}
 :decision/logic-style :llm
 :decision/realized-by #{:bt.review/judge-verdict}}    ; :ai leaf, haiku, ~438 tok / ~$0.0005 / ~1s
```

Add a **second** realization on Jev — same question, same alphabet, same
declared inputs, different engine. That is the engine-neutrality claim the
ontology was built to make (`:decision/realized-by` is cardinality-many
precisely for this), exercised against a vendor rather than against a
second in-house executor.

What it yields beyond a working integration:

- **A calibration harness.** Two realizations of one question, run on the
  same diffs, disagreeing or not. Both `:verdict/pass` and `:verdict/block`
  have already been observed from real executions on the ORC side, so there
  is a baseline to compare against rather than a vibe.
- **A cost/latency measurement** against a known reference point, not
  against the vendor's table.
- **The per-realization logic-style problem in concrete form**, which is the
  cheapest way to design the fix.

Keep `:decision.review/atlas-review-verdict` — the rich review — out of
this. Its value is the *document*; the verdict is a one-word summary of work
a thin leaf does not do. Different inputs mean a different question. That
split was made deliberately and should survive the adapter.

## Boundaries

- **Open-valued selection stays out.** 255 choices is a low ceiling for
  anything that selects from the registry — "pick a baseline version" is
  open-valued, which is why it could not move onto ORC either. Closed
  alphabet or nothing; the `decision-outcomes-closed` gate is the same gate.
- **`:atlas/llm-prompt` is untouched.** Jev cannot generate text or code, so
  `/define-ontology`, `/author-decision`, `/atlas-review`'s prose — none of
  it moves. The adapter replaces verdict leaves, not authoring. The model
  receives verdicts as input; where it makes them, it makes only closed ones.
- **Confidence is calibrated against the input it was given**, not against
  whether that input was fetched correctly. See the false-approve bug.
- **Hosted, closed, third-party.** Emit-and-bind only. Nothing in the design
  may assume access to internals, and a decision must remain executable by
  its other realizations if the service is unavailable — which the
  cardinality-many realization model already gives for free.
- **Module placement.** Emitter code in `core/src/atlas/adapter/jev.clj`
  follows the existing adapter precedent (allium, yaml-ld, overarch all sit
  in core and all target third-party artifacts). Registry *vocabulary* does
  not: `:jev/*` aspects belong on realization entities in a downstream
  module. Code boundaries are enforced by deps.edn; registry boundaries are
  not, which is exactly how `:behavior-tree/llm` got into `atlas/core@snap-005`.

## What the registry offers in the other direction

Worth stating, because it is the adoption bottleneck for any typed-decision
API: "which decisions in my system should be model calls, and what are their
alphabets?" is a registry query — entities carrying `:atlas/decision` with a
closed alphabet under 255 and a `:table`/`:llm` style. And "what breaks if
this alphabet changes" is `consumers-of` plus blast-radius, over
`[?d :decision/outcome :verdict/reject]` facts the extractor already emits.
Schema evolution for typed decisions is a problem any such service will
have, and it is one atlas already answers.

## Build sequence (proposed, nothing started)

1. Verify the claims that matter: request/response shape at
   `docs.typesafe.ai`, whether confidence is per-outcome or scalar, and
   whether the input side wants a flat map. Everything below depends on it.
2. `:decision/logic-style :model` in core — engine-neutral, no vendor name.
3. `atlas.adapter.jev/emit-decision` — alphabet → schema, with the
   2 ≤ cardinality ≤ 255 check at emit time. Input projection supplied, not
   inferred.
4. The CI-gate second realization; measure against the haiku leaf.
5. `:decision/confidence-threshold` + `:decision/below-threshold` and the
   write-path invariant — *before* any such decision reaches a write path,
   not after.
6. Per-realization logic-style, designed against the two-engine CI gate.

## Open decisions

- Style naming: `:model` vs `:classifier` vs `:probabilistic`. `:model` is
  shortest and least committal about the mechanism; `:classifier` is the
  most honest about the closed alphabet.
- Whether `:decision/confidence-threshold` belongs on the decision (the
  authority sets the bar) or on the realization (each engine calibrates
  differently). Leaning decision — it is a policy, not a tuning parameter.
- Whether a below-threshold outcome must be *in* `:decision/outcomes`
  (keeps the alphabet closed and total; costs every such decision an extra
  outcome) or sits outside it as an escape hatch (keeps alphabets clean;
  punches a hole in closedness). Leaning in-alphabet — closedness is the
  property everything else rests on.
