# Proposal: what the ORC ontology work suggests Atlas could do for its own LLM communication

> Date: 2026-07-13
> Status: proposal — none of this is built yet, ranked by how solid the case
> is
> Origin: this came out of auditing `examples/grain/src/orc_demo/*.clj`
> against ORC's real source (see `docs/adapter-orc.md` and the
> `docs/internal/review-grain-orc-*.md` series). The audit surfaced three
> mechanisms — a structure↔knowledge join, an ordering-aware dataflow
> contract, and a self-rendering-docs pattern (borrowed from grain, not ORC)
> — that turned out to be directly reusable for Atlas's *own* LLM-facing
> surface: `:atlas/llm-prompt`, the nine registered skills in
> `atlas_skills.cljc`, and the Prompt–Tool Sync convention in `CLAUDE.md`.

## The question

Atlas builds tooling for LLMs to consume (prompts, MCP tools, skills) the same
way ORC builds trees for LLMs to execute. Does anything from modeling ORC's
behavior-tree ontology in Atlas transfer back to how Atlas describes its own
prompt/tool surface to the LLMs that use it?

## Current state of `:atlas/llm-prompt` (baseline)

`core/src/atlas/ontology/llm_prompt.cljc` registers `:atlas/llm-prompt` as a
flat entity: `:llm-prompt/summary`, `:llm-prompt/file`, `:llm-prompt/mcp-deps`
(an unordered `s/coll-of qualified-keyword?`), `:llm-prompt/produces`. Three
invariants exist: every prompt needs an `:intent/*` aspect, write-prompts
need `:llm-prompt/produces`, and `mcp-deps` must resolve to registered
dev-ids (warning-severity). `cloud/src/atlas/ontology/atlas_skills.cljc`
registers the nine current prompts (`propose`, `review`, `explore-domain`,
`before-change`, …) against this shape.

What's missing, relative to what ORC's ontology now has:

- No link from a prompt to *why it exists* — no analogue to `:node/mitigates`.
- No per-step structure — `mcp-deps` is a flat bag, no analogue to a BT
  leaf's ordered `:reads`/`:writes` contract, even though every prompt's
  `.md` file has an explicit numbered protocol (`### 1. ... ### 2. ...`).
- Prompt docs are hand-authored prose, with no analogue to grain's
  `guides.clj` trick (render the guide *from* the enforcer's own data, so it
  can never disagree with what's checked).

## Proposal 1 — `:node/mitigates` for prompts (strongest: zero new types)

Reuse `:atlas/risk-failure-mode` (already core, already has the 14-key
schema — `detection`, `prevention-strategy`, `business-impact`, etc.; already
proven on the ORC judges) to declare what a prompt is *for*:

```clojure
(registry/register!
 :risk.atlas/vocabulary-drift
 :atlas/risk-failure-mode
 #{:domain/atlas :failure/naming}
 {:risk-failure-mode/detection "a new aspect is edit-distance close to an existing one"
  :risk-failure-mode/prevention-strategy "run aspect-anomalies before every push"})

(registry/register!
 :risk.atlas/dangling-reference
 :atlas/risk-failure-mode
 #{:domain/atlas :failure/integrity}
 {:risk-failure-mode/detection "a type-ref or dev-id points at a non-existent entity"
  :risk-failure-mode/prevention-strategy "cloud-branch-validate / entity-detail before merge"})
```

Then add `:node/mitigates` (already an existing type-ref, sourced generically
at `:atlas/execution-function` in `orc_demo/behavior_tree.clj` — would need a
second type-ref sourced at `:atlas/llm-prompt`, or generalize the existing one
to source at both) to each prompt:

```clojure
{:llm-prompt/mcp-deps [...]
 :node/mitigates #{:risk.atlas/vocabulary-drift :risk.atlas/dangling-reference}}
```

This gives, for free, the exact query ORC's headline claim demonstrates:
*"every prompt across the whole toolkit that guards against dangling
references"* — currently unanswerable except by re-reading nine `.md` files
by hand.

**Effort**: small. Register ~5-10 risk concepts (one per class of registry
problem the review protocol already checks for — drift, dangling refs,
collision, staleness), tag the nine existing prompts, add one type-ref if
`:node/mitigates` needs a second source declaration.

## Proposal 2 — self-rendering docs, borrowed from grain's `guides.clj`

Not an ORC idea directly — a grain idea, reinforced by direct experience:
this session's audit found `orc_demo`'s own hand-written docstrings had
drifted from ORC's real code (claimed "8-node faithful slice," reality was a
different, larger pipeline) — undetected until source was read directly.
That's the exact failure mode grain's `guides.clj` docstring warns about for
itself ("rendered FROM the validator's own connection-grammar, so the guide
cannot drift from what is enforced").

`CLAUDE.md`'s "Prompt–Tool Sync" section is currently a **manual** reminder:
run `consumers-of` on a changed tool, then go hand-edit the affected prompt's
`.md` file. It could instead render the current `mcp-deps` list *from* the
registry directly into the prompt file (or into a companion generated
section), the same way grain renders its flow-grammar guide from
`connection-grammar` instead of retyped prose.

**Effort**: small-to-medium. Needs one function (`atlas.llm-prompt` or
similar, querying `entity-detail` for a prompt dev-id and formatting its
`mcp-deps`) plus a place to inject the rendered section into each `.md` file
(or a separate generated companion file, to avoid hand-edited prose and
generated data living in the same document).

## Proposal 3 — per-step reads/writes ordering (deepest, but a real project)

The genuinely biggest transfer is `bt-contract-complete` (the ordering-aware
dataflow invariant built for ORC — writes must precede reads, parallel/
fallback branches are isolated). A prompt like `/atlas-review` has an
implicit step order in its `.md` file, but nothing registry-side captures
"step 4 assumes data step 2 produces." Modeling each protocol step as its own
entity (mirroring a BT leaf's `:reads`/`:writes`) would let the *same*
ordering-aware invariant catch a prompt whose steps got reordered during
editing and now assume data that isn't available yet — a real,
would-actually-catch-bugs check, not a cosmetic one.

**Effort**: real project, not a quick add. Requires:
- a sub-structure for "prompt step" (new type, or a repeated
  `:atlas/execution-function`-shaped entity per step, `:bt/children`-style
  ordered under the prompt)
- deciding what a step "produces" in Atlas terms (a tool's response key? a
  claim category? the step's own prose intent?)
- re-authoring all nine existing prompts' protocols into this structure

This is the one worth scoping properly before starting, not building
speculatively.

## Explicitly not pursued (lower value, not worth chasing yet)

- **`condition`/`llm-condition`-style branch modeling** for prompt
  mode-selection (e.g. `/atlas-review`'s diff-mode vs feature-mode choice).
  Same *shape* of idea as proposal 3, but each prompt has at most one branch
  point — not a repeated structural pattern worth reifying yet.
- **An `:executor`-style split** on `:atlas/llm-prompt` (deterministic
  template vs dynamically-constructed content). Speculative; no concrete
  friction identified that this would resolve today.

## Suggested order

1. Proposal 1 (risk-concept linking) — smallest, immediately reuses existing
   types, answers a real question ("which prompts guard against X") that's
   unanswerable today.
2. Proposal 2 (self-rendering docs) — closes the exact drift class this
   session's ORC audit found the hard way, in Atlas's own toolkit before it
   bites the same way.
3. Proposal 3 (per-step ordering) — only after 1 and 2 are in place and the
   team has a concrete case of a prompt-step-ordering bug to justify the
   modeling investment.

## Related

- `docs/adapter-orc.md` — where `:node/mitigates` and the ordering-aware
  invariant were built.
- `docs/ontology-extensibility.md` — the seams-in-core/specializations-
  downstream discipline this proposal would follow (no core change needed for
  proposal 1; `:atlas/llm-prompt`'s own ontology module gains the type-ref).
- `core/src/atlas/ontology/llm_prompt.cljc`,
  `cloud/src/atlas/ontology/atlas_skills.cljc` — what exists today.
- `docs/internal/review-grain-orc-v0.6.0-v0.7.0.md` — the audit that
  surfaced the drift-detection motivation for proposal 2.
