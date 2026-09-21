# Allium Adapter (Design)

> **Status: Design — not yet implemented.** Outcome of the grain-todo-list
> import work ([adapter-grain-import.md](adapter-grain-import.md)): the
> imported app carries [Allium](https://github.com/juxt/allium) behavioral
> specs (`spec/*.allium`, 928 lines covering its full command surface), and
> this doc records how Atlas should integrate with them. Companion:
> [adapter-grain.md](adapter-grain.md).
>
> **Follow-up:** [atlas-allium-boundaries.md](atlas-allium-boundaries.md)
> refines this design — the definable/specifiable boundary turns out to be
> a gradient (FSM → signals → justifications → conditions), and a
> registry-master inversion is on the table (allium *outputs* atlas
> definitions; `.allium` files become projections). If adopted, it
> supersedes this doc's "ingest forever, scaffold once" direction — ingest
> becomes a one-time migration.

Allium is JUXT's formal behavioral specification language for agentic
engineering: `.allium` files capture what a system *should do* — entities,
rules (`when:` / `requires:` / `ensures:`), actors, invariants, surfaces —
deliberately excluding mechanism. A skill toolchain (`elicit`, `distill`,
`tend`, `weed`, `propagate`) authors and maintains specs conversationally; a
Rust CLI (`allium-cli`) validates them and does semantic inference
(contradictions, lifecycle reachability, dead ends).

## Why specs when we have definitions

The two artifacts answer different questions — they *quantify over
different domains*:

- **A definition (Atlas) quantifies over the system's parts.**
  `:command.user/sign-up` exists, is a write command in `:domain/user`,
  produces `:event.user/signed-up`, is actor-gated. Claims about entities
  and wiring.
- **A spec (Allium) quantifies over the system's states.** `SignUp` may
  only succeed when no User with that normalized email exists and the two
  password entries match; afterwards the account must be active,
  unverified, verification pending. Claims about what is permissible across
  the runtime state space.

The grain-todo-list import made the gap concrete: after a complete
structural import, Atlas knows everything about sign-up's wiring — and
still has no idea that emails must be unique. That rule exists only in the
spec (and in the code, which cannot serve as the contract: code states what
the system *does*, bugs included).

Dropping specs would lose: pre/postconditions as checkable artifacts, test
propagation (`/propagate` derives failing behavioral tests from
`requires:`/`ensures:`), contradiction detection over intent, the
does-vs-should distinction, and intent-before-implementation (a spec can
exist from an `/elicit` conversation before any code does). Dropping
definitions would lose the mirror image: cross-domain data-flow, blast
radius, identity, versioned architecture, runtime coupling — Allium is
per-domain and deliberately binds to nothing.

| Artifact | Question | Example claim |
|---|---|---|
| Schema (malli) | what shape is the data? | `:name` is a string |
| Definition (Atlas) | what exists, how connected? | sign-up produces `signed-up`, Visitor-gated |
| Spec (Allium) | when is a change legitimate; what must hold after? | only if email is free; verification becomes pending |
| Impl (grain) | how does it mechanically happen? | handler + event store |

## Three layers, one enemy, and the drift triangle

Grain narrows the **runtime** (drift impossible in the substrate), Allium
pins **intent** (behavioral contracts that persist across sessions), Atlas
maps **structure** (identity + data-flow + invariants, org-wide,
versioned). Each has its own drift check; together they triangulate:

```
            .allium spec
     /weed ↙        ↘ spec-coverage invariant (NEW, atlas-side)
  code / runtime  ⇄  atlas registry
        verify! · observed-vs-declared
```

- `weed` — spec ↔ code (allium's)
- `verify!` / observed-vs-declared — registry ↔ runtime (atlas's, see
  [adapter-grain.md](adapter-grain.md))
- **spec-coverage** (new, cheap): every allium rule has an
  `:rule/implemented-by` command entity and every command has a rule —
  registry ↔ spec, an ordinary atlas invariant over type-refs.

When all three are green, intent, meaning, and mechanism provably agree.

### Case study: the justified exception

The import flagged `:event.user/logged-in` as an orphan (produced by login,
consumed by nothing). `accounts.allium` documents it as *intentional*:
sessions are stateless client-held JWTs, so login changes no server state —
the event's non-consumption is a design decision, recorded with rationale.
Atlas **detects**; Allium **justifies**. The correct action is annotating
the atlas entity as intentionally-unconsumed *citing the spec* — retiring
the warning knowingly. Neither layer alone handles this correctly: without
atlas, the next unintentional orphan ships silently; without allium, a
deliberate decision gets "fixed".

## Allium as an Atlas ontology

Reuse check against existing ontologies — most allium concepts already
have homes; **only one new type is needed**:

| Allium concept | Atlas modeling | Verdict |
|---|---|---|
| `entity` (User, Task) | `:atlas/data-schema` | reuse |
| `actor` (Visitor, Account, Owner) | `:atlas/identity-role` (`data-access`, `cannot-access`, `granted-by`) | reuse |
| `invariant` (UniqueEmailPerAccount) | `:atlas/governance-constraint` (`enforced-by`, `rationale`) — correctly declarative | reuse |
| propagated tests | `:atlas/test-case` (`:test-case/target` → rule) | reuse |
| `surface` | type-refs to the query/endpoint entities | reuse |
| `requires:`/`ensures:` bodies | stay in the `.allium` file — pointer only (the `:atlas/llm-prompt` content-file pattern) | out of scope by design |
| **`rule`** | **new type** — `:atlas/allium-rule` (or notation-agnostic `:atlas/behavior-rule`; naming open) | new |

Sketch of a rule entity:

```clojure
(registry/register!
 :rule.accounts/sign-up
 :atlas/allium-rule
 #{:domain/user :access/public}
 {:rule/spec "spec/accounts.allium"
  :rule/name "SignUp"                          ; pointer, not duplication
  :rule/when-event :event.user/signed-up-requested
  :rule/actor #{:role/visitor}                 ; → identity-role
  :rule/implemented-by :command.user/sign-up   ; ← the join allium deliberately lacks
  :rule/ensures-events #{:event.user/signed-up}})
```

What first-class rules buy over pointer props: contracts join the graph
(blast-radius on an event includes the promises made about it); the
spec-coverage invariant becomes ordinary; spec structure is versioned in
cloud snapshots (which rules appeared/retired between releases); the
business layer connects (`value-proposition → rule → command → event` —
allium has no why-layer, atlas does); even lifecycle fits (a spec's open
question → `:status/questioned`; a cut behavior → `:status/retired` +
reason).

Enrichment bonus discovered on grain-todo-list: actor scoping carries the
**meaning** of `:authorized?` that structural import cannot infer —
`[Public]` rules (SignUp, Login, VerifyEmail, password-reset flow) vs
Owner-scoped todo rules. Allium is the source that corrects the import's
presence-only `:access/enforced` guesses.

## Directions: ingest forever, scaffold once

**Steady state — allium → atlas (ingest).** Parse the structural layer
(rule names, when-events, actors, invariant names, surfaces — the grammar
is regular; `allium-cli` optional) into registry entities pointing back at
the file. Atlas never stores `requires:`/`ensures:` bodies and never grows
an evaluator — behavioral truth stays in the spec, checked by allium's own
tooling. Authoring stays allium-native (elicit/tend).

**Bootstrap only — atlas → allium (scaffold).** For systems with a
registry but no specs (a production app, the counter app), atlas can emit a
one-time skeleton — rules with `when:`/`ensures:` stubs from declared
data-flow, actors from access aspects, behavioral holes marked for
`/elicit`. Contrast with the Overarch/YAML-LD adapters, which are pure
*projections* (regenerate anytime, never hand-edited): an emitted `.allium`
file is a *scaffold* — the moment `/tend` touches it, it owns behavioral
truth and only the ingest direction applies thereafter.

Ownership split:

| | Owns | Authored via |
|---|---|---|
| Allium | behavioral truth (conditions, actor meaning, abstraction decisions) | elicit/tend + CLI |
| Atlas | structural truth (identity, kinds, data-flow, the joins) | registrations / imports |

## Build sequence (agreed, pending)

1. Define the ontology via the `define-ontology` protocol — the reuse table
   above is its Phase 2; validation data is free (29 real rules, 3 actors,
   5 invariants, 8 surfaces in grain-todo-list's specs).
2. Structural parser for `.allium` (rule/actor/invariant/surface extraction).
3. Re-run the grain-todo-list enrichment as first-class rule entities:
   corrected `:access/*` aspects from actors, `:allium/rule` joins,
   spec-coverage invariant, the `logged-in` orphan annotated as
   spec-justified.
4. user-service conversion to atlas defs
   ([adapter-grain-import.md](adapter-grain-import.md) follow-up) — by then
   encoding contracts, not just structure.

Open decision: type naming — `:atlas/allium-rule` (notation-honest,
matches the grain-over-cqrs precedent) vs `:atlas/behavior-rule`
(notation-agnostic).
