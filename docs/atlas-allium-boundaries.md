# Atlas ↔ Allium: Boundaries and Commons

> **Status: Design insights, 2026-07-10.** Synthesis of the design dialogue
> that followed [adapter-allium.md](adapter-allium.md) — where the
> definable/specifiable boundary actually sits, and the registry-master
> inversion that follows from it. Supersedes that doc's "ingest forever"
> stance *if* the inversion is adopted (open decision, costs below).

## The driving question

"Why do we need specs if we have definitions?" The first answer was
quantification domains: definitions quantify over the system's **parts**
(entities and wiring), specs over its **states** (what is permissible at
runtime). True, but too coarse — pressed further ("isn't behavior the
purpose of FSMs?", "aren't signals conditions?", "can't justification be a
property?"), the boundary kept moving. Each question annexed another
fragment of "behavior" into definable data. The end position is a gradient,
not a wall.

## The data boundary gradient

Behavior decomposes into rungs; each rung either is pure data or needs a
language. Every system in this ecosystem already occupies the rungs it can:

| Rung | Captures | Form | Who already does it |
|---|---|---|---|
| **FSM skeleton** | states, legal transitions, triggering events | data | atlas `:atlas/workflow`, tilakone-style FSMs, allium status enums, grain aggregates (implicitly) |
| **Verdict alphabets (signals)** | which distinctions each step can make — the branching structure | data | `:workflow-producer/signals`; grain's anomaly categories (implicit, undeclared) |
| **Justifications** | decisions — *why we chose*; read, never evaluated | data (prose props, presence-checkable) | `:grain/deprecated-reason`, `:governance-constraint/rationale`, shapeup cut reasons |
| **Conditions** | *which verdict, given this state* — predicate bodies with `exists` in them | expression language | allium `requires:`/`ensures:` — the irreducible residue |
| **Mechanism** | how it happens | code | grain |

Two litmus tests fell out:

- **Decision vs condition**: if a sentence explains *why we chose*, it's a
  property (checkable only for presence — which atlas invariants already
  enforce); if it states *what must hold*, prose rots — it needs a contract.
- **The signal boundary**: a signal declares a guard's *interface* (its
  verdict set) as data; the guard's *decision procedure* is the first thing
  that needs a language. The moment predicate bodies become data, the
  "definition system" has grown quantifiers — it *is* a spec language.
  Allium is precisely FSM + signals + a small first-order predicate
  language; its own CLI machine-checks the decidable FSM fragment
  (reachability, dead ends) and falls back to heuristics + generated tests
  past the signal boundary. The tooling split confirms the theory.

Consequence for grain: commands already have undeclared verdict alphabets —
`create-counter` returns `counter-created` *or* `::anom/conflict`. Declaring
them (`:grain/outcomes #{:outcome/created :outcome/name-conflict}`) moves
grain up a rung and gives observed-vs-declared an alphabet to audit.

## The commons

Atlas and allium (and grain) share: one enemy (drift), agents as
first-class consumers, meaning-as-artifact rather than meaning-in-heads,
drift checks as the core discipline (`weed` / `verify!` /
observed-vs-declared), and the gap-as-signal philosophy — deliberate
redundancy between layers is the verification mechanism, not duplication.

They differ only in *scope of quantification*: allium per-domain over
states, atlas system-wide over parts. Which is why they compose instead of
competing: neither can absorb the other's quantifier.

## The inversion: allium outputs atlas definitions

The same move already performed on grain — *eat the notation, keep the
engine* — applies to allium. The `.allium` file is notation; the CLI's
checks and the elicitation method are the engine:

| Adapter | Engine kept | Atlas emits |
|---|---|---|
| overarch | rendering (C4/GraphViz) | `model.edn` — projection |
| grain | execution (event store, processors) | registrations — materialization |
| allium | verification of intent (CLI) + elicitation method | generated `.allium` text — projection |
| test-cases | verification of behavior (runner, Kaocha) | atlas-native — nothing to fork |

Under registry-mastership, rule entities carry conditions as **opaque
allium-syntax strings** (`:rule/requires ["not exists User{...}"]`) — still
data, projected verbatim into valid `.allium` for the CLI, no grammar
ownership. Two earlier objections dissolve *because the file stops being a
source*: the scaffold-vs-projection dilemma (nothing hand-edits generated
files) and the "expression strings = two homes" objection (the prop is the
only home). `elicit`/`tend` become conversations ending in `cloud-propose`
— the atlas authoring loop that already exists.

**Honest costs, still open:** forking the allium ecosystem (upstream skills
are file-oriented and evolving — their updates stop applying); CLI
diagnostics need a source map back to entities; portability narrows to
read-only for non-atlas teams (mitigate: check generated specs into the
repo); one-time ingest migration for existing specs (grain-todo-list's 928
lines).

## Test-cases close the tower

Atlas's existing `:atlas/test-case` ontology (target/fixture/expectations,
`:test/tests` edge, with a runner precedent) is the propagate-equivalent —
and the verdict alphabet makes derivation mechanical: **a rule's outcome
set is its minimal test matrix** (success + one case per failed
`requires:`). Choosing counterexample fixtures is LLM authoring work; the
artifact is data.

On the grain substrate the tests are *fully* declarative — Given = events,
When = command map, Then = expected events/anomaly — one generic runner
against the in-memory event store, no generated code, the whole suite
serializable into cloud snapshots. Coverage ("every outcome has a
test-case") is an ordinary registry invariant.

Every behavioral claim then gets checked at three moments:

```
contract        rule entity — contradiction-checked via the .allium projection + CLI
pre-ship        test-case runner — one case per verdict, must fail first
post-ship       observed-vs-declared — event store audited against declarations
```

## The one-sentence end state

**Atlas defines — grain executes, overarch renders, allium verifies intent,
the test runner verifies behavior** — one registry as semantic master, four
engines consuming projections of it, and the only thing that ever needs a
language rather than data is the predicate body of a condition.

## Relation to the build sequence

The agreed sequence ([adapter-allium.md](adapter-allium.md)) gains richer
rule entities (conditions as strings, outcome alphabets), turns the parser
into a one-time migration ingest, and appends two legs: the emit-allium
projection + CLI round-trip, and the grain test runner with counter
test-cases (`create-counter`'s name-uniqueness conflict is the first
worked example). The ecosystem-fork tradeoff is the open decision gating
the inversion.
