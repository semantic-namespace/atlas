# Allium Adapter — Emit Direction

> **Status: Experimental, CLI-validated.** Namespace `atlas.adapter.allium`
> (`core/src/atlas/adapter/allium.clj`). Pure data — requires nothing beyond
> Clojure core (`clojure.string`, `clojure.java.io`). Covers the **emit** half
> (`registry->allium`, `emit!`); the **ingest** half (`allium->registry`, a
> structural `.allium` parser) is the sibling sketched in
> [adapter-allium.md](adapter-allium.md).

Emit [Allium](https://github.com/juxt/allium) v3 `.allium` text from an Atlas
registry slice. Sibling of the [Overarch](adapter-overarch.md) and
[YAML-LD](adapter-yaml-ld.md) adapters: same `{compound-id-set props-map}`
input, same reused selectors, same pure `registry->…` + file-writing `emit!`
shape. Validated end-to-end against JUXT's real `allium` CLI (v3.5.0).

## Why

Allium is JUXT's behavioral specification language: `.allium` files state what a
system *should do* — entities, rules (`when:`/`requires:`/`ensures:`), surfaces,
invariants — and a Rust CLI (`allium check`/`analyse`/`plan`/`model`) machine-checks
the decidable fragment. Atlas and Allium answer different questions and compose
rather than compete (the full argument is in
[atlas-allium-boundaries.md](atlas-allium-boundaries.md)):

| | **Atlas** | **Allium** |
|---|---|---|
| Quantifies over | the system's **parts** (identity, kinds, data-flow, joins) | one spec's **states** (what is permissible) |
| Owns | cross-spec graph, versioning, blast-radius, the why-layer | predicate bodies, actor meaning, per-spec verification |
| Missing | a behavioral contract language | cross-spec reasoning, versioning, the `rule → command` join |

This adapter makes Atlas the **structural master** and the `.allium` file a
**projection** — the "registry-master inversion" from the boundaries doc. Atlas
emits identity, kinds, and the joins Allium has no syntax for; Allium's own CLI
stays the only engine that reads a predicate.

## The load-bearing invariant

> Atlas writes **shape**, never **semantics it cannot check**. Every
> `requires:`/`ensures:`/`invariant` body is either a verbatim string copied
> from a rule entity (`:project`) or a marked hole (`:scaffold`). Atlas grows no
> evaluator and no grammar. `allium check` is the only thing that ever parses a
> predicate.

## Two modes

Auto-detected: `:project` iff the selected slice contains any `:atlas/allium-rule`
entity, else `:scaffold`. Override with `:mode`.

| Mode | When | Behavior | Regenerable? |
|---|---|---|---|
| **`:scaffold`** | structure only, no rule entities | rules inferred from data-flow (`context`→`when` params, `response`→ensures *hint*); preconditions/outcomes/actors emitted as `-- TODO(/elicit)` holes | **no** — one-time bootstrap |
| **`:project`** | slice has `:atlas/allium-rule` entities | verbatim projection of opaque allium bodies; no holes | **yes**, anytime — like Overarch/YAML-LD |

A `:scaffold` header warns against regenerating over a file that `/tend` or
`/elicit` has since taken ownership of; a `:project` header declares the file
generated and the registry authoritative.

## Mapping (atlas → allium v3)

| Atlas | → | Allium v3 |
|---|---|---|
| `:atlas/data-schema` | → | `entity <Name> { … }` |
| `:atlas/structure-component` | → | `external entity <Name> {}` (placeholder; scaffold) |
| `:atlas/execution-function` | → | `rule <Name> { when: … }` (scaffold — behavioral body is a hole) |
| `:atlas/allium-rule` | → | `rule <Name> { when:/requires:/ensures: }` (project — opaque strings) |
| `:atlas/interface-endpoint` | → | `surface <Name> { facing/provides: }` |
| endpoint→function `deps` edge | → | surface `provides:` matched to the rule's `when:` (the reachability join) |
| `:rule/implemented-by` | → | `-- atlas implemented-by: <fn>` (the join Allium lacks) |
| every construct | → | `-- atlas: <dev-id>` source-map comment |

### Identity and the source-map are comments, by necessity

`traces:` is **not** an Allium v3 clause (the CLI rejects it). So the atlas
source-map rides as a `-- atlas: <dev-id>` comment on every construct — still
greppable for the round-trip back to entities, and immune to grammar drift.

### The join Atlas supplies

Allium requires that every external-stimulus `when:` trigger be `provides:`-d by
some surface, or it is unreachable. Atlas already holds that link as the
endpoint→function `deps` edge, so `:scaffold` derives `surface Signup provides:
OpenAccount(email)` to match `rule OpenAccount when: OpenAccount(email)` for
free — and `:project` carries `:rule/implemented-by` as the explicit
command↔rule join Allium has no syntax for.

## API

```clojure
(require '[atlas.adapter.allium :as al]
         '[atlas.adapter.yaml-ld :as yld])   ; selectors are shared

;; Pure projection
(al/registry->allium reg
  {:select      (yld/aspect-selector :domain/accounts)  ; required — the module slice
   :module-name "accounts"
   :mode        :project})                               ; optional; default auto
;; => {:allium "-- allium: 3\n…"  :mode :project
;;     :stats {:entities 1 :rules 2 :surfaces 1}  :holes []}

;; Write <path>.allium
(al/emit! reg "/path/accounts"
  {:select (yld/aspect-selector :domain/accounts)})
;; => {:file "…/accounts.allium" :mode … :stats {…} :holes […]}
```

In `:scaffold` mode `:holes` is a machine-readable work-list of every
`/elicit`-able gap — feedable straight into nucleus `elicit` or `author-decision`.

### `:atlas/allium-rule` entity shape (`:project`)

```clojure
{:atlas/type          :atlas/allium-rule
 :atlas/dev-id        :rule/open-account
 :rule/name           "OpenAccount"
 :rule/when           "OpenAccount(email)"                       ; verbatim allium
 :rule/requires       ["not exists Account{email: email}"]      ; verbatim allium
 :rule/ensures        ["Account.created(email: email, status: pending)"]
 :rule/implemented-by :fn/open-account}                          ; → the join
```

Data-schema and surface carry their allium bodies as opaque `:allium/entity-body`
/ `:allium/surface-body` strings plus `:allium/name`. Atlas owns identity, kind,
the joins, and the *version history* of these strings; it never interprets them.

## Worked example A — scaffold (C4 Internet Banking)

The canonical [Internet Banking System](https://c4model.com), coded as an Atlas
registry in [`test/app/internet_banking.clj`](../test/app/internet_banking.clj),
has no rule entities — so the `:domain/auth` slice scaffolds:

```
-- allium: 3
-- @atlas scaffold (module auth) — do not regenerate;
-- /tend or /elicit transfers ownership of this file to allium.

entity Credentials {
  username: String            -- placeholder type; /elicit to refine
  password: String
  token: String
  -- atlas: schema/credentials
}

external entity Database {}  -- atlas: component/database
actor Client {}  -- TODO(/elicit): real actor(s)

rule Authenticate {
  when: Authenticate(username, password)     -- interface from :execution-function/context
  -- TODO(/elicit): requires <precondition>  -- behavioral truth atlas does not hold
  -- TODO(/elicit): ensures <outcome> (produces: token)
  -- atlas deps: component/database
  -- atlas: fn/authenticate
}

surface SignIn {
  facing _: Client        -- TODO(/elicit): actor
  provides: Authenticate(username, password) -- matches rule Authenticate's when:
  -- atlas: endpoint/sign-in
}
```

`allium check` → **0 errors** (3 `warning` + 3 `info`). Crucially, those
warnings are not noise — they independently confirm the emitter's own `:holes`:
`Credentials`/its fields read as "declared but not referenced" *because* the
rules' `requires:`/`ensures:` are still holes. Fill the holes and they clear.
Atlas's structural claim and JUXT's checker agree on exactly what behavioral
work remains.

## Worked example B — project (fully green)

An accounts spec modeled as `:atlas/allium-rule` + opaque-body entities projects to:

```
-- allium: 3
-- @atlas projection (module accounts) — generated;
-- source of truth is the atlas registry. Do not hand-edit.

entity Account {
  email: String
  status: pending | active
  transitions status {
    pending -> active
    terminal: active
  }
  -- atlas: schema/account
}

rule OpenAccount {
  when: OpenAccount(email)
  requires: not exists Account{email: email}
  ensures: Account.created(email: email, status: pending)
  -- atlas implemented-by: fn/open-account
  -- atlas: rule/open-account
}

rule ActivateAccount {
  when: ActivateAccount(account)
  requires: account.status = pending
  ensures: account.status = active
  -- atlas implemented-by: fn/activate-account
  -- atlas: rule/activate-account
}

surface Signup {
  facing _: Account
  provides: OpenAccount(email)
  provides: ActivateAccount(account)
  -- atlas: endpoint/signup
}
```

Against JUXT's `allium` v3.5.0:

| Command | Result |
|---|---|
| `allium check` | exit 0 — **0 diagnostics** (fully green) |
| `allium analyse` | exit 0 — 0 findings (data-flow / reachability clean) |
| `allium plan` | exit 0 — **derives test obligations**, incl. verifying the `pending → active` transition |

`plan` generating a test matrix *from Atlas-emitted output* is the
test-propagation leg the boundaries doc predicted — the same discipline as the
`:atlas/test-case` ontology, driven by the projection.

## Validation

Install JUXT's CLI (source: [allium-tools](https://github.com/juxt/allium-tools)):

```bash
cargo install allium-cli          # -> allium v3.5.0
# or: brew tap juxt/allium && brew install allium
```

Both worked examples above were run through it. Ground-truth facts the CLI
forced (and that earlier hand-written syntax got wrong):

1. **No `Unknown` type** — untyped scaffold fields emit `String` (the language
   reference's own advice). Primitives: `String Integer Decimal Boolean
   Timestamp Duration`.
2. **`ensures:` needs an assignment or `Entity.created(…)`**, never prose like
   "X is produced" — which is exactly why a scaffold *cannot* invent one and
   emits a hole instead.
3. **`traces:` is not a v3 clause** — the source-map is a comment.
4. **A declared transition needs a witnessing rule** — an action that
   `ensures: x.status = active`, not just a `transitions_to` trigger, or
   `allium.status.noExit` fires. (This shaped the project-mode example.)
5. **Compound identity is enforced structurally** — two rules cannot share a
   compound-id (it is the registry map key); they need distinct aspects. Atlas's
   core invariant doing real work.

## Non-goals

- No predicate evaluation or grammar ownership.
- No re-emit over hand-`tend`-ed `:scaffold` files (that is the ingest direction).
- No invention of behavioral truth in `:project` mode — a missing string stays a
  hole, never a guess.
- Not a nucleus replacement: nucleus `distill` compiles *prose → allium*; this
  compiles *registry → allium*. Complementary front-ends onto the same `.allium`
  + CLI.

## Roadmap

- **Ingest** (`allium->registry`) — the structural parser from
  [adapter-allium.md](adapter-allium.md), closing the round-trip.
- **`use` coordinates** — Allium wants immutable coords (git SHA / content hash);
  Atlas has content-versioned snapshots and could emit them for free.
- **Versioned condition diffs** — because Atlas versions the opaque strings, a
  cloud diff shows *which `requires:`/`ensures:` changed between releases* —
  spec-evolution history the stateless CLI cannot produce.
- **OKF sibling** — same selector + projection core, emitting OKF/markdown
  knowledge-pages instead of `.allium` (extends the YAML-LD adapter).
- If this graduates from experimental, extract to its own module so `core`
  carries no spec-tooling surface.
