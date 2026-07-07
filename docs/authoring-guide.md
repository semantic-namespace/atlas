# Atlas Authoring Guide

Covers the interactive authoring tools for building atlas entity definitions: aspect suggestion, similarity feedback, scaffolding, and the Emacs commands that surface them.

## The authoring surface

Atlas provides three ways to author entities:

| Path | Entry point | Best for |
|---|---|---|
| **Interactive wizard** | `N` in Emacs menu / `atlas-interactive-author-entity` | New entities from scratch |
| **Scaffold from ontology** | `e` in Emacs menu / `atlas-authoring-scaffold-entity` | Any registered entity type |
| **Scaffold from defmethod** | `atlas-authoring-scaffold-from-defmethod` | Migrating existing `defmethod ig/init-key` forms |

All three insert a `register!` form at point and rely on a live CIDER connection.

---

## Suggestion — how it works

### `suggest-aspects` (ontology-based)

**Function**: `atlas.ontology/suggest-aspects`, exposed as `:fn.ide/suggest-aspects`

**Algorithm**:
1. Takes the entity's current compound identity (partial set being built)
2. Runs `query/semantic-similarity` against the whole registry — Jaccard coefficient for every registered identity
3. Takes the **5 most similar** entities
4. Collects all aspects from those 5 entities, removes any already in the current identity
5. Ranks remaining aspects by frequency (most common across the 5 nearest neighbours first)
6. Returns the top 10 suggested aspects

**Jaccard coefficient**: `|A ∩ B| / |A ∪ B|` — 1.0 means identical aspect sets, 0.0 means no overlap.

```clojure
;; Example: entity has #{:atlas/execution-function :domain/auth :tier/service}
;; suggest-aspects finds the 5 most similar entities in the registry,
;; collects their aspects, and returns the most frequent ones not yet present:
;; => {:suggested-aspects [:operation/validate :effect/pure :compliance/pii ...]
;;     :similar-entries [#{...} #{...} #{...}]
;;     :rationale "Based on 5 most similar entries"}
```

**Emacs surface**: Advanced menu → `S` (Suggest aspects). Also fires implicitly in the interactive wizard (see below).

---

### `similar-with-diff` (full diff view)

**Function**: `atlas.ide/similar-with-diff`, exposed as `:fn.ide/similar-with-diff`

**Algorithm**:
1. Takes a compound identity set (the full `#{...}` including entity type)
2. Extracts aspects only (removes the entity-type keyword)
3. For every entity in the registry (via the datalog DB):
   - `shared = query-aspects ∩ entity-aspects`
   - `unique-to-query = query-aspects − entity-aspects` (aspects you have that it lacks)
   - `unique-to-entity = entity-aspects − query-aspects` (aspects it has that you lack)
   - `similarity = |shared| / |query-aspects ∪ entity-aspects|`
4. Discards entities with zero similarity (no shared aspects at all)
5. Returns sorted by similarity descending

```clojure
;; Example result entry:
{:dev-id :fn/validate-token
 :similarity 0.67
 :shared [:domain/auth :tier/service]
 :unique-to-query [:operation/fetch]   ; in your identity, missing from this entity
 :unique-to-entity [:effect/pure]}     ; in this entity, not in your identity
```

**Emacs surface**: `~` in the main authoring section — reads `#{...}` at point, shows the top results with ✓/−/+ annotation:
- `✓ Shared:` — overlap (aspects both have)
- `- Missing:` — in your identity but not in theirs (`unique-to-query`)
- `+ Extra:` — in theirs but not in yours (`unique-to-entity`)

**Difference from `suggest-aspects`**: `similar-with-diff` shows the full structural diff per entity; `suggest-aspects` aggregates across 5 nearest and returns a ranked aspect list. Use `suggest-aspects` to discover new aspects to add; use `similar-with-diff` to audit an identity you've already drafted.

---

## Interactive wizard (`atlas-interactive-author-entity`)

Keybinding: `N` in main Emacs menu.

Four steps:

1. **Entity type** — completing-read over all types from the live registry (including custom types)
2. **Dev-id** — create new (prompts namespace then name) or choose existing
3. **Aspects** — interactive loop with live similarity feedback:
   - After each aspect added, calls `similar-with-diff` on the accumulating identity
   - Reports `"%d similar entities found, closest: %.2f"` in the minibuffer prompt
   - Options: type an aspect directly, `Browse by namespace...` (two-step ns→name), `View similar entities...` (popup), or `DONE`/`RET` to finish
4. **Properties** — currently a placeholder; the scaffold fills in ontology keys with typed placeholders

After all steps, inserts the `register!` form at point and runs a final similarity check.

### Adding an aspect to an existing `#{...}` (`+`)

`atlas-interactive-add-aspect-to-set` (keybinding `+`):
- Finds the `#{...}` nearest to point
- Namespace-first aspect picker (same as wizard step 3)
- After insertion, reports similarity: `"Added :domain/auth — 12 similar entities found (closest: 0.71)"`

---

## Scaffold from ontology (`atlas-authoring-scaffold-entity`)

Keybinding: `e` in Advanced → Authoring Tools.

Reads entity types from the live registry then calls `atlas.ide/scaffold-entity`, which:
1. Looks up `ontology-keys-for` the chosen type
2. Generates a typed placeholder per key based on the key name suffix:

| Suffix | Placeholder |
|---|---|
| `-deps` | `#{}` |
| `-context` | `[]` |
| `-response` | `[]` |
| `-expectations`, `-mocks`, `-fixture` | `{}` / `[]` |
| `-fn` | `(fn [] nil)` |
| `impl` | `(fn [ctx] ctx)` |
| `target` | `:TARGET-DEV-ID` |
| anything else | `nil ;; TODO` |

The result is a complete `register!` skeleton ready to fill in.

---

## Aspect palette and completion

**Aspect palette** (`P`): popup of all aspects grouped by namespace. Click any aspect to copy to clipboard.

**Insert aspect** (`I`): two-step completing-read — first namespace (annotated with usage count), then name within that namespace. Inserts `:ns/name` at point.

**Completion annotations** in `atlas--completing-read-aspect`:
- Each aspect candidate shows `(N entities)` — how many entities in the registry carry it
- Low count = rare/unique; high count = well-established vocabulary

**Aspect stats** (`s` / `S`): for an entity, shows each of its aspects with a bar chart of how many other entities share it. Unique aspects (count=1) are highlighted in yellow; very common aspects (>50% of the entity's aspects) in green.

---

## Key differences between the suggestion tools

| Tool | Input | Output | Use when |
|---|---|---|---|
| `suggest-aspects` | Dev-id or partial identity | Ranked aspect list (top 10) | Discovering what aspects to add |
| `similar-with-diff` | Full compound identity `#{...}` | Per-entity structural diff | Auditing a drafted identity |
| `atlas-interactive` live feedback | Accumulating aspect set (while building) | Closest match score in minibuffer | Staying aware during authoring |
| `aspect-stats` | Entity dev-id | Per-aspect sharing count | Understanding connectivity |
| `aspect-catalog` | — | All aspects by namespace with counts | Browsing the vocabulary |

---

## Tool availability by context

| Tool | Needs live REPL | Works against cloud file store |
|---|---|---|
| `suggest-aspects` / `suggest-placement` | Yes | No |
| `similar-with-diff` | Yes | No |
| `scaffold-entity`, interactive wizard | Yes | No |
| `by-aspect`, `cloud-diff`, `cloud-overview` | No | Yes |
| `cloud-propose`, `cloud-push` | No (cloud-side only) | Yes |

The suggestion and similarity tools run Jaccard similarity over the in-memory registry atom. They require `cider-jack-in` with a registry loaded. Cloud file store tools work any time the atlas-cloud server is running.

---

## Dev workflows

### 1. Adding a new execution-function: "what aspects should this have?"

**Context**: you're adding `fn.logins-by-inbox/month-scan` — a new EF for the monthly inbox scan alongside the existing `fn.logins-by-inbox/month-aggregation` and `fn.logins-by-inbox/month-cursor`.

**Step 1 — scaffold the skeleton** (`e` in Emacs, or REPL):
```clojure
(ide/scaffold-entity :atlas/execution-function {:dev-id-prefix "fn.logins-by-inbox"})
;; Inserts:
;; (registry/register!
;;  :fn.logins-by-inbox/NAME
;;  :atlas/execution-function
;;  #{:atlas/execution-function}
;;  {:execution-function/context []
;;   :execution-function/response []
;;   :execution-function/deps #{}})
```

**Step 2 — get aspect suggestions** (`S` in Advanced menu, or REPL):
```clojure
;; Ask: "what do similar entities have that I don't yet?"
(ide/suggest-aspects :fn.logins-by-inbox/month-aggregation)
;; => {:ontology/suggested-aspects
;;       [:domain/logins :services/worker :tier/foundation
;;        :domain/cache :async/worker ...]
;;     :ontology/similar-entries [...]  ; top-3 most similar
;;     :rationale "Based on 5 most similar entries"}
```

**Step 3 — check the live prompt** (`N` wizard or `I` to insert each):
As you add `:domain/logins`, the minibuffer updates:
```
Add aspect (14 similar entities found, closest: 0.71): _
```
Keeps shrinking the field — you're converging on a cluster.

**Step 4 — run `similar-with-diff` on the draft** (`~` at point):
With the cursor on `#{:atlas/execution-function :domain/logins :services/worker}`:
```
0.83  fn.logins-by-inbox/month-aggregation
  ✓ Shared: :domain/logins :services/worker
  + Extra:  :atlas/execution-function :tier/foundation :async/worker

0.67  fn.logins-by-inbox/month-cursor
  ✓ Shared: :domain/logins :services/worker
  + Extra:  :atlas/execution-function :tier/foundation
  - Missing: (none)
```
The `+ Extra` column tells you what both existing siblings have that your draft is still missing — in this case `:tier/foundation` and `:async/worker`.

---

### 2. Spotting vocabulary drift before merging

**Context**: the atlas-wfs-4 branch introduces 17 new `:atlas/workflow-producer` entities. Before merging, you want to check whether any new aspect names duplicate existing vocabulary under a different name.

**At REPL, run `similar-with-diff` on a new WP**:
```clojure
(ide/similar-with-diff
  #{:atlas/workflow-producer
    :domain/logins
    :async/worker})
;; => [{:dev-id :accounts-by-email/logins-by-inbox-workflow-worker
;;      :similarity 0.60
;;      :shared [:domain/logins]
;;      :unique-to-query [:async/worker]          ; <-- your new aspect
;;      :unique-to-entity [:services/worker        ; <-- existing aspect for same concept
;;                         :atlas/structure-component ...]}]
```

The diff surfaces `:async/worker` vs `:services/worker` — both on entities in the same domain, likely meaning the same thing. This is the vocabulary drift the an atlas semantic review flagged.

**Fix before merge**: either reuse `:services/worker` on the new WPs, or rename the existing aspects — do not leave both in the registry.

---

### 3. Auditing a cluster for structural consistency

**Context**: a registry might have ~100 entities tagged `:domain/banking`. You want to check whether a new open-banking execution-function is consistent with the cluster.

**Step 1 — browse what's in the cluster** (cloud tool, no REPL needed):
```
;; via MCP or Emacs: by-aspect :domain/banking @v2.0.2
;; returns: fn.open-banking/user-data, fn.open-banking/recurring-transactions,
;;          fn.open-banking.recurring-fees-v2/banks-accounts, ...
```

**Step 2 — pick a representative and diff** (needs REPL):
```clojure
;; Draft for the new entity:
(ide/similar-with-diff
  #{:atlas/execution-function :domain/banking :tier/foundation})
;; Top result: fn.open-banking/user-data (similarity 0.80)
;;   ✓ Shared: :domain/banking :tier/foundation
;;   + Extra:  :atlas/execution-function :services/postgres :domain/cache
```

The `+ Extra` aspects on the most similar entity tell you what's conventional in this cluster — `:services/postgres` and `:domain/cache` appear on every `fn.open-banking/*` EF. Omitting them from your new entity would make it structurally inconsistent.

---

### 4. Reviewing a PR with cloud-diff then spot-checking with similarity

**Context**: a colleague pushes a registry snapshot to a feature branch. You want to review it before merging to main.

**Step 1 — see what changed** (cloud tool, no REPL needed):
```clojure
;; cloud-diff v2.0.1 → atlas-wfs-4/snap-001
;; shows: +78 entities, 17 new :atlas/workflow-producer, 2 new extractors, ...
```

**Step 2 — for each new entity type, check structural consistency** (needs REPL):
```clojure
;; For the first new WP:
(ide/suggest-aspects :producer.logins-by-inbox/month-cursor)
;; => suggests [:workflow-producer/execution-function :workflow-producer/signals ...]
;; All required ontology keys present → structurally sound.

;; For the two flagged "missing WP+EF" pairs:
(ide/similar-with-diff
  #{:atlas/execution-function :domain/logins :services/worker})
;; Shows nearest neighbours — if similarity > 0.7 without the WP aspects,
;; the entity exists as an EF but has no WP wrapper yet.
```

**Step 3 — check islands** (cloud tool):
```
;; cloud-branch-validate atlas-wfs-4 snap-001
;; reports: invariant violations? isolated pairs?
```

The combination of cloud-diff (what changed) + similar-with-diff (is it consistent) + cloud-validate (does it pass invariants) covers the full review loop without needing anything beyond the atlas-cloud server and a loaded REPL.
