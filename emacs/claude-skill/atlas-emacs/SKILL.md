---
name: atlas-emacs
description: Open an intent-driven Atlas layout in an Emacs frame the human is watching — LLM owns the daemon + CIDER, human attaches a terminal (`em`), LLM resolves the target, offers views labeled by anchoring intent, human picks, LLM verifies the screen against the REPL.
argument-hint: <intent or entity/aspect/data-key>
---

# Atlas Emacs Layout

The LLM's job here is **translation and option-derivation, not selection**.

- Resolve the entity/aspect from natural language (Momentum B — the map).
- Derive which views are semantically rich for it, from its ontology type-refs.
- Offer 2–3 options **labeled by what the human would anchor on**, not by function name.
- The human picks (Momentum A — the pen). Then execute, and **verify what the human sees**.

Never auto-select a layout when more than one view fits the intent. Fewness serves anchoring — offer at most three.

Why it is built this way (daemon model, frame targeting, known gaps): see [design.md](design.md).

---

## Setup (once per session)

The LLM owns the Emacs daemon; the human only attaches a terminal frame.
Everything goes through one script in the atlas repo. This skill lives in that
repo at `emacs/claude-skill/atlas-emacs/` (installed as a symlink), so resolve
the script from this skill's base directory — that works from any project:

```bash
ATLAS="$(readlink -f "<this skill's base directory>")/../../atlas-llm-daemon.sh"
```

Use `--project <dir>` for the project whose registry you're showing (usually
the current working directory), not the atlas repo.

(`$ATLAS` below.) It finds Emacs (>= 27), computes the per-project socket and
handles the attaching person's preferences — never hardcode binaries, sockets
or home paths.

1. **Pick the project** whose registry the human wants to see (usually the repo
   you are working in). Its REPL is found by directory — `.nrepl-port` first,
   then `clj-nrepl-eval --discover-ports` filtered by that directory. If none
   or several match, `ensure` fails with a candidate list: **ask the human**
   which REPL, or whether to start one — never guess.

   To start one (cider-nrepl version is read from the human's installed CIDER):
   ```bash
   $ATLAS repl --project <project-dir> [--aliases :dev:repl]
   ```
   For the atlas repo itself, then load core ontologies **and** a registry
   (example apps don't load ontologies):
   ```clojure
   (doseq [n '[atlas.ontology.execution-function atlas.ontology.interface-endpoint
               atlas.ontology.structure-component atlas.ontology.data-schema
               atlas.ontology.interface-protocol atlas.ide]]
     (require n :reload))
   (require 'app.pet-shop) (app.pet-shop/init-registry!) (atlas.datalog/reset-db-cache!)
   ```

2. **Ensure** daemon + CIDER (idempotent — safe to run every time):
   ```bash
   $ATLAS ensure --project <project-dir> [--port <nrepl-port>]
   ```
   Prints `connect:`, `ontologies:`, `status:` and `attach:`. The first start
   runs the human's full Emacs init (seconds to a minute). A
   `WARNING: core ontologies not loaded` means every dependents pane will be
   empty — fix the REPL before opening layouts.

3. **If `status` shows `frames=0`**, ask the human to attach from another
   terminal with the printed `attach:` command — typically their alias
   `em` (`alias em='<atlas repo>/emacs/atlas-llm-daemon.sh attach'`).
   Wait for them before opening a layout.

   Their preferences are theirs, not yours: `attach` reads
   `~/.config/atlas-emacs/env` (`ATLAS_EMACS_BACKGROUND=dark|light`,
   `ATLAS_EMACS_THEMES=off`), and environment variables override it. If colors
   look wrong to them, suggest that file — don't invent other attach commands.

   Mind which REPL a daemon uses: `ensure` keeps a daemon's current REPL
   while it's alive (pass `--port` to switch) and warns when a REPL lacks
   CIDER's middleware. Don't point a human's daemon at an agent-only REPL
   (e.g. one without CIDER middleware) unless they ask; start a full one with
   `$ATLAS repl` instead.

Below, `$EC` means `$ATLAS eval --project <project-dir>`: it evals elisp in
that project's daemon and prints strings as plain text.

---

## Protocol

### Step 1 — Resolve the target

Extract a concrete target from `$ARGUMENTS`:

- **Entity** — a `:dev-id` keyword or name fragment
- **Data key** — a `:ns/name` data key (e.g. `:pet/id`)
- **Aspect** — a `:ns/name` aspect (e.g. `:domain/auth`)
- **Global** — "system overview", "architecture" → no target needed

Resolve against the **local REPL** (it is what Emacs shows), not MCP/cloud —
they can disagree:
```bash
clj-nrepl-eval -p <port> "(contains? @atlas.registry/dev-id-index <kw>)"
```
Plain word ("auth") → try `:domain/auth`; entity fragment → filter
`(keys @atlas.registry/dev-id-index)` by name.

### Step 2 — Read entity type and dataflow from the live registry

```bash
$EC '(atlas-layout--entity-type ":<entity>")'          # => ":atlas/execution-function"
$EC '(atlas-layout--has-dataflow-p ":<entity-type>")'  # => t / nil
```

### Step 3 — Derive available views

| Situation | Available views |
|-----------|----------------|
| Type has dataflow verbs (execution-fn, endpoint, mcp-tool, workflow…) | data-flow view · entity anchor view |
| Type has no dataflow verbs (component, invariant, llm-prompt…) | dependents view · entity anchor view |
| Any entity | blast-radius always available as an option |
| Data key | producers · consumers · both |
| Aspect | domain survey — entities grouped by type, collapsible lists |
| Global | arch-overview — no ambiguity, proceed directly |

### Step 4 — Offer options labeled by anchoring intent

**Label by what the human anchors on, not by function name.**

> I found `:component/db` — a structure-component.
> Two views fit:
>
> **A** — *What it is, who uses it directly, and where it's defined*
>
> **B** — *What breaks if you change this* — every entity that depends on it, recursively, with a count by type
>
> Which?

Keep options to 2–3. If only one view fits clearly (aspect → domain-survey,
arch-overview), proceed without asking.

### Step 5 — Execute the chosen layout

```bash
$EC '(atlas-layout/entity-focus ":<entity>")'       # A: details · dataflow-or-dependents · source
$EC '(atlas-layout/blast-radius ":<entity>")'       # B: recursive dependents · count by type
$EC '(atlas-layout/domain-survey ":<aspect>")'
$EC '(atlas-layout/dataflow-focus ":<data-key>")'
$EC '(atlas-layout/arch-overview)'
```

Each layout opens in its own tab (`entity <id>`, `blast <id>`, `flow <key>`,
`aspect <a>`, `architecture`) in the most recently attached frame; re-running
reuses the tab.

### Step 6 — Verify, then report

Read back **exactly what the human sees** and check it against the REPL before
describing it — don't narrate a screen you haven't read:

```bash
$EC '(atlas-layout/llm-screen)'   # per window: buffer, size, point line, visible text
$EC '(atlas-layout/llm-status)'   # socket, CIDER endpoint, frames, tabs
```

Cross-check the numbers that matter for the view (e.g. dependents count vs
`(atlas.ide/dependents-of <kw>)`, blast total vs
`(atlas.ide/recursive-dependents-summary <kw>)`, source pane file vs
`$EC '(atlas-layout--definition-location ":<kw>")'`, which works with any atlas
version on the REPL's classpath). If they disagree, say so and investigate —
the view is wrong, not the human.

Reading the panes correctly:
- A dependency shown as `· <id>  not in registry` is not an atlas entity (e.g.
  an integrant component key). That's expected, not a loading problem.
- The Identity section shows the entity's own aspects only.
- A Source pane saying "No registration found" means no `register!` for that
  dev-id under the project's `src/` or `test/`; say so rather than guessing.

Report briefly: entity type, which view opened, what it shows. Then offer the
view the human *didn't* pick — once.

---

## View reference (internal — use anchoring-intent labels when speaking)

| Function | Anchoring-intent label | Pane arrangement |
|----------|----------------------|------------------|
| `entity-focus` | *what it is + what it processes/uses + where it lives* | entity-info · dataflow-or-dependents · source at register! line (stacked when frame < 140 cols) |
| `blast-radius` | *what breaks if you change this* | recursive dependents · count by type |
| `dataflow-focus` | *who produces / who consumes this data key* | producers · consumers |
| `domain-survey` | *all entities in this domain, by type* | collapsible groups |
| `arch-overview` | *how the whole system is ordered* | system summary · execution order |

---

## Guardrails

- Run `atlas-llm-daemon.sh ensure` before the first layout; if `emacsclient` errors later, run it again and retry once.
- **Never run tests in the shared REPL** — fixtures call `reset-all!` and wipe the registry the human is looking at. Use `clojure -M:test` (separate JVM).
- Only talk to the atlas daemon (`$ATLAS eval`) — never to the human's own Emacs server; evals there can hang on their init or interrupt their work.
- If the entity is not in the registry, the layout signals `Entity … not found in the connected registry` — say so explicitly.
- Never pick a layout unilaterally when more than one view fits. Offer and wait.
- One follow-up offer after execution — the view not chosen — then stop.
