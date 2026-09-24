# Emacs Integration

> **Status: Proof of Concept**

Atlas provides Emacs integration via CIDER, a transient-based interface for exploring semantic registries directly from your editor. Combined with the visual explorer UI, you get both code-editor and browser-based exploration.

## Screenshots

![Emacs transient menu](https://github.com/user-attachments/assets/1af2d36d-6fb9-44b8-b77d-2b262e396da0)
*Main transient menu with browse, entity details, and validation commands*

![Entity info buffer](https://github.com/user-attachments/assets/33a79aa4-cfa3-4484-b3c1-d2a14c197c2e)
*Entity details showing aspects, data flow, and dependencies*

![registry detail](https://github.com/user-attachments/assets/33a79aa4-cfa3-4484-b3c1-d2a14c197c2e)
*registry detail*

## Prerequisites

- Emacs 27.1+
- [CIDER](https://cider.mx/) 1.0+
- [Transient](https://github.com/magit/transient) 0.4+
- A running nREPL connection to your Clojure project

## Installation

1. Copy the `emacs/` directory to your Emacs load path:

```bash
git clone https://github.com/semantic-namespace/atlas.git
# Then add to your Emacs config:
```

2. Add to your Emacs config (`~/.emacs.d/init.el`):

```elisp
(add-to-list 'load-path "/path/to/atlas/emacs")
(require 'atlas)

;; Optional: bind main menu to a key
(global-set-key (kbd "M-F") 'atlas)
```

3. Ensure CIDER is installed:
```elisp
(use-package cider :ensure t)
(use-package transient :ensure t)
```

## Setup in CIDER

Before using Emacs exploration, start your Clojure REPL and initialize the registry:

```clojure
;; Start REPL with cider-jack-in
;; Then initialize your application
(require '[app.my-app])
(app/init-registry!)

;; Or load sample data
(require '[atlas.atlas-ui.sample-registry :as sample])
(reset! atlas.registry/registry sample/sample-registry)
```

## Quick Start

### Editor-based exploration (Emacs)
1. Start your Clojure REPL with `M-x cider-jack-in`
2. Initialize your registry: `(require '[app.my-app]) (app/init-registry!)`
3. Press `M-F` (or `M-x atlas`) to open the menu

### Browser-based exploration (Visual Explorer)
Optionally open the visual explorer alongside:
```clojure
(require '[atlas.atlas-ui.server :as ui])

;; Start v1 (graph view)
(ui/start! {:ui-version :v1})

;; Or v2 (dual map view)
(ui/start! {:ui-version :v2})
```

Then explore in your browser while using Emacs for detailed information.

Both provide complementary views of your registry.

## Main Menu

The main menu (`M-x atlas`) provides:

### Browse
| Key | Command | Description |
|-----|---------|-------------|
| `e` | List entities | Show all registered entities grouped by type |
| `a` | Find by aspect | Find entities with a specific aspect |
| `A` | List aspects | Show all aspects with usage counts |

### Authoring
| Key | Command | Description |
|-----|---------|-------------|
| `N` | Interactive author entity | Guided entity creation with live similarity feedback |
| `+` | Add aspect to set | Add aspect to `#{...}` with real-time similarity score |
| `s` | Aspect stats | Show how many entities share each aspect with an entity |
| `S` | Aspect stats at point | Show aspect stats for entity at cursor |
| `I` | Insert aspect | Two-step completion: select namespace, then name |
| `D` | Insert dev-id | Insert existing dev-id with type annotation |
| `P` | Aspect palette | Browse all available aspects (click to copy) |
| `~` | Similar at point | Show similar entities with aspect differences (✓/−/+) |

See [Authoring Guide](authoring-guide.md) for how suggestion and similarity work under the hood.

### Entity Details
| Key | Command | Description |
|-----|---------|-------------|
| `i` | Entity info | Detailed info for an entity |
| `D` | Dependencies | What this entity depends on |
| `R` | Dependents | What depends on this entity |

### Business Semantics
| Key | Command | Description |
|-----|---------|-------------|
| `b` | List business entities | Patterns, constraints, failure modes, etc. |
| `B` | Business info | Detailed business entity info |
| `M` | Business aspects of | Business aspects applied to a technical entity |

### Protocols
| Key | Command | Description |
|-----|---------|-------------|
| `@` | List protocols | Show all registered protocols |
| `#` | Protocol info | Protocol details and implementers |
| `$` | Component protocols | Protocols implemented by a component |

### Data Flow
| Key | Command | Description |
|-----|---------|-------------|
| `d` | Data flow trace | Trace data inputs for a function |
| `p` | Producers | Find what produces a data key |
| `u` | Consumers | Find what consumes a data key |
| `x` | Execution order | Topologically sorted execution order |

### Validation
| Key | Command | Description |
|-----|---------|-------------|
| `c` | Check invariants | Run all invariant validations |
| `G` | Refresh cache | Invalidate completion cache |
| `!` | Toggle debug | Enable/disable debug messages |

### Source (Lens)
| Key | Command | Description |
|-----|---------|-------------|
| `L` | Toggle lens mode | Overlay `register!` forms with signature cards |
| `m` | Cycle lens mode | Cycle: raw → semantic → impl |
| `r` | Refresh lens | Re-scan and re-fetch overlays |

### Slice Buffers

`atlas-slice` opens a **dedicated org-mode buffer** assembled from the live registry — not tied to any source file. Each entity becomes an org heading with a prose narrative body. Entities are **topologically sorted** (dependencies appear before dependents) and **tagged** by type, domain, and intent for native org filtering.

```
M-x atlas-slice-entity   RET :fn.ide/data-flow RET      ; entity only (depth 0)
C-u 1 M-x atlas-slice-entity RET :fn.ide/data-flow RET  ; entity + direct deps
M-x atlas-slice-aspect   RET :domain/ide RET            ; all entities in a domain
M-x atlas-slice          RET                            ; interactive dispatcher
```

Inside a slice buffer:

| Key | Action |
|-----|--------|
| `TAB` | Expand / collapse heading |
| `i` | Inspect entity at point — compact `:semantic` card in a bottom window |
| `s` | Jump to source — grep for the `register!` call of entity at point |
| `g` / `C-c C-l r` | Refresh (re-fetch from REPL) |
| `C-c \` | Org tag filter — e.g. `:trace:`, `:ide:`, `:execution_function:` |
| `q` | Bury buffer |

The buffer opens **folded** — all headings visible, bodies collapsed. Use `TAB` to expand individually or `S-TAB` to cycle global fold state. Requires an active CIDER connection and `atlas.ide.narrative` on the classpath.

**Tags** on each heading are derived from the entity's type, domain, and intent:
```
* fn.ide/data-flow  :execution_function:ide:trace:
```
Use `C-c \` then `:trace:` to sparse-tree only trace-intent entities, or `:ide:` for the full IDE domain.

**Internal links**: entity refs in `deps-prose` are clickable org links — `C-c C-o` on `[[*fn.ide/entity-info][fn.ide/entity-info]]` jumps to that entity's heading.

**Summary section**: a `* Summary` heading at the top shows total entity count and breakdown by type.

**Lens Mode** replaces verbose `register!` forms with template-driven views of the entity's semantic identity. Three modes cycle with `C-c C-l m`:

| Mode | What you see | Fetch |
|---|---|---|
| `:raw` | Actual source, no overlay | — |
| `:semantic` | Badge · dev-id · aspects · context · response · deps | entity props from registry |
| `:impl` | Implementation function · context · deps | entity props from registry |

For prose narrative view use `atlas-slice` instead — it generates a dedicated org-mode buffer with full graph-derived context.

Cards are purely visual — buffer content is untouched, so LSP/CIDER/paredit work normally. Moving the cursor into a form reveals the real source code.

Uses clojure-lsp to find `register!` calls regardless of namespace alias.

## Advanced Menu

Press `z` from the main menu to access advanced features:

### Architecture Analysis
| Key | Command | Description |
|-----|---------|-------------|
| `t` | By tier | Group entities by architectural tier |
| `V` | Architecture view | Full architecture documentation |
| `O` | Operations view | External integrations, pure functions |
| `C` | Domain coupling | Inter-domain dependency analysis |

### Impact & Refactoring
| Key | Command | Description |
|-----|---------|-------------|
| `I` | Impact of change | What's affected if an entity changes |
| `Y` | Aspect impact | What's affected if an aspect changes |
| `r` | Preview refactor | Dry-run aspect rename |
| `~` | Similar entities | Find semantically similar entities |

### Compliance & Quality
| Key | Command | Description |
|-----|---------|-------------|
| `P` | PII surface | Find entities handling PII |
| `E` | Error handler coverage | Check error handling coverage |
| `T` | Trace data flow | Trace a data key through the system |

### Authoring Tools (Advanced)
| Key | Command | Description |
|-----|---------|-------------|
| `n` | Create new aspect | Create a new aspect with guided prompts |
| `e` | Scaffold entity | Insert template code for a new entity |

### Ontology Tools
| Key | Command | Description |
|-----|---------|-------------|
| `S` | Suggest aspects | Get aspect suggestions for an entity |
| `X` | Inspect entity | Quick inspection |
| `K` | Aspect catalog | Browse all aspects with stats |
| `L` | List templates | Show available templates |

### Documentation & Export
| Key | Command | Description |
|-----|---------|-------------|
| `g` | Generate docs | Generate markdown documentation |
| `l` | LLM context | Export context for LLM consumption |
| `s` | System summary | High-level system overview |

## Buffer Navigation

In result buffers:

| Key | Action |
|-----|--------|
| Click on entity | Show entity info |
| Click on aspect | Find entities with aspect |
| `d` | Jump to definition (searches for `contract/def`) |
| `g` | Refresh buffer |
| `?` | Show menu |
| `q` | Quit window |

## Emacs vs Visual Explorer

Use **Emacs** when you want:
- Quick lookup without leaving your editor
- Detailed entity information (aspects, data flow, dependencies)
- Smart completion for entities, aspects, and data keys
- Deep analysis (impact of changes, PII surface, etc.)
- Integration with your code (jump-to-definition)

Use **Visual Explorer** when you want:
- Graph visualization of relationships
- Multi-aspect query builder
- Lens-based filtering
- High-level architecture overview
- Shareable views for team discussion

**Optimal workflow:** Use both simultaneously:
- Visual Explorer in browser for exploration and design discussions
- Emacs for detailed investigation and understanding
- Both auto-detect hostname for seamless remote access

## How It Works

The Emacs integration communicates with your running Clojure REPL via CIDER. It calls functions in the `atlas.ide` namespace, which provides a clean API returning EDN that Emacs can parse and display. The Visual Explorer is a separate browser-based UI that calls the HTTP API.

```
┌──────────────────────────────────────────────────────────────┐
│                     Clojure REPL                             │
│                  (cider-jack-in)                             │
│                                                              │
│  ┌────────────────────────────────────────────────────────┐ │
│  │  atlas.registry (global atom with all entities)        │ │
│  └────────────────────────────────────────────────────────┘ │
└──────────────────────────────────────────────────────────────┘
       ↑              ↑                          ↑
       │ CIDER/nREPL  │ HTTP API                │
       │ (atlas.ide)  │ (atlas.atlas-ui.server) │
       │              │                          │
   ┌───────────┐  ┌──────────────┐         ┌──────────────┐
   │   Emacs   │  │ Visual       │         │ shadow-cljs  │
   │  (Atlas)  │  │ Explorer     │         │ dev watch    │
   │           │  │ (Browser)    │         │              │
   └───────────┘  └──────────────┘         └──────────────┘
```

The `atlas.ide` namespace provides:
- EDN structures that Emacs can parse
- Caches reverse dependencies for fast lookups
- Indexes data keys for producer/consumer queries
- Completion candidates for entity, aspect, and data-key inputs

The Visual Explorer server provides:
- HTTP `/api/atlas/registry` endpoint
- Static file serving for compiled UI (v1 and v2)
- Registry watching for manual refresh prompts

## Completion

The package provides smart completion with annotations:

- **Entity completion**: Shows entity type (function, component, endpoint)
- **Aspect completion**: Shows usage count
- **Data key completion**: Autocomplete context/response keys

Works with ivy, selectrum, vertico, or default Emacs completion.

## Troubleshooting

### "CIDER not connected"
Run `M-x cider-jack-in` or `M-x cider-connect` first.

### "No entities found"
Make sure you've initialized your registry:
```clojure
(require '[atlas.ontology :as ont])
(ont/register-entity-types!)
;; Then register your entities...
```

### Stale data in Emacs
Press `G` (refresh cache) in the menu, or `g` in a result buffer.

### Visual Explorer shows "Loading registry..."
- Ensure the REPL server is running: `(atlas.atlas-ui.server/status)`
- Check the browser console (F12) for network errors
- If using `?port=8082`, verify that port's server is running
- The UI auto-detects hostname, but network connectivity issues can occur

### Emacs completion not working
Run `G` (refresh cache) to rebuild the index.

### Debug mode
Press `!` to toggle debug messages in Emacs (shows raw EDN responses).
For Visual Explorer, check the browser console (F12) for API responses.

## Remote Access

Both Emacs integration and Visual Explorer support remote development:

**Emacs Integration:**
- Emacs connects directly to CIDER via nREPL
- Works locally via `cider-jack-in` or `cider-connect` to remote nREPL
- No special configuration needed

**Visual Explorer:**
- Automatically detects the browser's hostname/IP
- When accessing `http://<server-ip>:8081/?port=8082`, API calls go to `http://<server-ip>:8082/api/atlas/registry`
- Works over any network (localhost, LAN, VPN, etc.)

## Pair with an LLM (Claude Code)

> **Experimental.**

An LLM session can drive a dedicated Emacs daemon while you watch it from a
terminal. You ask in plain words ("what breaks if I change the database?"), the
LLM opens the matching layout in your frame (one tab per view), and then reads
your screen back and compares it with the REPL before describing anything.

The LLM does the setup: it picks the project's REPL, starts the daemon, and
connects CIDER. You only attach.

### One-time setup (per person)

```bash
# 1. Make the /atlas-emacs skill available to Claude Code
#    (a symlink, so it stays in sync with the code)
emacs/atlas-llm-daemon.sh install-skill           # sessions opened in this repo
emacs/atlas-llm-daemon.sh install-skill --user    # or: every session

# 2. A short name for attaching (add to your shell profile)
alias em='/path/to/atlas/emacs/atlas-llm-daemon.sh attach'

# 3. Optional preferences, read by every `attach` (environment variables
#    with the same names override them)
mkdir -p ~/.config/atlas-emacs
cat > ~/.config/atlas-emacs/env <<'CONF'
ATLAS_EMACS_BACKGROUND=dark   # or light; default: the COLORFGBG hint, else Emacs's guess
ATLAS_EMACS_THEMES=off        # disable your Emacs themes in this daemon
CONF
```

Requirements: Emacs 27 or newer, CIDER, `clj-nrepl-eval`, and Python 3 (used
only to print results).

### Using it

In Claude Code, run `/atlas-emacs <intent>`. When the LLM says the daemon is
ready, run `em` in another terminal. `em` defaults to the current directory's
project; use `em --project <dir>` for another one.

For each request the LLM offers two or three views, labeled by the question
each one answers, and you pick one. Switch between the opened views with
`C-x t o`. `RET` on any entity follows it.

### What the script does

`emacs/atlas-llm-daemon.sh` is the only entry point. The LLM runs everything
except `attach`:

| Command | Purpose |
|---|---|
| `repl --project DIR` | start the project's nREPL with the cider-nrepl version your installed CIDER requires. If the project's dev alias starts the app, pass its source dirs with `--extra-paths` instead; `--boot FORM` runs a form once the REPL is up (e.g. loading the registry) |
| `ensure --project DIR` | start or reuse the daemon (one per project) and connect CIDER to that project's REPL |
| `attach --project DIR` | open your terminal frame (what `em` calls) |
| `eval FORM` | evaluate elisp in the daemon (what the LLM uses to open layouts and read your screen) |
| `status` | the daemon's project, git branch, REPL port and the REPL's working directory |
| `list` | every atlas daemon on this machine, with the same details |
| `stop` | shut down a daemon (`--socket PATH` for one shown by `list`) |

Each project directory gets its own daemon (the socket name includes a hash
of the full path, so worktrees with the same folder name stay separate), and
each daemon is connected to exactly one REPL.

The daemon is separate from your own Emacs server, and your preferences only
apply inside it. The script never refreshes or reloads code in your REPL.
When you work on the atlas repo itself, the LLM may load the core ontologies
and an example registry into the REPL so the views have data.

## Customization

```elisp
;; Cache TTL (default 5 seconds)
(setq atlas--cache-ttl 10)
```

## Running Emacs + Visual Explorer Together

For optimal development workflow, run both tools simultaneously:

**Terminal 1 - CIDER REPL:**
```bash
cd /path/to/atlas
clojure -M:dev
```

**Emacs:**
```elisp
M-x cider-jack-in
;; Then initialize registry
;; Then press M-F to open atlas menu
```

**Terminal 2 - Shadow-cljs (optional, for UI development):**
```bash
cd ui
npx shadow-cljs watch atlas-ui    # or atlas-ui-v2 for v2
```

**Browser - Visual Explorer:**
- After REPL starts, run: `(ui/start! {:ui-version :v2})`
- Opens at `http://localhost:8082` automatically

This gives you:
- CIDER REPL for code execution and testing
- Emacs for detailed entity inspection and analysis
- Visual Explorer for graph visualization and team discussion
- shadow-cljs watch (optional) for UI development with hot-reload

## Limitations (PoC)

- Emacs: Requires active CIDER connection
- Emacs: No real-time updates (manual refresh needed)
- Emacs: Jump-to-definition uses grep (not always accurate)
- Visual Explorer: Limited to single-project registries
- Visual Explorer: Presets not persisted across sessions
- Both: No collaborative real-time features
