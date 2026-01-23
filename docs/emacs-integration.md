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

The main menu (`M-x semantic-ns`) provides:

### Browse
| Key | Command | Description |
|-----|---------|-------------|
| `e` | List entities | Show all registered entities grouped by type |
| `a` | Find by aspect | Find entities with a specific aspect |
| `A` | List aspects | Show all aspects with usage counts |

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
- When accessing `http://10.147.17.100:8081/?port=8082`, API calls go to `http://10.147.17.100:8082/api/atlas/registry`
- Works over any network (localhost, LAN, VPN, etc.)

## Customization

```elisp
;; Cache TTL (default 5 seconds)
(setq semantic-ns--cache-ttl 10)

;; IDE namespace (if you've wrapped atlas.ide)
(setq semantic-ns-ide-ns "my.custom.ide")
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
