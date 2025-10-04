# Emacs Integration

> **Status: Proof of Concept**

Atlas provides Emacs integration via `semantic-ns.el`, a transient-based interface for exploring semantic registries directly from your editor.

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

1. Copy `emacs/semantic-ns-v2.el` to your Emacs load path
2. Add to your config:

```elisp
(require 'semantic-ns)

;; Optional: bind to a key
(global-set-key (kbd "M-F") 'semantic-ns)
```

## Quick Start

1. Start your Clojure REPL with `cider-jack-in`
2. Initialize your registry (e.g., `(require '[app.my-app]) (app/init-registry!)`)
3. Press `M-x semantic-ns` (or your keybinding) to open the menu

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

## How It Works

The Emacs integration communicates with your running Clojure REPL via CIDER. It calls functions in `atlas.ide` namespace, which provides a clean API returning EDN that Emacs can parse and display.

```
┌─────────────┐     CIDER/nREPL     ┌─────────────┐
│   Emacs     │ ←─────────────────→ │   Clojure   │
│ semantic-ns │                     │  atlas.ide  │
└─────────────┘                     └─────────────┘
```

The `atlas.ide` namespace:
- Returns EDN structures that Emacs can parse
- Caches reverse dependencies for fast lookups
- Indexes data keys for producer/consumer queries
- Provides completion candidates for entity, aspect, and data-key inputs

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

### Stale data
Press `G` (refresh cache) in the menu, or `g` in a result buffer.

### Debug mode
Press `!` to toggle debug messages, which show raw EDN responses.

## Customization

```elisp
;; Cache TTL (default 5 seconds)
(setq semantic-ns--cache-ttl 10)

;; IDE namespace (if you've wrapped atlas.ide)
(setq semantic-ns-ide-ns "my.custom.ide")
```

## Limitations (PoC)

- Requires active CIDER connection
- No real-time updates (manual refresh needed)
- Jump-to-definition uses grep (not always accurate)
- Limited to single-project registries
