# Visual Explorer (Atlas UI)

> **Status: Proof of Concept**
>
> The visual explorer is an experimental tool for browsing semantic registries. It's useful for understanding and exploring your architecture, but is not production-ready.

## Screenshots

![Atlas UI graph view](TODO_UI_IMAGE_1_URL)
*Graph visualization showing entities (colored by type) and aspect nodes*

![Lens filtering](TODO_UI_IMAGE_2_URL)
*Lens system filtering to a specific domain*

![Entity sidebar](TODO_UI_IMAGE_3_URL)
*Entity details sidebar with aspects, data flow, and dependencies*

## Overview

Atlas UI provides an interactive graph visualization of your semantic registry. It renders entities and aspects as nodes, with edges showing relationships (membership and dependencies).

## Quick Start

```clojure
(require '[atlas.atlas-ui.server :as ui])

;; Start with global registry (opens browser automatically)
(ui/start!)

;; Or with custom registry and options
(ui/start! my-registry {:port 8082 :open-browser? true})

;; Load sample data for exploration
(ui/start! {:load-sample? true})

;; Check status
(ui/status)

;; Stop
(ui/stop! 8082)
```

The UI opens at `http://localhost:8082` by default.

## Features

### Graph Visualization

- **Entity nodes**: Functions, components, endpoints, schemas (colored by type)
- **Aspect nodes**: Domain, tier, protocol aspects (smaller, grouped by namespace)
- **Edges**: Membership (entity → aspect) and dependency (entity → entity)
- **Auto-layout**: Nodes arrange automatically based on connections (Cytoscape.js)

### Multi-Aspect Query Builder

Click aspects to build boolean queries:

| Mode | Key | Description |
|------|-----|-------------|
| **AND** | `1` | Entities must have ALL selected aspects |
| **OR** | `2` | Entities must have ANY selected aspect |
| **COUNT** | `3` | Rank entities by number of matching aspects |

- **Click** aspect to add to query (positive selection)
- **Shift+click** aspect to negate (exclude entities with this aspect)
- Color intensity shows match score

### Lens System

Lenses filter the graph to focus on specific views. Press `L` to open the lens selector.

**Available Lenses:**

| Lens Type | Description |
|-----------|-------------|
| **Domain** | Show only entities in a specific domain (e.g., `:domain/auth`) |
| **Tier** | Show only entities in a tier (e.g., `:tier/service`) |
| **Protocol** | Show entities implementing a protocol |
| **Constraint** | Show entities with a constraint aspect |
| **Dependency Flow** | Trace dependencies from a specific entity |
| **Impact Analysis** | See what's affected if an entity changes |
| **Combination** | Combine multiple lenses (AND logic) |

When a lens is active:
- Only matching entities are shown
- Aspect nodes are hidden (shown as badges on entities)
- A banner shows the active lens
- URL updates for sharing

### Keyboard Shortcuts

| Key | Action |
|-----|--------|
| `ESC` | Clear selections / clear lens |
| `L` | Toggle lens selector |
| `P` | Toggle presets panel |
| `[` | Go back in lens history |
| `]` | Go forward in lens history |
| `1` | AND query mode |
| `2` | OR query mode |
| `3` | COUNT query mode |
| `Ctrl/Cmd+S` | Save current lens as preset |
| `Ctrl/Cmd+C` | Copy lens URL to clipboard |

### Entity Details

Click an entity node to see details in the sidebar:
- Semantic identity (all aspects)
- Data flow (context inputs, response outputs)
- Dependencies
- Definition values

### URL Sharing

When a lens is active, the URL updates to include lens parameters. Share the URL to give others the same filtered view:

```
http://localhost:8082/?lens=domain&value=domain/auth
```

### Presets

Save frequently-used lens configurations:
1. Activate a lens
2. Press `P` or click "Presets"
3. Click "Save Current Lens"
4. Name your preset

Presets are stored in browser memory (not persisted across sessions in this PoC).

## UI Components

### Header Bar
- **Refresh**: Reload registry from server
- **Choose Lens**: Open lens selector
- **Presets**: Save/load lens configurations
- **Share**: Copy lens URL (when lens active)
- **History**: Navigate back/forward through lens history

### Info Panel (bottom-left)
- Entity/aspect/edge counts
- Active lens info
- Query results (when aspects selected)

### Search Bar (top-right of graph)
- Filter entities by name

### Type Filter (left side)
- Filter by entity type (function, component, endpoint, etc.)

### Color Legend
- Entity type colors
- Aspect namespace colors

## How It Works

```
┌─────────────────────────────────────────────────────────────────┐
│                        Browser (ClojureScript)                  │
│                                                                 │
│  ┌──────────────────────────────────────────────────────────┐  │
│  │                    Reagent Components                     │  │
│  │  - Graph (Cytoscape.js)                                  │  │
│  │  - Lens selector                                          │  │
│  │  - Sidebar                                                │  │
│  │  - Controls                                               │  │
│  └──────────────────────────────────────────────────────────┘  │
│                              │                                  │
│                              │ HTTP GET /api/atlas/registry     │
│                              ▼                                  │
└─────────────────────────────────────────────────────────────────┘
                               │
                               │ EDN or Transit+JSON
                               ▼
┌─────────────────────────────────────────────────────────────────┐
│                    Server (Clojure/Ring)                        │
│                                                                 │
│  atlas.atlas-ui.server                                         │
│  - Serves static files (compiled UI)                           │
│  - /api/atlas/registry endpoint                                │
│  - Watches registry atom for changes                           │
└─────────────────────────────────────────────────────────────────┘
                               │
                               │ @registry-atom
                               ▼
┌─────────────────────────────────────────────────────────────────┐
│                    atlas.registry                               │
│                                                                 │
│  Global registry atom containing all semantic entities          │
└─────────────────────────────────────────────────────────────────┘
```

The UI fetches the registry on load and whenever you click "Refresh". Changes to the registry atom trigger a console message prompting browser refresh.

## Development

To modify the UI:

```bash
# Start shadow-cljs watcher
npx shadow-cljs watch atlas-ui

# UI will hot-reload at http://localhost:3000
```

Source files are in `src/atlas_ui/`:
- `core.cljs` - Main app, state management
- `graph.cljs` - Cytoscape.js graph rendering
- `lenses.cljs` - Lens filtering logic
- `sidebar.cljs` - Entity detail panel
- `selection_controls.cljs` - Query mode controls

## Limitations

This is a proof-of-concept with known limitations:

- **Performance**: May be slow with large registries (100+ entities)
- **State**: Presets are not persisted (browser memory only)
- **Updates**: No real-time updates; manual refresh required
- **Layout**: Graph layout can be chaotic with many nodes
- **Mobile**: Not optimized for mobile/touch devices
- **Accessibility**: Limited keyboard navigation and screen reader support

## Server API

The UI server exposes one endpoint:

```
GET /api/atlas/registry
Accept: application/edn          → Returns EDN
Accept: application/transit+json → Returns Transit+JSON

Response:
{:atlas-ui.api.response/registry {...}
 :timestamp 1234567890
 :count 42}
```

## Configuration

```clojure
(ui/start! registry-atom
  {:port 8082              ; HTTP port (default 8082)
   :open-browser? true     ; Auto-open browser (default true)
   :load-sample? false})   ; Load sample registry (default false)
```

## Troubleshooting

### "Loading registry..." stuck
- Check that your REPL is running
- Verify registry has entities: `(count @atlas.registry/registry)`
- Check browser console for errors

### Graph is empty
- Initialize entity types: `(ont/register-entity-types!)`
- Register some entities
- Click "Refresh"

### UI won't start
- Check if port is in use: `(ui/status)`
- Try a different port: `(ui/start! {:port 8083})`

### Slow/unresponsive
- Use lenses to filter to smaller subsets
- Hide unmatched nodes when querying
- Reduce registry size for exploration
