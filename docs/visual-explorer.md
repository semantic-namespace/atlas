# Visual Explorer (Atlas UI)

> **Status: Proof of Concept**
>
> The visual explorer is an experimental tool for browsing semantic registries. It's useful for understanding and exploring your architecture, but is not production-ready.

> **Note:** The UI automatically detects the current browser's hostname and uses it for API requests. This means you can access the UI from any machine (localhost, IP address, hostname) and the API calls will use the same host.

## Screenshots

![Atlas UI graph view](TODO_UI_IMAGE_1_URL)
*Graph visualization showing entities (colored by type) and aspect nodes*

![Lens filtering](TODO_UI_IMAGE_2_URL)
*Lens system filtering to a specific domain*

![Entity sidebar](TODO_UI_IMAGE_3_URL)
*Entity details sidebar with aspects, data flow, and dependencies*

## Overview

Atlas UI provides interactive visualization of your semantic registry in two versions:

- **v1**: Graph visualization with Cytoscape.js, multi-aspect queries, lens filtering
- **v2**: Dual-map view showing aspects (by namespace groups) and entities (by type)

Both versions let you explore entities, filter by aspects, trace dependencies, and understand architectural relationships. Choose the one that fits your mental model.

## Quick Start

```clojure
(require '[atlas.atlas-ui.server :as ui])

;; Start with global registry (opens browser automatically)
(ui/start!)

;; Or specify version and options
(ui/start! {:ui-version :v2 :port 8082 :open-browser? true})

;; Load sample data for exploration
(ui/start! {:load-sample? true})

;; Check status of all running servers
(ui/status)

;; Stop a server
(ui/stop! 8082)

;; Restart (useful after registry changes)
(ui/restart! {:ui-version :v2})
```

Default: v1 on `http://localhost:8082`

**Note:** The UI automatically detects the browser's hostname (localhost, 10.147.17.100, etc.) and uses it for API requests. No additional configuration needed for remote access.

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

## Development & Versions

The Atlas UI has two versions available:

### Version 1 (Graph View)
Graph visualization with multi-aspect query builder and lens system.

**Development:**
```bash
cd ui
npx shadow-cljs watch atlas-ui
# Hot-reload at http://localhost:8081
```

**From REPL:**
```clojure
(require '[atlas.atlas-ui.server :as ui])
(ui/start! {:ui-version :v1})  ; Default port 8082
```

Source files: `src/atlas_ui/`:
- `core.cljs` - Main app, state management
- `graph.cljs` - Cytoscape.js graph rendering
- `lenses.cljs` - Lens filtering logic
- `sidebar.cljs` - Entity detail panel
- `selection_controls.cljs` - Query mode controls

### Version 2 (Dual Map View)
Aspect-centric and entity-centric dual map visualization.

**Development:**
```bash
cd ui
npx shadow-cljs watch atlas-ui-v2
# Hot-reload at http://localhost:8083
```

**From REPL:**
```clojure
(require '[atlas.atlas-ui.server :as ui])
(ui/start! {:ui-version :v2})  ; Default port 8082
```

Source files: `src/atlas_ui_v2/`:
- `core.cljs` - App state and component tree
- `api.cljs` - Registry fetching
- Aspect map and entity map views

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

## Port Configuration

### Shadow-cljs Development Servers

The shadow-cljs configuration serves both versions:

```clojure
:dev-http {8081 "resources/public"      ; v1 on port 8081
           8083 "resources/public-v2"}  ; v2 on port 8083
```

**Access:**
- v1: `http://localhost:8081` (or `http://10.147.17.100:8081`)
- v2: `http://localhost:8083` (or `http://10.147.17.100:8083`)

### Backend API Server

The Atlas UI backend server (REPL) defaults to **port 8082**. When using shadow-cljs watch on a different machine:

```
Browser URL: http://10.147.17.100:8081/?port=8082
                  ↓
  Requests API from: http://10.147.17.100:8082/api/atlas/registry
```

The UI automatically uses the current browser's hostname, so requests work from any IP/hostname.

## Troubleshooting

### "Loading registry..." stuck
- Check that your REPL is running: `(atlas.atlas-ui.server/status)`
- Verify registry has entities: `(count @atlas.registry/registry)`
- Check browser console (F12) for network errors
- If using `?port=XXXX`, ensure that port's server is actually running

### Graph is empty
- Initialize entity types: `(ont/register-entity-types!)`
- Register some entities
- Click "Refresh" button in UI

### UI won't start (REPL server)
- Check if port is in use: `(ui/status)`
- Try a different port: `(ui/start! {:port 8085})`

### Slow/unresponsive
- Use lenses to filter to smaller subsets
- Hide unmatched nodes when querying
- Reduce registry size for exploration

### Wrong version loads on shadow-cljs watch
- Ensure you're accessing the correct port:
  - v1 on **8081**: `npx shadow-cljs watch atlas-ui`
  - v2 on **8083**: `npx shadow-cljs watch atlas-ui-v2`
- Ports may shift if others are in use
- Check shadow-cljs output for actual HTTP server port
