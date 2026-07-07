# Atlas UI v3 — SSE-Driven Semantic Browser

> **Status: Proof of Concept (Alpha)**

Atlas UI v3 is a browser-based registry explorer driven by **two independent momentum streams**:

- **Momentum A (server → browser)**: View state pushed via Server-Sent Events. Claude Code or any tool POSTs a new state; the browser reacts instantly without a page reload.
- **Momentum B (browser → server)**: User navigates inside the browser (clicking entities, aspects, back); the browser POSTs new state back to the server.

This decoupling means the browser is always in sync with both the LLM's intent and the user's exploration, without either side blocking the other.

## Quick Start

```clojure
(require '[atlas.atlas-ui.server :as ui])

;; Start v3 (file-root keeps hot-reload working without JAR rebuild)
(atlas.atlas-ui.server/start!
  {:ui-version :v3
   :open-browser? false
   :port 8082
   :file-root "/path/to/atlas/ui/resources/public-v3"})

;; Navigate from the REPL or any tool
(clojure.data.json/write-str
  {:mode "entity-focus" :entity ":fn/validate-token"})
;; → POST /api/v3/state
```

Or via `curl`:

```bash
# entity-focus
curl -s -X POST http://localhost:8082/api/v3/state \
  -H 'Content-Type: application/json' \
  -d '{"mode":"entity-focus","entity":":fn/validate-token"}'

# domain-survey
curl -s -X POST http://localhost:8082/api/v3/state \
  -H 'Content-Type: application/json' \
  -d '{"mode":"domain-survey","aspect":":domain/auth"}'

# dataflow-focus
curl -s -X POST http://localhost:8082/api/v3/state \
  -H 'Content-Type: application/json' \
  -d '{"mode":"dataflow-focus","data-key":":execution-function/context"}'
```

## Architecture

```
┌──────────────────────────────────────────────────────────────┐
│  Claude Code / REPL / curl                                   │
│                                                              │
│  POST /api/v3/state  {"mode":…,"entity":…,"aspect":…}       │
└───────────────────────────┬──────────────────────────────────┘
                            │
                            ▼
┌──────────────────────────────────────────────────────────────┐
│  atlas.atlas-ui.server (Clojure/Ring)                        │
│                                                              │
│  - Stores current view-state atom                            │
│  - GET /api/v3/sse  → streams state changes as SSE events    │
│  - GET /api/v3/registry  → full registry snapshot (JSON)     │
│  - POST /api/v3/state  → updates atom, fans out to SSE       │
└───────────────────────────┬──────────────────────────────────┘
                            │ SSE event stream
                            ▼
┌──────────────────────────────────────────────────────────────┐
│  Browser (ClojureScript / Reagent)                           │
│                                                              │
│  app-state atom:                                             │
│    :view-state    — current mode + entity/aspect/data-key    │
│    :registry      — fetched once on load                     │
│    :nav-history   — [{:view-state … :scroll-top … …}]        │
│    :expanded-lists — {:aspects true :home-types false …}     │
│    :filter-text   — domain-survey filter input               │
│    :search-text   — header search input                      │
│                                                              │
│  User clicks → navigate-to! → POST /api/v3/state → SSE loop │
└──────────────────────────────────────────────────────────────┘
```

The registry is fetched **once** at startup and cached client-side. View state is the only thing that flows over SSE — registry updates require a server restart and browser reload.

## Views

| Mode | Triggered by | Shows |
|------|-------------|-------|
| `home` | default / home button | Registry overview — entity count, type breakdown with bar chart |
| `entity-focus` | clicking an entity link | Entity identity (type chip, aspects), properties with semantic chips, data-flow panel |
| `blast-radius` | blast-radius tab (requires entity) | All entities whose `:*/deps` include the focused entity, grouped by type |
| `domain-survey` | clicking an aspect or type | All entities carrying a given aspect, grouped by type, with live filter input |
| `dataflow-focus` | clicking a data-key chip | Split panel: producers (green) on left, consumers (blue) on right |

### Semantic Chip Colors

| Color | Meaning | Behavior |
|-------|---------|----------|
| Blue `#3b82f6` | Entity reference (`:dev-id` resolves in registry) | Clickable → entity-focus |
| Green `#22c55e` | Aspect (keyword appears in compound IDs) | Clickable → domain-survey |
| Amber `#f59e0b` | Entity type (`:atlas/…` type keyword) | Clickable → domain-survey filtered to that type |
| Purple `#a855f7` | Data key (other qualified keyword) | Clickable → dataflow-focus |

Classification is done by `classify-kw` which checks the live registry:
1. Is the keyword a known `:atlas/dev-id`? → `:entity-ref`
2. Does the keyword appear in any compound identity set? → `:aspect`
3. Otherwise → `:data-key`

## Navigation & State Persistence

### Back stack

Every SSE-triggered navigation (i.e., any external tool navigating the browser) pushes the current state onto `nav-history` before applying the new state. The `←` button in the header pops the stack and restores:

- **scroll position** (`scrollTop` of `#v3-scroll-area`)
- **expanded collapsible lists** (keyed map in `:expanded-lists`)
- **domain-survey filter text** (`:filter-text`)

### Collapsible lists

Long lists auto-collapse to a configurable threshold with a `… N more` toggle. Thresholds:

| Location | Default threshold |
|----------|-----------------|
| Home type list | 8 |
| Entity aspects | 6 |
| Domain-survey / blast-radius entity lists | 10 |
| Property keyword collections | 5 |

Each list has a stable key (`:aspects`, `:home-types`, `(keyword entity-type)`, property keyword). Expanded state is stored in `app-state [:expanded-lists key]` so it survives re-renders and is saved/restored on back navigation.

### Entity search

The header search box (top-right) filters the in-memory registry by `:atlas/dev-id`. Type 2+ characters to show the top 10 matches; click to navigate to entity-focus. Search text resets on any SSE navigation.

## Header Mode Tabs

The five mode tabs in the header are context-aware:

- `home` — always clickable
- `entity-focus` / `blast-radius` — clickable when an entity is in scope
- `domain-survey` — clickable when an aspect is in scope
- `dataflow-focus` — clickable when a data-key is in scope

Tabs for unavailable context render in `#555` with `cursor: default`.

## Narrative Pane

When the server's view state includes a `:narrative` string, it appears as a pre-wrapped text block at the bottom of the screen (max 180px). The LLM or REPL sets this field to annotate the current view with context that wouldn't fit in the UI itself.

## Server API

```
GET  /api/v3/sse
     → text/event-stream; each event is {"mode":…,"entity":…,"aspect":…,"data-key":…,"narrative":…}

GET  /api/v3/state
     → current view-state as JSON

POST /api/v3/state
     Body: {"mode":"…","entity":"…","aspect":"…","data-key":"…","narrative":"…"}
     → updates server state, fans out to all SSE subscribers

GET  /api/v3/registry
     → full registry as JSON: {"<compound-id>": {":atlas/dev-id":"…", …}, …}
```

## Source Files

```
ui/src/atlas_ui_v3/
├── core.cljs        — all UI logic (single-file PoC)
└── api.cljs         — SSE subscription + registry fetch

ui/resources/public-v3/
└── index.html       — shell page; mounts #app
```

## Building & Running

```bash
# Compile ClojureScript (one-shot)
cd ui
npx shadow-cljs release atlas-ui-v3

# Watch mode (hot reload)
npx shadow-cljs watch atlas-ui-v3
```

Start the backend from the REPL:

```clojure
(atlas.atlas-ui.server/start!
  {:ui-version :v3
   :open-browser? false
   :port 8082
   :file-root "/path/to/atlas/ui/resources/public-v3"})
```

> **ZipException note:** Always pass `:file-root` pointing to the compiled resources directory. Without it, the JVM serves from the JAR — if the JAR is stale or open by another process you'll see `ZipFile invalid LOC header`. The `:file-root` option bypasses the JAR entirely.

## Known Limitations

- Registry is fetched once on load; changes require browser reload
- No URL routing — views aren't bookmarkable (planned)
- Blast-radius is one level deep (direct dependents only); recursive depth planned
- No real-time registry watch
- Not optimized for registries with 1000+ entities

## Roadmap

- **URL routing** — encode view state in `#hash` for bookmarks and native browser back/forward
- **Recursive blast-radius** — depth-first dependency tree, not just direct dependents
- **Orphans view** — entities with no deps and no dependents
- **Keyboard shortcuts** — `/` to focus search, `Esc` to back, `h` for home
- **Aspect co-occurrence** — from domain-survey, which other aspects cluster with this one
- **Structural gaps** — highlight entities missing expected aspects for their type
- **Linkified narrative** — auto-detect `:ns/name` patterns in narrative text and render as chips
