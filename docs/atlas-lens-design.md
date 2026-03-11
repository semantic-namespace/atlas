# Atlas Lens — Design

## Three Lens Modes

Atlas Lens overlays `register!` forms with different views, cycled via `C-c C-l m`:

| Mode | Shows | Use case |
|------|-------|----------|
| `:raw` | Actual source code | Authoring the registration itself |
| `:semantic` | Aspects, context, response, deps | Understanding what the entity means |
| `:impl` | Implementation fn, context, deps | Debugging, reading what it does |

When the cursor enters an overlaid form, the real code is revealed regardless of mode.

## Template-Driven Rendering

Each lens mode renders via a template string with `${key}` placeholders:

```
"── ${dev-id} ── ${entity-type}\n  aspects: ${entity/aspects}\n  context: ${execution-function/context}"
```

Placeholders are substituted with values fetched from the registry.

### Special keys

- `${dev-id}` — the entity's dev-id
- `${entity-type}` — the `:atlas/*` entity type
- `${entity/aspects}` — semantic aspects (computed from identity)

All other keys are looked up from the entity's properties.

## User-Customizable Specs

Templates are configured per entity type, per mode, in `atlas-lens-specs`:

```elisp
(setq atlas-lens-specs
  '((:atlas/execution-function
     (:semantic . "── ${dev-id} ── ${entity-type}\n  aspects: ${entity/aspects}\n  context: ${execution-function/context}\n  response: ${execution-function/response}\n  deps: ${execution-function/deps}")
     (:impl . ";; ${dev-id}\n${execution-function/impl}\n  :context ${execution-function/context}\n  :deps ${execution-function/deps}"))))
```

Different developers can define different lens views for the same entity types — the view is a client concern, not an ontology concern.

## Ontology Fallback

When no user template exists for a type+mode, the lens fetches `:ontology/keys` from the registry and builds a default template:

```
ontology defines keys → lens builds "key: ${key}" for each → generic fallback
```

This means custom entity types with custom ontologies get lens support automatically — no Emacs config needed.

## Architecture

```
┌──────────────────────────────────┐
│  atlas-lens-specs (Emacs)        │  ← user overrides
│  per-type, per-mode templates    │
└───────────────┬──────────────────┘
                │ fallback
                ▼
┌──────────────────────────────────┐
│  ontology/ontology-keys-for      │  ← registry query via REPL
│  (atlas.ontology)                │
└───────────────┬──────────────────┘
                │
                ▼
┌──────────────────────────────────┐
│  Template Engine                 │
│  ${key} → resolve from entity    │
│  → propertize → overlay          │
└──────────────────────────────────┘
```

## Keybindings

| Key | Command |
|-----|---------|
| `C-c C-l m` | Cycle lens mode (raw → semantic → impl) |
| `C-c C-l r` | Refresh overlays |

## Visual Styling

### Type Badges

Each entity type gets a colored badge at the start of the header line, providing instant visual scanning:

| Badge | Entity type | Color |
|-------|------------|-------|
| ` fn ` | `:atlas/execution-function` | Blue |
| ` ep ` | `:atlas/interface-endpoint` | Green |
| ` co ` | `:atlas/structure-component` | Amber |
| ` ds ` | `:atlas/data-schema` | Purple |
| ` ?? ` | (unknown) | Gray |

Customizable via `atlas-lens-type-badges`:

```elisp
;; Add a badge for a custom entity type
(push '("my-custom-type" . (" ct " . atlas-lens-badge-default-face))
      atlas-lens-type-badges)
```

### Face Hierarchy

The card uses a deliberate visual hierarchy — most important elements are largest/brightest:

```
 fn  fn.mailing-lists/source-emails    :atlas/execution-function
 ^^^  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^  ^^^^^^^^^^^^^^^^^^^^^^^^
badge   dev-id (large, bold, bright)      type (dim, italic)

     aspects:  domain/mailing-lists · tier/service
     ^^^^^^^^  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     dim label   each aspect colored individually

     context:  [inbox-service · mailing-list-id]
     ^^^^^^^^  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     dim label   value (string color, bright)
────────────────────────────────────
^^^ separator (dim)
```

Dedicated lens faces (defined in `atlas-core.el`):

| Face | Role | Default |
|------|------|---------|
| `atlas-lens-dev-id-face` | Dev-id in header | Bold, height 1.2, function-name color |
| `atlas-lens-type-face` | Entity type in header | Italic, comment color |
| `atlas-lens-label-face` | Property labels | Comment color |
| `atlas-lens-value-face` | Property values | String color |
| `atlas-lens-separator-face` | `──` lines and `·` dots | Light, comment color |
| `atlas-lens-badge-*-face` | Type badges | Colored background + light text |

All faces respect dark/light theme via `((background dark))` / `((background light))` specs.

### Aspects

Aspects on the `aspects:` line are colored individually with `atlas-aspect-face`, separated by dim `·` dots. This makes each aspect scannable at a glance.

## Finding `register!` Calls

Lens uses **clojure-lsp** (via `lsp-mode`) to find all references to `atlas.registry/register!` in the current buffer. This works regardless of the namespace alias used (`r/`, `registry/`, `cid/`, etc.).

When LSP is unavailable, a regex fallback matches any `(prefix/register!` pattern.

## Data Caching

Entity data and form positions are cached per buffer. Mode cycling re-renders from cache without re-fetching — only `C-c C-l r` or buffer save triggers a fresh REPL query.
