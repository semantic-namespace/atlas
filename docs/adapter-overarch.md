# Overarch Adapter

> **Status: Experimental.** Namespace `atlas.adapter.overarch`
> (`core/src/atlas/adapter/overarch.clj`). Pure data — requires nothing beyond
> Clojure core.

Export an Atlas registry into an [Overarch](https://github.com/soulspace-org/overarch)
model + views, so Overarch's diagram/doc generators can render Atlas architecture
as **C4 / UML / GraphViz** — capabilities Atlas does not have natively.

## Why

Atlas and Overarch are kindred but complementary:

| | **Atlas** | **Overarch** |
|---|---|---|
| Center of gravity | Semantics, reasoning, lifecycle | Representation & generation |
| Strengths | Compound identity, invariants, data-flow, versioning, MCP/LLM tools | C4/UML model, PlantUML/GraphViz/Markdown generators, template codegen |
| Gaps | No diagram/doc generation | No invariant engine, no versioning |

Both are Clojure + EDN + qualified-keyword graphs, so the bridge is a **pure data
projection** — no impedance layer. Atlas stays the reasoning system-of-record;
Overarch becomes the render/publish target.

## The mapping

| Atlas | → | Overarch |
|---|---|---|
| compound-id (the registry map **key**) | → | `:tags` (aspects, minus `:atlas/*` meta) |
| `:atlas/dev-id` | → | `:id` (identity preserved — round-trippable) |
| `:atlas/dev-id` (qualified) | → | `:name` (full `ns/name`, **not** the bare name) |
| `:atlas/type` | → | `:el` (element category, via `default-type->el`) |
| `:services/*` + `:tier/*` aspects | → | `:tech` |
| type-refs, verb `:entity/depends` | → | `:rel` edges ("depends on") |
| type-refs, verb `:entity/produces`/`:entity/consumes` | → | `:dataflow` edges |

### Edges come from type-refs, not hardcoded keys

Atlas encodes *what a relation is* as data: a `:atlas/type-ref` entity declares
that, for entities of `:type-ref/source`, the property `:type-ref/property` holds
references forming `:type-ref/datalog-verb` edges. The adapter reads these and
extracts every declared edge generically — so it works for any entity type and
any registry without hardcoding property names.

This matters because different entity types carry their edges under different
properties (`execution-function/deps`, an endpoint's `deps`/`serialisation`, a
component's `deps`, a cache's `serialisation`, …). Reading only one hardcoded key
misses the rest — e.g. endpoints would appear to have no dependencies. Verb →
relation mapping: `:entity/depends` → `:rel`; `:entity/consumes`/`:entity/produces`
→ matched into `:dataflow` edges; other verbs are available for future semantic
views.

### Qualified names

Display names are the **full qualified dev-id** (`fn.orders/place`), not the bare
name (`place`). Two entities sharing a short name across namespaces collide when
simplified; qualified names keep them distinct and preserve the namespaced
identity Atlas is built on. Ids are always qualified keywords; `safe-id` only
strips renderer-hostile characters (`! ? * ' + = < > & %`), never the namespace,
and stashes the original dev-id in `:atlas/dev-id` so identity is recoverable.

## API

```clojure
(require '[atlas.adapter.overarch :as ov]
         '[clojure.edn :as edn])

;; A registry map {compound-id-set props-map} — e.g. an atlas-cloud snapshot.
(def reg (edn/read-string (slurp "snapshot.edn")))

;; Pure projection → {:model #{…} :views #{…} :stats {…}}
(ov/registry->overarch reg
  {:select    (ov/aspect-selector :domain/orders
                #{:atlas/execution-function :atlas/interface-endpoint})
   :system-id :myapp/orders
   :system-name "MyApp — Orders"
   :view-title "Orders subsystem and its dependencies"
   :relations #{:deps}})

;; Or write model.edn + views.edn into an Overarch --model-dir:
(ov/emit! reg "/path/to/overarch/models/orders"
  {:select    (ov/aspect-selector :domain/orders
                #{:atlas/execution-function :atlas/interface-endpoint})
   :type->el  (assoc ov/default-type->el
                :atlas/execution-function :container   ; render as C4 containers
                :atlas/interface-endpoint :container)
   :system-id :myapp/orders
   :system-name "MyApp — Orders"
   :relations #{:deps}})
;; => {:nodes N :external M :rels R}
```

### `registry->overarch` options

| key | default | meaning |
|---|---|---|
| `:select` | *(required)* | `(fn [compound-id props] -> bool)` — which entities to include |
| `:type->el` | `default-type->el` | entity-type → Overarch `:el` map |
| `:system-id` | `:atlas/system` | keyword; wraps selected nodes in a C4 system boundary |
| `:system-name` | `"System"` | display name for the boundary |
| `:view-title` | `"Atlas export"` | title for the generated views |
| `:relations` | `#{:deps}` | subset of `#{:deps :dataflow}` |
| `:external-deps?` | `true` | add out-of-selection dep targets as external systems so edges resolve |

Selectors:
- `(ov/aspect-selector :domain/orders)` — entities carrying one aspect
- `(ov/aspect-selector :domain/orders #{types…})` — …restricted to entity types
- `(ov/aspects-selector #{:domain/orders :domain/billing} #{types…})` — entities
  carrying **any** of several aspects (span multiple domains in one view)

### Views emitted

Each call emits two views over the same slice:
- **`:container-view`** — rendered by Overarch's **PlantUML** backend (C4).
- **`:concept-view`** — rendered by Overarch's **GraphViz** backend (offline, `dot`).

Nodes destined for a C4 *container* view must be `:container` (not `:component`),
so pass a `:type->el` override when targeting it.

## Rendering with Overarch

Overarch's `deps.edn` currently omits two deps its `project.clj` declares, so
supply them on the command line:

```bash
cd /path/to/overarch

# C4 PlantUML (.puml)
clojure -Sdeps '{:deps {org.clojars.quoll/tiara {:mvn/version "0.5.2"}
                        zprint/zprint {:mvn/version "1.3.0"}}}' \
  -M -m org.soulspace.overarch.adapter.ui.cli \
  -m models/orders -r plantuml -R export --no-render-format-subdirs

# GraphViz (.dot) — then rasterize locally
clojure -Sdeps '{:deps {org.clojars.quoll/tiara {:mvn/version "0.5.2"}
                        zprint/zprint {:mvn/version "1.3.0"}}}' \
  -M -m org.soulspace.overarch.adapter.ui.cli \
  -m models/orders -r graphviz -R export --no-render-format-subdirs
dot -Tsvg export/myapp/orders-concept-view.dot -o orders.svg
```

A full worked example (multi-domain slice, rendered C4 + concept diagrams) built
against a real production registry lives in the **yorba-clj** repo under
`atlas-docs/adapter-overarch/` — kept there because the diagrams embed
company-specific entity data.

## Known limitations

1. **Overarch `deps.edn` is stale** — missing `org.clojars.quoll/tiara` and
   `zprint/zprint` (present only in its `project.clj`). Hence the `-Sdeps`
   workaround. Candidate one-line upstream PR.
2. **C4 PlantUML → PNG needs network** — the bundled PlantUML C4 stdlib fetches
   AWS/Azure sprite includes at render time, so rasterization fails offline. The
   `.puml` is valid; the GraphViz path is the offline-safe renderer.
3. **`:dataflow` depends on the registry** — in registries where `response`/
   `context` keys are self-referential, data-flow edges add little and the
   `:entity/depends` graph is the real backbone. Enable with
   `:relations #{:deps :dataflow}`.

## Roadmap

- Consume Atlas's own query/datalog layer instead of re-deriving traversal in the
  adapter (single source of truth for "what depends on what").
- System-landscape view across all `:domain/*`, driven by the `:atlas/system` entity.
- Reverse direction: Overarch model → Atlas registry (import a C4 model to gain
  invariants, versioning, blast-radius, MCP tooling).
- If this graduates from experimental, extract to its own module so core carries
  no diagram-tooling surface.
