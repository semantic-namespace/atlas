# Atlas Emacs Function Naming Reference

This document lists all interactive Emacs functions with their proper module-specific prefixes.

> **Note:** These Emacs functions provide editor-based exploration. For browser-based visual exploration, see the [Visual Explorer docs](../docs/visual-explorer.md).

## Naming Convention

All functions follow the pattern: `atlas-<module>-<function-name>`

- Functions in `atlas-browse.el` → `atlas-browse-*`
- Functions in `atlas-analysis.el` → `atlas-analysis-*`
- Functions in `atlas-business.el` → `atlas-business-*`
- Functions in `atlas-protocols.el` → `atlas-protocols-*`
- Functions in `atlas-lsp.el` → `atlas-lsp-*`
- Functions in `atlas-explorer.el` → `atlas-explorer-*`

## Function Reference

### Browse Commands (`atlas-browse.el`)

| Function | Description |
|----------|-------------|
| `atlas-browse-list-entities` | List all registered semantic entities |
| `atlas-browse-list-aspects` | List all semantic aspects with usage counts |
| `atlas-browse-find-by-aspect` | Find all entities with a specific aspect |
| `atlas-browse-entity-info` | Show detailed info for an entity |
| `atlas-browse-data-flow` | Show data flow for an entity |
| `atlas-browse-check-invariants` | Run invariant validation |
| `atlas-browse-dependents` | Find what depends on an entity |
| `atlas-browse-dependencies` | Find an entity's dependencies |
| `atlas-browse-producers` | Find functions that produce a data key |
| `atlas-browse-consumers` | Find functions that consume a data key |
| `atlas-browse-execution-order` | Show topologically sorted execution order |
| `atlas-browse-system-summary` | Show system overview |
| `atlas-browse-generate-docs` | Generate markdown documentation |

### Analysis Commands (`atlas-analysis.el`)

| Function | Description |
|----------|-------------|
| `atlas-analysis-trace-data-flow` | Trace data flow from endpoints to functions |
| `atlas-analysis-impact-of-change` | Analyze impact of changing an entity |
| `atlas-analysis-domain-coupling` | Analyze cross-domain coupling |
| `atlas-analysis-pii-surface` | Show PII exposure surface |
| `atlas-analysis-error-handler-coverage` | Check error handling coverage |
| `atlas-analysis-suggest-aspects` | Suggest aspects for an entity |
| `atlas-analysis-inspect-entity` | Deep inspection of entity metadata |
| `atlas-analysis-aspect-catalog` | Browse aspect taxonomy |
| `atlas-analysis-list-templates` | List available entity templates |
| `atlas-analysis-similar-entities` | Find similar entities |
| `atlas-analysis-by-tier` | View entities grouped by tier |
| `atlas-analysis-architecture-view` | Show architectural layers view |
| `atlas-analysis-operations-view` | View by operations/effects |
| `atlas-analysis-aspect-impact` | Analyze impact of aspect changes |
| `atlas-analysis-preview-refactor` | Preview refactoring operations |
| `atlas-analysis-llm-context` | Generate LLM-ready context |

### Business Commands (`atlas-business.el`)

| Function | Description |
|----------|-------------|
| `atlas-business-list-entities` | List business entities (patterns, constraints, etc.) |
| `atlas-business-info` | Show detailed business semantics info |
| `atlas-business-implementations` | Show technical entities implementing a business aspect |
| `atlas-business-aspects-of` | Show business aspects of a technical entity |

### Protocol Commands (`atlas-protocols.el`)

| Function | Description |
|----------|-------------|
| `atlas-protocols-list` | List all registered protocols |
| `atlas-protocols-info` | Show detailed info about a protocol |
| `atlas-protocols-of-component` | Show protocols implemented by a component |
| `atlas-protocols-components-implementing` | Find components implementing a protocol |

### LSP Integration (`atlas-lsp.el`)

| Function | Description |
|----------|-------------|
| `atlas-lsp-hover-at-point` | Show semantic hover info for keyword at point |
| `atlas-lsp-find-usages` | Find all usages of an entity in source files |
| `atlas-lsp-export` | Export registry to JSON for external tools |
| `atlas-lsp-write-search-index` | Write EDN search index |

### Explorer (`atlas-explorer.el`)

| Function | Description |
|----------|-------------|
| `atlas-explorer-list-aspects` | Browse aspects with AND/OR selection |
| `atlas-explorer-show-filtered` | Show entities matching current selection |
| `atlas-explorer-select-aspect` | Interactively select an aspect to toggle |

### Core Utilities (`atlas-core.el`)

| Function | Description |
|----------|-------------|
| `atlas-toggle-debug` | Toggle debug mode |
| `atlas--invalidate-cache` | Clear the entity cache |

### Display Utilities (`atlas-display.el`)

| Function | Description |
|----------|-------------|
| `atlas-refresh` | Refresh the current atlas buffer |
| `atlas-jump-to-definition-at-point` | Jump to entity definition at point |

## Main Entry Points

- `atlas` - Main transient menu (bound to M-F by default)
- `atlas-advanced` - Advanced features transient menu
- `atlas-explorer` - Explorer transient menu for aspect-based filtering

## Integration with Visual Explorer

These Emacs functions work alongside the browser-based Visual Explorer:

**Start Visual Explorer from REPL:**
```clojure
(require '[atlas.atlas-ui.server :as ui])
(ui/start! {:ui-version :v1})  ; Graph view
(ui/start! {:ui-version :v2})  ; Dual map view
```

**Complementary workflows:**

| Task | Use Emacs | Use Visual Explorer |
|------|-----------|-------------------|
| Quick entity lookup | ✓ | - |
| Detailed entity info | ✓ | - |
| Graph visualization | - | ✓ |
| Multi-aspect queries | - | ✓ |
| Data flow tracing | ✓ | - |
| Impact analysis | ✓ | ✓ |
| Share architecture view | - | ✓ |
| Code integration | ✓ | - |

**Optimal workflow:** Open both simultaneously - Visual Explorer in browser for high-level exploration, Emacs for detailed investigation.

