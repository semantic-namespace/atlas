# @semantic-namespace/atlas

Atlas JS compiles the [Atlas](../README.md) Clojure(Script) library to an ES module. Load an EDN registry snapshot or build one from scratch, then query and reason about software architecture — in the browser or Node.js, no server required.

The same `.cljc` source that runs on the JVM is compiled to JS. Every function behaves identically in both runtimes. The [Clojure docs](../docs/) are the primary reference — see [Translating from Clojure](#translating-from-clojure) below.

## Two Builds

| Build | File | Gzipped | Includes |
|-------|------|---------|----------|
| **Full** | `dist/atlas.js` | 125 KB | Registry + Query + Datalog + Invariants |
| **Slim** | `dist-slim/atlas.js` | 72 KB | Registry + Query + Ontology (no Datascript) |

Use **slim** for registration, querying, and analytics.
Use **full** when you also need datalog graph traversal (upstream/downstream closure, blast radius).

## Install

```bash
npm install @semantic-namespace/atlas
```

## Build from Source

```bash
cd js/
npm install
npm run build        # full  → dist/atlas.js
npm run build:slim   # slim  → dist-slim/atlas.js
npm run build:all    # both
npm run dev          # watch mode with source maps
```

## Quick Start

```javascript
import { loadRegistry, register, findByAspect, summary } from "@semantic-namespace/atlas";

// Option A — load an EDN snapshot exported from a Clojure REPL
// (spit "registry.edn" (pr-str @atlas.registry/registry))
import { readFileSync } from "fs";
loadRegistry(readFileSync("registry.edn", "utf-8"));

// Option B — register entities directly
register("fn/validate-token", "atlas/execution-function",
  ["domain/auth", "tier/service", "operation/validate"],
  {
    "execution-function/context":  ["auth/token"],
    "execution-function/response": ["auth/valid?"],
    "execution-function/deps":     ["component/oauth"]
  }
);

console.log(summary());
console.log(findByAspect("domain/auth"));
```

## Translating from Clojure

The Clojure docs use keywords and sets. In JS, replace them with strings and arrays:

| Clojure | JavaScript |
|---------|-----------|
| `:domain/auth` | `"domain/auth"` |
| `:atlas/execution-function` | `"atlas/execution-function"` |
| `#{:domain/auth :tier/service}` | `["domain/auth", "tier/service"]` |
| `{:execution-function/context [:auth/token]}` | `{ "execution-function/context": ["auth/token"] }` |
| `(registry/register! :fn/foo :atlas/execution-function #{...} {...})` | `register("fn/foo", "atlas/execution-function", [...], {...})` |
| `(query/find-by-aspect @registry :domain/auth)` | `findByAspect("domain/auth")` |

Function names follow camelCase: `find-by-aspect` → `findByAspect`, `load-registry!` → `loadRegistry`, etc.

See the [example/](./example/) folder for a working Node.js example using the [js-qualified-keywords](https://github.com/semantic-namespace/js-qualified-keywords) Babel plugin, which lets you write `:domain/auth` directly in JS source.

## API

All functions are named exports from `@semantic-namespace/atlas`.
Full documentation for each function lives in the [Clojure API reference](../docs/api-reference.md).

| Group | Functions |
|-------|-----------|
| Registry | `loadRegistry`, `getRegistry`, `resetRegistry`, `register` |
| Query | `findByAspect`, `findByDevId`, `findDevIdsWithAspect`, `findExact`, `where`, `allIdentities` |
| Scoring | `matchScore`, `queryMatches`, `semanticSimilarity` |
| Analytics | `aspectFrequency`, `relatedAspects`, `identityStats` |
| Architecture | `dependencyGraph`, `byTier`, `domainCoupling`, `impactOfChange` |
| Data Flow | `findProducers`, `findConsumers`, `traceDataFlow` |
| Set Algebra | `queryAlgebra` |
| Introspection | `registeredTypes`, `entityType`, `aspects`, `summary`, `validateTypes` |
| Ontology | `allOntologies`, `ontologyFor` |
| Invariants *(full only)* | `checkInvariants` |
| Datalog *(full only)* | `queryEntitiesWithAspect`, `queryDependencies`, `queryReverseDependencies`, `queryUpstreamClosure`, `queryDownstreamClosure`, `queryProducersOf`, `queryConsumersOf`, `rebuildDatalog` |
| Helpers | `toJS`, `toClj` |

## How It Works

```
core/src/atlas/
├── registry.cljc  ─┐
├── query.cljc      ├─ same source, compiled to JVM + JS
├── datalog.cljc    │
└── ontology.cljc  ─┘

js/src/atlas_js/
├── core.cljs       ← thin interop layer (Keyword ↔ string)
└── core_slim.cljs  ← same, without Datascript
```
