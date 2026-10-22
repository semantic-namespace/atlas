# @semantic-namespace/atlas

Atlas JS compiles the [Atlas](../README.md) Clojure(Script) library to an ES module. Load an EDN registry snapshot or build one from scratch, then query and reason about software architecture — in the browser or Node.js, no server required.

The same `.cljc` source that runs on the JVM is compiled to JS. Every function behaves identically in both runtimes. The [Clojure docs](../docs/) are the primary reference — see [Translating from Clojure](#translating-from-clojure) below.

## Two Builds

| Build | File | Gzipped | Includes |
|-------|------|---------|----------|
| **Full** | `dist/atlas.js` | 434 KB | Registry + Query + Datalog + Invariants + standard ontologies |
| **Slim** | `dist-slim/atlas.js` | 75 KB | Registry + Query + Ontology (no Datascript) |

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

## meta-source pack: endpoint calls

EXPERIMENTAL. `@semantic-namespace/atlas/meta-source/endpoint-call` is a
[meta-source](https://github.com/semantic-namespace/meta-source) pack for a
JavaScript/TypeScript service that calls another service's endpoints, the
other service being described by an atlas registry.

Each call is a registry entry naming the endpoint it reaches. The property
of the service's endpoints table that builds the call's path carries the
entry's id:

```clojure
{:endpoint-call/order {:atlas/type :atlas/endpoint-call
                       :endpoint-call/endpoint :endpoint/order}}
```

```ts
export const SHOP_ENDPOINTS = {
  /** @atlas.source/id :endpoint-call/order */
  order: (u: string, o: string) => `/resource/shop.users.${u}.orders/${o}`,
};
```

The upstream's endpoints come from a JSON export of its registry (method,
path, input, output). They become entities of the service's index, so
`where`, `uses` and `deps` connect a call to its endpoint. With a checkout
of the upstream (`:root`), each endpoint is also located there: its registry
entry, and the var carrying its id. `check` reports:

| problem | severity | |
|---|---|---|
| `path-drift` | error | a tagged property builds a path other than its endpoint's |
| `method-drift` | error | a caller passes it to a client method (`get`, `post`, …) other than its endpoint's |
| `unknown-endpoint` | error | an entry names an endpoint the upstream doesn't have |
| `entry-without-tag` | warning | an entry no code is tagged with |
| `untagged-match` | info | an untagged property of a tagged table builds exactly one endpoint's path |
| `not-in-registry` | info | an untagged property builds a path no upstream endpoint declares |

```clojure
;; meta-source.edn
{:registry ["atlas/registry"]
 :src ["src"]
 :id-key :atlas.source/id
 :js {:packs {"@semantic-namespace/atlas/meta-source/endpoint-call"
              {:entry-type :atlas/endpoint-call            ; default
               :reference-key :endpoint-call/endpoint      ; default
               :upstream {:name "shop"
                          :endpoints "atlas/upstream/shop.endpoints.json"
                          :root "../shop"                  ; optional: a checkout
                          :registry ["resources/registry"]
                          :src ["src"]}}}}}
```

```sh
npx -p github:semantic-namespace/meta-source -p @semantic-namespace/atlas meta-source-js check --fail-on error
```

Tests: `npm run test:pack` (fixtures in `test/fixtures/`).

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
| Registry | `loadRegistry`, `loadStore`, `getRegistry`, `resetRegistry`, `register` |
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
