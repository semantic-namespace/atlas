# atlas-dev Module

## What

`atlas-dev` is an optional dev-time companion to `atlas` core. It provides tooling that requires JVM-specific dependencies (tools.reader, git) that don't belong in the production JAR.

## Module Layout

```
atlas/
├── core/       → io.github.semantic-namespace/atlas       (production JAR)
├── dev-tools/  → io.github.semantic-namespace/atlas-dev   (dev tooling JAR)
├── ui/         → io.github.semantic-namespace/atlas-ui    (visual explorer)
└── dev/        → local REPL scratch (not published)
```

## Usage in Your Project

Add `atlas-dev` as a dev-only dependency:

```clojure
;; deps.edn
{:deps
 {io.github.semantic-namespace/atlas {:mvn/version "0.1.0"}}

 :aliases
 {:dev
  {:extra-deps
   {io.github.semantic-namespace/atlas-dev {:mvn/version "0.1.0"}}}}}
```

Then in your dev namespace:

```clojure
(require '[atlas.source-tracker]) ;; auto-registers tools

;; All tools available via handle-tool
(require '[atlas.registry.lookup :as lookup])
(lookup/handle-tool {:tool/name :atlas.source-tracker/staged-entities
                     :tool/args {}})
```

## What's Included

### atlas.source-tracker

Self-registered tools for git-based entity tracking:

| Tool | Purpose |
|------|---------|
| `:atlas.source-tracker/staged-entities` | Entities affected by staged git changes |
| `:atlas.source-tracker/commit-entities` | Entities touched in a commit |
| `:atlas.source-tracker/commit-range` | Entities across a PR |
| `:atlas.source-tracker/recent-entities` | Recently touched entities |
| `:atlas.source-tracker/recent-aspects` | Aspects from recent work |
| `:atlas.source-tracker/recent-context` | Combined recent work view |
| `:atlas.source-tracker/attach-source-locations` | Enrich registry with file/line metadata |
| `:atlas.source-tracker/entity-history` | Git history for a specific entity |
| `:atlas.source-tracker/registry-at-commit` | Build full registry map from source at a commit |
| `:atlas.source-tracker/commit-semantic-diff` | Semantic diff between two commits |
| `:atlas.source-tracker/git-history-snapshot` | Feed commits into `atlas.history` as version snapshots |

See [source-tracker-design.md](source-tracker-design.md) for how it works.

## Why Separate From Core

| Concern | core | dev-tools |
|---------|------|-----------|
| Runtime | Production + dev | Dev only |
| Platform | .cljc (JVM + ClojureScript) | .clj (JVM only) |
| Dependencies | datascript, data.json | tools.reader, java.shell |
| Git access | No | Yes |
| Included in production JAR | Yes | No |

## Building

```bash
cd dev-tools

./build-and-deploy.sh              # Build JAR
./build-and-deploy.sh --install    # Install to ~/.m2
./build-and-deploy.sh --deploy     # Deploy to Clojars
```

## Source Dirs Discovery

The source tracker reads `:paths` and `:extra-paths` from your project's `deps.edn` automatically. Falls back to `["src" "test" "dev/src"]` if no `deps.edn` is found. Override via the `:source/dirs` arg:

```clojure
(lookup/handle-tool {:tool/name :atlas.source-tracker/attach-source-locations
                     :tool/args {:source/dirs ["src/main" "src/test"]}})
```
