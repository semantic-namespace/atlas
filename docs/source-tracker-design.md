# Source Tracker — Design

## Problem

When reviewing a commit or PR, you want to know: **which entity definitions changed?**

Git knows which lines changed. Atlas knows which entities exist. The source tracker bridges the two by mapping `register!` calls to their exact line ranges in source files.

## Architecture

Source tracker self-registers its tools as `:atlas/execution-function` entities with `:atlas/impl`. All clients — Emacs, MCP, REPL — call them through the same entry point:

```clojure
(require '[atlas.registry.lookup :as lookup])

(lookup/handle-tool {:tool/name :atlas.source-tracker/staged-entities
                     :tool/args {}})
```

`handle-tool` is domain-agnostic: it resolves `:atlas/impl` from any registered entity, regardless of `:domain/ide` or `:domain/llm-ide`. Domain aspects describe *what* the tool is about, not *who* can call it.

```
┌─────────┐  ┌─────────┐  ┌──────┐
│  Emacs  │  │   MCP   │  │ REPL │
└────┬────┘  └────┬────┘  └──┬───┘
     │            │           │
     └────────────┼───────────┘
                  ▼
       lookup/handle-tool
                  │
                  ▼
       registry → :atlas/impl
```

## How It Works

```
For each commit:
  git show COMMIT:file.clj  →  file content at that point in time
          │
          ▼
    tools.reader (indexing)
    reads forms with :line / :end-line metadata
          │
          ▼
    Walk form tree, find register! calls
    extract dev-id + line range
          │
          ▼
    { :fn/validate-token {:file "src/app/auth.clj" :line 34 :end-line 38} }
          │
          ∩
    git diff COMMIT~1..COMMIT  →  changed line ranges per file
          │
          =
    set of dev-ids touched in that commit (accurate)
```

**Key**: files are scanned **as they existed at the commit**, not from the current working tree. This means line numbers are always correct, even when entities have moved, been renamed, or deleted since.

## Registered Tools

All tools are registered as `:atlas/execution-function` with `#{:tier/tooling :domain/llm-ide :intent/history}`.

| Tool dev-id | Context | Purpose |
|-------------|---------|---------|
| `:atlas.source-tracker/staged-entities` | — | Entities affected by staged git changes |
| `:atlas.source-tracker/commit-entities` | `:git/commit-ref` | Entities touched in a specific commit |
| `:atlas.source-tracker/commit-range` | `:git/base-ref` `:git/head-ref` | Entities across a commit range (PR) |
| `:atlas.source-tracker/recent-entities` | `:query/count` | Recently touched entities, most recent first |
| `:atlas.source-tracker/recent-aspects` | `:query/count` | Aspects from recent work, ranked by frequency |
| `:atlas.source-tracker/recent-context` | `:query/count` | Combined: entities + aspects + files |
| `:atlas.source-tracker/attach-source-locations` | `:source/dirs` | Enrich registry with `:atlas/source` metadata |
| `:atlas.source-tracker/entity-history` | `:entity/dev-id` | Git history for a specific entity's form |
| `:atlas.source-tracker/registry-at-commit` | `:git/commit-ref` | Build full registry map from source at a commit |
| `:atlas.source-tracker/commit-semantic-diff` | `:git/old-ref` `:git/new-ref` | Semantic diff: added/removed entities, aspect changes, renames |
| `:atlas.source-tracker/git-history-snapshot` | `:git/commit-refs` `:history/conn` | Feed commits into `atlas.history` as version snapshots |

## Usage

```clojure
(require '[atlas.registry.lookup :as lookup])

;; All calls go through handle-tool
(lookup/handle-tool {:tool/name :atlas.source-tracker/staged-entities
                     :tool/args {}})
;; => {:entity-count 2
;;     :entities [:schema/users :schema/oauth-credentials]
;;     :by-file {"test/app/calendar_availability.clj" [...]}}

(lookup/handle-tool {:tool/name :atlas.source-tracker/commit-entities
                     :tool/args {:git/commit-ref "HEAD"}})
;; => {:commit "HEAD" :message "Fix OAuth refresh" :entity-count 3 :entities [...]}

(lookup/handle-tool {:tool/name :atlas.source-tracker/recent-entities
                     :tool/args {:query/count 5}})
;; => [{:dev-id :fn/validate-token :file "..." :commit "abc" :message "..." :date "..."}]

(lookup/handle-tool {:tool/name :atlas.source-tracker/commit-range
                     :tool/args {:git/base-ref "main" :git/head-ref "feature-branch"}})
;; => {:commit-count 4 :entities-touched 5 :dev-ids [...] :by-entity {...}}
```

## Staged Changes

The tracker also works with `git diff --cached` to detect which entities are affected by currently staged changes — useful before committing.

```
  git diff --cached  →  changed line ranges per file
        │
        ∩
  scan-dirs (current working tree)  →  register! line ranges
        │
        =
  dev-ids touched in staged changes
```

Unlike commit analysis (which scans files at the commit), staged analysis uses current file content — because staged changes are about to become a commit, so the working tree has the right line numbers.

## Emacs Integration (atlas-recent.el)

Emacs calls `handle-tool` via CIDER nREPL eval — same path as MCP.

| Command | Key | Purpose |
|---------|-----|---------|
| `atlas-recent-entities` | `g` | Browse recently touched entities |
| `atlas-recent-aspects` | `G` | Browse aspects from recent work |
| `atlas-recent-jump` | `j` | Jump to a recent entity's source |
| `atlas-recent-commit-entities` | `C` | Show entities in a specific commit |
| `atlas-recent-staged-entities` | `S` | Show entities in staged changes |
| `atlas-recent-insert-staged-entities` | `I` | Insert staged dev-ids at point |
| `atlas-recent-insert-entity` | — | Insert a recent dev-id (authoring) |
| `atlas-recent-insert-aspect` | — | Insert a recent aspect (authoring) |

## Registry-at-Commit (Bridge to atlas.history)

The source tracker can extract **full registration data** from `register!` forms — not just dev-id and line range, but entity-type, aspects, and serialisable props. This builds a registry-shaped map at any commit:

```clojure
;; Build registry map from source at a commit
(lookup/handle-tool {:tool/name :atlas.source-tracker/registry-at-commit
                     :tool/args {:git/commit-ref "HEAD"}})
;; => {:commit "HEAD"
;;     :entity-count 352
;;     :entity-types {:atlas/execution-function 193, :atlas/data-schema 17, ...}
;;     :registry {#{:atlas/execution-function :domain/auth ...}
;;                {:atlas/dev-id :fn/validate-token
;;                 :atlas/type :atlas/execution-function
;;                 :execution-function/context [:auth/token]
;;                 ...}}}
```

### Semantic Diff

Compare two commits semantically — which entities were added/removed, which aspects changed, and potential renames (same type + aspects, different dev-id):

```clojure
(lookup/handle-tool {:tool/name :atlas.source-tracker/commit-semantic-diff
                     :tool/args {:git/old-ref "HEAD~5" :git/new-ref "HEAD"}})
;; => {:added [:atlas.source-tracker/staged-entities ...]
;;     :removed []
;;     :aspect-changes [{:dev-id :component/cache
;;                       :added-aspects #{:component.type/cache}
;;                       :removed-aspects #{:domain/cache}}]
;;     :renames [{:from :old/name :to :new/name :type :atlas/execution-function :aspects #{...}}]}
```

### Git → History Pipeline

Feed a sequence of commits into `atlas.history/snapshot-version!` to build a full semantic timeline from git:

```clojure
(require '[atlas.history :as history])

(let [conn (history/create-conn)
      shas ["abc123" "def456" "ghi789"]]  ;; oldest → newest
  (lookup/handle-tool {:tool/name :atlas.source-tracker/git-history-snapshot
                       :tool/args {:git/commit-refs shas :history/conn conn}})
  ;; Now query the timeline
  (history/version-diff @conn "def456")
  (history/vocabulary-diff @conn "abc123" "ghi789"))
```

This bridges two systems:
- **source-tracker**: reads `register!` forms from git → builds registry maps
- **atlas.history**: compares registry maps → tracks semantic movement (aspect diffs, renames, deletions)

The bridge works because Atlas is about **literal registrations** — `register!` calls contain all the semantic data (dev-id, entity-type, aspects, props), and `tools.reader` can extract it without evaluating code. Non-serialisable values (functions in `:atlas/impl`, `:invariant/fn`, etc.) are filtered out since they can't be read from source — but the ontology's `:ontology/not-serialisable-keys` defines exactly which keys those are.

## Design Decisions

**Why self-register as atlas entities?**
The source tracker's functions are tools. By registering them with `:atlas/impl`, they're discoverable, documented (`:atlas/docs`), and callable through the same `handle-tool` path as every other tool. No special-casing per client.

**Why `handle-tool` is domain-agnostic?**
Domain aspects (`:domain/ide`, `:domain/llm-ide`) describe what the tool is about, not who can call it. The dispatcher resolves any dev-id with `:atlas/impl` — Emacs, MCP, and REPL all use the same entry point. Discovery can scope by domain; execution doesn't.

**Why scan at each commit, not from current source?**
Line numbers drift as code evolves. An entity at line 34 today may have been at line 20 when a past commit modified it. Scanning files at each commit via `git show` gives the exact layout at that point in time, making the intersection with diff ranges accurate.

**Why tools.reader, not regex?**
Regex can find `register!` but can't reliably determine where the form ends (nested parens, strings with parens, etc.). `tools.reader` parses Clojure properly and provides `:end-line` metadata.

**Why not a macro?**
A macro wrapping `register!` can capture `:line` from `&form`, but not `:end-line`. And it would change `register!` from a function to a macro. The scanning approach needs no code changes and gives full ranges.

**Loop registrations are skipped.**
When `register!` is called inside a `doseq`/`map`/`for` with a variable dev-id (not a literal keyword), the scanner skips it. These are bulk registrations (like `register-entity-types!`) where the individual entity isn't defined at that site — the edit is about the loop, not the entity.

**Performance.**
Scanning at each commit is more expensive than scanning current files once. For `recent-entities 10`, it runs `git show` + `tools.reader` for each changed file in each of 10 commits. This is a dev-time tool with caching on the Emacs side (30s TTL), so the cost is acceptable.

## Dev-only

This module lives in `dev-tools/src` and is published as `atlas-dev` (separate JAR from core). It depends on `clojure.tools.reader` and `clojure.java.shell` for git commands. See [atlas-dev-module.md](atlas-dev-module.md) for setup.

```clojure
(require '[atlas.source-tracker]) ;; auto-registers tools
```
