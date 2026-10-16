# Sources: registry in data, code stays plain

> **Experimental** — see [EXPERIMENTAL.md](../EXPERIMENTAL.md). Namespaces:
> `atlas.source`, `atlas.source.index`, `atlas.source.hover`; CLI `bb source`;
> Emacs `emacs/atlas-source.el`.

The usual way to use atlas puts the definition next to the code, as a
`register!` call. This page describes a second way. The code stays ordinary
Clojure, and each function carries one metadata key naming its entity. The
definitions live in EDN files.

```clojure
;; src/app/cart_impl.clj — nothing here requires atlas
(defn ^{:atlas.source/id :fn/get-cart} get-cart
  [{data :session/data} _]
  ...)
```

```clojure
;; registry/functions.edn
{:fn/get-cart
 {:atlas/type :atlas/execution-function
  :atlas/aspects #{:tier/service :domain/cart :operation/read}
  :execution-function/context [:session/data]
  :execution-function/response [:cart/id :cart/items :cart/user-id :cart/shipping-address :cart/guest?]
  :execution-function/deps #{}}}
```

`defn` keeps working with clj-kondo, clojure-lsp, docstrings, stack traces
and the REPL. The application compiles without atlas on the classpath, so a
team can adopt it one function at a time. The cart example is written both
ways: `test/app/cart.clj` with `register!` calls, and `test/app/cart_impl.clj`
with `test/app/cart_registry/*.edn`. `test/app/cart_source_test.clj` checks
that both produce the same registry.

## Sources and how they merge

A **source** is anything that says something about entities:

- an EDN file, written by a person or an LLM, holding a map of dev-id to entry;
- a generator, as a fn or var of no args returning `{:source label :entities {...}}`, called on every load;
- a source map given as data.

The registry is the **merge** of its sources. An entity may be described by
several of them, each giving the keys it owns. Keys are qualified, so they
say who owns them. A business-modelling file can add
`:atlas/business-pattern`, or a generator can add `:mcp-tool/name`, to an
entity a developer wrote. The merge does not depend on the order the sources
are read in:

| Same entity, from several sources | Result |
|---|---|
| different keys | combined |
| the same key with the same value | fine |
| `:atlas/aspects` | the union of every source's aspects |
| any other key with different values | a **conflict**, naming every value and the sources that gave it. `:atlas/type` included. |

Sets other than aspects (deps, for example) are deliberately not unioned. A
union would hide a deletion: removing a dep in one file while another still
lists it would keep it.

Each registered entity carries `:atlas/source`, the labels of the sources
that gave it something: file paths as given, generators as symbols. It is
plain data, so it survives export and the store.

## Loading

```clojure
(require '[atlas.source :as source])

(source/load! ["app/registry/functions.edn"     ; file path or classpath resource
               "app/registry/endpoints.edn"
               #'app.tools/generate-mcp-tools]  ; generator, called on each load
              '[app.cart-impl])                 ; namespaces holding the annotated vars
```

`load!` merges the sources and finds every public var in the namespaces with
`:atlas.source/id`. It registers each entity with its var as `:atlas/impl`.
That is the var, not its value, so redefining the fn at the REPL is seen
without reloading. `load!` registers nothing and throws when:

- sources conflict;
- a var names a dev-id no source defines;
- an entity has no `:atlas/type` from any source;
- an `:atlas/execution-function` entity has no var;
- two vars claim one dev-id.

`(source/reload!)` replays every `load!` after you edit an EDN file. It
reads and checks everything before replacing anything, so a reload that
throws leaves the registry as it was. It returns
`{:added [...] :removed [...] :changed [...]}`. `forget-loads!` clears the
remembered loads, beside `atlas.registry/reset-all!` in test fixtures.

## `bb source`: answers from the files, no JVM

`atlas.source.index` reads the same sources without loading any code. It
reads the EDN files with the line each key is on, and the code through
clj-kondo: annotated vars, calls between vars, and the keywords each var
mentions. The code analysis is cached per file by modification time, so
each call sees the working tree as it is.

```
bb source where :fn/add-to-cart     # each file that defines it (line, keys given), its var (lines)
bb source uses  :fn/get-product     # entries that name it, code that calls its var, coverage
bb source deps  :endpoint/cart-graph-api
bb source data  :cart/subtotal      # producers and consumers
bb source check                     # where code and sources disagree, most severe first
bb source hover :fn/add-to-cart     # what an editor shows
bb source summary
```

Output is EDN, or JSON with `--json`. The directories come from
`--registry dir --src dir`, or from `atlas-source.edn` at the project root:

```clojure
{:registry ["registry"] :src ["src"]}
```

From another repo: `bb --config <atlas-checkout>/bb.edn source ...`. The
cache is `<root>/.atlas/source-index.transit`; add `.atlas/` to
`.gitignore`.

`check` reports:

- **errors**: conflicts, unreadable files, linkage problems (as for `load!`), a dev-id on two vars;
- **warnings**: a linked var calling another linked var whose entity its entry doesn't name;
- **info**: data keys a function mentions that are in neither its own contract nor the response of an entity it names.

`bb registry` answers from the *recorded store* (versions CI wrote).
`bb source` answers from the *working tree*, uncommitted edits included.

### Speed

Measured on a 535-file, 49k-line project:

| | Time |
|---|---|
| First analysis (writes the cache) | ~7.7s |
| Each call after that, in a new bb process | ~0.16s |
| After editing one file | that file alone is re-analysed (0.01–0.1s) |

Reading a 2,000-entity registry from 40 EDN files takes about 70 ms.

## Emacs: hover, `M-.`, `M-?`

```elisp
(load "<atlas checkout>/emacs/atlas-source.el")
(atlas-source-setup)   ; enable in Clojure buffers of projects with an atlas-source.edn
```

Point on any `:ns/name` keyword, in a `.clj` file or a registry `.edn` file:

| | |
|---|---|
| eldoc | **Entity:** type, aspects, contract, deps, use count, problems. **Data key:** producers and consumers. **Aspect:** the entities that carry it. |
| `M-.` | each registry file that defines it, and the var that implements it |
| `M-?` | the entries that name it, and the code that calls its var |
| `C-c .` | a `*atlas*` buffer with everything, `file:line` links and keywords clickable |

Keywords atlas doesn't know fall through to clojure-lsp and CIDER. No REPL is
needed. Each project gets one long-lived `bb source serve` process, which
keeps the index in memory and refreshes it from disk before each answer
(about 12 ms per hover once warm). Saved files are seen; unsaved edits are
not. `M-x atlas-source-stop` stops the servers.

Batch test: `emacs --batch -Q -L <dir with clojure-mode.el> -l emacs/test/atlas-source-test.el`.

## Sources and the store

These are two different things, not two competing formats:

- **Sources are input.** They are edited by people and LLMs, reviewed in
  PRs, keyed by dev-id, grouped as the authors like, and can hold comments.
- **The store is the record.** `atlas.store`, written by CI, holds the
  *evaluated* registry: authored entities, generated ones and ontologies.
  It is canonical, sorted and grouped by type for compact history.

The store can't be the source, because it holds entities nobody wrote and is
rewritten canonically. The sources can't be the store, because they lack the
generated entities. `:atlas/source` links the record back to the input.

## Limits

- **Declared deps aren't checked against injected deps.** `check` sees a
  linked var calling another linked var directly. It can't see a dependency
  passed in as an argument.
- **Data keys are found by prop name.** A prop whose name is `context` is
  read as consumed keys, and one named `response` as produced keys. That fits
  the built-in types; other ontologies would need a mapping.
- **Nested data shows up as info notes.** `merge-carts` reads `:item/*` fields
  inside `:cart/items`, and its contract doesn't mention them.
- **Generated entities need a fn source.** An entity a generator creates only
  at runtime, and that no source returns, is invisible to `bb source`.
- **Namespace refresh replaces vars.** With clj-reload or tools.namespace
  refresh, a reloaded namespace gets new vars; call `load!` again, as a
  system restart does. `require :reload` and CIDER evaluation keep the vars.
- **The rest of the Emacs layer still assumes `register!`.** The source pane
  in layouts, lens and `atlas-at-point` don't know about EDN sources yet.
