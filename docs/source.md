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
- a source map given as data;
- for the index and the editor (below): a `register!` call written as data, the
  REPL's snapshot, and the registry store CI records.

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

## Projects written with `register!`

Nothing has to move to EDN to get the index and the editor support. A
`register!` call is a source too when its dev-id, type and aspects are written
as data. clj-kondo finds every call to `atlas.registry/register!`, and the
index reads that one form. `::keywords` are resolved with the file's namespace
and aliases.

- **Props given as code** (an inline `:atlas/impl (fn …)`, a var reference) are
  left out and named in `where` and the describe buffer. An inline `:atlas/impl`
  counts as the implementation.
- **Calls whose dev-id, type or aspects are computed** are not guessed. `check`
  lists them as `:register-call-not-read`, and `summary` counts them.
- **A 3-argument call** gets the dev-id `register!` would generate.
- **Mixing styles:** a registry file and a `register!` call for the same entity
  merge like any two sources.

Coverage on real code:

| | Calls read |
|---|---|
| the cart (`test/app/cart.clj`) | 20 of 20, identical to its EDN files |
| the other example apps | 127 of 127 |
| atlas `core/src` | 520 of 536 |
| a 535-file production app | 336 of 344 |

## Data keys from clojure.spec and malli: the schema pack

A project that already shapes its data keys with clojure.spec or malli can
have the index read those shapes. Name the pack in `atlas-source.edn`:

```clojure
{:registry ["registry"] :src ["src"]
 :packs {:spec {}                                    ; every s/def call
         :malli {:registries [shop.schema/registry]}}} ; vars holding a malli registry map
```

Each `s/def` and each qualified registry entry becomes an entity keyed by its data key (an unqualified one, like `:map`, overrides a malli schema and is skipped),
in the pack's own vocabulary: `:spec/form`, and for `s/keys` the
`:spec/req`, `:spec/opt`, `:spec/req-un` and `:spec/opt-un` keys; `:malli/form`,
and for a `:map` its qualified `:malli/keys` and its `:malli/unqualified-keys`.
Forms are kept as text. Nothing is loaded; `.cljc` reader conditionals are
read as `:clj`.

A data key is one entity whatever describes it, so a registry file can add
facts the schema has no place for:

```clojure
;; registry/privacy.edn
{:user/email {:data/privacy :pii}}
```

`bb source data :user/email` then gives its shape and where it is written,
`:data/privacy :pii`, the `s/keys` it belongs to, and who produces and
consumes it. The hover shows the same. A data key needs no `:atlas/type`, and
data keys are not listed as dependencies.

`check` adds, all as info:

- `:schema-key-without-shape`: an `s/keys` names a key with no spec of its own.
  spec allows it, and it is also how a typo looks;
- `:unqualified-schema-keys`: a malli `:map` with unqualified keys, which no
  dataflow query can follow;
- `:data-key-without-schema`: keys in an entity's contract that no schema shapes;
- `:pack-form-not-read`: an `s/def` or registry the pack couldn't read (any pack reports what it couldn't read this way, with `:pack` and `:reason`).

Keys a schema shapes also count as data keys for `:keys-outside-contract`, so
code that reads a spec'd key its contract doesn't name is reported even when
no other contract names that key.

Not read yet: specs or schemas declared through a project's own wrapper fn or
macro, malli schemas held in vars rather than a registry, and function schemas
(`s/fdef`, `m/=>`). Every entry of a malli registry gets the line of its
`def`. On a 535-file app the pack read 1,770 `s/def` calls, none unreadable;
`test/fixtures/source-packs` is a small project using both libraries, checked
by `bb source-packs-smoke`, which also covers the Pathom pack below.

## Resolvers and mutations: the Pathom pack

Pathom resolvers already say what they consume and produce. With
`:packs {:pathom {}}`, each `defresolver` and `defmutation` (Pathom 3,
`com.wsscode.pathom3.connect.operation`, and Pathom 2,
`com.wsscode.pathom.connect`) becomes an entity keyed by its op-name, `ns/name`
as a keyword:

```clojure
:shop.resolvers/order-total
#:pathom{:op :resolver
         :input [:order/lines]
         :output [:order/total]
         :inferred #{:input :output}}
```

- `:pathom/input`, `:pathom/params` (a mutation's) and `:pathom/output` come
  from `::pco/input`, `::pco/params` and `::pco/output`. Output joins
  contribute their key and the keys inside them.
- When they aren't written, they are inferred as Pathom 3 does: the input from
  the destructuring of the input argument, the output from a map literal
  returned last. `:pathom/inferred` says which.
- An option may name a `def` in the same file whose value is data
  (`{::pco/output concept-properties}`); it is read through it.
- An option written as code (`(->> registry keys vec)`) is not guessed:
  `:pathom/not-read` lists it, and `check` reports it.
- A project's own wrapper macro that takes `defresolver`'s or `defmutation`'s
  arguments is read when named: `:pathom {:resolver-macros [app.connect/defresolver]
  :mutation-macros [...]}`.

Resolvers are producers and consumers next to everything else.
`bb source data :user/email` lists the resolvers that need it alongside the
functions whose contracts produce it, its spec and its privacy class. `check`
adds, as info:

- `:input-without-producer`: a resolver needs a key that nothing produces, no
  resolver and no other contract. The query can still pass it in (that is how
  a graph's entry points look); when nothing does, the resolver is
  unreachable. When some operation's output couldn't be read,
  `:unread-outputs` names those operations: any of them may produce the key;
- `:operation-option-not-read`: an input, params or output written as code;
- `:pack-form-not-read` with `:pack :pathom`: a form the pack couldn't read.

On two open-source Pathom 3 apps:

| | Operations read | Notes |
|---|---|---|
| [hermes](https://github.com/wardle/hermes) (SNOMED CT server) | 38 of 38, plus 227 `s/def` | one unproduced input: `:info.snomed/id`, the graph's entry point; one `s/keys` naming optional keys with no spec |
| [duck-repled](https://github.com/mauricioszabo/duck-repled) | 29 of 29 through its wrapper macro, plus 38 malli registry entries | the seed resolver's output is code, so unproduced inputs name it; two resolver outputs (`:var/spec`, `:repl/cljs`) are missing from the registry its wrapper validates against, so they are never validated |

Not read from the code: `pco/resolver` and `pco/mutation` calls (resolvers
built as values), dynamic resolvers, and options naming a def in another
namespace. kondo doesn't attribute the keywords inside a resolver's body to it,
so `:keys-outside-contract` doesn't cover resolvers. The running environment
covers the first three:

### What the running environment registered: the Pathom snapshot

A running Pathom environment holds the exact index of what it registered,
however each operation was built. Write it from the app's REPL, or wherever
the env is built:

```clojure
(atlas.source.pack.pathom/snapshot! [env])   ; Pathom 3 env, or Pathom 2 indexes; several at once
;; => ".atlas/packs/pathom.edn"
```

Pathom need not be on atlas's classpath, nor atlas on the app's beyond this
one call: the index is read by keyword. The file is
`{:pack :pathom :written <ms> :entities {op-id entry}}`, in the pack's
vocabulary; `:pathom {:snapshot "path"}` in `atlas-source.edn` reads it from
elsewhere. With it, the index:

- **fills** what reading the code couldn't: an output written as code takes
  the registered one, listed under `:pathom/from-runtime`, so the inputs it
  produces are no longer reported as unproduced;
- **adds** what only the environment has, such as a `pco/resolver` value, as
  an entity the hover marks "running process";
- **checks**, beside the static checks:
  - `:runtime-disagrees` (warning): an operation whose inputs, params or
    outputs differ between the code and the environment, with the keys only
    each side has. `:changed-since-snapshot` is true when the file changed after
    the snapshot was written, so the snapshot may be the stale side;
  - `:not-in-runtime` (info): defined in the code, never registered;
  - `:runtime-only` (info): registered, in no file the pack read.

`bb source summary` gives the snapshot's file, time and count under
`:pack-snapshots`. On the shop fixture, a snapshot from real Pathom 3 and 2
agrees with the code on all nine operations both have. `bb pathom-snapshot-check`
rebuilds it from real Pathom and fails if the operations change.

## A system's components: the integrant pack

```clojure
:integrant {:configs ["resources/config"]   ; config files, or directories of .edn
            :vars [app.system/config]       ; configs written in code
            :provided [:duct/logger]}       ; keys something else adds, like a duct module
```

Each config key becomes a component, in the pack's own vocabulary:

```clojure
:shop/db
#:integrant{:config-in ["resources/system.edn:2" "resources/system.test.edn:2"]
            :env ["DATABASE_URL"]           ; aero's #env, from any branch
            :varies true                    ; the files configure it differently
            :init-key true :halt-key! true} ; the defmethods the code has
```

- `:integrant/refs` comes from `#ig/ref` and `#ig/refset` anywhere in the
  config, `(ig/ref k)` in a config written in code. Refs are references, so
  `bb source uses :shop/db` lists the components that need it, and an EDN
  contract naming a component in its deps links a function to it.
- Config files are read all at once: a key that a `.mock.edn` or test variant
  configures too is one component with every file listed, not a conflict.
  Tags no reader knows (`#env`, `#or`, `#profile`) are kept as data.
- A composite key is named by the keyword no other config key uses, the others
  being what it derives from (`:integrant/derives`): `:app/web` in duct's
  `[:duct/server :app/web]`, and `:password.slack/webhook` in
  `[:password.slack/webhook :app/rotable-password]` when every password shares
  the second. Projects write them both ways round. When the config doesn't single
  one out, the last keyword is the name.
- A duct profile key (`:duct.profile/base {...}`) holds components, each read
  as one with `:integrant/profiles`.
- `(derive child parent)` calls are read in files that use integrant
  (`:integrant/isa`): there they build component hierarchies, while elsewhere
  `derive` also builds hierarchies of exceptions and the like.
- `defmethod`s of integrant's multimethods (`init-key`, `halt-key!`,
  `pre-init-spec`…) are read from the form, so `::keywords` get the file's
  namespace. clj-kondo's own dispatch string gives them the wrong one.

`check` adds:

- `:ref-to-undefined` (warning): a ref to a key no config read defines,
  directly, as a composite's parent or by `derive`, and not `:provided`;
- `:init-key-without-config` (info): an `init-key` for a key nothing configures
  or derives from: unused, or configured somewhere not read.

On real systems:

| | Read | What was left |
|---|---|---|
| a production app, 18 config files | 409 components, defmethods and derives | 16 `init-key`s no config read configures; of the four checked, two are configured nowhere in the repo, one only in tests, one in code |
| [Swirrl/datahost-prototypes](https://github.com/Swirrl/datahost-prototypes), two services | 55 | none from integrant |
| [walkable-server/realworld-fulcro](https://github.com/walkable-server/realworld-fulcro) (duct and Pathom) | 27 components, 16 resolvers and mutations | `:duct/logger`, which duct's logging module adds, declared with `:provided` |

Those systems shaped the pack. Each of these was found on one of them:

- composite keys written name-first;
- `::keyword` dispatch values;
- duct profiles;
- `derive` used to share an `init-key`;
- a `:refer` of a pack's macro.

## One id, two packs

Data keys are global: a spec and a malli schema shaping `:user/email` are one
entity. A spec under a component's own key, as integrant systems often have, is
that component's shape: the entity stays a component, with `:spec/form` beside
its `:integrant/*` keys. Two packs that declare things giving one id (a Pathom
resolver and an integrant component, say) are two things under one name, and
`check` reports `:id-in-several-packs` (warning).

## Which keys carry data: `:dataflow`

The index knows an entry consumes or produces data keys in two ways: from props
named `context` and `response` (atlas's types), and from `:dataflow` in
`atlas-source.edn`, which lists keys in any vocabulary:

```clojure
{:dataflow {:consumes [:job/reads] :produces [:job/writes]}}
```

Each pack adds its own (the Pathom pack: `:pathom/input`, `:pathom/params`,
`:pathom/output`). With this, a nightly job written as
`{:job/nightly-digest {:job/reads [:user/email]}}` is a consumer of
`:user/email` without any atlas type.

## Writing a pack

A pack is a map; the built-in ones are `atlas.source.pack.schema/pack` and
`atlas.source.pack.pathom/pack`. A project enables its own by naming the var
under `:packs`, with its options as the value:

```clojure
{:packs {:spec {} app.source/job-pack {}}}
```

A pack needs `:pack/find` and `:pack/read` (forms in code), or
`:pack/data-files` and `:pack/read-data` (data files), or both. This one reads a project's
own `defjob` macro, and makes a job a consumer of the data keys it lists:

```clojure
(def job-pack
  {:pack/id :jobs
   :pack/kinds #{:job}                       ; the :kind of its sources; one pack per kind
   :pack/vocabulary #{"job"}                 ; the key namespaces its entries use
   :pack/dataflow {:consumes [:job/reads]}
   ;; where its forms are, from clj-kondo's analysis
   :pack/find (fn [analysis opts]
                (for [u (:var-usages analysis)
                      :when (and (= 'app.scheduler (:to u)) (= 'defjob (:name u)))]
                  (select-keys u [:filename :row :name-row :name-col])))
   ;; one file's forms, read as data; what it returns is cached, so it is data
   :pack/read (fn [sites {:keys [form-at own-ns]} opts]
                (vec (for [{:keys [row name-row name-col]} sites
                           :let [[_ nm job] (form-at name-row name-col)]]
                       {:row row :kind :job :dev-id (keyword own-ns (name nm)) :entry job})))
   :pack/describe (fn [entry] (some->> (:job/schedule entry) (str "job  ")))})
```

The optional keys:

| Key | What it gives |
|---|---|
| `:pack/config-keys` | for a built-in pack, the `:packs` keys that enable it |
| `:pack/data-keys?` | true when its entities are data keys, as a schema's are |
| `:pack/checks` | `(fn [view])` → problems; `view` has `:entities`, `:of-kinds`, `:data-key?`, `:loc` and `:flow` |
| `:pack/inline-data-keys` | `(fn [entities])` → data keys its entities define with no entity of their own |
| `:pack/shape`, `:pack/members` | a data key's shape as text, and the keys a schema is made of |
| `:pack/describe` | one line for the hover |
| `:pack/data-files`, `:pack/read-data` | a pack that reads data files rather than code: `(fn [opts])` gives paths (files, or directories of `.edn`), and `(fn [files opts])` reads all of them at once, so a key several files define can be one result. `atlas.source.read/keyed-lines` reads EDN with each key's line, keeping unknown tags |
| `:pack/runtime-fill`, `:pack/runtime-diff` | its runtime half: `(fn [entry runtime])`, filling what static reading couldn't read, and saying where code and the running process disagree. The process writes `{:pack id :written ms :entities {...}}` to `.atlas/packs/<id>.edn` (or the pack's `:snapshot` option) |

`form-at` reads the form whose head symbol is at a row and column, with
`::keywords` resolved and `.cljc` reader conditionals read as `:clj`. A
result is `{:row :kind :dev-id :entry}`, or `{:row :kind :skipped reason}` when
the form can't be read; the index reports those as `:pack-form-not-read` and
counts each kind's reads in `bb source summary`. A pack that throws on a
file is reported the same way, for that file. Two packs giving the same
`:kind` are refused; a `:refer` in an `ns` form is never handed to a pack. `bb source-packs-smoke` runs a pack like this one.

## What only the running code knows: REPL snapshot and store

Some entities are made at runtime: by a generator, in a loop, or by a
`register!` call with computed arguments. Two more sources fill them in. Both
apply **only to dev-ids no file defines**, so an edit always wins over a stale
copy, without a conflict.

- **The REPL snapshot.** `(atlas.source/watch! ".atlas/runtime.edn")` keeps that
  file holding what the running process registered: the last registration of
  each dev-id, its data props, the namespace that made it and that namespace's
  file. `atlas-source.el` starts it when CIDER connects (`cider-connected-hook`;
  set `atlas-source-watch-repl` to nil to opt out). The hover marks these
  "REPL only (ns)", and `M-.` opens the namespace's file.
- **The store.** Point `bb source` at a checkout of the registry store CI
  writes (`--store dir [--store-prefix p] [--store-ref ref]`, or
  `:store {:dir :prefix :ref}` in `atlas-source.edn`; the ref defaults to
  `origin/main`). The index reads the version recorded at that ref and is
  re-read when the ref moves. The hover marks these "store only (commit)", and
  `M-.` goes to the entity's `:atlas/source` file when it has one. No REPL is
  needed.

In order: registry files and `register!` calls, then the REPL snapshot, then
the store.

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

## Emacs: hover, `M-.`, `M-?`, Flymake

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

**Flymake** underlines what `check` finds, in the file where it is: a key code
uses outside its function's contract, at that key; an integrant ref no config
defines; a resolver input nothing produces, at its name in the destructuring;
two specs for one key, naming both places. Errors, warnings and info keep
`check`'s severity (info is a note). `M-x flymake-show-buffer-diagnostics`
lists a buffer's, `flymake-show-project-diagnostics` the project's open
files. `atlas-source-mode` turns Flymake on; set `atlas-source-flymake` to
nil to add the backend without that.

The server reads files from disk, so while a buffer has unsaved changes the
backend doesn't answer: Flymake keeps the markers it has, which move with the
text, and asks again on save. Each answer runs the whole `check` on the
index in memory: about 70 ms on a 535-file app. `bb source diagnostics <file>`
gives the same, as data: each problem's line and, when there is a token to
underline, its columns.

Keywords atlas doesn't know fall through to clojure-lsp and CIDER. No REPL is
needed. Each project gets one long-lived `bb source serve` process, which
keeps the index in memory and refreshes it from disk before each answer
(about 12 ms per hover once warm). Saved files are seen; unsaved edits are
not. `M-x atlas-source-stop` stops the servers.

Batch tests: `bb emacs-smoke` runs `emacs/test/atlas-source-flymake-test.el`
(Flymake, on `test/fixtures/source-packs`), and `emacs/test/atlas-source-test.el`
(eldoc, xref, describe, on the cart example) when it finds clojure-mode. Both
start a real `bb source serve`. CI runs them on Emacs from Ubuntu.

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
- **Runtime entities are only as fresh as their copy.** An entity made at
  runtime is seen through the REPL snapshot (current while a REPL runs) or the
  store (as of the recorded ref). Neither gives a line number, only a
  namespace or an `:atlas/source` file.
- **Namespace refresh replaces vars.** With clj-reload or tools.namespace
  refresh, a reloaded namespace gets new vars; call `load!` again, as a
  system restart does. `require :reload` and CIDER evaluation keep the vars.
- **The rest of the Emacs layer still assumes `register!`.** The source pane
  in layouts, lens and `atlas-at-point` don't know about EDN sources yet.
