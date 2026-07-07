# YAML-LD / JSON-LD Adapter

> **Status: Experimental.** Namespace `atlas.adapter.yaml-ld`
> (`core/src/atlas/adapter/yaml_ld.clj`). Export-only. Requires
> `clojure.data.json` (already a core dep).

Export an Atlas registry to [JSON-LD](https://www.w3.org/TR/json-ld11/) and its
YAML serialization [YAML-LD](https://w3c.github.io/yaml-ld/) — W3C linked data.
This makes Atlas registries consumable by the entire semantic-web ecosystem
(triple stores, SPARQL, RDF tooling, existing vocabularies).

## Why

Atlas already *is* a linked-data graph — it just uses EDN instead of RDF syntax:

| Atlas | → | Linked data |
|---|---|---|
| `:atlas/dev-id` | → | `@id` (a CURIE / IRI) |
| `:atlas/type` | → | `@type` (rdf:type) |
| compound-id aspects | → | `atlas:aspect` (each an `@id` resource) |
| qualified-keyword namespaces | → | `@context` prefix → IRI mappings |
| **type-refs** (property + `:type-ref/datalog-verb`) | → | **`@context` term defs**: property CURIE → the verb IRI, `{@type: @id, @container: @set}` |
| ref edges (deps / consumes / produces) | → | RDF triples on the verb predicate |

JSON-LD/YAML-LD is the standard *name* for the model Atlas already implements, so
this adapter is a near 1:1 projection, not a translation.

### The type-ref meta-model generates the @context

This is the elegant part. A type-ref like
`{:type-ref/source :atlas/interface-endpoint, :type-ref/property :interface-endpoint/deps,
:type-ref/datalog-verb :entity/depends}` becomes a `@context` entry:

```yaml
interface-endpoint:deps:
  "@id": entity:depends     # the semantic predicate
  "@type": "@id"            # values are resources, not strings
  "@container": "@set"
```

Because multiple properties share a verb, they **unify to one predicate**:
`execution-function/deps` and `interface-endpoint/deps` both expand to
`entity:depends` in the resulting RDF. The same meta-model that drives
the Overarch adapter's edges drives the JSON-LD context here.

## Mapping decisions

- **Aspects** become values of a dedicated `atlas:aspect` predicate (each an
  `@id` resource), *not* extra `@type` values — this preserves Atlas's
  type-vs-aspect distinction and round-trips cleanly. (`:atlas/*` meta-aspects
  are dropped.)
- **Entity type** is the single `@type`.
- **Predicates** are the datalog verbs (`entity:depends/consumes/produces`), so
  the RDF is semantically deduplicated across property names.
- **CURIEs**: every keyword namespace becomes a `@context` prefix under a base
  IRI (default `https://semantic-namespace.org/atlas/`), so `:domain/auth` →
  `domain:auth`.

## API

```clojure
(require '[atlas.adapter.yaml-ld :as yld]
         '[clojure.edn :as edn])

(def reg (edn/read-string (slurp "snapshot.edn")))   ; a registry map

;; Pure projection → {"@context" {…} "@graph" [nodes…]}
(yld/registry->jsonld reg {:select (yld/aspect-selector :domain/orders)})

;; Write <path>.jsonld + <path>.yaml
(yld/emit! reg "/tmp/orders"
  {:select (yld/aspect-selector :domain/orders
             #{:atlas/execution-function :atlas/interface-endpoint})})
;; => {:nodes N :namespaces M :context-terms K}
```

Selectors mirror the other adapters: `aspect-selector` (one aspect, optional
type set) and `aspects-selector` (any of several aspects). `:base` overrides the
base IRI. Serializers: `->jsonld` and `->yaml`.

Note: JSON is a subset of YAML, so the emitted `.jsonld` is *itself* valid
YAML-LD; `->yaml` additionally renders the friendlier YAML block form (with the
`@`-keywords quoted, as YAML requires).

## Validation

The prototype was verified end-to-end against a real production registry
(~1000 entities), sliced to one domain's functions + endpoints (21 nodes,
50 namespace prefixes, 65 context terms):

- **round-trip**: `yaml.safe_load(out.yaml) == json.load(out.jsonld)` ✓
- **RDF expansion** (via `rdflib`): the JSON-LD expands to **610 valid RDF
  triples**, with predicates `atlas/aspect`, `entity/depends` (57 triples),
  `entity/consumes`, `entity/produces`, and `rdf:type` — confirming the
  type-ref-generated `@context` resolves correctly and the verb unification
  works (e.g. `execution-function/deps` and `interface-endpoint/deps` both →
  `entity:depends`).

Example node (YAML-LD):

```yaml
- "@id": interface-endpoint:place-order
  "@type": atlas:interface-endpoint
  atlas:aspect:
    - domain:orders
    - tier:api
  entity:depends:
    - execution-function:create-order
```

A full worked example (`.jsonld` + `.yaml`) built against a real production
registry is kept in the downstream consumer's own repo — out of this repo
because it embeds that project's entity data.

## Known limitations / next steps

1. **Export only.** Import (YAML-LD → Atlas) must infer compound identity from
   `@type` + `atlas:aspect`; deferred.
2. **Verb-unified predicates lose the property name** (`endpoint/deps` vs
   `endpoint/serialisation` both become `entity:depends`). If per-property
   predicates are wanted, map each property term to its own IRI instead of the
   verb.
3. **Non-edge scalar properties are omitted** for now (the projection is
   graph-focused: id, type, aspects, edges). Adding literals (labels, mcp-tool
   descriptions) is straightforward.
4. **Vocabulary reuse** — the biggest opportunity: map Atlas's history/provenance
   layer to **PROV-O**, metadata to schema.org/Dublin Core, instead of minting
   atlas-only IRIs. This is where linked-data interop pays off.
5. **Own module** — like the other adapters, a candidate to live outside `core`
   (it pulls `data.json`, and a real YAML lib if the hand-rolled emitter is
   replaced).
