# Compound Identity Refactoring Guide

A prompt and methodology for reviewing and improving Atlas compound-ids to avoid semantic drift. Use this as context for an LLM-assisted refactoring session.

---

## What This Guide Is For

When entities are first migrated into Atlas, compound-ids are often assigned quickly. Over time this creates **semantic drift**: aspects that conflict in naming, redundant dimensions, sparse identities that don't capture enough meaning, or namespace conventions that clash with other conventions. This guide provides the process for a systematic second pass.

---

## LLM Session Setup

Paste this as context at the start of a refactoring session:

```
I am reviewing Atlas registry compound-ids for semantic drift.
Atlas uses compound identities — sets of qualified keywords — as the
semantic identity of every entity. Each identity has exactly one entity
type (e.g. :atlas/execution-function) and zero or more aspect keywords
(e.g. :domain/auth, :tier/service, :action/create).

My registry is loaded in a running REPL. I want to:
1. Identify aspect vocabulary problems (inconsistencies, redundancy, overloading)
2. Propose aspect renames safe for LSP global rename
3. Propose per-entity additions/removals
4. Generate a new EDN and validate with compound-id-changed?

The REPL tool is: clj-nrepl-eval -p <port> "<code>"
```

---

## Step-by-Step Process

### 1. Load and orient

```clojure
;; Discover REPL
clj-nrepl-eval --discover-ports

;; Load registry
(dev/load-registry-snapshot! "/path/to/snapshot.edn")
(count @registry/registry)

;; Overview by entity type
(->> @registry/registry
     (group-by (fn [[_k v]] (:atlas/type v)))
     (map (fn [[t es]] [t (count es)]))
     (sort-by second >))

;; Aspect namespace frequency
(->> @registry/registry
     keys
     (mapcat identity)
     (map namespace)
     frequencies
     (sort-by val >))
```

### 2. Read the docs

Before proposing anything, read:
- `docs/concepts.md` — the intended aspect vocabulary (tier/, operation/, effect/, data/, etc.)
- `docs/examples.md` — annotated patterns showing correct usage

This grounds proposals in the project's conventions rather than generic opinions.

### 3. Identify drift categories

Run these queries to surface problems:

```clojure
;; Sparse compound-ids (entity-type + 1 aspect only — under-specified)
(->> @registry/registry
     (filter (fn [[k _]] (= (count k) 2)))
     (map (fn [[k v]] {:compound-id k :dev-id (:atlas/dev-id v) :type (:atlas/type v)})))

;; All aspect values per namespace — spot overloading and inconsistency
(->> @registry/registry keys
     (mapcat identity)
     (filter #(= "your-namespace" (namespace %)))
     frequencies
     (sort-by val >))

;; Aspect co-occurrence — find always-paired aspects (candidates for redundancy)
;; e.g. if :domain/metrics always appears with :metrics/counter, one is redundant
(->> @registry/registry
     (filter (fn [[k _]] (contains? k :aspect/to-check)))
     (map (fn [[k v]] {:has-paired-aspect (contains? k :other/aspect)
                       :dev-id (:atlas/dev-id v)}))
     (group-by :has-paired-aspect))
```

### 4. Classify problems

| Category | Signal | Action |
|----------|--------|--------|
| **Namespace conflict** | Aspect namespace same as dev-id namespace (e.g. `serial/collection` when `serial/` is a dev-id prefix) | Rename aspect to a neutral namespace |
| **Redundant aspect** | Always co-occurs with another aspect and adds no query value | Remove globally |
| **Inconsistency** | Same concept named differently in different places (e.g. `entity/counter` vs `metrics/counter`) | Rename to the dominant form |
| **Overloaded domain** | Domain keyword encodes a sub-concept (e.g. `domain/mailing-lists-stats`) | Decompose into `domain/X` + qualifier |
| **Sparse identity** | Too few aspects to distinguish entity from similar ones | Add missing dimensions (tier, action, cardinality) |
| **Semantically empty** | Aspect conveys no information (e.g. `filter/none`) | Remove |
| **Too specific** | Aspect value is a one-off business state unlikely to be reused or queried | Generalise or remove |
| **Missing cardinality** | Entity represents a collection but nothing in the identity says so | Add `cardinality/many` |

### 5. Separate renames from targeted changes

**LSP-safe global renames** (same keyword, new name, apply everywhere):
- Any aspect whose namespace conflicts with another convention
- Any aspect that is inconsistently named relative to a dominant form

**Targeted per-entity changes** (add/remove for specific dev-ids):
- Adding `cardinality/many` to collection entities that didn't have `serial/collection`
- Decomposing overloaded domains
- Removing entity-specific semantically empty aspects

Always list the renames first — they reduce the surface of targeted changes.

### 6. Check for collisions before writing

Before generating the new EDN, verify that no two old compound-ids produce the same new compound-id:

```clojure
(def transformed-pairs
  (->> @registry/registry
       (remove (fn [[_k v]] (entities-to-remove (:atlas/dev-id v))))
       (map (fn [[k v]] {:new-id (transform-id k v)
                         :old-id k
                         :dev-id (:atlas/dev-id v)}))))

(->> transformed-pairs
     (group-by :new-id)
     (filter (fn [[_ es]] (> (count es) 1)))
     vals)
```

A collision means either:
- Two entities that were semantically distinct are now identical → they may be genuine duplicates exposed by the refactor
- The proposed rename is too aggressive → the old aspect was carrying a real distinction

Resolve each collision explicitly before writing the EDN.

### 7. Apply transformations and write EDN

```clojure
(defn transform-id [compound-id value]
  (let [dev-id (:atlas/dev-id value)
        ;; 1. Global renames
        step1 (reduce (fn [id [old new]]
                        (if (contains? id old)
                          (-> id (disj old) (conj new)) id))
                      compound-id renames)
        ;; 2. Conditional removals (e.g. redundant aspects)
        step2 (cond-> step1
                (and (contains? step1 :redundant/aspect)
                     (contains? step1 :real/aspect))
                (disj :redundant/aspect))
        ;; 3. Unconditional removals
        step3 (disj step2 :semantically/empty)
        ;; 4. Domain decompositions
        step4 (if (contains? step3 :domain/overloaded-name)
                (-> step3
                    (disj :domain/overloaded-name)
                    (conj :domain/actual-domain)
                    (conj :qualifier/concept))
                step3)
        ;; 5. Per-entity targeted changes
        step5 (case dev-id
                :my/entity (-> step4 (conj :cardinality/many) (disj :old/aspect))
                step4)]
    step5))

(def new-registry
  (->> @registry/registry
       (remove (fn [[_k v]] (entities-to-remove (:atlas/dev-id v))))
       (map (fn [[k v]] [(transform-id k v) v]))
       (into {})))

(spit "path/to/registry-v2.edn"
      (with-out-str (clojure.pprint/pprint new-registry)))
```

### 8. Validate with compound-id-changed?

```clojure
(def old-registry @registry/registry)

(def changes
  (->> new-registry
       (keep (fn [[new-cid value]]
               (let [dev-id (:atlas/dev-id value)
                     old-entry (first (filter (fn [[_k v]] (= (:atlas/dev-id v) dev-id))
                                              old-registry))
                     old-cid (first old-entry)]
                 (when (and old-cid (not= old-cid new-cid))
                   {:dev-id dev-id
                    :old    old-cid
                    :new    new-cid
                    :added  (clojure.set/difference new-cid old-cid)
                    :removed (clojure.set/difference old-cid new-cid)}))))
       (sort-by (comp str :dev-id))))

(println "Changed:" (count changes))
(doseq [c changes]
  (println (:dev-id c))
  (println " - removed:" (:removed c))
  (println " + added:  " (:added c)))
```

Review the output: global renames will appear across all entity types, not just the one you were focusing on. This is expected and correct — it confirms the rename propagated consistently.

---

## Aspect Vocabulary Reference

These are the conventions established for this project. Add to this list as new dimensions are introduced.

| Namespace | Purpose | Examples |
|-----------|---------|---------|
| `domain/` | Business domain | `:domain/auth`, `:domain/payments` |
| `tier/` | Architectural layer | `:tier/foundation`, `:tier/service`, `:tier/api` |
| `action/` | Operation performed | `:action/search`, `:action/create`, `:action/retry` |
| `entity/` | Shape of data | `:entity/token`, `:entity/config`, `:entity/signature` |
| `cardinality/` | Collection vs single | `:cardinality/many` |
| `scope/` | Scope of the operation target | `:scope/inbox`, `:scope/user`, `:scope/stream` |
| `services/` | External service dependency | `:services/stripe`, `:services/microsoft` |
| `representation/` | Wire format | `:representation/csv`, `:representation/raw` |
| `filter/` | Query filter qualifier | `:filter/pending` |
| `metrics/` | Measurement type | `:metrics/counter`, `:metrics/stats` |
| `temporal/` | Time dimension | `:temporal/historical`, `:temporal/recurring` |
| `output/` | Result of operation | `:output/success`, `:output/error` |
| `intent/` | Tooling/meta purpose | `:intent/query`, `:intent/diagnose` |
| `env/` | Environment | `:env/test` |
| `debt/` | Technical debt signal | `:debt/remove`, `:debt/refactor`, `:debt/review` |

---

## Common Pitfalls

**Aspect namespace same as dev-id namespace.** If your dev-ids use `serial/` as a prefix, then having `:serial/collection` as an aspect conflates two different naming systems. Always check that aspect namespaces don't overlap with dev-id namespaces.

**Two aspects, one concept.** `domain/metrics` always appearing alongside `metrics/counter` means one is noise. The more specific namespace wins; remove the general one.

**Collision reveals a duplicate.** When a proposed rename causes two compound-ids to merge, stop. This is the refactor telling you that two entries in the registry may actually be the same entity. Investigate before resolving the collision with a workaround.

**Global renames propagate to entity types not in scope.** If you're refactoring serialisation entities, a global rename of `serial/collection → cardinality/many` will also affect cache and endpoint entities that shared the same aspect. This is correct behaviour — consistency is the goal — but review the full change list before committing.

**Don't confuse `scope/` with `entity/`.** `scope/token` (the OAuth token scope configuration) and `entity/token` (the token data structure) are different. Renaming one to the other can silently merge semantically distinct entities.

---

## Technical Debt Aspects

Aspects in the `debt/` namespace mark entities with known issues, without removing them from the registry. This keeps the debt visible and queryable.

```clojure
:debt/remove    ; entity is scheduled for removal
:debt/refactor  ; compound-id or structure needs rework
:debt/review    ; semantics are unclear, needs a decision
:debt/migrate   ; needs migration to a newer pattern
```

Query your debt:
```clojure
(query/find-by-aspect @registry/registry :debt/remove)
(query/find-by-aspect @registry/registry :debt/refactor)
```

This is preferable to removing the entity speculatively — it preserves the historical record and makes the debt visible in architectural tooling.
