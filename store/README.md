# atlas-store

Track how a semantic registry changes over time, against whatever storage you
already trust.

```clojure
(atlas.store/write-if-changed!
  (github/github-store {:repo "org/registry" :token tok :prefix "org/project/"})
  @registry/registry
  {:message (str "registry: " sha)})
;; => {:changed? true :ref "0a452b07" :entities 1083 :files 20}
```

No server required. A registry is a plain map, so this library does not depend
on atlas core — any application can snapshot its own registry.

## Why the serialisation is not just `pr-str`

`pr-str` emits maps and sets in hash order, so the *same* registry serialises
differently on each run. That defeats change detection by content, delta
compression in a git-backed store, and reviewable diffs, all at once.

Everything in `atlas.store.canonical` exists to make the bytes a function of the
data: sorted keys, sorted set members, one entity per file group, one prop per
line.

## Layout, chosen by measurement

Measured over 8 consecutive real registry versions of a ~1100-entity codebase:

| layout | base | per change |
|---|---|---|
| single 700KB line (naive) | — | 4.9 KB |
| one file, one entity per line | 137.1 KB | 2.6 KB |
| **one file per entity type, pretty** | **143.2 KB** | **2.7 KB** |
| one file per entity | 482.7 KB | 17.0 KB |

Per-entity files lose cross-entity redundancy — a thousand blobs each compress
alone — which costs far more than the finer deltas gain.

## Two guards, both earned

**`:max-shrink`** — building a registry means loading namespaces in the right
order, and getting it subtly wrong produces a well-formed registry that is
merely incomplete. One real attempt yielded 614 of 1101 entities and looked
healthy. Written, that reads as a mass architectural deletion. Writes that lose
more than 10% of entities are refused; pass `:force? true` for a genuine one.

**`assert-reproducible!`** — a registry that embeds timestamps or random ids is
not a function of its source, so every snapshot differs and no-op detection
never fires. Build twice, compare, fail in CI rather than a month later in a
baffling diff. (`:test-case/fixture` is excluded by default for exactly this
reason; the better fix is a deterministic fixture at source.)

## Stores

- `atlas.store.file/file-store` — a directory. Zero infrastructure.
- `atlas.store.github/github-store` — one commit per version in a private repo,
  via the Git Data API.

Implement `atlas.store.protocol/Store` for anything else. Writes must be atomic
where the backend allows, must replace rather than merge within their prefix,
and must leave everything outside that prefix untouched.

### GitHub notes

Uses the Git Data API (blob → tree → commit → ref), not the Contents API: that
endpoint writes one file per call, so a 20-file version would land as 20 commits
with no ref meaning "complete", and it is documented only to ~1MB — a real
snapshot measured ~937KB base64 and grows with the codebase.

The Git Data API returns **409 against a repository with no commits**. Seed one
first (a README via the Contents API).

Ref updates use `force: false`, so a concurrent writer gets a 422 rather than
silently winning.
