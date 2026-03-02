# Integration: `js-qualified-keywords`

This doc outlines the planned adaptation to make the Atlas JS API
interoperate with [`js-qualified-keywords`](https://github.com/semantic-namespace/js-qualified-keywords).

## Goal

Replace plain strings (`'domain/auth'`) with first-class `Keyword`
instances at the JS boundary, both for **input** and **output**.

```js
// Before
register('fn/validate-token', 'atlas/execution-function',
         ['domain/auth', 'tier/service'], {...})

findByAspect('domain/auth')

// After (with js-qualified-keywords)
register(kw('fn', 'validate-token'), kw('atlas', 'execution-function'),
         [kw('domain', 'auth'), kw('tier', 'service')], {...})

findByAspect(kw('domain', 'auth'))

// Or, with the Babel plugin
findByAspect(:domain/auth)
```

## Changes Required

### 1. `package.json`

Add `js-qualified-keywords` as a dependency:

```json
"dependencies": {
  "js-qualified-keywords": "^0.1.1",
  "shadow-cljs": "^2.27.5"
}
```

### 2. `core.cljs` / `core-slim.cljs`

#### Add require

```clojure
["js-qualified-keywords" :refer [Keyword kw]]
```

#### New helper: `->clj-kw`

Replaces bare `(keyword x)` calls on user-provided arguments so that
`Keyword` instances, strings, and CLJ keywords are all accepted:

```clojure
(defn- ->clj-kw [x]
  (cond
    (keyword? x)      x
    (instance? Keyword x) (keyword (.-fqn x))  ; fqn = "ns/name"
    (string? x)       (keyword x)
    :else             (keyword (str x))))
```

#### Rewrite `to-js`

Return `Keyword` instances for keyword values; keep `"ns/name"` strings
as JS object keys (plain objects need string keys):

```clojure
(defn to-js [x]
  (cond
    (nil? x)        nil
    (boolean? x)    x
    (number? x)     x
    (string? x)     x
    (keyword? x)    (kw (namespace x) (name x))
    (map? x)        (let [obj (js-obj)]
                      (doseq [[k v] x]
                        (aset obj
                              (if (keyword? k)
                                (if (namespace k)
                                  (str (namespace k) "/" (name k))
                                  (name k))
                                (str k))
                              (to-js v)))
                      obj)
    (set? x)        (into-array (map to-js x))
    (sequential? x) (into-array (map to-js x))
    :else x))
```

#### Rewrite `to-clj`

Handle `Keyword` instances before falling back to `js->clj`:

```clojure
(defn to-clj [x]
  (cond
    (nil? x)            nil
    (boolean? x)        x
    (number? x)         x
    (string? x)         (if (re-find #"^[a-z].*/" x) (keyword x) x)
    (keyword? x)        x
    (instance? Keyword x) (->clj-kw x)
    (array? x)          (mapv to-clj (array-seq x))
    :else               ; plain JS object
    (into {}
      (map (fn [k] [(->clj-kw k) (to-clj (aget x k))])
           (array-seq (js/Object.keys x))))))
```

> **Why not `js->clj` + postwalk?** `js->clj` recurses into any JS
> object, including `Keyword` instances, and would corrupt them by
> converting their properties into a CLJS map. Explicit branching
> handles them before that happens.

#### Rewrite `js-set->clj-set`

Accept arrays containing `Keyword` instances, strings, or both:

```clojure
(defn- js-set->clj-set [js-val]
  (cond
    (set? js-val)             js-val
    (keyword? js-val)         #{js-val}
    (instance? Keyword js-val) #{(->clj-kw js-val)}
    (string? js-val)          #{(keyword js-val)}
    (array? js-val)           (into #{} (map ->clj-kw) (array-seq js-val))
    :else                     (into #{} (map ->clj-kw) (js->clj js-val))))
```

#### Update all user-arg keyword conversions

Every place that does `(keyword some-user-arg)` — i.e. `register!`,
`find-by-aspect`, `find-by-dev-id`, `find-dev-ids-with-aspect`,
`related-aspects`, `dependency-graph`, `by-tier`, `domain-coupling`,
`impact-of-change`, `find-producers`, `find-consumers`,
`trace-data-flow`, `ontology-for`, and all datalog functions — must
replace `(keyword x)` with `(->clj-kw x)`.

`(keyword mode)` for the `mode` argument of `match-score` can stay as
`(keyword mode)` since mode is always a literal string (`"and"`,
`"or"`, `"count"`).

## Output format after the change

| Value type        | Before         | After                     |
|-------------------|----------------|---------------------------|
| Qualified keyword | `"domain/auth"`| `Keyword { ns, name, fqn }`|
| Unqualified kw    | `"auth"`       | `Keyword { ns: null, name }`|
| Map key           | `"domain/auth"`| `"domain/auth"` (unchanged)|
| Compound-id array | `["atlas/execution-function", "domain/auth"]` | `[Keyword, Keyword]` |

Map keys remain plain strings so that standard JS object property
access (`entity['execution-function/context']`) continues to work.

## What stays the same

- `(keyword mode)` for literal string arguments
- JS object keys in returned entity maps (`"ns/name"` strings)
- Overall function signatures and return shapes
- EDN loading via `loadRegistry` / `load-registry!`
