# Using ORC with Atlas Definitions — Getting Started

> A practical recipe, not a design doc. For the *why* and the full debugging
> trail, see `adapter-orc.md` (read direction: mirroring ORC into Atlas),
> and `adapter-orc-execution-bridge.md` (Path B: ephemeral trees). This doc is
> the condensed "just show me how" version. The Path A write-up (persisted
> workflows) is not yet published.

## The mental model, in one paragraph

You author entities in the Atlas registry — the same `:atlas/execution-function`
+ `:execution-function/context`/`:response` primitive you'd use for any Atlas
entity, plus a handful of `:behavior-tree/*` aspects that give it tree shape
(`:sequence`, `:fallback`, `:parallel`, `:leaf`, …). A small reshaper walks
that tree and hands it to ORC in whichever native shape ORC expects. **ORC
executes it for real** — no mock, no reimplemented interpreter. Atlas never
runs anything itself; it's the source of truth, ORC is the engine.

## Step 0 — pick your lane

| You want... | Use | Gives you |
|---|---|---|
| A one-shot, throwaway tree (e.g. dynamically assembled at runtime) | **Path B** — `execute-tree` | Fast, no persistence, but **no `fallback`/`condition`** — `sequence`/`parallel`/`code`/`llm` only |
| A named, reusable, production-shaped workflow you'll invoke repeatedly | **Path A** — `build-workflow!` + `execute` | Deterministic identity, idempotent rebuilds, **full node vocabulary including `fallback`/`condition`** |

If you're not sure, default to Path A — it's what ORC's own real production
sheets (`evaluation-suite`, `csv-to-ontology-pipeline`) actually run through,
and it's the stricter, more production-honest contract.

## Step 1 — author the entities in Atlas

Ordinary Atlas registration, nothing ORC-specific yet:

```clojure
(registry/register!
 :retry/root :atlas/execution-function
 #{:behavior-tree/fallback :domain/resilience :workflow/retry-with-breaker}
 {:behavior-tree/inputs #{:bb/request}
  :behavior-tree/children [:retry/call-sequence :retry/open-circuit]})

(registry/register!
 :retry/call-sequence :atlas/execution-function
 #{:behavior-tree/sequence :domain/resilience :workflow/retry-with-breaker}
 {:behavior-tree/children [:retry/attempt-call :retry/check-success]})

(registry/register!
 :retry/attempt-call :atlas/execution-function
 #{:behavior-tree/leaf :behavior-tree/code :domain/resilience :operation/call}
 {:execution-function/context [:bb/request]
  :execution-function/response [:bb/response :bb/call-error]})

(registry/register!
 :retry/check-success :atlas/execution-function
 #{:behavior-tree/condition :domain/resilience}
 {:execution-function/context [:bb/call-error]
  :behavior-tree/check {:key :call-error :op :equals :value nil}})   ; Path A only — see Step 3

(registry/register!
 :retry/open-circuit :atlas/execution-function
 #{:behavior-tree/leaf :behavior-tree/code :domain/resilience :operation/fallback}
 {:execution-function/context [:bb/call-error]
  :execution-function/response [:bb/fallback-response :bb/circuit-state]})
```

**Watch for the aspect-collision trap**: if two leaves share an identical
aspect set (entity type + aspects), they silently collapse onto one registry
slot. It happened twice building the examples in the companion docs — always
give sibling leaves a distinguishing aspect (`:target/db`, `:operation/call`
vs `:operation/fallback`, etc.) if their other aspects would otherwise match.

Validate before going any further — `(inv/check-all)` should be clean
(`bt-contract-complete` in particular, which catches a node reading a
blackboard key nothing upstream ever writes).

## Step 2a — Path B: ephemeral, one call does it all

```clojure
;; the reshaper — ~15 lines, walks :behavior-tree/children into s-expressions
(defn bt->orc-sexpr [dev-id fn-table]
  (let [id (entity/identity-for dev-id), props (entity/props-for dev-id)
        kids (:behavior-tree/children props)
        reads (mapv #(keyword (name %)) (:execution-function/context props))
        writes (mapv #(keyword (name %)) (:execution-function/response props))]
    (cond
      (contains? id :behavior-tree/sequence)
      (apply list 'sheet/sequence (map #(bt->orc-sexpr % fn-table) kids))
      (contains? id :behavior-tree/parallel)
      (apply list 'sheet/parallel (map #(bt->orc-sexpr % fn-table) kids))
      (and (contains? id :behavior-tree/leaf) (contains? id :behavior-tree/code))
      (list 'sheet/code :reads reads :writes writes :fn (get fn-table dev-id))
      :else (throw (ex-info "unsupported (fallback/condition need Path A)" {:dev-id dev-id})))))

;; build a real, LMDB-free execution context — see adapter-orc-execution-bridge.md
;; for why (glibc) and the full mem-kv + async-processor setup
(def tree (bt->orc-sexpr :retry/attempt-call {:retry/attempt-call '(fn [{:keys [inputs]}] ...)}))
(rte/execute-tree (list 'sheet/sequence tree) ctx {:blackboard {:request "payment-gateway"}
                                                    :blackboard-schemas {:request :string}})
;; => {:status :success :outputs {:response "called-payment-gateway" ...}}
```

Note: `:retry/root` itself (the `fallback` node) **cannot** go through this
path — only its `sequence`/`leaf` sub-parts can. That's the signal you need
Path A.

## Step 2b — Path A: persist once, tick many times

```clojure
;; the reshaper targets ORC's plain-map DSL, not s-expressions, and every
;; node needs a :name (used for deterministic id derivation)
(defn bt->orc-workflow-node [dev-id fn-table]
  (let [id (entity/identity-for dev-id), props (entity/props-for dev-id)
        kids (:behavior-tree/children props), nm (name dev-id)
        reads (mapv #(keyword (name %)) (:execution-function/context props))
        writes (mapv #(keyword (name %)) (:execution-function/response props))]
    (cond
      (contains? id :behavior-tree/sequence)
      {:node-type :sequence :name nm :children (mapv #(bt->orc-workflow-node % fn-table) kids)}
      (contains? id :behavior-tree/fallback)
      {:node-type :fallback :name nm :children (mapv #(bt->orc-workflow-node % fn-table) kids)}
      (contains? id :behavior-tree/condition)
      {:node-type :condition :name nm :check (:behavior-tree/check props) :on-fail :failure}
      (and (contains? id :behavior-tree/leaf) (contains? id :behavior-tree/code))
      {:node-type :leaf :name nm :executor :code :fn (get fn-table dev-id) :reads reads :writes writes}
      :else (throw (ex-info "unsupported node kind" {:dev-id dev-id})))))

(defn bt->orc-workflow [root-dev-id fn-table]
  {:workflow-name (str (namespace root-dev-id) "/" (name root-dev-id))   ; qualify! bare "root" collides
   :blackboard-schema {:request :string :response :string :call-error :any
                        :fallback-response :keyword :circuit-state :keyword}
   :root-node (bt->orc-workflow-node root-dev-id fn-table)})

;; leaves need REAL named fns (fully-qualified symbol strings), not inline
;; closures -- a persisted sheet must survive a JVM restart
(defn attempt-call-fn [{:keys [inputs]}] {:response (str "called-" (:request inputs)) :call-error nil})
(defn open-circuit-fn [{:keys [inputs]}] {:fallback-response :cached :circuit-state :open})

(def workflow-def (bt->orc-workflow :retry/root
                     {:retry/attempt-call "user/attempt-call-fn"
                      :retry/open-circuit "user/open-circuit-fn"}))

(def sheet-id (dsl/build-workflow! ctx workflow-def))   ; deterministic, idempotent

(sheet/execute ctx sheet-id {:request "payment-gateway"})   ; tick #1
(sheet/execute ctx sheet-id {:request "inventory-service"}) ; tick #2, same sheet-id
```

## The gotcha checklist (all hit for real, all confirmed with evidence)

- **Aspect collision** — sibling leaves with identical aspect sets silently
  overwrite each other in the Atlas registry. Add a distinguishing aspect.
- **`glibc`/LMDB** (Path B only, if building your own test context) — ORC's
  default test context needs glibc 2.36+; older hosts need the atom-based
  `MemKV` substitute.
- **No processors running** (Path B only) — `create-test-context` alone
  starts no pubsub/todo-processors; use `create-async-test-context`'s pattern
  or ticks silently time out.
- **Path A rejects inline closures** — `:fn` must be a fully-qualified symbol
  string.
- **Path A requires an explicit `:blackboard-schema`** — it has no tick-time
  inputs to infer types from, unlike Path B.
- **`:workflow-name` must be qualified** (Path A) — bare node names like
  `"root"` collide across unrelated trees; use the full dev-id.

## What you get once it's running

Everything Atlas already gives you elsewhere in the registry, now backed by
real execution: `bt-contract-complete` catches dataflow bugs *before* you
ever hit ORC; `:node/mitigates` + `:concept/broader` let you query "every
node across every workflow guarding this failure mode" in one graph query,
something ORC's own split stores can't answer; and `cloud-diff`/`atlas-review`
give you a real semantic diff on tree changes instead of a raw text diff.
