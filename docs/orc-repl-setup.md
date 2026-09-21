# ORC REPL Setup — running `:ai` leaves from Atlas

> A runbook for getting a local ORC nREPL to the point where an `:executor :ai`
> behaviour-tree leaf makes a real Claude call. Every trap below was hit for
> real; the error-decoder table is the most useful part.
>
> Recipe context: `adapter-orc-getting-started.md`.

## 0. The API key — read this first

**Never paste the key into a chat, an issue, a commit, or a REPL eval.** Anything
pasted into a conversation persists in transcripts, logs, and backups. If it has
been pasted anywhere, treat it as burned: rotate it in the Anthropic Console and
issue a fresh one. A leaked key is billable by whoever finds it.

The key reaches ORC **only** as an environment variable, set by you, in your own
shell. Nothing in this repo should ever read it from a file, a config, or an
argument.

## 1. Start the ORC nREPL — export *before* launch

**Env vars are fixed at JVM launch.** A running JVM cannot pick up an export made
afterwards, and a relaunch started from a *different* shell (or by a process
manager) will not inherit yours. This is the single most common failure here — it
cost two rounds in the session that produced this doc.

```bash
# 1. export in THIS shell (the one that will launch the REPL)
export ANTHROPIC_API_KEY='sk-ant-...'

# 2. verify it's actually set — prints y/n, never the key
[ -n "$ANTHROPIC_API_KEY" ] && echo "set" || echo "NOT set"

# 3. make sure nothing else already holds :7898 (a stale listener looks like
#    a successful relaunch but has the OLD environment)
lsof -ti tcp:7898 | xargs -r kill

# 4. launch from this same shell
cd ~/git/orc && clojure -Sdeps '{:deps {nrepl/nrepl {:mvn/version "1.3.0"}}}' \
  -M:dev -m nrepl.cmdline --port 7898
```

Confirm the JVM actually sees it — **presence only, never the value**:

```clojure
(println :key-present? (boolean (System/getenv "ANTHROPIC_API_KEY")))
;; => :key-present? true
```

If that prints `false`, stop. No amount of REPL work will fix it; go back to step 1.

## 2. Register the provider

`dscloj/quick-setup!` reads `ANTHROPIC_API_KEY` from the environment **at call
time** (`litellm.router:229`) and registers the `:anthropic` provider.

```clojure
(require '[dscloj.core :as dscloj])
(dscloj/quick-setup!)
```

> `(dscloj/list-providers)` returns the provider *types* litellm supports — it
> lists `:anthropic` whether or not a key was found. It is **not** proof of a
> working credential. Trust the `getenv` check above instead.

## 3. Build an execution context

ORC's own `create-test-context` does not work here, for two independent reasons.

**(a) It is LMDB-backed.** `kv-store-lmdb`'s native library needs glibc 2.36+;
older hosts fail with `UnsatisfiedLinkError: … GLIBC_ABI_DT_RELR not found`.
Substitute the atom-backed `MemKV` (the same one
`examples/grain/src/grain_demo/mem_kv.clj` uses for the grain demo).

**(b) It starts no pubsub or todo-processors.** Only
`create-async-test-context` does. Without them a tick is *recorded as an event
and never consumed* — you get a silent `{:status :timeout}` after the full
timeout budget, with no error. Sixteen processors must be running
(`execute-leaf-node`, `execute-composite-node`, `update-blackboard`, …).

```clojure
(require '[ai.obney.grain.kv-store.interface.protocol :as p]
         '[ai.obney.grain.kv-store.interface :as kv]
         '[ai.obney.grain.event-store-v3.interface :as es]
         '[ai.obney.grain.command-processor-v2.interface :as cp]
         '[ai.obney.grain.query-processor.interface :as qp]
         '[ai.obney.grain.read-model-processor-v2.interface :as rmp]
         '[ai.obney.grain.pubsub.interface :as pubsub]
         '[ai.obney.grain.todo-processor-v2.interface :as tp]
         '[ai.obney.orc.orc-service.core.dsl :as dsl]
         '[ai.obney.orc.orc-service.interface :as sheet])

(defrecord MemKV [store]
  p/KVStore
  (start [this] this)
  (stop [this] this)
  (get! [_ {:keys [k]}] (get @store (if (bytes? k) (vec k) k)))
  (put! [_ {:keys [k v]}] (swap! store assoc (if (bytes? k) (vec k) k) v) nil)
  (put-batch! [_ {:keys [entries]}]
    (swap! store (fn [m] (reduce (fn [m {:keys [k v]}]
                                   (assoc m (if (bytes? k) (vec k) k) v)) m entries)))
    nil))

(rmp/l1-clear!)
(def ps (pubsub/start {:type :core-async :topic-fn :event/type}))
(def event-store (es/start {:conn {:type :in-memory} :event-pubsub ps :logger nil}))
(def base-ctx {:event-store event-store
               :cache (kv/start (->MemKV (atom {})))
               :tenant-id #uuid "00000000-0000-0000-0000-000000000000"
               :command-registry (cp/global-command-registry)
               :query-registry (qp/global-query-registry)
               :dscloj-provider :anthropic})          ; <-- required for :ai leaves
(def processors
  (reduce-kv (fn [acc n {:keys [handler-fn topics]}]
               (assoc acc n (tp/start {:event-pubsub ps :topics topics
                                       :handler-fn handler-fn :context base-ctx})))
             {} @tp/processor-registry*))
(def actx (assoc base-ctx :event-pubsub ps :processors processors))

(println :processors (count processors))   ; => 16
```

## 4. An `:ai` leaf

```clojure
(def wf
  {:workflow-name "demo/judge"                 ; qualify it — bare names collide
   :blackboard-schema {:input :any :verdict :any}
   :root-node {:node-type :leaf :name "judge" :executor :ai
               :model "claude-haiku-4-5"
               :instruction "Answer with exactly one lowercase word: yes or no."
               :reads [:input] :writes [:verdict]}})

(def sheet-id (dsl/build-workflow! actx wf))
(sheet/execute actx sheet-id {:input "is 2 greater than 1?"} :timeout-ms 45000)
;; => {:status :success, :outputs {:verdict "yes"}, ...}
```

**Model IDs pass straight through.** The vendored litellm's `register-anthropic!`
still defaults to the ancient `claude-3-sonnet-20240229`, but a current ID like
`claude-haiku-4-5` is forwarded to `api.anthropic.com` unchanged — no
model-registry rejection. (Confirmed empirically: a request with a current ID
reached Anthropic and returned a *billing* error, which only an accepted,
forwarded request can produce.) Use the cheapest model that fits — a
closed-alphabet verdict over a small payload is a Haiku job.

## 5. Error decoder

Three auth-ish failures that look similar and mean completely different things:

| Error | Means | Fix |
|---|---|---|
| `Anthropic API key not provided and ANTHROPIC_API_KEY env var not set` | The JVM's environment has no key. `quick-setup!` registered nothing. | §1 — export, kill the stale listener, relaunch from that shell |
| `Your credit balance is too low to access the Anthropic API` | The key is **valid and authenticated**; the request reached Anthropic. Account has no credits. | Add credits. Nothing to change in code — this error *proves* the plumbing works end to end |
| `API key is invalid.` | The key was rejected. Usually the JVM holds a **rotated/revoked** value — common right after you issue a fresh key but don't relaunch | Relaunch `:7898` with the current key exported |

Other traps that are *not* auth:

| Symptom | Cause |
|---|---|
| `{:status :timeout}` with no error, after the full budget | No todo-processors running — §3(b) |
| `Cannot invoke "Object.getClass()" because "x" is null` | A node read a blackboard key that was never declared. Usually ORC's own schema inference: `(number? v) → :number` is **not valid Malli**, so `:sheet/declare-key` silently rejects it and the failure surfaces three steps later. Pass `:blackboard-schemas {:k :int}` explicitly, or declare `:any` |
| `Unknown tree node type: sheet/fallback` | You're on the **ephemeral** executor (`execute-tree`), which has no `fallback`/`condition`. Use Path A (`build-workflow!` + `execute`) |
| `{:fn [should be a string]}` | Path A rejects inline closures — `:fn` must be a fully-qualified symbol string (`"user/my-fn"`) |
| `Unknown blackboard keys in reads: [...]` | Path A needs an explicit `:blackboard-schema`; it has no tick-time inputs to infer from |

## 6. Reading results back

Usage and traces are event-sourced — query them rather than trusting stdout:

```clojure
(def evs (into [] (es/read event-store {:tenant-id (:tenant-id actx)
                                        :tags #{[:sheet sheet-id]}})))
(doseq [e evs :when (:usage e)]
  (println (:event/type e) (:usage e)))
;; => :sheet/node-execution-completed {:prompt-tokens 418, :completion-tokens 20, :total-tokens 438}
```

A real API call shows `:duration-ms` around 1000 — a local function returns
sub-millisecond. That's a cheap way to tell a genuine call from a stub.

## 7. Cost

A verdict over a small payload is ~438 tokens ≈ **$0.0005** on Haiku 4.5
($1/$5 per 1M in/out). Cheap enough for CI; not free — don't loop it in a test
suite without intent.
