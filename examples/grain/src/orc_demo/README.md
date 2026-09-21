# Try Atlas on your ORC workflows — 5 minutes

Everything below runs against **your** ORC checkout and **your** workflows.
No migration, no rewrite: Atlas ingests the data `dsl/workflow` already
returns.

## 1. Get the code on your classpath

From this repo, the only namespaces you need are `orc_demo/behavior_tree.clj`
(the whole ORC ontology: 3 type-refs + 1 invariant) and
`orc_demo/contract_check.clj` (the ingest + CI gate). Add to your `deps.edn`:

```clojure
{:deps {io.github.semantic-namespace/atlas {:local/root "../atlas/core"}}
 :paths ["src" "../atlas/examples/grain/src"]}
```

(or copy the two files — they are ~180 lines total and depend only on
`atlas` core.)

## 2. Extract a workflow you already have

In your ORC REPL, eval any workflow definition — the gate consumes ORC's
native data shape directly:

```clojure
(require '[ai.obney.orc.evaluation.sheets :as sheets])
(def wf (sheets/evaluation-suite))   ; => {:name ".." :blackboard [..] :root {..}}
(spit "/tmp/my-workflow.edn" (pr-str wf))
```

## 3. Run the gate

```clojure
(require '[orc-demo.contract-check :as cc])

(cc/check-tree (cc/orc->ingest (clojure.edn/read-string (slurp "/tmp/my-workflow.edn"))))
;; => nil                                    ; contract-complete
;; => {:violation :dead-read
;;     :details [{:node .. :reads-unproduced :bb/..}]}   ; pinpointed bug
```

This is the **ordering-aware** data-flow check your sheet validator stubs out
(`check-data-flow`: *"full data flow analysis would be more complex"*): reads
must be produced *before* they run; parallel siblings can't see each other's
writes; map-each binds its item key; read-modify-write is allowed. It caught a
step-reorder injected into the real 25-node `csv-to-ontology` that
blackboard-coverage cannot see. In CI it's one line per workflow:

```clojure
(deftest my-workflow-contract
  (is (nil? (cc/check-tree (cc/orc->ingest (my-workflow))))))
```

## 4. Three queries your two stores can't answer

Ingest a couple of workflows plus your failure concepts into one registry
(see `evaluation.clj` for the pattern — concepts are
`:atlas/risk-failure-mode` entities, `:concept/broader` builds the hierarchy,
and the enrichment overlay links nodes to the failures they guard):

```clojure
;; who guards hallucination, across ALL workflows?
[?n :node/mitigates :failure.orc/hallucination]

;; every node guarding ANY failure under output-quality (structure ⋈ knowledge):
[?n :node/mitigates ?f] [?f :concept/broader :failure.orc/output-quality]

;; which workflows share this subbehavior?
[?w :entity/depends :bt.eval/run-judges]
```

These are the point: structure and knowledge in **one graph**, cross-workflow.
The gate is just the on-ramp.

## Where to read more

- `docs/adapter-orc.md` — the full argument, evidence tables, honest
  boundaries.
- `docs/ontology-extensibility.md` — why none of this needed changes to Atlas
  core (and none would need your PRs merged either).
