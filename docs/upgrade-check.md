# Upgrade check: what a new atlas version changes for your project

> **Experimental** — see [EXPERIMENTAL.md](../EXPERIMENTAL.md). Task:
> `bb upgrade-check`; scripts `bb/upgrade_check.clj`, `bb/upgrade_runner.clj`.

atlas's own tests show that atlas works. They can't show whether *your*
registry still comes out the same, or whether the atlas tools still give the
same answers on it. `bb upgrade-check` answers that for one project and two
atlas versions:

1. **Builds the project's registry with each version**, each in a fresh JVM:
   two versions of a library can't share one.
2. **Compares the two registries entity by entity**: dev-ids only on one side,
   changed compound identities, and changed props by key. A prop that differs
   only in values the build makes fresh each time (UUIDs, instants, long digit
   runs inside strings and symbols, such as a nanosecond timestamp in an id)
   is listed as **volatile**, not counted as a change.
3. **Calls the read-only `atlas.llm-ide` tools on each registry** (blast
   radius, consumers, bottlenecks, islands, …) and compares the answers tool
   by tool, with the time each took.

The tools' arguments are chosen from the first registry: its best-connected
functions, its most used data keys and aspects. The same arguments are then
given to the second side, so both answer the same questions.

```
bb upgrade-check --build app.registry/build! \
                 --from current --to mvn:0.1.15 \
                 --paths src,resources,test
```

| Option | |
|---|---|
| `--build` | How the project builds its registry: a qualified fn of no args, or a `.clj` file to load. It must not start services. |
| `--project` | The project directory (default `.`). |
| `--from`, `--to` | `current` (the project's own deps, the default), `mvn:<version>`, `local:<path to atlas core>`, or `git:<sha>`. |
| `--paths` | The `:paths` the registry build needs, comma separated. Registries often register entities from `test/` or `dev/` too. |
| `--probes` | A file of `[[tool args] …]` to call instead of the chosen probes. |
| `--out` | Where both sides and `report.edn` are written (default: a temp dir). |

From another repo: `bb --config <atlas checkout>/bb.edn upgrade-check …`.

The exit code is **0** when the registry and every answer are identical, **1**
when anything differs, and **3** when a side fails to run (its `run.log` is in
`--out`). In a project's CI, run it on the PR that bumps atlas.

## Reading the result

A difference is not necessarily a regression. On a production app (1,348
entities), moving from atlas `0.1.13` to the `main` that unified the
dependency-edge definition gave:

- **the registry: no changes.** Five test cases came out volatile: their
  fixtures carry a fresh UUID, the time or a nanosecond id on every build.
- **14 of 26 tool answers identical, 12 different.**
- **all probes together about 6× faster** (13.8s → 2.4s; `bottlenecks` and
  `islands` 4.6s → under 25 ms).

Checked against the registry data, the new answers were the correct ones:

- the old top "bottleneck" had 219 incoming edges, while only 2 entities name it;
- the old `aspect-anomalies` reported 0 sparse aspects, while 311 are used by
  exactly one entity;
- the new blast radius also counts the test cases that target a function.

So the report tells you *what* changed. Whether each change is a fix, a policy
change or a regression is for the person bumping atlas to say in that PR.

## Limits

- **A hermetic build.** The build must give the same registry on every run:
  no services, no network, a fixed environment. Values it makes fresh are only
  recognised by shape (UUIDs, instants, long digit runs). Any other fresh value,
  a random word for instance, shows up as a change.
- **Only the `atlas.llm-ide` tools are probed.** Code calling atlas functions
  directly isn't, unless you list it with `--probes` as a tool call.
- **The runner can only use atlas APIs present in both versions.**
  `bb/upgrade_runner.clj` uses only `atlas.registry/registry`,
  `atlas.registry.serialise/sanitize` and `atlas.registry.lookup/handle-tool`,
  which atlas has had since 0.1.13.
- **The two sides run one after the other**, because the second needs the
  probe arguments the first chose.
