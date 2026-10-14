# review

The [sdiff](https://github.com/semantic-namespace/diff) review server for a
project with an atlas registry. The report page of a pull request gets
derived decorations from the registry, beside each changed form and at the
top:

- the entity a form declares, with its contract and the delta between the
  recorded version at the PR's merge base and the registry CI built for its head;
- who depends on that entity, who consumes what it produces, which test
  cases cover it;
- the data keys the changed code mentions, with their producers and consumers;
- a header listing every entity the PR's registry diff touched, linked to the
  form that changed it, or marked when no form in the diff did;
- a risk ranking from the system graph: for each changed entity, whether its
  contract changed, how many entities depend on it, which entry points
  (endpoints, MCP tools, workflows, prompts) it reaches, which services it
  talks to, whether it writes, and whether any test case covers it or what
  depends on it. Each level shows the reasons that produced it.

All of it answers through `atlas.ide` with the stored version bound as the
registry. Inferred annotations, from a reviewer or a model, render apart and
labelled.

```
clojure -M:serve --store <registry store checkout> --prefix <path inside it> [--branch origin/main] [--repo owner/name] [--envs dev,staging,production] [--port 7878] [--host 127.0.0.1]
```

Two registries are compared. The candidate is the registry CI built for the
PR head, downloaded through `gh` from the workflow run's artifact
(`SDIFF_REGISTRY_WORKFLOW` and `SDIFF_REGISTRY_ARTIFACT` name them; defaults
`atlas-registry` and `registry`). When CI has no run for the head, the page
says so and shows entities as on main.

The base is a version the project's CI recorded in its registry store (the
git repository written by `atlas.store`, one commit per recorded registry,
the source sha in the commit message and a `Source-Sha:` trailer). The store
records only commits that changed the registry, and CI builds the candidate
from a merge of the PR into the line as it was then, so the base is the
newest version recorded at or before the commit the candidate was built on:

- The artifact may carry a sidecar, `atlas-registry.meta.edn`, beside
  `atlas-registry.edn`: `{:base-sha "<full sha>" :head-sha "<full sha>"}`,
  the base being the first parent of the merge commit CI built. The version
  built from that sha is used when the store has it; otherwise the newest
  version whose source is an ancestor of it (GitHub's compare, through `gh`);
  when ancestry cannot be read, the newest version the store recorded by that
  commit's time.
- Without the sidecar, the newest version the store recorded by the time the
  CI run started, and the header says the base was inferred.
- Without a candidate, the same rules over the PR's merge base.

The header says how the base was chosen: the PR's base itself, or that the
registry was unchanged from the chosen version to it. When the store has no
version that old, it compares with the oldest one and warns. When versions
were recorded on the line after the chosen one, the header marks the
candidate stale, counts them, and asks for CI to be re-run to see the PR
against the current line.

### Comparing against another store branch

A store can hold more than the line `--branch` names: a `deployed/<env>`
branch with one commit per deploy (subject `registry: <sha> deployed to <env>
from <git branch>`, trailer `Source-Sha: <full sha>`), for example. The
`against` request parameter picks one of them as the base:

```
/pr?ref=owner/repo#12&against=deployed/dev
```

From the MCP tools, pass `params` `"against=deployed/dev"`. The header and
the text output always name the base used.

- Absent, or naming the configured branch (`main` finds `origin/main`): the
  base is chosen as above.
- Another branch of the store: the base is the newest version on it, what that
  environment runs. The version on the configured
  branch, chosen as above, is read too, and the header shows the two steps,
  `<env> → merge base (merged, not yet there) → this PR`, each with its own
  new, changed and deleted entities. Per-form states and risk rows say which
  step a difference belongs to.
- The first step is called merged since the deploy only when the PR's base
  descends from what the environment runs: the environment's `Source-Sha`
  is the base version's commit or an ancestor of it, by the order of the
  line's versions when both are recorded there, else by GitHub's compare.
  An environment is often deployed from a branch that already holds newer
  line commits than the PR's base, and then the step runs backwards. The
  header then says `Differs between <env> and this PR's base: <env> runs
  <branch>; this PR is built on an older <line>` (or `on a <line> that
  diverged from it`, or just `Differs between <env> and this PR's base` when
  GitHub cannot tell), and the risk ranking lists those entities after the
  PR's own, as `context`, `differs between <env> and this PR's base, not
  this PR`, with no risk level. The counts are the same either way.
- A name that is not a branch of the store: the header says so and the base
  is chosen as when absent.

The value is matched against the store's own branches (`git for-each-ref`
over `refs/heads` and `refs/remotes`; `deployed/dev` finds
`origin/deployed/dev` when only the remote has it) and never reaches git
otherwise. Resolving it fetches the store, at most once a minute per branch,
since nothing else refreshes the checkout.

### The environment page

`/env` (listed on the index as Environments) shows what every environment
runs against the newest version of the line, computed from the store when
the page is asked for. One line per `deployed/<env>` branch: the git branch
and project commit it was deployed from (or the pull request that commit
belongs to, see below), which line merges with registry changes are waiting
to reach it, by their `#<n>`, how many entities only its branch has, and how
long ago it was deployed. Two environments on the same commit say so.
`--envs dev,staging,production` names the environments to expect; one with
no branch yet shows as `not recorded yet`. Without `--envs` only the
recorded ones are listed. Only a commit whose subject says `deployed to
<env>` is a deploy: a `deployed/<env>` branch that holds only the line
commits it was seeded with shows as `not recorded yet (branch seeded from
<line>)`, here, on the environment's own page and in the PR page's
environment switcher. A record that leaves the registry as it was is an
empty store commit; it still counts as a version, a deploy or a line merge
with no registry changes.

`/env?against=deployed/<env>` compares one environment with the line's
newest version, entity by entity, each marked new, changed or deleted:

- `On <line>, not deployed`: what the line has and the environment does not
  run, under the line merge that last changed each entity, newest merge
  first. A merge is named by its subject, its `(#<n>)` linked to the pull
  request, and the commit it was built from. A merge that changed nothing
  still waiting is listed on one line, as having no registry changes or as
  having changes that already run on the environment. An entity several
  merges changed is listed under the newest and names the others.
- `Only on <env>'s branch`: what the environment has and the line does not,
  as the branch it was deployed from changed it.

Within a merge the entities are grouped by type (exec-fns, endpoints, MCP
tools, data keys, test cases, invariants; any other type by its own name
and a count, as `workflow-producer (3)`); a long group whose rows
differ in nothing but their names is one line, the names relative to the
namespace they share. A risk reason every entity on the page gives is said
once above the lists, so each row keeps only what tells it apart; a high
risk is tagged on its row. An empty list is one line.

An environment behind the line shows only the first list, one deployed from
a branch that already holds the line's newest commit only the second, and a
diverged one both, each from the newest line version the environment's
commit descends from. When the order cannot be told, every difference is
listed together. A sentence above the lists says which case it is.

When the commit an environment runs is not on the line, GitHub is asked
which pull requests contain it. When one was merged and the environment's
branch adds nothing to it, the environment runs that pull request's head
from before the merge: the page says so, with the merge commit, instead of
calling the branch diverged, and leaves that merge out of what is waiting.
An environment at a commit that descends from the line's newest version,
with nothing of its own, runs the line past its newest recorded registry,
and is said so, naming the pull request when the commit is its merge. An open
pull request is named as not merged yet. Without an answer from
GitHub the page reads as before.

Times are shown relative to now (`20h ago`, `yesterday 13:38`, a weekday
within a week, the date beyond), the exact time on hover; store commits are
shown small, the project commits they were built from in full.

`--repo owner/name` names the project's GitHub repository: commits and pull
requests link to it, the page links the GitHub compare of the two commits,
and ancestry that the store's own order cannot settle, and the pull requests
holding a commit, are asked of GitHub through `gh`. `against` is matched
against the store's branches as on the PR page, and the store is fetched at
most once a minute; an unknown branch is reported above the overview.

A view over a pull request of the atlas repo itself, with the registry
decoration of an entity the view names:

![A view section with the entity :fn.ide/check-invariants decorated from the registry](docs/img/view.png)

The entity block alone: what it declares, with its state against the base
version, and who depends on it, what consumes what it produces, which test
cases cover it:

![The registry decoration of one entity](docs/img/entity.png)
