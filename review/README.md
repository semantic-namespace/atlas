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
  form that changed it, or marked when no form in the diff did.

All of it answers through `atlas.ide` with the stored version bound as the
registry. Inferred annotations, from a reviewer or a model, render apart and
labelled.

```
clojure -M:serve --store <registry store checkout> --prefix <path inside it> [--branch origin/main] [--port 7878] [--host 127.0.0.1]
```

Two registries are compared. The base is the version the project's CI
recorded in its registry store (the git repository written by `atlas.store`,
one commit per recorded registry, the source sha in the commit message) for
the PR's merge base, or the newest recorded version when that commit was not
recorded. The candidate is the registry CI built for the PR head, downloaded
through `gh` from the workflow run's artifact (`SDIFF_REGISTRY_WORKFLOW` and
`SDIFF_REGISTRY_ARTIFACT` name them; defaults `atlas-registry` and
`registry`). When CI has no run for the head, the page says so and shows
entities as on main.

A view over a pull request of the atlas repo itself, with the registry
decoration of an entity the view names:

![A view section with the entity :fn.ide/check-invariants decorated from the registry](docs/img/view.png)

The entity block alone: what it declares, with its state against the base
version, and who depends on it, what consumes what it produces, which test
cases cover it:

![The registry decoration of one entity](docs/img/entity.png)

`ATLAS_CLOUD_URL` names the cloud holding the versions (default
`http://localhost:8090`). The base is the newest `vN.N.N` version. The
candidate is the version named `pr<N>-<head sha>`; when it is missing the
server downloads the registry CI built for that head (the `registry` artifact
of the project's `atlas-registry` workflow, through `gh`) and stages it with
`ATLAS_CLOUD_KEY`. When CI has no run for the head, the page says so and
shows entities as on main.
