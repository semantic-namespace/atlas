# review

The [sdiff](https://github.com/semantic-namespace/diff) review server for a
project with an atlas registry. The report page of a pull request gets
derived decorations from the registry, beside each changed form and at the
top:

- the entity a form declares, with its contract and the delta between the
  main version and the PR's candidate version from the atlas-cloud diff;
- who depends on that entity, who consumes what it produces, which test
  cases cover it;
- the data keys the changed code mentions, with their producers and consumers;
- a header listing every entity the PR's registry diff touched, linked to the
  form that changed it, or marked when no form in the diff did.

All of it answers through `atlas.ide` with the stored version bound as the
registry. Inferred annotations, from a reviewer or a model, render apart and
labelled.

```
clojure -M:serve --org acme --project shop [--port 7878] [--host 127.0.0.1]
```

`ATLAS_CLOUD_URL` names the cloud holding the versions (default
`http://localhost:8090`). The base is the newest `vN.N.N` version; the
candidate is the version named `pr<N>-<head sha>` when one is staged, else
the page says contracts are shown as on main.
