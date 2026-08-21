(ns atlas.store.protocol
  "Where snapshots live. Three operations, because that is all change tracking
  needs: read the current state, replace it, and name what you read.

  Kept this small on purpose -- a filesystem directory, a private git repo and
  an HTTP service can all satisfy it, which is what lets a team start with zero
  infrastructure and move to a hosted service by changing one line of config.")


(defprotocol Store
  (head [store]
    "Identifier of the current state (commit sha, directory name, version
    label), or nil when the store is empty.")

  (read-at [store ref]
    "`{path content}` for everything under the store's prefix at `ref`. A nil
    `ref` means the head.")

  (write! [store {:keys [message parent]} files]
    "Replace the prefix's contents with `files`, atomically where the backend
    allows it, and return `{:ref ...}`.

    Atomicity matters more than it looks: a version spread over 20 files that
    lands as 20 separate writes has intermediate states in which the registry
    is internally inconsistent, and no identifier that means \"complete\".

    Replacement, not merge -- an entity deleted upstream must disappear rather
    than linger from the previous write. Implementations must scope that
    replacement to their own prefix and leave everything else untouched."))
