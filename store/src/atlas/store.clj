(ns atlas.store
  "Track registry changes over time, against any backing store.

  The intended call site is one line at the end of a CI job:

      (atlas.store/write-if-changed!
        (github/github-store {:repo \"org/registry\" :token tok :prefix \"org/project/\"})
        @registry/registry
        {:message \"registry: my-app@abc1234\"})

  Everything else in this namespace exists to stop that line doing damage when
  the registry it was handed is wrong."
  (:require
   [atlas.store.canonical :as canon]
   [atlas.store.protocol :as p]))


(def ^:const default-max-shrink
  "Refuse a write that loses more than this fraction of entities.

  This guard is not paranoia. Building a registry means loading namespaces in
  the right order, and getting it subtly wrong yields a well-formed registry
  that is simply missing things -- a real attempt produced 614 of 1101 entities
  and looked entirely healthy. Pushed, that reads as a mass architectural
  deletion and corrupts the history it was meant to record. A skipped write
  costs one commit; a bad write costs the record."
  0.10)


(defn- shrink-check!
  [previous current max-shrink]
  (when (seq previous)
    (let [before (count previous)
          after  (count current)
          lost   (- before after)]
      (when (> lost (* max-shrink before))
        (throw (ex-info
                (format "Registry shrank %d -> %d entities (%.0f%%). Refusing to write: this is almost always an incomplete build, not a real deletion."
                        before after (* 100.0 (/ (double lost) before)))
                {:reason ::suspicious-shrink :before before :after after}))))))


(defn write-if-changed!
  "Canonicalise `registry`, compare it byte-for-byte with what the store holds,
  and write only if it differs. Returns
  `{:changed? bool :ref ... :entities n}`.

  Comparison is on the canonical bytes rather than a semantic diff, which makes
  the common case -- nothing changed -- free: no diff engine, no service call.
  On real data that was 5 of every 8 versions.

  Options:
    :message       commit message (required for stores that commit)
    :volatile      prop keys to exclude (default `canon/default-volatile-props`)
    :max-shrink    fraction of entities that may disappear (default 0.10)
    :force?        skip the shrink guard -- for a genuine mass deletion
    :dry-run?      compute :changed? and stop; never writes"
  [store registry {:keys [message volatile max-shrink force? dry-run?]
                   :or   {volatile   canon/default-volatile-props
                          max-shrink default-max-shrink}}]
  (let [clean    (canon/strip-volatile registry volatile)
        files    (canon/registry->files clean)
        existing (p/read-at store nil)]
    (if (= files (select-keys existing (keys files)))
      {:changed? false :entities (count clean) :ref (p/head store)}
      (do
        ;; The shrink guard runs before a dry run reports too, so a collapsed
        ;; build fails the check locally rather than only on the real write.
        (when-not force?
          (shrink-check! (canon/files->registry existing) clean max-shrink))
        (if dry-run?
          {:changed? true :ref (p/head store) :entities (count clean)
           :files (count files) :dry-run? true}
          (let [{:keys [ref]} (p/write! store {:message message} files)]
            {:changed? true :ref ref :entities (count clean) :files (count files)}))))))


(defn assert-reproducible!
  "Throw unless two builds of the same source produce the same registry.

  Worth running once in CI. A registry that embeds timestamps or random ids is
  not a function of its source, so every snapshot differs, no-op detection never
  fires, and diffs are permanent noise -- with a symptom that points nowhere
  near the cause."
  [reg-a reg-b & {:keys [volatile] :or {volatile canon/default-volatile-props}}]
  (let [diffs (canon/non-deterministic (canon/strip-volatile reg-a volatile)
                                       (canon/strip-volatile reg-b volatile))]
    (when (seq diffs)
      (throw (ex-info (str "Registry is not reproducible: " (count diffs)
                           " entities differ between two builds of the same source.")
                      {:reason ::non-deterministic
                       :prop-keys (into #{} (mapcat val) diffs)
                       :examples  (take 3 (keys diffs))})))
    true))
