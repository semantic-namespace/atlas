(ns atlas.review.deps-bridge
  "What sdiff's dependency graph cannot see in an atlas codebase: a form that
  names a registered keyword depends on the form that registers it. An
  `exec-fn` call by dev-id, an entity's deps and an integrant component key are
  all keywords, so each becomes a call to the registration or the
  `init-key` method that defines it."
  (:require [sdiff.deps :as deps]))

(defn install!
  "Adds the bridge to sdiff's dependency graph; `defining` matches the var
  that defines a keyword, such as `register!` or `ig/init-key`."
  [defining]
  (deps/add-bridge! ::keywords (deps/keyword-bridge defining)))
