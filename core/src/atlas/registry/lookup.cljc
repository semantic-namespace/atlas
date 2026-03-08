(ns atlas.registry.lookup
  "Runtime that drives behavior from semantic registry."
  (:require [atlas.registry :as registry]
            [atlas.query :as query]))

;; =============================================================================
;; ENTITY LOOKUP
;; =============================================================================

(defn- fetch-by-dev-id
  "Find the [identity value] pair for a given dev-id.
   Delegates to atlas.query/find-by-dev-id."
  [dev-id]
  (query/find-by-dev-id @registry/registry dev-id))

(defn identity-for [dev-id]
  (first (fetch-by-dev-id dev-id)))

(defn props-for [dev-id]
  (second (fetch-by-dev-id dev-id)))

;; =============================================================================
;; PROPERTY ACCESS (Dynamic)
;; =============================================================================

(defn has-aspect? [dev-id aspect]
  (contains? (identity-for dev-id) aspect))

(defn all-with-aspect
  "Find all dev-ids with given aspect.
   Delegates to atlas.query/find-dev-ids-with-aspect."
  [aspect]
  (query/find-dev-ids-with-aspect @registry/registry aspect))

(defn handle-tool
  "Execute any registered entity that has :atlas/impl.
   Domain-agnostic — works for any domain.

   Input: {:tool/name :qualified/dev-id
           :tool/args {:qualified/keyword value ...}}

   Returns:
   - {:tool/result <value>} on success
   - {:tool/error :not-found} when dev-id doesn't exist in registry
   - {:tool/error :no-impl, :tool/entity <props>} when entity exists but has no :atlas/impl"
  [{:tool/keys [name args]}]
  (if-let [props (props-for name)]
    (if-let [handler (:atlas/impl props)]
      {:tool/result (handler (or args {}))}
      {:tool/error :no-impl
       :tool/entity (dissoc props :atlas/impl)})
    {:tool/error :not-found
     :tool/name name}))

