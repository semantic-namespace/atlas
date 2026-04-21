(ns atlas.core
  "Atlas public API - register and query semantic architecture"
  (:require [atlas.registry :as reg]
            [atlas.query :as q]
            [atlas.invariant :as inv]
            [atlas.registry.lookup :as g]
            [atlas.ontology :as o]
            [atlas.ide.trace :as trace]))


;; Registration
(defn register
  "Register entity with compound identity"
  ([dev-id type aspects props]
   (reg/register! dev-id type aspects props))
  ([type aspects props]
   (reg/register! type aspects props)))


 ;; Common queries
(defn find* [aspect] (q/find-by-aspect @reg/registry aspect))
(defn identity-for [dev-id] (q/find-by-dev-id @reg/registry dev-id))


(comment 
;; Re-export for convenience
(def all-aspects reg/all-aspects)
(def all-entities reg/all-entities)

  )
(defn deps-of [dev-id] (o/deps-for dev-id))

;; Validation
(defn check-invariants [] (inv/check-all))

;; =============================================================================
;; Exception enrichment — semantic snapshots
;; =============================================================================
;;
;; entity-snapshot* functions are designed to be called from exception paths.
;; They never throw and return nil if the dev-id is unknown. See
;; docs/exception-enrichment.md for the design rationale.

(defn entity-snapshot-fast
  "Cheap semantic snapshot — identity, type, and aspects only.
   Safe for hot exception paths under load. Never throws.
   Returns nil if dev-id is unknown.

   2-arity accepts pre-fetched props to avoid a redundant lookup
   when the caller already has them (e.g. an executor)."
  ([dev-id]
   (entity-snapshot-fast dev-id nil))
  ([dev-id _props]
   (try
     (when-let [[cid] (q/find-by-dev-id @reg/registry dev-id)]
       {:atlas/dev-id      dev-id
        :atlas/compound-id cid
        :atlas/type        (reg/entity-type cid)
        :atlas/declared    (reg/declared-aspects dev-id)
        :atlas/derived     (reg/derived-aspects dev-id)})
     (catch #?(:clj Throwable :cljs :default) _ nil))))

(defn entity-snapshot
  "Full semantic snapshot — identity, aspects, deps, blast radius, dataflow.
   Includes transitive dependency summary and reverse blast-radius via the
   trace cache. Never throws. Returns nil if dev-id is unknown.

   Use entity-snapshot-fast on hot production paths; use this in dev/staging
   or behind an environment gate. See docs/exception-enrichment.md.

   2-arity accepts pre-fetched props to avoid a redundant registry lookup
   and to strip impl function references from the serialized payload."
  ([dev-id]
   (entity-snapshot dev-id (some->> dev-id
                                    (q/find-by-dev-id @reg/registry)
                                    first
                                    reg/fetch)))
  ([dev-id props]
   (try
     (when-let [[cid] (q/find-by-dev-id @reg/registry dev-id)]
       {:atlas/dev-id       dev-id
        :atlas/compound-id  cid
        :atlas/type         (reg/entity-type cid)
        :atlas/declared     (reg/declared-aspects dev-id)
        :atlas/derived      (reg/derived-aspects dev-id)
        :atlas/deps         (trace/dependencies-of dev-id)
        :atlas/dep-summary  (trace/recursive-dependencies-summary dev-id)
        :atlas/blast-radius (trace/recursive-dependents-summary dev-id)
        :atlas/data-flow    (trace/data-flow dev-id)
        :atlas/props        (when props
                              (dissoc props :atlas/impl :execution-function/impl))})
     (catch #?(:clj Throwable :cljs :default) _ nil))))
;;(defn valid? [] (inv/valid?))

