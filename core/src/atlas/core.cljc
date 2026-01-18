(ns atlas.core
  "Atlas public API - register and query semantic architecture"
  (:require [atlas.registry :as reg]
            [atlas.query :as q]
            [atlas.invariant :as inv]
            [atlas.entity :as g]))


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
(defn deps-of [dev-id] (g/deps-for dev-id))

;; Validation
(defn check-invariants [] (inv/check-all))
;;(defn valid? [] (inv/valid?))

