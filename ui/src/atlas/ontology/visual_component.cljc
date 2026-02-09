(ns atlas.ontology.visual-component
  "Visual-component ontology module for UI components.

   This module defines the `:atlas/visual-component` entity type for
   visual/rendering components with UI-specific properties.
   Auto-registers on require.

   Usage:
     (require '[atlas.ontology.visual-component])"
  (:require [atlas.registry :as registry]
            [clojure.spec.alpha :as s]))

;; =============================================================================
;; ONTOLOGY DEFINITION
;; =============================================================================

(def ontology-definition
  "The ontology definition for :atlas/visual-component"
  {:ontology/for :atlas/visual-component
   :ontology/keys [:visual-component/consumes
                   :visual-component/emits
                   :visual-component/visual-purpose
                   :visual-component/rendering-features
                   :visual-component/provides]})

;; =============================================================================
;; SPECS
;; =============================================================================

(s/def :visual-component/consumes (s/coll-of qualified-keyword?))
(s/def :visual-component/emits (s/coll-of qualified-keyword?))
(s/def :visual-component/visual-purpose string?)
(s/def :visual-component/rendering-features (s/coll-of keyword?))
(s/def :visual-component/provides (s/coll-of qualified-keyword?))

;; =============================================================================
;; AUTO-REGISTRATION (top-level, like clojure.spec)
;; =============================================================================

;; Ontology
(registry/register!
 :atlas/visual-component
 :atlas/ontology
 #{:atlas/visual-component}
 ontology-definition)
