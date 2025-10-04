(ns atlas-ui.colors-v2
  "Enhanced color system with gradient-based match scoring"
  (:require [atlas.query :as q]
            [atlas-ui.graph-data :as gd]))

;; =============================================================================
;; Base Color Palette
;; =============================================================================

(def base-colors
  "Core color palette with soft, readable colors"
  {:entity-no-match "#f3f4f6" ; Very light gray
   :entity-full-match "#ddd6fe" ; Soft purple
   :entity-excluded "#fee2e2" ; Soft red tint (excluded by negation)
   :aspect-selected "#c4b5fd" ; Medium purple (selected in query)
   :aspect-negated "#fca5a5" ; Soft red (negated in query)
   :aspect-unselected "#e5e7eb" ; Light gray (default)
   :aspect-highlighted "#fde68a" ; Soft yellow (belongs to highlighted entity)

   ;; Aspect category colors (consistent per namespace)
   :aspect-tier "#fca5a5" ; Soft red
   :aspect-domain "#a78bfa" ; Soft purple
   :aspect-protocol "#93c5fd" ; Soft blue
   :aspect-operation "#86efac" ; Soft green
   :aspect-constraint "#fbbf24" ; Soft yellow
   :aspect-lens-type "#fb923c" ; Soft orange
   :aspect-feature-type "#f472b6" ; Soft pink
   :aspect-effect "#d1d5db" ; Light gray
   :aspect-other "#fed7aa"}) ; Soft peach

;; Entity type colors (atlas/* and semantic-namespace/*) - SOFT PASTELS
(def entity-type-colors
  {:atlas/execution-function "#bfdbfe" ; Soft blue
   :atlas/structure-component "#a7f3d0" ; Soft green
   :atlas/interface-endpoint "#fde68a" ; Soft amber
   :atlas/interface-protocol "#ddd6fe" ; Soft purple
   :atlas/data-schema "#e0e7ff" ; Soft indigo
   :atlas/identity-role "#fcd34d" ; Soft gold
   :atlas/experience-journey "#c7d2fe" ; Soft slate-blue
   :atlas/governance-constraint "#fecaca" ; Soft red
   :atlas/risk-failure-mode "#fed7aa" ; Soft peach
   :atlas/business-pattern "#fed7aa" ; Soft orange
   :atlas/value-proposition "#ccfbf1" ; Soft teal
   :atlas/type "#f5d0fe" ; Soft fuchsia (for type definitions themselves)
   :semantic-namespace/interaction-intent "#fbcfe8" ; Soft pink
   :semantic-namespace/visual-feature "#d1fae5" ; Soft emerald
   :semantic-namespace/state-atom "#cffafe" ; Soft cyan
   :semantic-namespace/state-derived "#e9d5ff" ; Soft violet
   :default "#e5e7eb"}) ; Soft gray

;; Category colors (for blending when multiple categories selected)
(def tier-colors
  {:tier/foundation "#ef4444" ; Red
   :tier/service "#f59e0b" ; Amber
   :tier/api "#10b981" ; Green
   :tier/ui "#3b82f6"}) ; Blue

(def domain-colors
  {:domain/users "#8b5cf6" ; Purple
   :domain/auth "#ec4899" ; Pink
   :domain/scheduling "#06b6d4" ; Cyan
   :domain/admin "#eab308" ; Yellow
   :domain/google "#3b82f6" ; Blue
   :domain/system "#6b7280"}) ; Gray

(def operation-colors
  {:operation/create "#10b981" ; Green
   :operation/read "#3b82f6" ; Blue
   :operation/update "#f59e0b" ; Amber
   :operation/delete "#ef4444" ; Red
   :operation/query "#06b6d4" ; Cyan
   :operation/list "#8b5cf6" ; Purple
   :operation/check "#a78bfa" ; Light purple
   :operation/collect "#34d399" ; Emerald
   :operation/generate "#fbbf24"}) ; Gold

;; =============================================================================
;; Color Math Utilities
;; =============================================================================

(defn hex-to-rgb [hex]
  "Convert hex color to [r g b] vector"
  (let [hex (if (= (first hex) \#) (subs hex 1) hex)
        r (js/parseInt (subs hex 0 2) 16)
        g (js/parseInt (subs hex 2 4) 16)
        b (js/parseInt (subs hex 4 6) 16)]
    [r g b]))

(defn rgb-to-hex [[r g b]]
  "Convert [r g b] to hex string"
  (str "#"
       (.padStart (.toString (js/Math.round r) 16) 2 "0")
       (.padStart (.toString (js/Math.round g) 16) 2 "0")
       (.padStart (.toString (js/Math.round b) 16) 2 "0")))

(defn interpolate-color [color1 color2 ratio]
  "Interpolate between two hex colors by ratio (0.0 to 1.0)"
  (let [[r1 g1 b1] (hex-to-rgb color1)
        [r2 g2 b2] (hex-to-rgb color2)
        r (+ r1 (* (- r2 r1) ratio))
        g (+ g1 (* (- g2 g1) ratio))
        b (+ b1 (* (- b2 b1) ratio))]
    (rgb-to-hex [r g b])))

(defn blend-colors [colors]
  "Average multiple hex colors together"
  (if (empty? colors)
    (:entity-no-match base-colors)
    (let [rgb-values (map hex-to-rgb colors)
          n (count rgb-values)
          avg-rgb (reduce (fn [[r g b] [r2 g2 b2]]
                            [(+ r r2) (+ g g2) (+ b b2)])
                          [0 0 0]
                          rgb-values)
          final-rgb (mapv #(/ % n) avg-rgb)]
      (rgb-to-hex final-rgb))))

;; =============================================================================
;; Category-Based Coloring
;; =============================================================================

(defn get-entity-value-in-category
  "Find which value an entity has in a given category namespace"
  [entity-aspects category-ns]
  (first (filter #(= (namespace %) category-ns) entity-aspects)))

(defn get-category-color [category-ns value-keyword]
  "Get color for a specific category value"
  (case category-ns
    "tier" (get tier-colors value-keyword)
    "domain" (get domain-colors value-keyword)
    "operation" (get operation-colors value-keyword)
    nil))

(defn collect-category-colors
  "Collect colors from entity's values in selected categories"
  [entity-aspects selected-aspects]
  (let [category-selections (group-by namespace selected-aspects)]
    (for [[category-ns aspects] category-selections
          :when (contains? #{"tier" "domain" "operation"} category-ns)
          :let [entity-value (get-entity-value-in-category entity-aspects category-ns)
                color (when entity-value (get-category-color category-ns entity-value))]
          :when color]
      color)))

;; =============================================================================
;; Main Coloring Functions
;; =============================================================================

(defn get-entity-type
  "Extract semantic type from entity aspects.

   DEPRECATED: Use (:atlas-ui.graph.node/entity-type node) directly instead.
   This function is kept for backward compatibility only."
  [entity-aspects]
  (first (filter #(contains? #{"atlas" "semantic-namespace"} (namespace %)) entity-aspects)))

(defn entity-color-by-type
  "Calculate entity color based on semantic type.

   Parameters:
   - entity-type: The entity type keyword (e.g., :atlas/interface-endpoint)
                  Pass directly from (:atlas-ui.graph.node/entity-type node)
   - aspects: Pure aspect set (without type mixed in)
   - query: Query map with selected/negated aspects"
  [entity-type aspects query]
  (let [{::q/keys [selected negated]} query
        excluded? (q/excluded-by-negation? aspects negated)]
    (cond
      excluded?
      (:entity-excluded base-colors)

      entity-type
      (get entity-type-colors entity-type (:default entity-type-colors))

      :else
      (:entity-no-match base-colors))))

(defn aspect-color
  "Calculate aspect color based on selection state, entity highlighting, and aspect category"
  [aspect-id query highlighted-entity-aspects]
  (cond
    ;; Negated in query (soft red)
    (contains? (set (or (::q/negated query) (:negated query))) aspect-id)
    (:aspect-negated base-colors)

    ;; Selected in query (medium purple)
    (contains? (set (or (::q/selected query) (:selected query))) aspect-id)
    (:aspect-selected base-colors)

    ;; Related to highlighted entity (soft yellow)
    (contains? highlighted-entity-aspects aspect-id)
    (:aspect-highlighted base-colors)

    ;; Color by aspect category (namespace)
    :else
    (let [aspect-ns (namespace aspect-id)]
      (cond
        (= aspect-ns "tier") (:aspect-tier base-colors)
        (= aspect-ns "domain") (:aspect-domain base-colors)
        (= aspect-ns "protocol") (:aspect-protocol base-colors)
        (= aspect-ns "operation") (:aspect-operation base-colors)
        (= aspect-ns "constraint") (:aspect-constraint base-colors)
        (= aspect-ns "lens-type") (:aspect-lens-type base-colors)
        (= aspect-ns "feature-type") (:aspect-feature-type base-colors)
        (= aspect-ns "effect") (:aspect-effect base-colors)
        :else (:aspect-other base-colors)))))

(defn node-color
  "Main entry point for node coloring.

   Now uses direct access to :entity-type and :aspects for better performance.
   Falls back to :identity for backward compatibility if new fields not present."
  [node query highlighted-entity-id highlighted-entity-aspects]
  (let [node-id (:atlas-ui.graph.node/id node)
        node-type (:atlas-ui.graph.node/type node)]
    (case node-type
      :entity
      (let [;; Use new fields if available, fall back to identity
            entity-type (or (:atlas-ui.graph.node/entity-type node)
                            (get-entity-type (:atlas-ui.graph.node/identity node)))
            aspects (or (:atlas-ui.graph.node/aspects node)
                        (:atlas-ui.graph.node/identity node))]
        (entity-color-by-type entity-type aspects query))

      :aspect (aspect-color node-id query highlighted-entity-aspects)

      (:entity-no-match base-colors))))

(defn edge-color
  "Calculate edge color"
  [edge-type]
  (case edge-type
    :dependency (:edge-dependency base-colors)
    :membership (:edge base-colors)
    (:edge base-colors)))

;; =============================================================================
;; Opacity Calculation
;; =============================================================================

(defn calculate-opacity
  "Calculate node opacity based on match score (with stronger fade for non-matches).

   Note: Pass entity aspects (pure aspects, not including type) for accurate scoring."
  [entity-aspects query]
  (let [{::q/keys [selected negated mode]} query
        selected-set (set selected)
        excluded? (q/excluded-by-negation? entity-aspects negated)
        mode (or mode :count)]
    (cond
      (empty? selected-set)
      (if excluded? 0.15 1.0)

      :else
      (let [score (q/query-score entity-aspects (assoc query ::q/selected selected-set ::q/mode mode))]
        (if (= score 0.0)
          0.2  ; Much more faded for non-matches
          (+ 0.5 (* 0.5 score)))))))  ; Range: 0.2 to 1.0, stronger fade at low scores
