(ns atlas-ui.colors
  "Soft, harmonious color system that provides visual differentiation
   without harsh contrast. Colors are contextual based on selection.")

;; =============================================================================
;; Soft Color Palette - Low saturation, comfortable brightness
;; =============================================================================

(def base-colors
  "Default colors when nothing is selected"
  {:entity "#d1d5db"         ; Warm gray
   :aspect "#e5e7eb"         ; Light warm gray
   :selected "#7c3aed"       ; Soft purple for selection
   :highlighted "#a78bfa"    ; Light purple for related items
   :edge "#f3f4f6"          ; Very light gray
   :edge-dependency "#cbd5e1" ; Slightly darker for dependencies
   :background "#fafafa"})   ; Off-white background

;; Soft tier colors - muted, comfortable
(def tier-colors
  {:tier/foundation "#fca5a5"   ; Soft coral-red
   :tier/service "#fdba74"      ; Soft peach-orange
   :tier/api "#86efac"          ; Soft mint-green
   :tier/ui "#93c5fd"})         ; Soft sky-blue

;; Soft domain colors - pastel range
(def domain-colors
  {:domain/users "#c4b5fd"      ; Soft lavender
   :domain/auth "#f9a8d4"       ; Soft pink
   :domain/scheduling "#a5f3fc" ; Soft cyan
   :domain/admin "#fde68a"      ; Soft yellow
   :domain/google "#93c5fd"     ; Soft blue
   :domain/system "#d1d5db"})   ; Soft gray

;; Soft operation colors
(def operation-colors
  {:operation/create "#86efac"   ; Soft green
   :operation/read "#93c5fd"     ; Soft blue
   :operation/update "#fde68a"   ; Soft yellow
   :operation/delete "#fca5a5"   ; Soft red
   :operation/query "#a5f3fc"    ; Soft cyan
   :operation/list "#c4b5fd"     ; Soft purple
   :operation/check "#d8b4fe"    ; Soft violet
   :operation/collect "#a7f3d0"  ; Soft emerald
   :operation/generate "#fbbf24"}) ; Soft amber

;; Soft role colors
(def role-colors
  {:role/administrator "#fde68a" ; Soft gold
   :role/team-member "#a5f3fc"   ; Soft cyan
   :role/scheduler "#c4b5fd"})   ; Soft lavender

;; Entity type colors (when filtering by type)
(def type-colors
  {:atlas/execution-function "#fcd34d"        ; Soft yellow
   :atlas/structure-component "#fca5a5"       ; Soft coral
   :atlas/interface-endpoint "#86efac"        ; Soft green
   :semantic-namespace/interaction-intent "#a5f3fc"       ; Soft cyan
   :atlas/governance-constraint "#fca5a5"      ; Soft red
   :atlas/value-proposition "#fde68a" ; Soft gold
   :atlas/identity-role "#c4b5fd"       ; Soft purple
   :atlas/data-schema "#d1d5db"})   ; Soft gray

;; =============================================================================
;; Color Logic
;; =============================================================================

(defn is-category-aspect?
  "Check if an aspect represents a category (tier/domain/operation/role)"
  [aspect]
  (when aspect
    (contains? #{"tier" "domain" "operation" "role"} 
               (namespace aspect))))

(defn is-type-aspect?
  "Check if aspect is a semantic-namespace type marker"
  [aspect]
  (when aspect
    (= "semantic-namespace" (namespace aspect))))

(defn get-entity-value-in-category
  "Find which value an entity has in a given category namespace"
  [entity-aspects category-ns]
  (first (filter #(= (namespace %) category-ns) entity-aspects)))

(defn category-color
  "Get color for a specific category value"
  [category-ns value-keyword]
  (case category-ns
    "tier" (get tier-colors value-keyword (:entity base-colors))
    "domain" (get domain-colors value-keyword (:entity base-colors))
    "operation" (get operation-colors value-keyword (:entity base-colors))
    "role" (get role-colors value-keyword (:entity base-colors))
    (:entity base-colors)))

(defn type-color
  "Get color for a semantic-namespace type"
  [type-keyword]
  (get type-colors type-keyword (:entity base-colors)))

(defn contextual-entity-color
  "Compute entity color based on current selection.
   
   Rules:
   1. No selection → base gray
   2. Category aspect selected (tier/domain/etc) → color by entity's value in that category
   3. Type aspect selected → color by entity's type
   4. Regular aspect selected → highlight if entity has it
   5. Entity selected → stay gray (highlights happen on aspects instead)"
  [entity-aspects selected-aspect]
  (cond
    ;; No selection - everything gray
    (nil? selected-aspect)
    (:entity base-colors)
    
    ;; Category aspect selected (e.g., :tier/service)
    (is-category-aspect? selected-aspect)
    (let [category-ns (namespace selected-aspect)
          entity-value (get-entity-value-in-category entity-aspects category-ns)]
      (if entity-value
        (category-color category-ns entity-value)
        (:entity base-colors)))  ; No value in this category
    
    ;; Type aspect selected (e.g., :semantic-namespace/function)
    (is-type-aspect? selected-aspect)
    (if (contains? entity-aspects selected-aspect)
      (type-color selected-aspect)
      (:entity base-colors))
    
    ;; Regular aspect selected - subtle highlight if entity has it
    (contains? entity-aspects selected-aspect)
    (:highlighted base-colors)
    
    ;; Doesn't have the selected aspect
    :else
    (:entity base-colors)))

(defn aspect-color
  "Compute aspect color based on selection.
   
   Rules:
   1. The selected aspect itself → bright purple
   2. Aspect belongs to selected entity → gold highlight
   3. Otherwise → light gray"
  [aspect selected-node selected-entity-aspects]
  (cond
    ;; This aspect is selected
    (and (= (:type selected-node) :aspect)
         (= aspect (:id selected-node)))
    (:selected base-colors)
    
    ;; An entity is selected and this aspect belongs to it
    (and (= (:type selected-node) :entity)
         (contains? selected-entity-aspects aspect))
    "#fbbf24"  ; Gold highlight for aspects of selected entity
    
    ;; Default
    :else
    (:aspect base-colors)))

(defn node-color
  "Main function: compute color for any node based on current selection"
  [node selection registry]
  (let [node-id (:id node)
        node-type (:type node)
        selected-id (:id selection)
        selected-type (:type selection)]
    
    (cond
      ;; The node itself is selected
      (= node-id selected-id)
      (:selected base-colors)
      
      ;; Node is an entity
      (= node-type :entity)
      (contextual-entity-color 
        (set (:identity node))
        (when (= selected-type :aspect) selected-id))
      
      ;; Node is an aspect
      (= node-type :aspect)
      (let [selected-entity-aspects (when (= selected-type :entity)
                                      (set (:identity (get-node-by-id registry selected-id))))]
        (aspect-color node-id {:id selected-id :type selected-type} selected-entity-aspects))
      
      ;; Default
      :else
      (:entity base-colors))))

(defn edge-color
  "Compute edge color - slightly darker for dependencies, very light otherwise"
  [edge-type]
  (case edge-type
    :dependency (:edge-dependency base-colors)
    :membership (:edge base-colors)
    (:edge base-colors)))

;; =============================================================================
;; Helper
;; =============================================================================

(defn get-node-by-id
  "Find node data in registry by dev-id"
  [registry node-id]
  ;; Assuming registry is a map of {compound-id props}
  ;; where props contains :dev/id
  (->> registry
       (filter (fn [[_ props]] (= (:atlas/dev-id props) node-id)))
       first
       second))
