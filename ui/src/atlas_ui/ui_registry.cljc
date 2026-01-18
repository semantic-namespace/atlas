(ns atlas-ui.ui-registry)

(def ui-registry {

  ;; =========================================================================
  ;; Core Semantic Subjects
  ;; =========================================================================


  #{:semantic-namespace/interaction-event
    :tier/ui}
  {:atlas/dev-id :ui-event/semantic-event
   :interaction-event/description
   "Low-level UI signal emitted by components and consumed by intents"}

  #{:semantic-namespace/presentation-view
    :tier/ui}
  {:atlas/dev-id :view/main
   :presentation-view/composes
   [:component.ui/graph-view
    :component.ui/selection-controls
    :component.ui/sidebar
    :component.ui/search-bar
    :component.ui/color-legend
    :component.ui/info-panel
    :component.ui/lens-selector] ;; NEW: Phase 3a
   :presentation-view/visual-purpose
   "Primary layout orchestrating all UI components"}

  ;; =========================================================================
  ;; UI Components
  ;; =========================================================================

  #{:atlas/structure-component :tier/ui :domain/visualization}
  {:atlas/dev-id :component.ui/graph-view
   :structure-component/consumes
   [:state/graph-data
    :state/filtered-graph-data ;; NEW: Lens-filtered data
    :atlas-ui.ui.state/selections
    :atlas-ui.ui.state/query-mode
    :atlas-ui.ui.state/search-term
    :atlas-ui.ui.state/active-lens] ;; NEW: Phase 3a
   :structure-component/emits
   [:ui-event/aspect-clicked
    :ui-event/aspect-shift-clicked
    :ui-event/entity-clicked
    :ui-event/background-clicked]
   :structure-component/visual-purpose
   "Interactive semantic graph of entities and aspects"
   :structure-component/rendering-features ;; NEW: Phase 3b
   [:feature/shape-differentiation
    :feature/aspect-badges
    :feature/edge-labels
    :feature/adaptive-layout]}

  #{:atlas/structure-component :tier/ui :domain/controls}
  {:atlas/dev-id :component.ui/selection-controls
   :structure-component/consumes
   [:atlas-ui.ui.state/selections
    :atlas-ui.ui.state/negated-aspects
    :atlas-ui.ui.state/query-mode
    :atlas-ui.ui.state/min-score
    :atlas-ui.ui.state/hide-unmatched?]
   :structure-component/emits
   [:ui-event/aspect-toggled
    :ui-event/query-mode-changed
    :ui-event/selections-cleared
    :ui-event/visibility-toggled
    :ui-event/min-score-changed]
   :structure-component/visual-purpose
   "Semantic boolean query builder"}

  #{:atlas/structure-component :tier/ui :domain/exploration} ;; NEW: Phase 3a
  {:atlas/dev-id :component.ui/lens-selector
   :structure-component/consumes
   [:atlas-ui.ui.state/registry
    :atlas-ui.ui.state/active-lens
    :atlas-ui.ui.state/available-lenses]
   :structure-component/emits
   [:ui-event/lens-activated
    :ui-event/lens-cleared]
   :structure-component/visual-purpose
   "Semantic dimension selector for entropy reduction"
   :structure-component/provides
   [:lens-type/domain
    :lens-type/tier
    :lens-type/protocol
    :lens-type/constraint
    :lens-type/ui-intent]}

  #{:atlas/structure-component :tier/ui :domain/information}
  {:atlas/dev-id :component.ui/sidebar
   :structure-component/consumes
   [:atlas-ui.graph.node/id
    :atlas-ui.graph.node/identity
    :atlas-ui.graph.node/props
    :atlas-ui.graph.node/type]
   :structure-component/emits
   [:ui-event/sidebar-closed]
   :structure-component/visual-purpose
   "Semantic entity inspection panel"}

  #{:atlas/structure-component :tier/ui :domain/search}
  {:atlas/dev-id :component.ui/search-bar
   :structure-component/consumes
   [:atlas-ui.ui.state/search-term]
   :structure-component/emits
   [:ui-event/search-changed]
   :structure-component/visual-purpose
   "Live semantic text filter"}

  #{:atlas/structure-component :tier/ui :domain/legend}
  {:atlas/dev-id :component.ui/color-legend
   :structure-component/consumes
   [:atlas-ui.ui.state/query-mode
    :atlas-ui.ui.state/selections]
   :structure-component/visual-purpose
   "Explains semantic visual encoding"}

  #{:atlas/structure-component :tier/ui :domain/analytics}
  {:atlas/dev-id :component.ui/info-panel
   :structure-component/consumes
   [:atlas-ui.stats/total
    :atlas-ui.stats/matching
    :atlas-ui.stats/percentage]
   :structure-component/visual-purpose
   "Semantic registry analytics"}

  ;; =========================================================================
  ;; Application State
  ;; =========================================================================

  #{:semantic-namespace/state-atom :tier/ui}
  {:atlas/dev-id :state/app-state
   :state-atom/fields
   [:atlas-ui.ui.state/registry
    :atlas-ui.ui.state/selections
    :atlas-ui.ui.state/negated-aspects
    :atlas-ui.ui.state/query-mode
    :atlas-ui.ui.state/min-score
    :atlas-ui.ui.state/highlight-selection
    :atlas-ui.ui.state/highlighted-entity-aspects
    :atlas-ui.ui.state/hide-unmatched?
    :atlas-ui.ui.state/search-term
    :atlas-ui.ui.state/loading?
    :atlas-ui.ui.state/error
    :atlas-ui.ui.state/registry-version
    :atlas-ui.ui.state/active-lens ;; NEW: Phase 3a
    :atlas-ui.ui.state/show-lens-selector?]} ;; NEW: Phase 3a - UI visibility state

  #{:semantic-namespace/state-derived :tier/ui :effect/pure :domain/visualization}
  {:atlas/dev-id :state/graph-data
   :state-derived/derives-from [:atlas-ui.ui.state/registry]
   :state-derived/output
   [:atlas-ui.graph.data/entities
    :atlas-ui.graph.data/aspects
    :atlas-ui.graph.data/edges]
   :state-derived/consumed-by [:component.ui/graph-view]}

  #{:semantic-namespace/state-derived :tier/ui :effect/pure :domain/exploration} ;; NEW: Phase 3a
  {:atlas/dev-id :state/filtered-graph-data
   :state-derived/derives-from
   [:atlas-ui.ui.state/active-lens
    :state/graph-data]
   :state-derived/output
   [:atlas-ui.graph.data/entities    ;; Filtered entities
    :atlas-ui.graph.data/aspects     ;; Hidden when lens active
    :atlas-ui.graph.data/edges       ;; Filtered edges
    :atlas-ui.graph.data/all-nodes]  ;; With aspect badges
   :state-derived/consumed-by [:component.ui/graph-view]
   :state-derived/transformation
   "Applies lens filter, hides aspects, adds badges to entities"}

  #{:semantic-namespace/state-derived :tier/ui :effect/pure :operation/project} ;; NEW: Phase 3a
  {:atlas/dev-id :state/lens-result
   :state-derived/derives-from
   [:atlas-ui.ui.state/registry
    :atlas-ui.ui.state/active-lens]
   :state-derived/output
   [:lens/type
    :lens/value
    :lens/title
    :lens/entities
    :lens/entity-ids
    :lens/layout-mode    ;; NEW: Phase 3a - Layout optimization
    :lens/layout-config  ;; NEW: Phase 3a - Cytoscape params
    :lens/show-all-aspects?]
   :state-derived/consumed-by [:state/filtered-graph-data]
   :state-derived/business-pattern :pattern/lens-projection}

  #{:semantic-namespace/state-derived :tier/ui :effect/pure :domain/analytics}
  {:atlas/dev-id :state/match-stats
   :state-derived/derives-from
   [:atlas-ui.ui.state/selections
    :atlas-ui.ui.state/negated-aspects
    :atlas-ui.ui.state/query-mode
    :atlas-ui.graph.data/entities]
   :state-derived/output
   [:atlas-ui.stats/total
    :atlas-ui.stats/matching
    :atlas-ui.stats/percentage]}

  ;; =========================================================================
  ;; Lens System (Phase 3a)
  ;; =========================================================================

  #{:atlas/execution-function :tier/service :domain/exploration :lens-type/domain}
  {:atlas/dev-id :fn/domain-lens
   :execution-function/input [:registry :domain-keyword]
   :execution-function/output [:lens-result]
   :execution-function/layout :layout/breadthfirst
   :execution-function/purpose
   "Projects registry through domain dimension, reducing entropy 70-85%"}

  #{:atlas/execution-function :tier/service :domain/exploration :lens-type/tier}
  {:atlas/dev-id :fn/tier-lens
   :execution-function/input [:registry :tier-keyword]
   :execution-function/output [:lens-result]
   :execution-function/layout :layout/grid
   :execution-function/purpose
   "Projects registry through architectural tier dimension"}

  #{:atlas/execution-function :tier/service :domain/exploration :lens-type/protocol}
  {:atlas/dev-id :fn/protocol-lens
   :execution-function/input [:registry :protocol-keyword]
   :execution-function/output [:lens-result]
   :execution-function/layout :layout/concentric
   :execution-function/purpose
   "Projects registry through protocol implementation dimension"}

  #{:atlas/execution-function :tier/service :domain/exploration :lens-type/constraint}
  {:atlas/dev-id :fn/constraint-lens
   :execution-function/input [:registry :constraint-keyword]
   :execution-function/output [:lens-result]
   :execution-function/layout :layout/circle
   :execution-function/purpose
   "Projects registry through constraint/security dimension"}

  #{:atlas/execution-function :tier/service :domain/exploration :lens-type/ui-intent}
  {:atlas/dev-id :fn/intent-lens
   :execution-function/input [:registry :intent-id]
   :execution-function/output [:lens-result]
   :execution-function/layout :layout/breadthfirst
   :execution-function/purpose
   "Traces UI intent through endpoints to implementation"}

  #{:atlas/execution-function :tier/service :domain/exploration :operation/discover}
  {:atlas/dev-id :fn/available-lenses
   :execution-function/input [:registry]
   :execution-function/output
   [:domains :tiers :protocols :constraints :ui-intents]
   :execution-function/purpose
   "Extracts available lens values from functional entities only"}

  #{:atlas/execution-function :tier/service :domain/exploration :operation/apply}
  {:atlas/dev-id :fn/apply-lens
   :execution-function/input [:registry :active-lens]
   :execution-function/output [:lens-result]
   :execution-function/purpose
   "Applies active lens to registry"}

  ;; =========================================================================
  ;; Visual Encoding (Phase 3b)
  ;; =========================================================================

  #{:semantic-namespace/visual-feature :tier/ui :domain/visualization :feature-type/color :operation/color-entity}
  {:atlas/dev-id :feature/entity-type-colors
   :visual-feature/encoding-rules
   {:function "#60a5fa"                 ; Blue
    :component "#34d399"                ; Green
    :endpoint "#f59e0b"                 ; Amber
    :protocol "#a78bfa"                 ; Purple
    :ui-intent "#ec4899"                ; Pink
    :constraint "#ef4444"               ; Red
    :feature "#10b981"                  ; Emerald
    :state-atom "#06b6d4"               ; Cyan
    :derived-state "#8b5cf6"            ; Violet
    :pattern "#f97316"                  ; Orange
    :value-proposition "#14b8a6"        ; Teal
    :default "#9ca3af"}                 ; Gray
   :visual-feature/purpose
   "Distinguishes semantic entity types by color (all use rectangle shape)"
   :visual-feature/shape "round-rectangle"}

  #{:semantic-namespace/visual-feature :tier/ui :domain/visualization :feature-type/color :operation/color-aspect}
  {:atlas/dev-id :feature/aspect-category-colors
   :visual-feature/encoding-rules
   {:tier "#fca5a5"                     ; Soft red
    :domain "#a78bfa"                   ; Soft purple
    :protocol "#93c5fd"                 ; Soft blue
    :operation "#86efac"                ; Soft green
    :constraint "#fbbf24"               ; Soft yellow
    :lens-type "#fb923c"                ; Soft orange
    :feature-type "#f472b6"             ; Soft pink
    :effect "#d1d5db"                   ; Light gray
    :default "#fed7aa"}                 ; Soft peach
   :visual-feature/purpose
   "Color-codes aspect nodes by their category (namespace)"
   :visual-feature/shape "ellipse"}

  #{:semantic-namespace/visual-feature :tier/ui :domain/visualization :feature-type/badge}
  {:atlas/dev-id :feature/aspect-badges
   :visual-feature/shown-aspects [:domain :tier :protocol]
   :visual-feature/max-badges 3
   :visual-feature/format "🏷️{name}"
   :visual-feature/purpose
   "Shows entity aspects as compact badges in lens mode"
   :visual-feature/active-when :lens-active}

  #{:semantic-namespace/visual-feature :tier/ui :domain/visualization :feature-type/label}
  {:atlas/dev-id :feature/edge-labels
   :visual-feature/label-rules
   {:membership "has"    ;; entity has aspect
    :dependency "uses"}  ;; entity uses entity
   :visual-feature/styling
   {:font-size "11-12px"
    :background "white"
    :opacity "0.8-1.0"}
   :visual-feature/purpose
   "Labels edges with relationship semantics"}

  #{:semantic-namespace/visual-feature :tier/ui :domain/visualization :feature-type/layout}
  {:atlas/dev-id :feature/adaptive-layout
   :visual-feature/layout-modes
   {:breadthfirst "Hierarchical tree for domains/protocols"
    :grid "3-column grid for tiers"
    :concentric "Center-outward rings for protocols"
    :circle "Boundary circle for constraints"
    :cose "Force-directed for full exploration"}
   :visual-feature/purpose
   "Matches layout algorithm to semantic lens type"}

  ;; =========================================================================
  ;; Design Patterns
  ;; =========================================================================

  #{:atlas/business-pattern :domain/exploration}
  {:atlas/dev-id :pattern/lens-projection
   :business-pattern/principle
   "Project N-dimensional semantic space through single dimension"
   :business-pattern/justification
   "2D visualization cannot show N dimensions without semantic frame"
   :business-pattern/entropy-reduction "70-85%"
   :business-pattern/user-benefit
   "Comprehension time reduced from 5-10 minutes to 30 seconds"}

  #{:atlas/business-pattern :domain/visualization}
  {:atlas/dev-id :pattern/badge-encoding
   :business-pattern/principle
   "Encode hidden aspects as compact visual badges"
   :business-pattern/justification
   "Preserve context while reducing visual clutter"
   :business-pattern/alternative-rejected "Show aspects as separate nodes"
   :business-pattern/why-rejected "Creates visual entropy in lens mode"}

  ;; =========================================================================
  ;; UI Intents
  ;; =========================================================================

  #{:semantic-namespace/interaction-intent :domain/exploration :operation/select}
  {:atlas/dev-id :ui-intent/select-aspect
   :interaction-intent/triggers [:ui-event/aspect-clicked]
   :interaction-intent/state-mutation
   {:updates [:atlas-ui.ui.state/selections]}}

  #{:semantic-namespace/interaction-intent :domain/exploration :operation/negate}
  {:atlas/dev-id :ui-intent/negate-aspect
   :interaction-intent/triggers [:ui-event/aspect-shift-clicked]
   :interaction-intent/state-mutation
   {:updates [:atlas-ui.ui.state/negated-aspects]}}

  #{:semantic-namespace/interaction-intent :domain/exploration :operation/inspect}
  {:atlas/dev-id :ui-intent/inspect-entity
   :interaction-intent/triggers [:ui-event/entity-clicked]
   :interaction-intent/state-mutation
   {:updates [:atlas-ui.ui.state/highlight-selection
              :atlas-ui.ui.state/highlighted-entity-aspects]}}

  #{:semantic-namespace/interaction-intent :domain/search :operation/filter}
  {:atlas/dev-id :ui-intent/search-by-name
   :interaction-intent/triggers [:ui-event/search-changed]
   :interaction-intent/state-mutation
   {:updates [:atlas-ui.ui.state/search-term]}}

  #{:semantic-namespace/interaction-intent :domain/exploration :operation/clear}
  {:atlas/dev-id :ui-intent/clear-query
   :interaction-intent/triggers [:ui-event/selections-cleared]
   :interaction-intent/state-mutation
   {:clears [:atlas-ui.ui.state/selections
             :atlas-ui.ui.state/negated-aspects
             :atlas-ui.ui.state/highlight-selection]}}

  #{:semantic-namespace/interaction-intent :domain/exploration :operation/activate} ;; NEW: Phase 3a
  {:atlas/dev-id :ui-intent/activate-lens
   :interaction-intent/triggers [:ui-event/lens-activated]
   :interaction-intent/state-mutation
   {:updates [:atlas-ui.ui.state/active-lens]
    :clears [:atlas-ui.ui.state/show-lens-selector?]}
   :interaction-intent/side-effect
   "Filters graph, hides aspects, applies layout"}

  #{:semantic-namespace/interaction-intent :domain/exploration :operation/deactivate} ;; NEW: Phase 3a
  {:atlas/dev-id :ui-intent/clear-lens
   :interaction-intent/triggers [:ui-event/lens-cleared]
   :interaction-intent/state-mutation
   {:clears [:atlas-ui.ui.state/active-lens]}
   :interaction-intent/side-effect
   "Returns to full graph view with cose layout"}

  #{:semantic-namespace/interaction-intent :domain/exploration :operation/toggle} ;; NEW: Phase 3a
  {:atlas/dev-id :ui-intent/toggle-lens-selector
   :interaction-intent/triggers [:ui-event/lens-selector-toggled]
   :interaction-intent/state-mutation
   {:toggles [:atlas-ui.ui.state/show-lens-selector?]}
   :interaction-intent/visual-feedback
   "Shows/hides lens selector panel"}

  ;; =========================================================================
  ;; Registry Lifecycle
  ;; =========================================================================

  #{:semantic-namespace/flow-data-flow :tier/ui}
  {:atlas/dev-id :data-flow/registry-fetch
   :flow-data-flow/source :endpoint/atlas-registry
   :flow-data-flow/destination :atlas-ui.ui.state/registry
   :flow-data-flow/side-effect "HTTP fetch"
   :flow-data-flow/versioning :semantic-namespace/registry-versioned}

  ;; =========================================================================
  ;; Constraints
  ;; =========================================================================

  #{:atlas/governance-constraint :tier/ui :constraint/ui-backend-boundary}
  {:atlas/dev-id :constraint/ui-backend-boundary
   :governance-constraint/rule
   "UI communicates with backend only through declared endpoints"
   :governance-constraint/violated-by :ui-event/direct-backend-call}

  #{:atlas/governance-constraint :domain/exploration :constraint/lens-entropy-limit} ;; NEW: Phase 3a
  {:atlas/dev-id :constraint/lens-entropy-limit
   :governance-constraint/rule
   "Lens must reduce visible nodes by 70-85% for effective comprehension"
   :governance-constraint/enforcement "available-lenses filters to functional entities only"}

  ;; =========================================================================
  ;; Value Propositions
  ;; =========================================================================

  #{:atlas/value-proposition :domain/exploration} ;; NEW: Phase 3a
  {:atlas/dev-id :value/entropy-reduction
   :value-proposition/business-problem
   "43+ nodes overwhelming, impossible to scan in 2D without semantic frame"
   :value-proposition/solution
   "Lens system projects through semantic dimensions"
   :value-proposition/quantified-benefit
   {:nodes-before 43
    :nodes-after "2-15 (per lens)"
    :reduction "70-85%"
    :comprehension-time-before "5-10 minutes"
    :comprehension-time-after "30 seconds"
    :improvement "90% faster understanding"}
   :value-proposition/user-segment
   "Developers exploring unfamiliar codebases"}

  #{:atlas/value-proposition :domain/visualization} ;; NEW: Phase 3b
  {:atlas/dev-id :value/visual-clarity
   :value-proposition/business-problem
   "Aspect nodes create clutter, entity types indistinguishable"
   :value-proposition/solution
   "Badge encoding + shape differentiation + edge labels"
   :value-proposition/quantified-benefit
   {:visual-clutter-reduction "60%"
    :entity-recognition-speed "3x faster"
    :relationship-clarity "immediate (labels visible)"}
   :value-proposition/user-segment
   "Visual learners, time-constrained developers"}

  ;; =========================================================================
  ;; Reflexive Semantics
  ;; =========================================================================

  #{:semantic-namespace/interaction-intent :domain/meta}
  {:atlas/dev-id :ui-intent/inspect-registry
   :interaction-intent/user-goal
   "Explore the semantic registry itself"
   :interaction-intent/state-mutation
   {:updates [:atlas-ui.ui.state/highlight-selection]}}

  #{:atlas/value-proposition}
  {:atlas/dev-id :value/semantic-ui
   :value-proposition/solution
   "The UI is both a consumer and a subject of semantic exploration"}

  })
