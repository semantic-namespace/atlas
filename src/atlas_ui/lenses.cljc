(ns atlas-ui.lenses
  "Semantic lenses for reducing visual entropy by filtering registry projections.

  A lens is a focused view through one semantic dimension (domain, tier, protocol, etc.)
  that dramatically reduces the number of visible nodes while maintaining coherence."
  (:require [clojure.set :as set]
            [clojure.string :as str]))

;; =============================================================================
;; Lens Core
;; =============================================================================

(defn extract-aspect-values
  "Extract all values for a given aspect keyword from registry entities.

  Example: (extract-aspect-values registry :domain/*)
           => #{:domain/auth :domain/users :domain/scheduling}"
  [registry aspect-ns]
  (->> registry
       (mapcat (fn [[identity entity]]
                 (filter #(and (keyword? %)
                               (= (namespace %) (name aspect-ns)))
                         identity)))
       (into #{})))

(defn entity-has-aspect?
  "Check if an entity's identity contains the given aspect keyword."
  [identity-set aspect-kw]
  (contains? identity-set aspect-kw))

(defn find-entities-by-aspect
  "Find all entities that have the given aspect in their identity."
  [registry aspect-kw]
  (->> registry
       (filter (fn [[identity entity]]
                 (contains? identity aspect-kw)))
       (map second)
       (into [])))

(defn find-entity-by-id
  "Find a single entity by its :atlas/dev-id."
  [registry dev-id]
  (->> registry
       vals
       (filter #(= (:atlas/dev-id %) dev-id))
       first))

(defn- first-present
  [m keys]
  (some (fn [k]
          (when (contains? m k)
            (get m k)))
        keys))

(defn- find-prop
  [entity keys]
  (or (first-present entity keys)
      (first-present (get entity :props {}) keys)))

(defn- deps-for-entity
  [entity]
  (let [deps (find-prop entity [:interface-endpoint/deps
                                :execution-function/deps
                                :structure-component/deps
                                :semantic-namespace/deps])]
    (cond
      (set? deps) deps
      (coll? deps) (set deps)
      :else #{})))

(defn- triggers-for-entity
  [entity]
  (find-prop entity [:interaction-intent/triggers
                     :semantic-namespace/triggers]))

;; =============================================================================
;; Domain Lens
;; =============================================================================

(defn domain-lens
  "Show all entities within a specific domain.

  Filters to show only entities that have the domain aspect in their identity.
  Uses hierarchical layout to show domain internal structure.

  Example: (domain-lens registry :domain/auth)
           => Shows only auth-related entities"
  [registry domain-kw]
  (let [entities (find-entities-by-aspect registry domain-kw)
        entity-ids (set (map :atlas/dev-id entities))]
    {:type :domain
     :value domain-kw
     :title (str "Domain: " (name domain-kw))
     :entities entities
     :entity-ids entity-ids
     :layout-mode :breadthfirst ;; Hierarchical tree
     :layout-config {:directed true
                     :spacingFactor 1.8
                     :animate true
                     :animationDuration 500
                     :avoidOverlap true
                     :nodeDimensionsIncludeLabels true}
     :show-all-aspects? false
     :description (str "Focused view of " (name domain-kw) " domain")}))

;; =============================================================================
;; Tier Lens
;; =============================================================================

(defn tier-lens
  "Show all entities within a specific tier.

  Filters to show only entities in the given tier.
  Groups by domain within the tier.

  Example: (tier-lens registry :tier/service)
           => Shows all service layer entities grouped by domain"
  [registry tier-kw]
  (let [entities (find-entities-by-aspect registry tier-kw)
        entity-ids (set (map :atlas/dev-id entities))]
    {:type :tier
     :value tier-kw
     :title (str "Tier: " (name tier-kw))
     :entities entities
     :entity-ids entity-ids
     :layout-mode :grid ;; Grid layout for layers
     :layout-config {:cols 3 ;; 3 columns
                     :animate true
                     :animationDuration 500
                     :spacingFactor 1.5
                     :avoidOverlapPadding 50}
     :show-all-aspects? false
     :description (str "All entities in " (name tier-kw) " tier")}))

;; =============================================================================
;; Protocol Lens
;; =============================================================================

(defn protocol-lens
  "Show entities implementing a specific protocol.

  Filters to show protocol definition and all components/functions that implement it.
  Uses concentric layout to show protocol at center with implementers in rings.

  Example: (protocol-lens registry :protocol/oauth)
           => Shows OAuth protocol and all implementers"
  [registry protocol-kw]
  (let [entities (find-entities-by-aspect registry protocol-kw)
        entity-ids (set (map :atlas/dev-id entities))]
    {:type :protocol
     :value protocol-kw
     :title (str "Protocol: " (name protocol-kw))
     :entities entities
     :entity-ids entity-ids
     :layout-mode :concentric ;; Concentric circles (center outward)
     :layout-config {:animate true
                     :animationDuration 500
                     :spacingFactor 2.0
                     :minNodeSpacing 80
                     :avoidOverlap true}
     :show-all-aspects? false
     :description (str "Protocol " (name protocol-kw) " and its implementations")}))

;; =============================================================================
;; UI Intent Lens
;; =============================================================================

(defn intent-lens
  "Show the flow from a UI intent through endpoints to functions.

  Traces the complete path from user intent to implementation.

  Example: (intent-lens registry :ui-intent/query-availability)
           => Shows intent → endpoint → functions flow"
  [registry intent-id]
  (let [intent (find-entity-by-id registry intent-id)
        triggers (triggers-for-entity intent)

        ;; Find triggered endpoints
        endpoints (when triggers
                    (mapcat #(find-entities-by-aspect registry %) triggers))

        ;; Find functions called by endpoints
        functions (when endpoints
                    (mapcat (fn [ep]
                              (let [deps (deps-for-entity ep)]
                                (mapcat #(find-entities-by-aspect registry %) deps)))
                            endpoints))

        all-entities (into [] (concat [intent] endpoints functions))
        entity-ids (set (map :atlas/dev-id all-entities))]
    {:type :ui-intent
     :value intent-id
     :title (str "Intent: " (name intent-id))
     :entities all-entities
     :entity-ids entity-ids
     :layout-mode :breadthfirst ;; Flow from intent to implementation
     :layout-config {:directed true
                     :spacingFactor 1.5
                     :animate true
                     :animationDuration 500}
     :show-all-aspects? false
     :description (str "Flow from " (name intent-id) " to implementation")}))

;; =============================================================================
;; Constraint Lens
;; =============================================================================

(defn constraint-lens
  "Show all entities related to a specific constraint.

  Useful for understanding security boundaries, access control, etc.

  Example: (constraint-lens registry :constraint/admin-only-user-management)
           => Shows all entities enforcing admin access"
  [registry constraint-kw]
  (let [entities (find-entities-by-aspect registry constraint-kw)
        entity-ids (set (map :atlas/dev-id entities))]
    {:type :constraint
     :value constraint-kw
     :title (str "Constraint: " (name constraint-kw))
     :entities entities
     :entity-ids entity-ids
     :layout-mode :circle ;; Circle layout for boundaries
     :layout-config {:animate true
                     :animationDuration 500
                     :spacingFactor 1.5
                     :avoidOverlap true
                     :nodeDimensionsIncludeLabels true}
     :show-all-aspects? false
     :description (str "All entities constrained by " (name constraint-kw))}))

;; =============================================================================
;; Dependency Flow Lens
;; =============================================================================

(defn- collect-dependencies
  "Recursively collect all dependencies from an entity.

  Returns a set of entity IDs that are dependencies (direct and transitive)."
  [registry start-id visited]
  (if (contains? visited start-id)
    visited
    (let [entity (find-entity-by-id registry start-id)
          deps (deps-for-entity entity)
          new-visited (conj visited start-id)]
      (if (seq deps)
        (reduce (fn [acc dep-id]
                  (collect-dependencies registry dep-id acc))
                new-visited
                deps)
        new-visited))))

(defn- collect-dependents
  "Recursively collect all entities that depend on the target entity (reverse deps).

  Returns a set of entity IDs that would be impacted if target changes."
  [registry target-id visited]
  (if (contains? visited target-id)
    visited
    (let [;; Find all entities that list target-id in their deps
          direct-dependents (->> registry
                                 vals
                                 (filter (fn [entity]
                                           (let [deps (deps-for-entity entity)]
                                             (when deps
                                               (cond
                                                 (set? deps) (contains? deps target-id)
                                                 (coll? deps) (some #(= % target-id) deps)
                                                 :else false)))))
                                 (map :atlas/dev-id)
                                 (remove nil?))
          new-visited (conj visited target-id)]
      #?(:cljs (js/console.log "ðŸ”� Impact analysis:"
                               "\nTarget:" (str target-id)
                               "\nDirect dependents:" (clj->js direct-dependents)
                               "\nVisited so far:" (clj->js new-visited)))
      ;; Recursively collect dependents of dependents (ripple effect)
      (reduce (fn [acc dependent-id]
                (collect-dependents registry dependent-id acc))
              new-visited
              direct-dependents))))

(defn dependency-flow-lens
  "Show dependency flow from a specific entity.

  Traces all dependencies (direct and transitive) from the starting entity,
  showing the complete call chain.

  Example: (dependency-flow-lens registry :endpoint/oauth-callback)
           => Shows endpoint → functions → components it depends on"
  [registry start-id]
  (let [;; Collect all entities in the dependency chain
        entity-ids (collect-dependencies registry start-id #{})
        entities (->> registry
                      vals
                      (filter #(contains? entity-ids (:atlas/dev-id %)))
                      (into []))
        start-entity (find-entity-by-id registry start-id)]
    {:type :dependency-flow
     :value start-id
     :title (str "Flow from: " (name start-id))
     :entities entities
     :entity-ids entity-ids
     :layout-mode :breadthfirst ;; Hierarchical flow
     :layout-config {:directed true
                     :spacingFactor 1.8
                     :animate true
                     :animationDuration 500
                     :roots [(str start-id)] ;; Start from the selected entity
                     :avoidOverlap true}
     :show-all-aspects? false
     :description (str "Dependency chain from " (name start-id))}))

;; =============================================================================
;; Impact Analysis Lens
;; =============================================================================

(defn impact-analysis-lens
  "Show impact analysis - what would break if this entity changes?

  Traces all entities that depend on the target (direct and transitive),
  showing the complete ripple effect.

  Example: (impact-analysis-lens registry :component/db)
           => Shows all functions/endpoints that use the DB component"
  [registry target-id]
  (let [;; Collect all entities that depend on the target
        entity-ids (collect-dependents registry target-id #{})
        entities (->> registry
                      vals
                      (filter #(contains? entity-ids (:atlas/dev-id %)))
                      (into []))
        target-entity (find-entity-by-id registry target-id)
        impact-count (dec (count entity-ids)) ;; Exclude target itself
        impact-score (cond
                       (= impact-count 0) :none
                       (<= impact-count 3) :low
                       (<= impact-count 10) :medium
                       (<= impact-count 20) :high
                       :else :critical)]
    {:type :impact-analysis
     :value target-id
     :title (str "Impact: " (name target-id))
     :entities entities
     :entity-ids entity-ids
     :impact-count impact-count
     :impact-score impact-score
     :layout-mode :breadthfirst ;; Hierarchical - target at root
     :layout-config {:directed true
                     :spacingFactor 2.0
                     :animate true
                     :animationDuration 500
                     :roots [(str target-id)]
                     :avoidOverlap true}
     :show-all-aspects? false
     :description (str "Impact analysis: " impact-count " entities affected")}))

;; =============================================================================
;; Available Lenses
;; =============================================================================

(defn available-lenses
  "Extract all available lens options from the registry.

  Returns a map of lens categories to available values.

  Only extracts from FUNCTIONAL ENTITIES (those with semantic markers like
  :semantic-namespace/function, :component, :endpoint, etc.), not aspect nodes.
  This prevents showing aspect metadata as if they were entity domains.

  Registry format: {identity-set entity-map, ...}"
  [registry]
  (let [;; Semantic markers that indicate a functional entity
        semantic-markers #{:atlas/execution-function
                           :atlas/structure-component
                           :atlas/interface-endpoint
                           :atlas/interface-protocol
                           :semantic-namespace/interaction-intent
                           :atlas/governance-constraint}

        ;; Filter to only functional entities (have at least one semantic marker)
        entities (->> registry
                      (filter (fn [[identity entity]]
                              ;; Check if identity contains any semantic marker
                                (some #(contains? identity %) semantic-markers)))
                      (into {}))

        ui-intent-aspect :semantic-namespace/interaction-intent

        ;; Extract lens values
        result {:domains (extract-aspect-values entities :domain)
                :tiers (extract-aspect-values entities :tier)
                :protocols (extract-aspect-values entities :protocol)
                :constraints (extract-aspect-values entities :constraint)
                :ui-intents (->> entities
                                 (filter (fn [[identity entity]]
                                          ;; Check if identity contains the ui-intent aspect
                                           (contains? identity ui-intent-aspect)))
                                 (map second)
                                 (keep :atlas/dev-id)
                                 (into #{}))}]

    ;; DEBUG LOGGING - see what we're actually extracting
    #?(:cljs (js/console.log "📊 Available lenses:"
                             "\nDomains (" (count (:domains result)) "):" (clj->js (:domains result))
                             "\nTiers (" (count (:tiers result)) "):" (clj->js (:tiers result))
                             "\nProtocols (" (count (:protocols result)) "):" (clj->js (:protocols result))))

    result))

;; =============================================================================
;; Lens Combinations
;; =============================================================================

(declare apply-lens) ;; Forward declaration

(defn combine-lenses
  "Combine multiple lens results using intersection.

  Returns a combined lens result showing only entities that appear in ALL lenses.

  Example: (combine-lenses registry [{:type :domain :value :domain/auth}
                                       {:type :tier :value :tier/service}])
           => Shows only auth domain entities in service tier"
  [registry lens-specs]
  (when (seq lens-specs)
    (let [;; Apply each lens individually
          lens-results (mapv #(apply-lens registry %) lens-specs)

          ;; Intersect all entity-id sets
          combined-entity-ids (reduce set/intersection
                                      (map :entity-ids lens-results))

          ;; Get entities that are in the intersection
          all-entities (mapcat :entities lens-results)
          combined-entities (filter #(contains? combined-entity-ids (:atlas/dev-id %))
                                    all-entities)

          ;; Build combined title
          titles (mapv :title lens-results)
          combined-title (str "Combined: " (str/join " ∩ " titles))]

      {:type :combination
       :lens-specs lens-specs
       :title combined-title
       :entities (vec combined-entities)
       :entity-ids combined-entity-ids
       :layout-mode :cose ;; Force-directed for combinations
       :layout-config {:animate true
                       :animationDuration 500
                       :nodeRepulsion 4000
                       :idealEdgeLength 100}
       :show-all-aspects? false
       :description (str "Intersection of " (count lens-specs) " lenses")})))

;; =============================================================================
;; Apply Lens
;; =============================================================================

(defn apply-lens
  "Apply a lens specification to the registry.

  Returns a lens result with filtered entities and metadata.
  If no lens is active, returns nil (full explorer mode).

  Lens spec format:
    Single lens: {:type :domain :value :domain/auth}
    Combination: {:type :combination :lenses [{:type :domain :value :domain/auth}
                                                {:type :tier :value :tier/service}]}
    Flow: {:type :dependency-flow :value :endpoint/some-endpoint}

  Example: (apply-lens registry {:type :domain :value :domain/auth})
           => {:type :domain
               :value :domain/auth
               :title \"Domain: auth\"
               :entities [...]
               :entity-ids #{...}
               :layout-mode :hierarchical
               :show-all-aspects? false}"
  [registry lens-spec]
  (when lens-spec
    (case (:type lens-spec)
      :domain (domain-lens registry (:value lens-spec))
      :tier (tier-lens registry (:value lens-spec))
      :protocol (protocol-lens registry (:value lens-spec))
      :ui-intent (intent-lens registry (:value lens-spec))
      :constraint (constraint-lens registry (:value lens-spec))
      :combination (combine-lenses registry (:lenses lens-spec))
      :dependency-flow (dependency-flow-lens registry (:value lens-spec))
      :impact-analysis (impact-analysis-lens registry (:value lens-spec))
      nil)))

(defn lens-active?
  "Check if a lens is currently active."
  [lens-spec]
  (some? lens-spec))

(defn should-show-entity?
  "Determine if an entity should be shown given the current lens.

  If no lens is active, show all entities.
  If lens is active, only show entities in the lens's entity-ids set."
  [entity lens-result]
  (if lens-result
    (contains? (:entity-ids lens-result) (:atlas/dev-id entity))
    true))
