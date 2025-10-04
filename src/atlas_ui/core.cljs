(ns atlas-ui.core
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [atlas.query :as q]
            [atlas-ui.api :as api]
            [atlas-ui.graph :as graph]
            [atlas-ui.graph-data :as gd]
            [atlas-ui.sidebar :as sidebar]
            [atlas-ui.selection-controls :as controls]
            [atlas-ui.search-bar :as search]
            [atlas-ui.color-legend :as legend]
            [atlas-ui.lenses :as lenses]))

;; =============================================================================
;; State
;; =============================================================================
(declare update-url-with-lens!)

(defonce app-state
  (r/atom
   {:atlas-ui.ui.state/registry {}
    :atlas-ui.ui.state/selections #{} ; SET of selected aspect keywords
    :atlas-ui.ui.state/negated-aspects #{} ; SET of negated aspect keywords (NOT)
    :atlas-ui.ui.state/query-mode :count ; :and | :or | :count
    :atlas-ui.ui.state/min-score 0.0 ; Minimum score threshold when filtering (0.0-1.0)
    :atlas-ui.ui.state/highlight-selection nil ; For sidebar detail view
    :atlas-ui.ui.state/highlighted-entity-aspects #{} ; Aspects of currently highlighted entity
    :atlas-ui.ui.state/highlight-aspect nil
    :atlas-ui.ui.state/legend-focus nil
    :atlas-ui.ui.state/hide-unmatched? false ; Hide nodes with low match scores
    :atlas-ui.ui.state/search-term "" ; Filter entities by name
    :atlas-ui.ui.state/selected-types #{} ; SET of selected entity types for filtering
    :atlas-ui.ui.state/loading? true
    :atlas-ui.ui.state/error nil
    :atlas-ui.ui.state/registry-version nil ; Registry version for cache invalidation
    :atlas-ui.ui.state/active-lens nil ; Current lens: {:type :domain :value :domain/auth} or nil
    :atlas-ui.ui.state/show-lens-selector? false
    ;; Phase 3c: Lens History
    :atlas-ui.ui.state/lens-history [] ; Vector of lens specs (stack)
    :atlas-ui.ui.state/lens-history-index -1 ; Current position in history (-1 = no history)
    ;; Phase 3c: Lens Presets
    :atlas-ui.ui.state/lens-presets {} ; Map of preset-id -> {:name "..." :lens-spec {...}}
    :atlas-ui.ui.state/show-presets? false
    ;; Phase 3d: Combination builder
    :atlas-ui.ui.state/combination-lenses []}))

;; Derived graph data
(defonce graph-data
  (r/reaction
   (gd/build-graph-data (:atlas-ui.ui.state/registry @app-state))))

;; Derived lens result (computed when lens is active)
(defonce lens-result
  (r/reaction
   (let [active-lens (:atlas-ui.ui.state/active-lens @app-state)
         registry (:atlas-ui.ui.state/registry @app-state)]
     (when (and active-lens (seq registry))
       (lenses/apply-lens registry active-lens)))))

;; Derived filtered graph data (applies lens filter and type filter)
(defonce filtered-graph-data
  (r/reaction
   (let [lens @lens-result
         graph @graph-data
         selected-types (:atlas-ui.ui.state/selected-types @app-state)
         ;; Helper to check if entity matches type filter
         type-filter-fn (if (seq selected-types)
                          (fn [entity]
                            (contains? selected-types (:atlas-ui.graph.node/entity-type entity)))
                          (constantly true))]
     (if lens
       ;; Lens active - filter entities and show aspects as badges
       (let [entity-ids (:entity-ids lens)
             filtered-entities (filter #(and (contains? entity-ids (:atlas-ui.graph.node/id %))
                                             (type-filter-fn %))
                                       (:atlas-ui.graph.data/entities graph))
             filtered-entity-ids (set (map :atlas-ui.graph.node/id filtered-entities))

             ;; For lens mode: collect aspects from filtered entities as "badges"
             ;; These will be shown as small labels on entity nodes
             entity-aspects (reduce (fn [acc entity]
                                      (into acc (:atlas-ui.graph.node/identity entity)))
                                    #{}
                                    filtered-entities)

             ;; Create badge info for each entity (list of its aspects)
             entities-with-badges (mapv (fn [entity]
                                          (assoc entity :atlas-ui.graph.node/aspect-badges
                                                 (vec (filter #(or (= "domain" (namespace %))
                                                                   (= "tier" (namespace %))
                                                                   (= "protocol" (namespace %)))
                                                              (:atlas-ui.graph.node/identity entity)))))
                                        filtered-entities)

             ;; When lens is active, hide aspect nodes but keep aspect info as badges
             filtered-aspects []

             ;; Filter edges to only show edges between visible entities
             filtered-edges (filter (fn [edge]
                                      (and (contains? filtered-entity-ids (:atlas-ui.graph.edge/source edge))
                                           (contains? filtered-entity-ids (:atlas-ui.graph.edge/target edge))))
                                    (:atlas-ui.graph.data/edges graph))

             ;; CRITICAL: Update all-nodes to only include filtered entities (with badges)
             filtered-all-nodes entities-with-badges]
         (assoc graph
                :atlas-ui.graph.data/entities entities-with-badges
                :atlas-ui.graph.data/aspects filtered-aspects
                :atlas-ui.graph.data/all-nodes filtered-all-nodes
                :atlas-ui.graph.data/edges filtered-edges))
       ;; No lens - apply type filter only
       (if (seq selected-types)
         (let [filtered-entities (filter type-filter-fn (:atlas-ui.graph.data/entities graph))
               filtered-entity-ids (set (map :atlas-ui.graph.node/id filtered-entities))
               aspect-ids (set (map :atlas-ui.graph.node/id (:atlas-ui.graph.data/aspects graph)))
               ;; Keep edges where:
               ;; 1. Source is a filtered entity AND target is an aspect (membership edges)
               ;; 2. Both source and target are filtered entities (dependency edges)
               filtered-edges (filter (fn [edge]
                                        (let [source (:atlas-ui.graph.edge/source edge)
                                              target (:atlas-ui.graph.edge/target edge)]
                                          (and (contains? filtered-entity-ids source)
                                               (or (contains? aspect-ids target)
                                                   (contains? filtered-entity-ids target)))))
                                      (:atlas-ui.graph.data/edges graph))]
           (assoc graph
                  :atlas-ui.graph.data/entities filtered-entities
                  :atlas-ui.graph.data/all-nodes (concat filtered-entities (:atlas-ui.graph.data/aspects graph))
                  :atlas-ui.graph.data/edges filtered-edges))
         graph)))))

;; Derived match statistics
(defonce match-stats
  (r/reaction
   (let [selections (:atlas-ui.ui.state/selections @app-state)
         negated-aspects (:atlas-ui.ui.state/negated-aspects @app-state)
         query-mode (:atlas-ui.ui.state/query-mode @app-state)
         min-score (:atlas-ui.ui.state/min-score @app-state)
         hide-unmatched? (:atlas-ui.ui.state/hide-unmatched? @app-state)
         entities (:atlas-ui.graph.data/entities @filtered-graph-data)] ;; Use filtered data
     (when (or (seq selections) (seq negated-aspects))
       (let [query {::q/selected selections
                    ::q/negated negated-aspects
                    ::q/mode query-mode
                    ::q/min-score (if hide-unmatched? min-score 0.0)}
             matches (filter #(q/query-matches? (:atlas-ui.graph.node/identity %) query) entities)]
         {:atlas-ui.stats/total (count entities)
          :atlas-ui.stats/matching (count matches)
          :atlas-ui.stats/percentage (if (pos? (count entities))
                                       (int (* 100 (/ (count matches) (count entities))))
                                       0)})))))

;; =============================================================================
;; Event Handlers
;; =============================================================================

(defn handle-registry-loaded [response]
  (js/console.log "=== HANDLER CALLED (Registry) ===")
  (js/console.log "Registry count:" (count (:atlas-ui.api.response/registry response)))
  (swap! app-state assoc
         :atlas-ui.ui.state/registry (:atlas-ui.api.response/registry response)
         :atlas-ui.ui.state/loading? false
         :atlas-ui.ui.state/error nil))

(defn- toggle-set-entry [s x]
  (let [s (if (set? s) s #{})]
    (if (contains? s x)
      (disj s x)
      (conj s x))))

(defn handle-toggle-aspect
  "Toggle an aspect in the selections or negations set.

  - Click toggles positive selection.
  - Shift+click toggles negation (NOT)."
  ([aspect-id]
   (handle-toggle-aspect aspect-id false))
  ([aspect-id shift-key?]
   (js/console.log "ðŸŽ¯ Toggling aspect:" aspect-id "shift:" shift-key?)
   (swap! app-state
          (fn [state]
            (if shift-key?
              (-> state
                  (update :atlas-ui.ui.state/negated-aspects toggle-set-entry aspect-id)
                  (update :atlas-ui.ui.state/selections disj aspect-id))
              (-> state
                  (update :atlas-ui.ui.state/selections toggle-set-entry aspect-id)
                  (update :atlas-ui.ui.state/negated-aspects disj aspect-id)))))))

(defn handle-legend-focus
  "Toggle focus based on legend selection."
  [focus]
  (swap! app-state
         (fn [state]
           (if (= (:atlas-ui.ui.state/legend-focus state) focus)
             (assoc state
                    :atlas-ui.ui.state/legend-focus nil
                    :atlas-ui.ui.state/highlight-aspect nil)
             (assoc state
                    :atlas-ui.ui.state/legend-focus focus
                    :atlas-ui.ui.state/highlight-selection nil
                    :atlas-ui.ui.state/highlighted-entity-aspects #{}
                    :atlas-ui.ui.state/highlight-aspect nil)))))

(defn handle-click-aspect
  "Toggle aspect selection and keep its relations pinned."
  ([aspect-id]
   (handle-click-aspect aspect-id false))
  ([aspect-id shift-key?]
   (if (nil? aspect-id)
     (swap! app-state assoc :atlas-ui.ui.state/highlight-aspect nil)
     (swap! app-state
            (fn [state]
              (let [updated (if shift-key?
                              (-> state
                                  (update :atlas-ui.ui.state/negated-aspects toggle-set-entry aspect-id)
                                  (update :atlas-ui.ui.state/selections disj aspect-id))
                              (-> state
                                  (update :atlas-ui.ui.state/selections toggle-set-entry aspect-id)
                                  (update :atlas-ui.ui.state/negated-aspects disj aspect-id)))
                    selected? (contains? (:atlas-ui.ui.state/selections updated) aspect-id)
                    negated? (contains? (:atlas-ui.ui.state/negated-aspects updated) aspect-id)
                    active? (or selected? negated?)]
                (cond
                  active?
                  (assoc updated
                         :atlas-ui.ui.state/highlight-aspect aspect-id
                         :atlas-ui.ui.state/highlight-selection nil
                         :atlas-ui.ui.state/highlighted-entity-aspects #{}
                         :atlas-ui.ui.state/legend-focus nil)
                  (= (:atlas-ui.ui.state/highlight-aspect state) aspect-id)
                  (assoc updated :atlas-ui.ui.state/highlight-aspect nil)
                  :else updated)))))))

(defn handle-click-entity [entity]
  "Show entity details in sidebar and highlight its aspects (toggle on re-click)"
  (let [current-highlight (:atlas-ui.ui.state/highlight-selection @app-state)]
    (if (and entity current-highlight (= (:atlas-ui.graph.node/id entity) (:atlas-ui.graph.node/id current-highlight)))
      ;; Clicking same entity again - deselect
      (do
        (js/console.log "Deselecting entity")
        (swap! app-state assoc
               :atlas-ui.ui.state/highlight-selection nil
               :atlas-ui.ui.state/highlighted-entity-aspects #{}
               :atlas-ui.ui.state/highlight-aspect nil))
      ;; New entity or different entity
      (if entity
        (do
          (js/console.log "Showing entity details:" (:atlas-ui.graph.node/id entity))
          (js/console.log "   Aspects:" (clj->js (:atlas-ui.graph.node/identity entity)))
          (swap! app-state assoc
                 :atlas-ui.ui.state/highlight-selection entity
                 :atlas-ui.ui.state/highlighted-entity-aspects (or (:atlas-ui.graph.node/identity entity) #{})
                 :atlas-ui.ui.state/legend-focus nil
                 :atlas-ui.ui.state/highlight-aspect nil))
        ;; Clear highlight when clicking background
        (swap! app-state assoc
               :atlas-ui.ui.state/highlight-selection nil
               :atlas-ui.ui.state/highlighted-entity-aspects #{}
               :atlas-ui.ui.state/highlight-aspect nil)))))

(defn handle-clear-selections []
  "Clear all aspect selections"
  (swap! app-state assoc
         :atlas-ui.ui.state/selections #{}
         :atlas-ui.ui.state/negated-aspects #{}
         :atlas-ui.ui.state/min-score 0.0
         :atlas-ui.ui.state/hide-unmatched? false
         :atlas-ui.ui.state/highlight-selection nil
         :atlas-ui.ui.state/highlighted-entity-aspects #{}
         :atlas-ui.ui.state/highlight-aspect nil
         :atlas-ui.ui.state/legend-focus nil))

(defn handle-set-query-mode [mode]
  "Change query mode: :and | :or | :count"
  (js/console.log "ðŸ”„ Query mode:" mode)
  (swap! app-state assoc :atlas-ui.ui.state/query-mode mode))

(defn handle-toggle-visibility []
  "Toggle hiding of unmatched nodes"
  (swap! app-state update :atlas-ui.ui.state/hide-unmatched? not))

(defn handle-set-min-score [min-score]
  (swap! app-state assoc :atlas-ui.ui.state/min-score min-score))

(defn handle-search-change [term]
  "Update search term"
  (swap! app-state assoc :atlas-ui.ui.state/search-term term))

(defn handle-toggle-type [entity-type]
  "Toggle entity type in the selected-types filter"
  (js/console.log "🔀 Toggle type:" entity-type)
  (swap! app-state update :atlas-ui.ui.state/selected-types
         (fn [types]
           (if (contains? types entity-type)
             (disj types entity-type)
             (conj types entity-type)))))

(defn- push-to-history!
  "Push current lens to history, clearing any forward history"
  [lens-spec]
  (when lens-spec
    (swap! app-state
           (fn [state]
             (let [history (:atlas-ui.ui.state/lens-history state)
                   index (:atlas-ui.ui.state/lens-history-index state)
                   ;; Clear forward history and append new lens
                   new-history (conj (subvec history 0 (inc index)) lens-spec)
                   new-index (dec (count new-history))]
               (assoc state
                      :atlas-ui.ui.state/lens-history new-history
                      :atlas-ui.ui.state/lens-history-index new-index))))))

(defn handle-activate-lens! [lens-type lens-value]
  "Activate a lens filter on the graph"
  (js/console.log "🔍 Activating lens:" lens-type lens-value)
  (let [lens-spec (if (= lens-type :combination)
                   ;; For combination, lens-value is already a map with :lenses
                    (assoc lens-value :type :combination)
                   ;; For other types, create standard lens-spec
                    {:type lens-type :value lens-value})]
    (push-to-history! lens-spec)
    (update-url-with-lens! lens-spec) ;; Update URL for sharing
    (swap! app-state assoc
           :atlas-ui.ui.state/active-lens lens-spec
           :atlas-ui.ui.state/show-lens-selector? false
           :atlas-ui.ui.state/combination-lenses []) ;; Clear combination builder
    ;; Log filtered data after state update
    (js/setTimeout
     #(let [filtered @filtered-graph-data]
        (js/console.log "🔍 Lens filtered to:"
                        (count (:atlas-ui.graph.data/entities filtered)) "entities,"
                        (count (:atlas-ui.graph.data/aspects filtered)) "aspects,"
                        (count (:atlas-ui.graph.data/all-nodes filtered)) "total nodes"))
     100)))

(defn handle-clear-lens! []
  "Clear active lens and return to full explorer mode"
  (js/console.log "🗺️ Clearing lens - returning to explorer mode")
  (update-url-with-lens! nil) ;; Clear URL params
  (swap! app-state assoc :atlas-ui.ui.state/active-lens nil))

(defn handle-toggle-lens-selector! []
  "Toggle visibility of lens selector panel"
  (swap! app-state update :atlas-ui.ui.state/show-lens-selector? not))

;; =============================================================================
;; Lens History Handlers (Phase 3c)
;; =============================================================================

(defn handle-lens-back! []
  "Navigate backward in lens history"
  (let [state @app-state
        index (:atlas-ui.ui.state/lens-history-index state)]
    (when (> index 0)
      (let [new-index (dec index)
            history (:atlas-ui.ui.state/lens-history state)
            prev-lens (get history new-index)]
        (js/console.log "⬅️  Going back in history to:" prev-lens)
        (swap! app-state assoc
               :atlas-ui.ui.state/lens-history-index new-index
               :atlas-ui.ui.state/active-lens prev-lens)))))

(defn handle-lens-forward! []
  "Navigate forward in lens history"
  (let [state @app-state
        index (:atlas-ui.ui.state/lens-history-index state)
        history (:atlas-ui.ui.state/lens-history state)]
    (when (< index (dec (count history)))
      (let [new-index (inc index)
            next-lens (get history new-index)]
        (js/console.log "➡️  Going forward in history to:" next-lens)
        (swap! app-state assoc
               :atlas-ui.ui.state/lens-history-index new-index
               :atlas-ui.ui.state/active-lens next-lens)))))

;; =============================================================================
;; URL Sharing (Phase 3d)
;; =============================================================================

(defn lens-spec-to-url-params
  "Encode lens spec to URL parameters"
  [lens-spec]
  (when lens-spec
    (let [type (name (:type lens-spec))
          value (if (keyword? (:value lens-spec))
                  (str (namespace (:value lens-spec)) "/" (name (:value lens-spec)))
                  (str (:value lens-spec)))]
      (str "?lens=" type "&value=" (js/encodeURIComponent value)))))

(defn url-params-to-lens-spec
  "Decode URL parameters to lens spec"
  []
  (let [params (js/URLSearchParams. js/window.location.search)
        lens-type (.get params "lens")
        lens-value (.get params "value")]
    (when (and lens-type lens-value)
      {:type (keyword lens-type)
       :value (keyword lens-value)})))

(defn update-url-with-lens!
  "Update browser URL with current lens (without reload)"
  [lens-spec]
  (let [url-params (lens-spec-to-url-params lens-spec)
        new-url (str js/window.location.pathname (or url-params ""))]
    (.pushState js/window.history nil "" new-url)))

(defn copy-lens-url-to-clipboard!
  "Copy current lens URL to clipboard"
  []
  (let [lens-spec (:atlas-ui.ui.state/active-lens @app-state)
        url-params (lens-spec-to-url-params lens-spec)
        full-url (str js/window.location.origin js/window.location.pathname url-params)]
    (when lens-spec
      (.. js/navigator.clipboard (writeText full-url))
      (js/console.log "📋 Copied lens URL to clipboard:" full-url)
      (js/alert "Lens URL copied to clipboard!"))))

(defn load-lens-from-url!
  "Load lens from URL parameters on page load"
  []
  (let [lens-spec (url-params-to-lens-spec)]
    (when lens-spec
      (js/console.log "🔗 Loading lens from URL:" lens-spec)
      (push-to-history! lens-spec)
      (swap! app-state assoc :atlas-ui.ui.state/active-lens lens-spec))))

;; =============================================================================
;; Lens Preset Handlers (Phase 3c)
;; =============================================================================

(defn handle-save-preset! [preset-name]
  "Save current lens as a preset"
  (let [active-lens (:atlas-ui.ui.state/active-lens @app-state)]
    (when active-lens
      (let [preset-id (keyword (str "preset-" (random-uuid)))]
        (swap! app-state assoc-in
               [:atlas-ui.ui.state/lens-presets preset-id]
               {:name preset-name
                :lens-spec active-lens})
        (js/console.log "💾 Saved preset:" preset-name)))))

(defn handle-load-preset! [preset-id]
  "Load a saved preset"
  (let [presets (:atlas-ui.ui.state/lens-presets @app-state)
        preset (get presets preset-id)]
    (when preset
      (js/console.log "📂 Loading preset:" (:name preset))
      (let [lens-spec (:lens-spec preset)]
        (push-to-history! lens-spec)
        (swap! app-state assoc
               :atlas-ui.ui.state/active-lens lens-spec
               :atlas-ui.ui.state/show-presets? false)))))

(defn handle-delete-preset! [preset-id]
  "Delete a saved preset"
  (swap! app-state update :atlas-ui.ui.state/lens-presets dissoc preset-id)
  (js/console.log "🗑️  Deleted preset:" preset-id))

(defn handle-toggle-presets! []
  "Toggle visibility of presets panel"
  (swap! app-state update :atlas-ui.ui.state/show-presets? not))

(defn refresh-registry! []
  (js/console.log "Refreshing registry...")
  (swap! app-state assoc :atlas-ui.ui.state/loading? true :atlas-ui.ui.state/error nil)
  (api/fetch-registry-with-fallback! handle-registry-loaded))

;; =============================================================================
;; Components
;; =============================================================================

(defn header-component []
  (let [state @app-state
        has-lens? (:atlas-ui.ui.state/active-lens state)
        history (:atlas-ui.ui.state/lens-history state)
        history-index (:atlas-ui.ui.state/lens-history-index state)
        can-go-back? (> history-index 0)
        can-go-forward? (< history-index (dec (count history)))]
    [:div.header
     {:style {:padding "16px 24px"
              :backgroundColor "#111827"
              :color "#ffffff"
              :display "flex"
              :justifyContent "space-between"
              :alignItems "center"
              :boxShadow "0 1px 3px rgba(0, 0, 0, 0.1)"}}

     [:div
      [:h1 {:style {:margin "0"
                    :fontSize "20px"
                    :fontWeight "600"
                    :letterSpacing "-0.025em"}}
       "Atlas Viewer"]
      [:p {:style {:margin "2px 0 0 0"
                   :fontSize "12px"
                   :color "#9ca3af"}}
       "Multi-Aspect Query Explorer"]]

     [:div {:style {:display "flex"
                    :alignItems "center"
                    :gap "12px"}}
      (when (:atlas-ui.ui.state/loading? state)
        [:span {:style {:fontSize "12px"
                        :color "#9ca3af"}}
         "Loading..."])

      ;; History navigation buttons
      (when has-lens?
        [:div {:style {:display "flex"
                       :gap "4px"
                       :marginRight "8px"}}
         [:button
          {:on-click handle-lens-back!
           :disabled (not can-go-back?)
           :title "Go back in lens history ([)"
           :style {:padding "6px 10px"
                   :backgroundColor (if can-go-back? "#374151" "#1f2937")
                   :color (if can-go-back? "#ffffff" "#6b7280")
                   :border "none"
                   :borderRadius "4px"
                   :fontSize "14px"
                   :cursor (if can-go-back? "pointer" "not-allowed")
                   :opacity (if can-go-back? "1" "0.5")}}
          "◀"]
         [:button
          {:on-click handle-lens-forward!
           :disabled (not can-go-forward?)
           :title "Go forward in lens history (])"
           :style {:padding "6px 10px"
                   :backgroundColor (if can-go-forward? "#374151" "#1f2937")
                   :color (if can-go-forward? "#ffffff" "#6b7280")
                   :border "none"
                   :borderRadius "4px"
                   :fontSize "14px"
                   :cursor (if can-go-forward? "pointer" "not-allowed")
                   :opacity (if can-go-forward? "1" "0.5")}}
          "▶"]])

      (when has-lens?
        [:button
         {:on-click copy-lens-url-to-clipboard!
          :title "Copy lens URL to clipboard (Ctrl/Cmd+C)"
          :style {:padding "8px 16px"
                  :backgroundColor "#8b5cf6"
                  :color "#ffffff"
                  :border "none"
                  :borderRadius "6px"
                  :fontSize "13px"
                  :fontWeight "500"
                  :cursor "pointer"
                  :transition "all 0.2s"
                  :boxShadow "0 1px 2px rgba(0, 0, 0, 0.05)"}}
         "🔗 Share"])

      [:button
       {:on-click handle-toggle-presets!
        :title "Manage presets (P)"
        :style {:padding "8px 16px"
                :backgroundColor "#10b981"
                :color "#ffffff"
                :border "none"
                :borderRadius "6px"
                :fontSize "13px"
                :fontWeight "500"
                :cursor "pointer"
                :transition "all 0.2s"
                :boxShadow "0 1px 2px rgba(0, 0, 0, 0.05)"}}
       "💾 Presets"]

      [:button
       {:on-click handle-toggle-lens-selector!
        :title "Choose lens (L)"
        :style {:padding "8px 16px"
                :backgroundColor (if has-lens? "#3b82f6" "#6b7280")
                :color "#ffffff"
                :border "none"
                :borderRadius "6px"
                :fontSize "13px"
                :fontWeight "500"
                :cursor "pointer"
                :transition "all 0.2s"
                :boxShadow "0 1px 2px rgba(0, 0, 0, 0.05)"}}
       (if has-lens? "🔍 Lens Active" "🔍 Choose Lens")]

      [:button
       {:on-click refresh-registry!
        :style {:padding "8px 16px"
                :backgroundColor "#7c3aed"
                :color "#ffffff"
                :border "none"
                :borderRadius "6px"
                :fontSize "13px"
                :fontWeight "500"
                :cursor "pointer"
                :transition "background-color 0.2s"
                :boxShadow "0 1px 2px rgba(0, 0, 0, 0.05)"}}
       "↻ Refresh"]]]))

(defn info-panel []
  "Info panel showing stats and query results"
  (let [entities (:atlas-ui.graph.data/entities @filtered-graph-data) ;; Use filtered data
        aspects (:atlas-ui.graph.data/aspects @filtered-graph-data)
        edges (:atlas-ui.graph.data/edges @filtered-graph-data)
        selections (:atlas-ui.ui.state/selections @app-state)
        active-lens (:atlas-ui.ui.state/active-lens @app-state)
        lens @lens-result
        stats @match-stats]
    [:div.info-panel
     {:style {:position "absolute"
              :bottom "20px"
              :left "20px"
              :backgroundColor "rgba(255, 255, 255, 0.95)"
              :backdropFilter "blur(8px)"
              :padding "16px"
              :borderRadius "8px"
              :boxShadow "0 4px 6px rgba(0, 0, 0, 0.1)"
              :fontSize "12px"
              :color "#6b7280"
              :zIndex 1000
              :minWidth "200px"}}
     [:div {:style {:marginBottom "8px"
                    :fontWeight "600"
                    :color "#374151"}}
      "Registry Statistics"]

     (when lens
       [:div {:style {:marginBottom "12px"
                      :padding "8px"
                      :backgroundColor "#dbeafe"
                      :borderRadius "6px"
                      :border "1px solid #3b82f6"}}
        [:div {:style {:fontSize "11px"
                       :fontWeight "600"
                       :color "#1e40af"
                       :marginBottom "4px"}}
         "🔍 LENS ACTIVE"]
        [:div {:style {:fontSize "10px"
                       :color "#1e3a8a"}}
         (:title lens)]
        [:div {:style {:fontSize "9px"
                       :color "#6b7280"
                       :marginTop "2px"}}
         (count entities) " entities visible"]])

     [:div "Entities: " (count entities)]
     [:div "Aspects: " (count aspects)]
     [:div "Edges: " (count edges)]

     (when stats
       [:div {:style {:marginTop "12px"
                      :paddingTop "12px"
                      :borderTop "1px solid #e5e7eb"}}
        [:div {:style {:fontWeight "600"
                       :color "#374151"
                       :marginBottom "4px"}}
         "Query Results"]
        [:div {:style {:color "#059669"
                       :fontWeight "500"}}
         (:atlas-ui.stats/matching stats) " / " (:atlas-ui.stats/total stats) " matching"]
        [:div {:style {:fontSize "10px"
                       :color "#9ca3af"
                       :marginTop "2px"}}
         (:atlas-ui.stats/percentage stats) "% match"]])

     [:div {:style {:marginTop "12px"
                    :paddingTop "12px"
                    :borderTop "1px solid #e5e7eb"
                    :fontSize "11px"}}
      [:div "💡 Click aspects to build query"]
      [:div {:style {:marginTop "4px"}}
       "🎨 Color intensity = match score"]
      [:div {:style {:marginTop "4px"}}
       "⌨️  ESC to clear"]]]))

(defn lens-banner []
  "Banner showing active lens at top of graph"
  (let [lens @lens-result
        impact-score (:impact-score lens)
        impact-count (:impact-count lens)]
    (when lens
      [:div.lens-banner
       {:style {:position "absolute"
                :top "20px"
                :left "50%"
                :transform "translateX(-50%)"
                :backgroundColor (case (:type lens)
                                   :impact-analysis "rgba(239, 68, 68, 0.95)" ;; Red for impact
                                   "rgba(59, 130, 246, 0.95)") ;; Blue default
                :backdropFilter "blur(8px)"
                :color "white"
                :padding "12px 24px"
                :borderRadius "12px"
                :boxShadow (case (:type lens)
                             :impact-analysis "0 4px 12px rgba(239, 68, 68, 0.3)"
                             "0 4px 12px rgba(59, 130, 246, 0.3)")
                :zIndex 1500
                :display "flex"
                :alignItems "center"
                :gap "12px"
                :fontWeight "500"
                :fontSize "14px"}}
       [:span {:style {:fontSize "16px"}}
        (case (:type lens)
          :impact-analysis "💥"
          "🔍")]
       [:div
        [:div {:style {:fontSize "12px"
                       :opacity 0.9}}
         (case (:type lens)
           :impact-analysis "IMPACT ANALYSIS"
           "LENS ACTIVE")]
        [:div {:style {:fontSize "14px"
                       :fontWeight "600"}}
         (:title lens)]
        (when (= (:type lens) :impact-analysis)
          [:div {:style {:fontSize "11px"
                         :opacity 0.9
                         :marginTop "2px"}}
           (str impact-count " entities affected - "
                (name impact-score) " risk")])]
       [:button {:on-click handle-clear-lens!
                 :style {:marginLeft "8px"
                         :padding "6px 12px"
                         :backgroundColor "rgba(255, 255, 255, 0.2)"
                         :border "1px solid rgba(255, 255, 255, 0.3)"
                         :borderRadius "6px"
                         :color "white"
                         :fontSize "11px"
                         :fontWeight "600"
                         :cursor "pointer"
                         :transition "all 0.2s"}}
        "✕ Clear"]])))
(defn lens-selector-component []
  "Lens selector panel for choosing semantic views"
  (let [registry (:atlas-ui.ui.state/registry @app-state)
        show? (:atlas-ui.ui.state/show-lens-selector? @app-state)
        active-lens (:atlas-ui.ui.state/active-lens @app-state)
        available (when (seq registry)
                    (lenses/available-lenses registry))]
    [:div.lens-selector
     {:style {:position "absolute"
              :left "20px"
              :top "80px"
              :backgroundColor "rgba(255, 255, 255, 0.98)"
              :backdropFilter "blur(12px)"
              :padding "20px"
              :borderRadius "12px"
              :boxShadow "0 8px 24px rgba(0, 0, 0, 0.15)"
              :zIndex 2000
              :minWidth "280px"
              :maxWidth "320px"
              :maxHeight "70vh"
              :overflowY "auto"
              :display (if show? "block" "none")
              :border "2px solid #e5e7eb"}}

     [:div {:style {:display "flex"
                    :justifyContent "space-between"
                    :alignItems "center"
                    :marginBottom "16px"}}
      [:h3 {:style {:margin "0"
                    :fontSize "16px"
                    :fontWeight "600"
                    :color "#111827"}}
       "🔍 Choose Lens"]
      [:button {:on-click handle-toggle-lens-selector!
                :style {:background "none"
                        :border "none"
                        :fontSize "20px"
                        :cursor "pointer"
                        :color "#9ca3af"
                        :padding "0"
                        :lineHeight "1"}}
       "×"]]

     (when active-lens
       [:div {:style {:padding "12px"
                      :backgroundColor "#f3f4f6"
                      :borderRadius "8px"
                      :marginBottom "16px"
                      :display "flex"
                      :justifyContent "space-between"
                      :alignItems "center"}}
        [:div {:style {:fontSize "12px"
                       :color "#6b7280"}}
         [:div {:style {:fontWeight "600"
                        :color "#111827"
                        :marginBottom "2px"}}
          "Active: "
          (name (:type active-lens))]
         [:div {:style {:fontSize "11px"}}
          (let [lens-value (:value active-lens)]
            (cond
              (keyword? lens-value) (name lens-value)
              (map? lens-value) (str (count (:lenses lens-value)) " combined")
              :else (str lens-value)))]]
        [:button {:on-click handle-clear-lens!
                  :style {:padding "6px 12px"
                          :backgroundColor "#7c3aed"
                          :color "white"
                          :border "none"
                          :borderRadius "6px"
                          :fontSize "11px"
                          :fontWeight "500"
                          :cursor "pointer"}}
         "Clear"]])

     ;; Domain lenses
     (when (seq (:domains available))
       [:div.lens-category
        {:style {:marginBottom "20px"}}
        [:h4 {:style {:margin "0 0 8px 0"
                      :fontSize "13px"
                      :fontWeight "600"
                      :color "#6b7280"
                      :textTransform "uppercase"
                      :letterSpacing "0.05em"}}
         "By Domain"]
        (for [domain (sort (:domains available))]
          ^{:key domain}
          [:button.lens-btn
           {:on-click #(handle-activate-lens! :domain domain)
            :style {:display "block"
                    :width "100%"
                    :padding "10px 12px"
                    :marginBottom "6px"
                    :backgroundColor (if (and active-lens
                                              (= (:type active-lens) :domain)
                                              (= (:value active-lens) domain))
                                       "#ddd6fe"
                                       "#ffffff")
                    :border "1px solid #e5e7eb"
                    :borderRadius "6px"
                    :fontSize "13px"
                    :fontWeight "500"
                    :color "#374151"
                    :cursor "pointer"
                    :textAlign "left"
                    :transition "all 0.2s"}}
           "🏢 " (name domain)])])

     ;; Tier lenses
     (when (seq (:tiers available))
       [:div.lens-category
        {:style {:marginBottom "20px"}}
        [:h4 {:style {:margin "0 0 8px 0"
                      :fontSize "13px"
                      :fontWeight "600"
                      :color "#6b7280"
                      :textTransform "uppercase"
                      :letterSpacing "0.05em"}}
         "By Tier"]
        (for [tier (sort (:tiers available))]
          ^{:key tier}
          [:button.lens-btn
           {:on-click #(handle-activate-lens! :tier tier)
            :style {:display "block"
                    :width "100%"
                    :padding "10px 12px"
                    :marginBottom "6px"
                    :backgroundColor (if (and active-lens
                                              (= (:type active-lens) :tier)
                                              (= (:value active-lens) tier))
                                       "#ddd6fe"
                                       "#ffffff")
                    :border "1px solid #e5e7eb"
                    :borderRadius "6px"
                    :fontSize "13px"
                    :fontWeight "500"
                    :color "#374151"
                    :cursor "pointer"
                    :textAlign "left"
                    :transition "all 0.2s"}}
           "📊 " (name tier)])])

     ;; Protocol lenses
     (when (seq (:protocols available))
       [:div.lens-category
        {:style {:marginBottom "20px"}}
        [:h4 {:style {:margin "0 0 8px 0"
                      :fontSize "13px"
                      :fontWeight "600"
                      :color "#6b7280"
                      :textTransform "uppercase"
                      :letterSpacing "0.05em"}}
         "By Protocol"]
        (for [protocol (sort (:protocols available))]
          ^{:key protocol}
          [:button.lens-btn
           {:on-click #(handle-activate-lens! :protocol protocol)
            :style {:display "block"
                    :width "100%"
                    :padding "10px 12px"
                    :marginBottom "6px"
                    :backgroundColor (if (and active-lens
                                              (= (:type active-lens) :protocol)
                                              (= (:value active-lens) protocol))
                                       "#ddd6fe"
                                       "#ffffff")
                    :border "1px solid #e5e7eb"
                    :borderRadius "6px"
                    :fontSize "13px"
                    :fontWeight "500"
                    :color "#374151"
                    :cursor "pointer"
                    :textAlign "left"
                    :transition "all 0.2s"}}
           "🔌 " (name protocol)])])

     ;; Constraint lenses
     (when (seq (:constraints available))
       [:div.lens-category
        {:style {:marginBottom "20px"}}
        [:h4 {:style {:margin "0 0 8px 0"
                      :fontSize "13px"
                      :fontWeight "600"
                      :color "#6b7280"
                      :textTransform "uppercase"
                      :letterSpacing "0.05em"}}
         "By Constraint"]
        (for [constraint (sort (:constraints available))]
          ^{:key constraint}
          [:button.lens-btn
           {:on-click #(handle-activate-lens! :constraint constraint)
            :style {:display "block"
                    :width "100%"
                    :padding "10px 12px"
                    :marginBottom "6px"
                    :backgroundColor (if (and active-lens
                                              (= (:type active-lens) :constraint)
                                              (= (:value active-lens) constraint))
                                       "#ddd6fe"
                                       "#ffffff")
                    :border "1px solid #e5e7eb"
                    :borderRadius "6px"
                    :fontSize "13px"
                    :fontWeight "500"
                    :color "#374151"
                    :cursor "pointer"
                    :textAlign "left"
                    :transition "all 0.2s"}}
           "🔒 " (name constraint)])])

     ;; UI Intent lenses
     (when (seq (:ui-intents available))
       [:div.lens-category
        {:style {:marginBottom "20px"}}
        [:h4 {:style {:margin "0 0 8px 0"
                      :fontSize "13px"
                      :fontWeight "600"
                      :color "#6b7280"
                      :textTransform "uppercase"
                      :letterSpacing "0.05em"}}
         "By UI Intent"]
        (for [intent (sort (:ui-intents available))]
          ^{:key intent}
          [:button.lens-btn
           {:on-click #(handle-activate-lens! :ui-intent intent)
            :style {:display "block"
                    :width "100%"
                    :padding "10px 12px"
                    :marginBottom "6px"
                    :backgroundColor (if (and active-lens
                                              (= (:type active-lens) :ui-intent)
                                              (= (:value active-lens) intent))
                                       "#ddd6fe"
                                       "#ffffff")
                    :border "1px solid #e5e7eb"
                    :borderRadius "6px"
                    :fontSize "13px"
                    :fontWeight "500"
                    :color "#374151"
                    :cursor "pointer"
                    :textAlign "left"
                    :transition "all 0.2s"}}
           "🎯 " (name intent)])])

     ;; Advanced lenses
     [:div.lens-category
      {:style {:marginBottom "20px"
               :borderTop "2px solid #e5e7eb"
               :paddingTop "16px"}}
      [:h4 {:style {:margin "0 0 8px 0"
                    :fontSize "13px"
                    :fontWeight "600"
                    :color "#6b7280"
                    :textTransform "uppercase"
                    :letterSpacing "0.05em"}}
       "Advanced Lenses"]

      ;; Dependency Flow
      [:div {:style {:marginBottom "12px"}}
       [:label {:style {:display "block"
                        :fontSize "12px"
                        :fontWeight "600"
                        :color "#374151"
                        :marginBottom "4px"}}
        "🔄 Dependency Flow"]
       [:select
        {:on-change #(let [entity-id (keyword (.. % -target -value))]
                       (when entity-id
                         (handle-activate-lens! :dependency-flow entity-id)))
         :style {:width "100%"
                 :padding "8px"
                 :border "1px solid #e5e7eb"
                 :borderRadius "6px"
                 :fontSize "12px"
                 :backgroundColor "#ffffff"
                 :cursor "pointer"}}
        [:option {:value ""} "Select entity to trace..."]
        (for [[identity entity] registry
              :let [dev-id (:atlas/dev-id entity)]
              :when dev-id]
          ^{:key dev-id}
          [:option {:value (str (namespace dev-id) "/" (name dev-id))}
           (str dev-id)])]]

      ;; Impact Analysis
      [:div {:style {:marginBottom "12px"}}
       [:label {:style {:display "block"
                        :fontSize "12px"
                        :fontWeight "600"
                        :color "#374151"
                        :marginBottom "4px"}}
        "💥 Impact Analysis"]
       [:select
        {:on-change #(let [entity-id (keyword (.. % -target -value))]
                       (when entity-id
                         (handle-activate-lens! :impact-analysis entity-id)))
         :style {:width "100%"
                 :padding "8px"
                 :border "1px solid #e5e7eb"
                 :borderRadius "6px"
                 :fontSize "12px"
                 :backgroundColor "#ffffff"
                 :cursor "pointer"}}
        [:option {:value ""} "Select entity to analyze..."]
        (for [[identity entity] registry
              :let [dev-id (:atlas/dev-id entity)]
              :when dev-id]
          ^{:key dev-id}
          [:option {:value (str (namespace dev-id) "/" (name dev-id))}
           (str dev-id)])]]

      ;; Combination Builder
      [:div {:style {:marginBottom "12px"}}
       [:label {:style {:display "block"
                        :fontSize "12px"
                        :fontWeight "600"
                        :color "#374151"
                        :marginBottom "4px"}}
        "🔀 Combine Lenses"]
       [:div {:style {:fontSize "11px"
                      :color "#6b7280"
                      :marginBottom "6px"
                      :fontStyle "italic"}}
        "Select a lens, then click + to add more"]

       ;; Lens selector
       (let [combo-lenses (:atlas-ui.ui.state/combination-lenses @app-state)]
         [:div
          [:select
           {:on-change #(let [value (.. % -target -value)
                              [lens-type lens-value] (clojure.string/split value #":")
                              lens-spec {:type (keyword lens-type)
                                         :value (keyword lens-value)}]
                          (when (and lens-type lens-value)
                            (swap! app-state update :atlas-ui.ui.state/combination-lenses conj lens-spec))
                         ;; Reset select
                          (set! (.-value (.-target %)) ""))
            :style {:width "100%"
                    :padding "8px"
                    :border "1px solid #e5e7eb"
                    :borderRadius "6px"
                    :fontSize "12px"
                    :backgroundColor "#ffffff"
                    :cursor "pointer"
                    :marginBottom "6px"}}
           [:option {:value ""} "Add lens to combine..."]

           ;; Domain options
           (when (seq (:domains available))
             [:optgroup {:label "Domains"}
              (for [domain (sort (:domains available))]
                ^{:key domain}
                [:option {:value (str "domain:" (namespace domain) "/" (name domain))}
                 (str "Domain: " (name domain))])])

           ;; Tier options
           (when (seq (:tiers available))
             [:optgroup {:label "Tiers"}
              (for [tier (sort (:tiers available))]
                ^{:key tier}
                [:option {:value (str "tier:" (namespace tier) "/" (name tier))}
                 (str "Tier: " (name tier))])])

           ;; Protocol options
           (when (seq (:protocols available))
             [:optgroup {:label "Protocols"}
              (for [protocol (sort (:protocols available))]
                ^{:key protocol}
                [:option {:value (str "protocol:" (namespace protocol) "/" (name protocol))}
                 (str "Protocol: " (name protocol))])])]

          ;; Show selected lenses
          (when (seq combo-lenses)
            [:div {:style {:marginTop "8px"}}
             [:div {:style {:fontSize "11px"
                            :color "#6b7280"
                            :marginBottom "4px"}}
              "Selected lenses:"]
             (for [[idx lens] (map-indexed vector combo-lenses)]
               ^{:key idx}
               [:div {:style {:fontSize "11px"
                              :padding "4px 8px"
                              :backgroundColor "#f3f4f6"
                              :borderRadius "4px"
                              :marginBottom "2px"
                              :display "flex"
                              :justifyContent "space-between"
                              :alignItems "center"}}
                [:span (str (name (:type lens)) ": " (name (:value lens)))]
                [:button
                 {:on-click #(swap! app-state update :atlas-ui.ui.state/combination-lenses
                                    (fn [lenses] (vec (concat (subvec lenses 0 idx)
                                                              (subvec lenses (inc idx))))))
                  :style {:background "none"
                          :border "none"
                          :color "#ef4444"
                          :cursor "pointer"
                          :fontSize "10px"}}
                 "✕"]])
             [:button
              {:on-click #(handle-activate-lens! :combination {:lenses combo-lenses})
               :style {:width "100%"
                       :padding "8px"
                       :marginTop "4px"
                       :backgroundColor "#8b5cf6"
                       :color "white"
                       :border "none"
                       :borderRadius "6px"
                       :fontSize "12px"
                       :fontWeight "600"
                       :cursor "pointer"}}
              (str "🔀 Apply " (count combo-lenses) " Lenses")]])])]]]))

(defn presets-panel []
  "Presets panel for saving and loading lens configurations"
  (let [show? (:atlas-ui.ui.state/show-presets? @app-state)
        presets (:atlas-ui.ui.state/lens-presets @app-state)
        active-lens (:atlas-ui.ui.state/active-lens @app-state)]
    [:div.presets-panel
     {:style {:position "absolute"
              :right "20px"
              :top "80px"
              :backgroundColor "rgba(255, 255, 255, 0.98)"
              :backdropFilter "blur(12px)"
              :padding "20px"
              :borderRadius "12px"
              :boxShadow "0 8px 24px rgba(0, 0, 0, 0.15)"
              :zIndex 2000
              :minWidth "280px"
              :maxWidth "320px"
              :maxHeight "70vh"
              :overflowY "auto"
              :display (if show? "block" "none")
              :border "2px solid #e5e7eb"}}

     [:div {:style {:display "flex"
                    :justifyContent "space-between"
                    :alignItems "center"
                    :marginBottom "16px"}}
      [:h3 {:style {:margin "0"
                    :fontSize "16px"
                    :fontWeight "600"
                    :color "#111827"}}
       "💾 Saved Presets"]
      [:button {:on-click handle-toggle-presets!
                :style {:background "none"
                        :border "none"
                        :fontSize "20px"
                        :cursor "pointer"
                        :color "#9ca3af"
                        :padding "0"
                        :lineHeight "1"}}
       "×"]]

     ;; Save current lens button
     (when active-lens
       [:button
        {:on-click #(let [name (js/prompt "Preset name:")]
                      (when name (handle-save-preset! name)))
         :style {:width "100%"
                 :padding "12px"
                 :marginBottom "16px"
                 :backgroundColor "#10b981"
                 :color "white"
                 :border "none"
                 :borderRadius "8px"
                 :fontSize "13px"
                 :fontWeight "600"
                 :cursor "pointer"
                 :transition "all 0.2s"}}
        "💾 Save Current Lens"])

     ;; Preset list
     (if (empty? presets)
       [:div {:style {:padding "20px"
                      :textAlign "center"
                      :color "#9ca3af"
                      :fontSize "13px"}}
        "No saved presets yet"
        [:div {:style {:marginTop "8px"
                       :fontSize "12px"}}
         "Activate a lens and save it"]]

       [:div.preset-list
        (for [[preset-id preset] presets]
          ^{:key preset-id}
          [:div.preset-item
           {:style {:marginBottom "8px"
                    :padding "12px"
                    :backgroundColor "#f9fafb"
                    :borderRadius "8px"
                    :border "1px solid #e5e7eb"
                    :display "flex"
                    :justifyContent "space-between"
                    :alignItems "center"}}
           [:div
            {:on-click #(handle-load-preset! preset-id)
             :style {:flex "1"
                     :cursor "pointer"}}
            [:div {:style {:fontWeight "600"
                           :fontSize "13px"
                           :color "#111827"
                           :marginBottom "4px"}}
             (:name preset)]
            [:div {:style {:fontSize "11px"
                           :color "#6b7280"}}
             (str (name (:type (:lens-spec preset)))
                  ": "
                  (name (:value (:lens-spec preset))))]]
           [:button
            {:on-click #(handle-delete-preset! preset-id)
             :style {:padding "4px 8px"
                     :backgroundColor "#ef4444"
                     :color "white"
                     :border "none"
                     :borderRadius "4px"
                     :fontSize "11px"
                     :cursor "pointer"}}
            "🗑️"]])])]))

(defn main-view []
  (let [selections (:atlas-ui.ui.state/selections @app-state)
        negated-aspects (:atlas-ui.ui.state/negated-aspects @app-state)
        query-mode (:atlas-ui.ui.state/query-mode @app-state)
        min-score (:atlas-ui.ui.state/min-score @app-state)
        registry (:atlas-ui.ui.state/registry @app-state)
        highlight (:atlas-ui.ui.state/highlight-selection @app-state)
        highlighted-entity-id (:atlas-ui.graph.node/id highlight)
        highlighted-entity-aspects (:atlas-ui.ui.state/highlighted-entity-aspects @app-state)
        highlighted-aspect (:atlas-ui.ui.state/highlight-aspect @app-state)
        legend-focus (:atlas-ui.ui.state/legend-focus @app-state)
        hide-unmatched? (:atlas-ui.ui.state/hide-unmatched? @app-state)
        search-term (:atlas-ui.ui.state/search-term @app-state)
        selected-types (:atlas-ui.ui.state/selected-types @app-state)
        active-lens (:atlas-ui.ui.state/active-lens @app-state)
        lens @lens-result]
    [:div.app
     {:style {:display "flex"
              :flexDirection "column"
              :height "100vh"
              :fontFamily "system-ui, -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif"}}

     [header-component]

     [:div.content
      {:style {:display "flex"
               :flex "1"
               :overflow "hidden"}}

      ;; Graph container
      [:div.graph-wrapper
       {:style {:flex "1"
                :position "relative"
                :overflow "hidden"}}

       (if (empty? registry)
         [:div {:style {:display "flex"
                        :alignItems "center"
                        :justifyContent "center"
                        :height "100%"
                        :color "#9ca3af"}}
          "Loading registry..."]

         [graph/graph-component
          @filtered-graph-data ;; Use filtered data
          selections
          negated-aspects
          query-mode
          min-score
         registry
         highlighted-entity-id
         highlighted-entity-aspects
         highlighted-aspect
         hide-unmatched?
         search-term
         lens ;; Pass lens for layout configuration
         handle-toggle-aspect
         handle-click-entity
         handle-click-aspect
         legend-focus])

       ;; Search bar
       [search/search-bar search-term handle-search-change]

       ;; Lens banner (shows active lens)
       [lens-banner]

       ;; Lens selector panel
       [lens-selector-component]

       ;; Presets panel
       [presets-panel]

       ;; Selection controls overlay
       (when (or (seq selections) (seq negated-aspects))
         [controls/selection-controls
          selections
          negated-aspects
          query-mode
          min-score
          handle-toggle-aspect
          handle-set-query-mode
          handle-clear-selections
          handle-toggle-visibility
          hide-unmatched?
          handle-set-min-score])

       ;; Color legend
       [legend/semantic-legend legend-focus handle-legend-focus]
       [legend/color-legend selections query-mode]

       ;; Type filter panel (Phase 2)
       [controls/type-filter-panel @filtered-graph-data selected-types handle-toggle-type]

       [info-panel]]

      ;; Sidebar (for detailed entity view)
      (when highlight
        [sidebar/sidebar-component
         highlight
         @filtered-graph-data ;; Use filtered data
         registry
         #(swap! app-state assoc :atlas-ui.ui.state/highlight-selection nil :atlas-ui.ui.state/highlighted-entity-aspects #{})
         handle-toggle-aspect
         handle-click-entity])]]))

;; =============================================================================
;; Keyboard Shortcuts
;; =============================================================================

(defn setup-keyboard-shortcuts! []
  (js/document.addEventListener "keydown"
                                (fn [e]
                                  (let [key (.-key e)
                                        ctrl? (.-ctrlKey e)
                                        meta? (.-metaKey e)
                                        modifier? (or ctrl? meta?)]
                                    (cond
                                      ;; ESC - Clear selections or lens
                                      (= key "Escape")
                                      (if (:atlas-ui.ui.state/active-lens @app-state)
                                        (handle-clear-lens!)
                                        (handle-clear-selections))

                                      ;; L - Toggle lens selector
                                      (and (= key "l") (not modifier?))
                                      (do (.preventDefault e)
                                          (handle-toggle-lens-selector!))

                                      ;; P - Toggle presets
                                      (and (= key "p") (not modifier?))
                                      (do (.preventDefault e)
                                          (handle-toggle-presets!))

                                      ;; [ - Go back in history
                                      (= key "[")
                                      (do (.preventDefault e)
                                          (handle-lens-back!))

                                      ;; ] - Go forward in history
                                      (= key "]")
                                      (do (.preventDefault e)
                                          (handle-lens-forward!))

                                      ;; Cmd/Ctrl+S - Save preset
                                      (and (= key "s") modifier?)
                                      (do (.preventDefault e)
                                          (when (:atlas-ui.ui.state/active-lens @app-state)
                                            (let [name (js/prompt "Preset name:")]
                                              (when name
                                                (handle-save-preset! name)))))

                                      ;; Cmd/Ctrl+C - Copy lens URL
                                      (and (= key "c") modifier?)
                                      (when (:atlas-ui.ui.state/active-lens @app-state)
                                        (do (.preventDefault e)
                                            (copy-lens-url-to-clipboard!)))

                                      ;; Query mode shortcuts (1, 2, 3)
                                      (= key "1")
                                      (do (.preventDefault e) (handle-set-query-mode :and))

                                      (= key "2")
                                      (do (.preventDefault e) (handle-set-query-mode :or))

                                      (= key "3")
                                      (do (.preventDefault e) (handle-set-query-mode :count))

                                      :else nil)))))

;; =============================================================================
;; Initialize
;; =============================================================================

(defn ^:dev/after-load mount-root []
  (rdom/render [main-view]
               (js/document.getElementById "app")))

(defn init []
  (js/console.log "🚀 Atlas UI starting with multi-selection...")
  (setup-keyboard-shortcuts!)
  (load-lens-from-url!) ;; Load lens from URL if present
  (refresh-registry!)
  (mount-root))
