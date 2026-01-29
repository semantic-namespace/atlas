(ns atlas-ui-v2.core
  "Atlas UI v2 - Dual Map Explorer with bidirectional filtering.

   Two complementary views:
   1. Aspects Map: namespace -> aspect names
   2. Entities Map: type -> dev-id -> compound identity

   Click on one map to filter the other."
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [clojure.set :as set]
            [atlas-ui-v2.api :as api]
            [atlas-ui-v2.data :as data]
            [atlas-ui-v2.aspects-map :as aspects-map]
            [atlas-ui-v2.entities-map :as entities-map]))

;; =============================================================================
;; State
;; =============================================================================

(defonce app-state
  (r/atom {:registry nil
           :aspect-stats nil          ; Aspect usage stats from backend
           :loading? true
           :error nil
           ;; Selection state for bidirectional filtering (all sets for multi-select)
           :aspects-and #{}           ; Aspects in AND mode (blue) - must have ALL
           :aspects-or #{}            ; Aspects in OR mode (green) - must have ANY
           :aspects-not #{}           ; Aspects in NOT mode (red) - must NOT have
           :selected-types #{}        ; Set of selected entity types
           :selected-entities #{}     ; Set of selected dev-ids (positive selection)
           :selected-entities-not #{} ; Set of excluded dev-ids (negative selection)
           ;; Filter mode
           :filter-mode :hide         ; :highlight or :hide
           ;; Sort options
           :sort-aspects-ns :alpha-asc        ; Namespace level sort
           :sort-aspects-items :alpha-asc     ; Aspect names sort
           :sort-entities-type :alpha-asc     ; Entity type level sort
           :sort-entities-items :alpha-asc})) ; Dev-ids sort

;; =============================================================================
;; Derived Data (computed from registry)
;; =============================================================================

(defn aspects-map-data
  "Build and sort aspects map: namespace -> #{aspect-names}"
  [registry aspect-stats sort-ns sort-items aspects-and aspects-or aspects-not]
  (when (and registry aspect-stats)
    (let [aspect-stats-map (data/aspect-stats->map aspect-stats)
          _ (js/console.log "aspect-stats-map size:" (count aspect-stats-map)
                           "first 3:" (clj->js (take 3 aspect-stats-map)))
          aspects-map (data/build-aspects-map registry)
          sorted-map (data/sort-aspects-map aspects-map aspect-stats-map sort-ns)
          ;; Convert to vector to ensure order is preserved in React rendering
          aspects-vec (vec sorted-map)
          first-3-keys (vec (take 3 (keys sorted-map)))
          ;; Count entities per aspect - filtered by current query
          entity-counts (data/count-entities-by-aspect-filtered registry aspects-and aspects-or aspects-not)]
      (js/console.log "aspects-map-data - sort-ns:" (name sort-ns)
                      "sort-items:" (name sort-items)
                      "namespaces:" (count sorted-map)
                      "first 3:" (clj->js first-3-keys))
      {:aspects-map aspects-vec
       :aspect-stats-map aspect-stats-map
       :entity-counts entity-counts
       :sort-items sort-items})))

(defn entities-map-data
  "Build and sort entities map: type -> {dev-id -> identity}"
  [registry sort-type sort-items query-aspects]
  (when registry
    (let [entities-map (data/build-entities-map registry)
          sorted-map (data/sort-entities-map entities-map sort-type)
          ;; Convert to vector to ensure order is preserved in React rendering
          entities-vec (vec sorted-map)
          all-keys (vec (keys sorted-map))]
      (js/console.log "entities-map-data - sort-type:" (name sort-type)
                      "sort-items:" (name sort-items)
                      "types:" (count sorted-map)
                      "order:" (clj->js all-keys))
      {:entities-map entities-vec
       :sort-items sort-items
       :query-aspects query-aspects})))

;; =============================================================================
;; Selection Handlers
;; =============================================================================

(defn cycle-aspect!
  "Cycle aspect through: not selected → AND (blue) → OR (green) → NOT (red) → not selected"
  [aspect]
  (swap! app-state
         (fn [state]
           (let [in-and? (contains? (:aspects-and state) aspect)
                 in-or? (contains? (:aspects-or state) aspect)
                 in-not? (contains? (:aspects-not state) aspect)
                 new-state (cond
                             ;; Not selected → AND
                             (and (not in-and?) (not in-or?) (not in-not?))
                             (update state :aspects-and conj aspect)

                             ;; AND → OR
                             in-and?
                             (-> state
                                 (update :aspects-and disj aspect)
                                 (update :aspects-or conj aspect))

                             ;; OR → NOT
                             in-or?
                             (-> state
                                 (update :aspects-or disj aspect)
                                 (update :aspects-not conj aspect))

                             ;; NOT → Not selected
                             in-not?
                             (update state :aspects-not disj aspect))
                 ;; Auto-switch sorting: distance-asc if aspects selected, alpha-asc if none
                 has-aspects? (or (seq (:aspects-and new-state))
                                  (seq (:aspects-or new-state))
                                  (seq (:aspects-not new-state)))
                 new-sort (if has-aspects? :distance-asc :alpha-asc)]
             (js/console.log "cycle-aspect! auto-sort:" (name new-sort) "has-aspects:" has-aspects?)
             (assoc new-state :sort-entities-items new-sort)))))

(defn toggle-type!
  "Toggle selection of an entity type"
  [entity-type]
  (swap! app-state update :selected-types
         (fn [selected]
           (if (contains? selected entity-type)
             (disj selected entity-type)
             (conj selected entity-type)))))

(defn toggle-entity!
  "Cycle entity through: not selected → selected (blue) → NOT (red) → not selected"
  [dev-id]
  (swap! app-state
         (fn [state]
           (let [selected? (contains? (:selected-entities state) dev-id)
                 not-selected? (contains? (:selected-entities-not state) dev-id)]
             (cond
               ;; Not selected → Selected
               (and (not selected?) (not not-selected?))
               (update state :selected-entities conj dev-id)

               ;; Selected → NOT
               selected?
               (-> state
                   (update :selected-entities disj dev-id)
                   (update :selected-entities-not conj dev-id))

               ;; NOT → Not selected
               not-selected?
               (update state :selected-entities-not disj dev-id))))))

(defn clear-selection!
  "Clear all selections"
  []
  (swap! app-state assoc
         :aspects-and #{}
         :aspects-or #{}
         :aspects-not #{}
         :selected-types #{}
         :selected-entities #{}
         :selected-entities-not #{}
         :sort-entities-items :alpha-asc))

(defn toggle-filter-mode!
  "Toggle between highlight and hide modes"
  []
  (swap! app-state update :filter-mode
         (fn [mode]
           (if (= mode :highlight) :hide :highlight))))

(defn set-sort!
  "Set sort option for a specific level"
  [key value]
  (js/console.log "Setting sort:" (name key) "->" (name value))
  (swap! app-state assoc key value))

;; =============================================================================
;; Filter Logic
;; =============================================================================

(defn entities-matching-aspects
  "Find all dev-ids matching the AND/OR/NOT aspect criteria.
   - Entity matches if it has ALL aspects from aspects-and
   - OR entity matches if it has ANY aspect from aspects-or
   - Entity is excluded if it has ANY aspect from aspects-not
   - If only NOT is specified, show all entities except those with NOT aspects"
  [registry aspects-and aspects-or aspects-not]
  (when (or (seq aspects-and) (seq aspects-or) (seq aspects-not))
    (->> registry
         (filter (fn [[identity _props]]
                   (let [matches-and? (and (seq aspects-and)
                                           (every? #(contains? identity %) aspects-and))
                         matches-or? (and (seq aspects-or)
                                          (some #(contains? identity %) aspects-or))
                         matches-not? (and (seq aspects-not)
                                           (some #(contains? identity %) aspects-not))
                         has-positive-criteria? (or (seq aspects-and) (seq aspects-or))
                         ;; Include if matches positive criteria (AND/OR), or if no positive criteria exist
                         base-match? (if has-positive-criteria?
                                       (or matches-and? matches-or?)
                                       true)]  ; If only NOT criteria, match all by default
                     (and base-match? (not matches-not?)))))
         (map (fn [[_identity props]] (:atlas/dev-id props)))
         (filter some?)
         set)))

(defn aspects-for-entity
  "Get all aspects for a dev-id"
  [registry dev-id]
  (->> registry
       (filter (fn [[_identity props]]
                 (= dev-id (:atlas/dev-id props))))
       first
       first)) ; compound identity

(defn aspects-for-entities
  "Get union of all aspects for multiple dev-ids, excluding aspects from not-dev-ids.
   - If only positive dev-ids: return their aspects
   - If only negative dev-ids: return all aspects EXCEPT theirs
   - If both: return positive aspects minus negative aspects"
  [registry dev-ids not-dev-ids]
  (let [has-positive? (seq dev-ids)
        has-negative? (seq not-dev-ids)]
    (when (or has-positive? has-negative?)
      (let [negative-aspects (when has-negative?
                               (->> not-dev-ids
                                    (map #(aspects-for-entity registry %))
                                    (filter some?)
                                    (apply set/union)))]
        (if has-positive?
          ;; Has positive selection: show positive aspects minus negative
          (let [positive-aspects (->> dev-ids
                                      (map #(aspects-for-entity registry %))
                                      (filter some?)
                                      (apply set/union))]
            (if negative-aspects
              (set/difference positive-aspects negative-aspects)
              positive-aspects))
          ;; Only negative selection: show all aspects except negative
          (let [all-aspects (->> registry
                                 keys
                                 (mapcat identity)
                                 (filter keyword?)
                                 set)]
            (set/difference all-aspects negative-aspects)))))))

(defn aspects-for-type
  "Get all aspects used by entities of a type"
  [registry entity-type]
  (->> registry
       (filter (fn [[identity _props]]
                 (contains? identity entity-type)))
       (mapcat (fn [[identity _props]] identity))
       set))

(defn aspects-for-types
  "Get union of all aspects for multiple types"
  [registry entity-types]
  (when (seq entity-types)
    (->> entity-types
         (map #(aspects-for-type registry %))
         (apply set/union))))

;; =============================================================================
;; Components
;; =============================================================================

(defn header []
  [:div {:style {:padding "1rem"
                 :background "#1a1a2e"
                 :color "#eee"
                 :display "flex"
                 :justify-content "space-between"
                 :align-items "center"}}
   [:h1 {:style {:margin 0 :font-size "1.5rem"}}
    "Atlas Explorer v2"]
   [:div {:style {:display "flex" :gap "1rem" :align-items "center"}}
    [:button {:on-click clear-selection!
              :style {:padding "0.5rem 1rem"
                      :background "#4a4a6a"
                      :color "#eee"
                      :border "none"
                      :border-radius "4px"
                      :cursor "pointer"}}
     "Clear Selection"]
    [:button {:on-click toggle-filter-mode!
              :style {:padding "0.5rem 1rem"
                      :background (if (= (:filter-mode @app-state) :highlight)
                                    "#6a6a8a" "#8a4a4a")
                      :color "#eee"
                      :border "none"
                      :border-radius "4px"
                      :cursor "pointer"}}
     (if (= (:filter-mode @app-state) :highlight)
       "Mode: Highlight"
       "Mode: Hide")]]])

(defn selection-info []
  (let [{:keys [aspects-and aspects-or aspects-not selected-types selected-entities selected-entities-not]} @app-state]
    (when (or (seq aspects-and) (seq aspects-or) (seq aspects-not)
              (seq selected-types) (seq selected-entities) (seq selected-entities-not))
      [:div {:style {:padding "0.5rem 1rem"
                     :background "#2a2a4e"
                     :color "#aaa"
                     :font-size "0.85rem"
                     :display "flex"
                     :flex-wrap "wrap"
                     :gap "1rem"}}
       (when (seq aspects-and)
         [:span {:style {:color "#4a9eff"}} "AND: " (count aspects-and)])
       (when (seq aspects-or)
         [:span {:style {:color "#4aef7a"}} "OR: " (count aspects-or)])
       (when (seq aspects-not)
         [:span {:style {:color "#ef4a4a"}} "NOT: " (count aspects-not)])
       (when (seq selected-types)
         [:span "Types: " (count selected-types)])
       (when (seq selected-entities)
         [:span {:style {:color "#4a9eff"}} "Entities: " (count selected-entities)])
       (when (seq selected-entities-not)
         [:span {:style {:color "#ef4a4a"}} "Entities NOT: " (count selected-entities-not)])])))

(defn loading-view []
  [:div {:style {:display "flex"
                 :justify-content "center"
                 :align-items "center"
                 :height "100vh"
                 :background "#0f0f1a"
                 :color "#eee"}}
   [:div "Loading registry..."]])

(defn error-view [error]
  [:div {:style {:display "flex"
                 :justify-content "center"
                 :align-items "center"
                 :height "100vh"
                 :background "#0f0f1a"
                 :color "#ff6b6b"}}
   [:div
    [:h2 "Error loading registry"]
    [:pre (pr-str error)]]])

(defn sort-dropdown
  "Sort dropdown component"
  [label current-value options on-change]
  [:div {:style {:display "flex"
                 :align-items "center"
                 :gap "0.5rem"}}
   [:label {:style {:font-size "0.8rem"
                    :color "#aaa"}} label]
   [:select {:value (name current-value)
             :on-change #(on-change (keyword (-> % .-target .-value)))
             :style {:padding "0.3rem"
                     :background "#2a2a4a"
                     :color "#eee"
                     :border "1px solid #4a4a6a"
                     :border-radius "4px"
                     :font-size "0.8rem"
                     :cursor "pointer"}}
    (for [[value label-text] options]
      ^{:key value}
      [:option {:value (name value)} label-text])]])

(defn sort-controls []
  (let [{:keys [sort-aspects-ns sort-aspects-items
                sort-entities-type sort-entities-items]} @app-state]
    [:div {:style {:padding "0.5rem 1rem"
                   :background "#1a1a2e"
                   :border-bottom "1px solid #333"
                   :display "flex"
                   :gap "2rem"
                   :flex-wrap "wrap"}}
     [:div {:style {:display "flex"
                    :gap "1rem"
                    :align-items "center"}}
      [:span {:style {:color "#8a8aaa"
                      :font-size "0.85rem"
                      :font-weight "bold"}}
       "Aspects Sort:"]
      [sort-dropdown "Namespace"
       sort-aspects-ns
       [[:alpha-asc "A-Z"]
        [:alpha-desc "Z-A"]
        [:usage-desc "Most Used"]
        [:usage-asc "Least Used"]
        [:count-desc "Most Aspects"]
        [:count-asc "Fewest Aspects"]]
       #(set-sort! :sort-aspects-ns %)]
      [sort-dropdown "Items"
       sort-aspects-items
       [[:alpha-asc "A-Z"]
        [:alpha-desc "Z-A"]
        [:usage-desc "Most Used"]
        [:usage-asc "Least Used"]]
       #(set-sort! :sort-aspects-items %)]]
     [:div {:style {:display "flex"
                    :gap "1rem"
                    :align-items "center"}}
      [:span {:style {:color "#8a8aaa"
                      :font-size "0.85rem"
                      :font-weight "bold"}}
       "Entities Sort:"]
      [sort-dropdown "Type"
       sort-entities-type
       [[:alpha-asc "A-Z"]
        [:alpha-desc "Z-A"]
        [:count-desc "Most Entities"]
        [:count-asc "Fewest Entities"]]
       #(set-sort! :sort-entities-type %)]
      [sort-dropdown "Items"
       sort-entities-items
       [[:alpha-asc "A-Z"]
        [:alpha-desc "Z-A"]
        [:aspect-count-desc "Most Aspects"]
        [:aspect-count-asc "Fewest Aspects"]]
       #(set-sort! :sort-entities-items %)]]]))

(defn main-view []
  (let [{:keys [registry aspect-stats aspects-and aspects-or aspects-not selected-types selected-entities selected-entities-not filter-mode
                sort-aspects-ns sort-aspects-items sort-entities-type sort-entities-items]} @app-state
        ;; Combine all query aspects for filtering display
        query-aspects (vec (set/union aspects-and aspects-or aspects-not))
        aspects-data (aspects-map-data registry aspect-stats sort-aspects-ns sort-aspects-items aspects-and aspects-or aspects-not)
        entities-data (entities-map-data registry sort-entities-type sort-entities-items query-aspects)
        ;; Compute what to highlight/hide based on selections
        ;; Aspects -> highlight matching entities
        highlight-entities (when (or (seq aspects-and) (seq aspects-or) (seq aspects-not))
                             (entities-matching-aspects registry aspects-and aspects-or aspects-not))
        ;; Entities/Types -> highlight their aspects
        highlight-aspects (cond
                            (or (seq selected-entities) (seq selected-entities-not))
                            (aspects-for-entities registry selected-entities selected-entities-not)
                            (seq selected-types) (aspects-for-types registry selected-types)
                            :else nil)]
    [:div {:style {:display "flex"
                   :flex-direction "column"
                   :height "100vh"
                   :background "#0f0f1a"}}
     [header]
     [selection-info]
     [:div {:style {:display "flex"
                    :flex 1
                    :overflow "hidden"}}
      ;; Aspects Map (left panel)
      [:div {:style {:flex 1
                     :overflow "auto"
                     :border-right "1px solid #333"}}
       [aspects-map/aspects-map-view
        aspects-data
        {:aspects-and aspects-and
         :aspects-or aspects-or
         :aspects-not aspects-not
         :highlight-aspects highlight-aspects
         :filter-mode filter-mode
         :on-click cycle-aspect!
         :sort-ns sort-aspects-ns
         :sort-items sort-aspects-items
         :on-sort-ns #(set-sort! :sort-aspects-ns %)
         :on-sort-items #(set-sort! :sort-aspects-items %)}]]
      ;; Entities Map (right panel)
      [:div {:style {:flex 1
                     :overflow "auto"}}
       [entities-map/entities-map-view
        entities-data
        {:selected-types selected-types
         :selected-entities selected-entities
         :selected-entities-not selected-entities-not
         :highlight-entities highlight-entities
         :filter-mode filter-mode
         :query-aspects query-aspects
         :aspects-and aspects-and
         :aspects-or aspects-or
         :aspects-not aspects-not
         :on-type-click toggle-type!
         :on-entity-click toggle-entity!
         :on-aspect-click cycle-aspect!
         :sort-type sort-entities-type
         :sort-items sort-entities-items
         :on-sort-type #(set-sort! :sort-entities-type %)
         :on-sort-items #(set-sort! :sort-entities-items %)}]]]]))

(defn app []
  (let [{:keys [loading? error registry]} @app-state]
    (cond
      loading? [loading-view]
      error [error-view error]
      (nil? registry) [loading-view]
      :else [main-view])))

;; =============================================================================
;; Init
;; =============================================================================

(defn on-registry-loaded [data]
  (let [registry (or (:atlas-ui.api.response/registry data) data)
        aspect-stats (or (:atlas-ui.api.response/aspect-stats data) [])]
    (js/console.log "Registry loaded - entities:" (count registry)
                    "aspect-stats:" (count aspect-stats))
    (when (> (count aspect-stats) 0)
      (js/console.log "First aspect-stat:" (first aspect-stats)))
    (swap! app-state assoc
           :registry registry
           :aspect-stats aspect-stats
           :loading? false
           :error nil)))

(defn on-registry-error [error]
  (swap! app-state assoc
         :loading? false
         :error error))

(defn mount-root []
  (when-let [el (js/document.getElementById "app")]
    (rdom/render [app] el)))

(defn init []
  (js/console.log "Atlas UI v2 initializing...")
  (api/fetch-registry! on-registry-loaded on-registry-error)
  (mount-root))
