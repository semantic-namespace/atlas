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
           :loading? true
           :error nil
           ;; Selection state for bidirectional filtering (all sets for multi-select)
           :aspects-and #{}           ; Aspects in AND mode (blue) - must have ALL
           :aspects-or #{}            ; Aspects in OR mode (green) - must have ANY
           :selected-types #{}        ; Set of selected entity types
           :selected-entities #{}     ; Set of selected dev-ids
           ;; Filter mode
           :filter-mode :hide})) ; :highlight or :hide

;; =============================================================================
;; Derived Data (computed from registry)
;; =============================================================================

(defn aspects-map-data
  "Build aspects map: namespace -> #{aspect-names}"
  [registry]
  (when registry
    (data/build-aspects-map registry)))

(defn entities-map-data
  "Build entities map: type -> {dev-id -> identity}"
  [registry]
  (when registry
    (data/build-entities-map registry)))

;; =============================================================================
;; Selection Handlers
;; =============================================================================

(defn cycle-aspect!
  "Cycle aspect through: not selected → AND (blue) → OR (green) → not selected"
  [aspect]
  (swap! app-state
         (fn [state]
           (let [in-and? (contains? (:aspects-and state) aspect)
                 in-or? (contains? (:aspects-or state) aspect)]
             (cond
               ;; Not selected → AND
               (and (not in-and?) (not in-or?))
               (update state :aspects-and conj aspect)

               ;; AND → OR
               in-and?
               (-> state
                   (update :aspects-and disj aspect)
                   (update :aspects-or conj aspect))

               ;; OR → Not selected
               in-or?
               (update state :aspects-or disj aspect))))))

(defn toggle-type!
  "Toggle selection of an entity type"
  [entity-type]
  (swap! app-state update :selected-types
         (fn [selected]
           (if (contains? selected entity-type)
             (disj selected entity-type)
             (conj selected entity-type)))))

(defn toggle-entity!
  "Toggle selection of a specific entity by dev-id"
  [dev-id]
  (swap! app-state update :selected-entities
         (fn [selected]
           (if (contains? selected dev-id)
             (disj selected dev-id)
             (conj selected dev-id)))))

(defn clear-selection!
  "Clear all selections"
  []
  (swap! app-state assoc
         :aspects-and #{}
         :aspects-or #{}
         :selected-types #{}
         :selected-entities #{}))

(defn toggle-filter-mode!
  "Toggle between highlight and hide modes"
  []
  (swap! app-state update :filter-mode
         (fn [mode]
           (if (= mode :highlight) :hide :highlight))))

;; =============================================================================
;; Filter Logic
;; =============================================================================

(defn entities-matching-aspects
  "Find all dev-ids matching the AND/OR aspect criteria.
   - Entity matches if it has ALL aspects from aspects-and
   - OR entity matches if it has ANY aspect from aspects-or"
  [registry aspects-and aspects-or]
  (when (or (seq aspects-and) (seq aspects-or))
    (->> registry
         (filter (fn [[identity _props]]
                   (let [matches-and? (and (seq aspects-and)
                                           (every? #(contains? identity %) aspects-and))
                         matches-or? (and (seq aspects-or)
                                          (some #(contains? identity %) aspects-or))]
                     (or matches-and? matches-or?))))
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
  "Get union of all aspects for multiple dev-ids"
  [registry dev-ids]
  (when (seq dev-ids)
    (->> dev-ids
         (map #(aspects-for-entity registry %))
         (filter some?)
         (apply set/union))))

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
  (let [{:keys [aspects-and aspects-or selected-types selected-entities]} @app-state]
    (when (or (seq aspects-and) (seq aspects-or) (seq selected-types) (seq selected-entities))
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
       (when (seq selected-types)
         [:span "Types: " (count selected-types)])
       (when (seq selected-entities)
         [:span "Entities: " (count selected-entities)])])))

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

(defn main-view []
  (let [{:keys [registry aspects-and aspects-or selected-types selected-entities filter-mode]} @app-state
        aspects-data (aspects-map-data registry)
        entities-data (entities-map-data registry)
        ;; Compute what to highlight/hide based on selections
        ;; Aspects -> highlight matching entities
        highlight-entities (when (or (seq aspects-and) (seq aspects-or))
                             (entities-matching-aspects registry aspects-and aspects-or))
        ;; Entities/Types -> highlight their aspects
        highlight-aspects (cond
                            (seq selected-entities) (aspects-for-entities registry selected-entities)
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
         :highlight-aspects highlight-aspects
         :filter-mode filter-mode
         :on-click cycle-aspect!}]]
      ;; Entities Map (right panel)
      [:div {:style {:flex 1
                     :overflow "auto"}}
       [entities-map/entities-map-view
        entities-data
        {:selected-types selected-types
         :selected-entities selected-entities
         :highlight-entities highlight-entities
         :filter-mode filter-mode
         :on-type-click toggle-type!
         :on-entity-click toggle-entity!}]]]]))

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
  (let [registry (or (:atlas-ui.api.response/registry data)
                     data)]
    (swap! app-state assoc
           :registry registry
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
