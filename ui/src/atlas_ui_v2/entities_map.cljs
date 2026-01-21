(ns atlas-ui-v2.entities-map
  "Entities Map component - displays type -> dev-id -> identity hierarchy.

   Click on a type or entity to filter the aspects map."
  (:require [atlas-ui-v2.data :as data]))

(defn sort-dropdown
  "Sort dropdown component"
  [label current-value options on-change]
  [:div {:style {:display "flex"
                 :align-items "center"
                 :gap "0.5rem"}}
   [:label {:style {:font-size "0.75rem"
                    :color "#999"}} label]
   [:select {:value (name current-value)
             :on-change #(on-change (keyword (-> % .-target .-value)))
             :style {:padding "0.25rem 0.4rem"
                     :background "#2a2a4a"
                     :color "#eee"
                     :border "1px solid #4a4a6a"
                     :border-radius "3px"
                     :font-size "0.75rem"
                     :cursor "pointer"}}
    (for [[value label-text] options]
      ^{:key value}
      [:option {:value (name value)} label-text])]])

(defn identity-aspects
  "Render the sorted compound identity aspects (excluding atlas entity type)"
  [identity text-color]
  (let [;; Filter out the atlas entity type and sort the remaining aspects
        aspects (->> identity
                     (filter keyword?)
                     (remove #(= "atlas" (namespace %)))
                     sort
                     vec)]
    [:span {:style {:font-size "0.7rem"
                    :color text-color
                    :font-family "monospace"}}
     (str aspects)]))

(defn entity-row
  "Render a single entity as a clickable row"
  [dev-id identity {:keys [selected-entities selected-entities-not highlight-entities filter-mode on-entity-click]}]
  (let [selected? (contains? selected-entities dev-id)
        not-selected? (contains? selected-entities-not dev-id)
        highlighted? (and highlight-entities (contains? highlight-entities dev-id))
        should-hide? (and highlight-entities
                          (not highlighted?)
                          (= filter-mode :hide))
        dimmed? (and highlight-entities
                     (not highlighted?)
                     (= filter-mode :highlight))
        ;; Determine text color for compound identity based on background
        identity-text-color (cond
                              selected? "#cce5ff"      ; Light blue for selected (blue bg)
                              not-selected? "#ffcccc"  ; Light red for NOT selected (red bg)
                              highlighted? "#a8e6c1"   ; Light green for highlighted (green bg)
                              dimmed? "#444"           ; Dark for dimmed
                              :else "#888")]           ; Medium grey for default
    (when-not should-hide?
      [:div {:on-click #(on-entity-click dev-id)
             :style {:padding "0.3rem 0.5rem"
                     :margin "0.2rem 0"
                     :border-radius "4px"
                     :cursor "pointer"
                     :background (cond
                                   selected? "#4a9eff"
                                   not-selected? "#ef4a4a"
                                   highlighted? "#3a7a5a"
                                   :else "transparent")
                     :color (cond
                              selected? "#fff"
                              not-selected? "#fff"
                              dimmed? "#444"
                              :else "#bbb")
                     :opacity (if dimmed? 0.4 1)
                     :border-left (cond
                                    selected? "3px solid #6ab4ff"
                                    not-selected? "3px solid #ff6a6a"
                                    :else "3px solid transparent")
                     :transition "all 0.2s ease"
                     :display "flex"
                     :justify-content "space-between"
                     :align-items "center"}}
       [:span {:style {:font-size "0.85rem"}}
        (str dev-id)]
       [identity-aspects identity identity-text-color]])))

(defn type-section
  "Render an entity type with its entities"
  [entity-type dev-id-map sort-items opts]
  (let [{:keys [selected-types highlight-entities filter-mode on-type-click]} opts
        selected? (contains? selected-types entity-type)
        ;; Check if any entity in this type is highlighted
        type-has-highlight? (and highlight-entities
                                 (some (fn [[dev-id _]]
                                         (contains? highlight-entities dev-id))
                                       dev-id-map))
        ;; In hide mode, completely hide types with no matching entities
        should-hide-type? (and highlight-entities
                               (not type-has-highlight?)
                               (= filter-mode :hide))
        dimmed-type? (and highlight-entities
                          (not type-has-highlight?)
                          (= filter-mode :highlight))
        ;; Sort dev-ids according to sort-items setting
        sorted-entities (data/sort-dev-ids dev-id-map sort-items)]
    (when-not should-hide-type?
      [:div {:style {:margin-bottom "1.5rem"
                     :opacity (if dimmed-type? 0.4 1)
                     :transition "opacity 0.2s ease"}}
       ;; Type header (clickable)
       [:div {:on-click #(on-type-click entity-type)
              :style {:font-weight "bold"
                      :color (if selected? "#4a9eff" "#9a9aba")
                      :font-size "0.95rem"
                      :margin-bottom "0.5rem"
                      :padding "0.3rem 0.5rem"
                      :background (if selected? "#2a3a5a" "#1a1a2e")
                      :border-radius "4px"
                      :cursor "pointer"
                      :display "flex"
                      :justify-content "space-between"
                      :align-items "center"
                      :border-left (if selected?
                                     "4px solid #4a9eff"
                                     "4px solid #4a4a6a")}}
        [:span (name entity-type)]
        [:span {:style {:color "#5a5a7a"
                        :font-weight "normal"
                        :font-size "0.85rem"}}
         (str "(" (count dev-id-map) ")")]]
       ;; Entities list
       [:div {:style {:padding-left "0.5rem"}}
        (for [[dev-id identity] sorted-entities]
          ^{:key dev-id}
          [entity-row dev-id identity opts])]])))

(defn entities-map-view
  "Main entities map component"
  [entities-data opts]
  (let [{:keys [entities-map sort-items]} entities-data
        {:keys [sort-type on-sort-type on-sort-items]} opts]
    [:div {:style {:display "flex"
                   :flex-direction "column"
                   :height "100%"}}
     ;; Header with sort controls
     [:div {:style {:padding "0.75rem 1rem"
                    :background "#1a1a2e"
                    :border-bottom "1px solid #333"}}
      [:h2 {:style {:font-size "1.1rem"
                    :margin "0 0 0.75rem 0"
                    :color "#aaa"}}
       "Entities by Type"]
      [:div {:style {:display "flex"
                     :gap "0.75rem"
                     :flex-wrap "wrap"}}
       [sort-dropdown "Type"
        sort-type
        [[:alpha-asc "A-Z"]
         [:alpha-desc "Z-A"]
         [:count-desc "Most Entities"]
         [:count-asc "Fewest Entities"]]
        on-sort-type]
       [sort-dropdown "Items"
        sort-items
        [[:alpha-asc "A-Z"]
         [:alpha-desc "Z-A"]
         [:aspect-count-desc "Most Aspects"]
         [:aspect-count-asc "Fewest Aspects"]]
        on-sort-items]]]
     ;; Scrollable content
     [:div {:style {:flex 1
                    :overflow "auto"
                    :padding "1rem"
                    :color "#eee"}}
      (if (empty? entities-map)
        [:div {:style {:color "#666"}} "No entities found"]
        (for [[entity-type dev-id-map] entities-map]
          ^{:key entity-type}
          [type-section entity-type dev-id-map sort-items opts]))]]))
