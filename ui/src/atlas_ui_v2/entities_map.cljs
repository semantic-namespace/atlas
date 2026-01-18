(ns atlas-ui-v2.entities-map
  "Entities Map component - displays type -> dev-id -> identity hierarchy.

   Click on a type or entity to filter the aspects map.")

(defn identity-preview
  "Render a compact preview of a compound identity"
  [identity]
  (let [;; Group aspects by namespace for compact display
        by-ns (->> identity
                   (filter keyword?)
                   (group-by namespace))]
    [:span {:style {:font-size "0.7rem"
                    :color "#666"}}
     (for [[ns-name aspects] (take 3 (sort by-ns))]
       ^{:key ns-name}
       [:span {:style {:margin-right "0.3rem"}}
        (str ns-name ":" (count aspects))])
     (when (> (count by-ns) 3)
       [:span "..."])]))

(defn entity-row
  "Render a single entity as a clickable row"
  [dev-id identity {:keys [selected-entities highlight-entities filter-mode on-entity-click]}]
  (let [selected? (contains? selected-entities dev-id)
        highlighted? (and highlight-entities (contains? highlight-entities dev-id))
        should-hide? (and highlight-entities
                          (not highlighted?)
                          (= filter-mode :hide))
        dimmed? (and highlight-entities
                     (not highlighted?)
                     (= filter-mode :highlight))]
    (when-not should-hide?
      [:div {:on-click #(on-entity-click dev-id)
             :style {:padding "0.3rem 0.5rem"
                     :margin "0.2rem 0"
                     :border-radius "4px"
                     :cursor "pointer"
                     :background (cond
                                   selected? "#4a9eff"
                                   highlighted? "#3a7a5a"
                                   :else "transparent")
                     :color (cond
                              selected? "#fff"
                              dimmed? "#444"
                              :else "#bbb")
                     :opacity (if dimmed? 0.4 1)
                     :border-left (if selected?
                                    "3px solid #6ab4ff"
                                    "3px solid transparent")
                     :transition "all 0.2s ease"
                     :display "flex"
                     :justify-content "space-between"
                     :align-items "center"}}
       [:span {:style {:font-size "0.85rem"}}
        (str dev-id)]
       [identity-preview identity]])))

(defn type-section
  "Render an entity type with its entities"
  [entity-type dev-id-map opts]
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
                          (= filter-mode :highlight))]
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
        (for [[dev-id identity] dev-id-map]
          ^{:key dev-id}
          [entity-row dev-id identity opts])]])))

(defn entities-map-view
  "Main entities map component"
  [entities-map opts]
  [:div {:style {:padding "1rem"
                 :color "#eee"}}
   [:h2 {:style {:font-size "1.1rem"
                 :margin-bottom "1rem"
                 :color "#aaa"}}
    "Entities by Type"]
   (if (empty? entities-map)
     [:div {:style {:color "#666"}} "No entities found"]
     (for [[entity-type dev-id-map] entities-map]
       ^{:key entity-type}
       [type-section entity-type dev-id-map opts]))])
