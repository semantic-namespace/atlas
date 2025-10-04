(ns atlas-ui.sidebar
  (:require [atlas-ui.graph-data :as gd]
            [atlas-ui.colors-v2 :as colors]
            [atlas.ontology :as ot]))

(defn- definition-entries
  [props identity]
  (let [definition-keys (ot/definition-keys-for-identity identity)]
    (if (seq definition-keys)
      (keep (fn [k]
              (when (contains? props k)
                (let [v (get props k)]
                  (when (some? v)
                    [k v]))))
            definition-keys)
      (->> props
           (remove (fn [[_ v]] (nil? v)))
           (sort-by (comp str key))))))

(defn aspect-tag
  "Render an aspect as a styled tag"
  [aspect]
  [:span.aspect-tag
   {:key (str aspect)
    :style {:display "inline-block"
            :padding "4px 10px"
            :margin "4px 4px 4px 0"
            :backgroundColor "#f3f4f6"
            :border "1px solid #e5e7eb"
            :borderRadius "12px"
            :fontSize "11px"
            :fontFamily "ui-monospace, monospace"
            :color "#6b7280"}}
   (str aspect)])

(defn- value-tag
  [value on-click]
  (let [clickable? (some? on-click)]
    [:span.value-tag
     {:key (str value)
      :style {:display "inline-block"
              :padding "4px 10px"
              :margin "4px 6px 4px 0"
              :backgroundColor (if clickable? "#e0e7ff" "#f3f4f6")
              :border "1px solid #e5e7eb"
              :borderRadius "10px"
              :fontSize "11px"
              :fontFamily "ui-monospace, monospace"
              :color "#374151"
              :cursor (when clickable? "pointer")}
     :on-click (when clickable? #(on-click value))}
     (str value)]))

(defn- ordered-items
  [value]
  (cond
    (set? value) (sort value)
    (sequential? value) value
    :else nil))

(defn- keyword-items?
  [items]
  (and (seq items) (every? keyword? items)))

(defn- render-value
  [value on-click clickable?]
  (cond
    (keyword? value)
    [value-tag value (when (clickable? value) on-click)]

    (map? value)
    [:div {:style {:display "flex" :flexDirection "column" :gap "6px"}}
     (for [[k v] (sort-by (comp str key) value)]
       ^{:key (str k)}
       [:div {:style {:display "flex" :gap "8px" :alignItems "baseline"}}
        [:span {:style {:fontFamily "ui-monospace, monospace"
                        :fontSize "11px"
                        :color "#6b7280"
                        :minWidth "120px"}}
         (str k)]
        [render-value v on-click clickable?]])]

    :else
    (if-let [items (ordered-items value)]
      (if (keyword-items? items)
        [:div {:style {:display "flex" :flexWrap "wrap"}}
         (for [item items]
           ^{:key (str item)}
           [value-tag item (when (clickable? item) on-click)])]
        [:div {:style {:display "flex" :flexDirection "column" :gap "4px"}}
         (for [item items]
           ^{:key (str item)}
           [:div {:style {:fontFamily "ui-monospace, monospace"
                          :fontSize "11px"
                          :color "#374151"}}
            [render-value item on-click clickable?]])])
      [:div {:style {:fontFamily "ui-monospace, monospace"
                     :fontSize "11px"
                     :color "#374151"}}
       (str value)])))

;; =============================================================================
;; Entity Type Badge Component (Phase 2)
;; =============================================================================

(defn entity-type-badge
  "Display entity type as a colored badge with proper formatting.
   Uses the color from the entity-type-colors palette."
  [entity-type]
  (when entity-type
    (let [type-color (get colors/entity-type-colors entity-type (:default colors/entity-type-colors))
          type-name (name entity-type)
          ;; Format: "atlas/interface-endpoint" -> "Interface Endpoint"
          formatted-name (-> type-name
                            (clojure.string/replace #"-" " ")
                            (clojure.string/split #"/")
                            last
                            (clojure.string/split #" ")
                            (->> (map clojure.string/capitalize)
                                 (clojure.string/join " ")))]
      [:div {:style {:display "inline-flex"
                     :alignItems "center"
                     :padding "4px 12px"
                     :backgroundColor type-color
                     :borderRadius "6px"
                     :fontSize "12px"
                     :fontWeight "600"
                     :color "#1f2937"
                     :marginTop "8px"
                     :boxShadow "0 1px 2px rgba(0, 0, 0, 0.05)"}}
       formatted-name])))

(defn entity-detail
  "Detail panel for an entity.
   Enhanced in Phase 2 to display type badge and separate aspects."
  [entity graph-data registry on-toggle-aspect on-select-entity]
  (let [props (:atlas-ui.graph.node/props entity)
        ;; Use new fields with fallback to identity for backward compatibility
        entity-type (:atlas-ui.graph.node/entity-type entity)
        aspects (or (:atlas-ui.graph.node/aspects entity)
                    (:atlas-ui.graph.node/identity entity))
        full-identity (:atlas-ui.graph.node/identity entity)
        entity-ids (set (map :atlas-ui.graph.node/id (:atlas-ui.graph.data/entities graph-data)))
        aspect-ids (set (map :atlas-ui.graph.node/id (:atlas-ui.graph.data/aspects graph-data)))
        clickable? (fn [value]
                     (or (contains? entity-ids value)
                         (contains? aspect-ids value)))
        on-click (fn [value]
                   (cond
                     (and on-select-entity (contains? entity-ids value))
                     (when-let [node (gd/find-node-by-id graph-data value)]
                       (on-select-entity node))
                     (and on-toggle-aspect (contains? aspect-ids value))
                     (on-toggle-aspect value)
                     :else nil))
        entries (definition-entries props full-identity)]
    [:div.entity-detail
     ;; Entity Type Section (NEW in Phase 2)
     (when entity-type
       [:div {:style {:marginBottom "20px"}}
        [:h3 {:style {:fontSize "13px"
                      :color "#6b7280"
                      :textTransform "uppercase"
                      :letterSpacing "0.05em"
                      :marginBottom "4px"}}
         "Entity Type"]
        [entity-type-badge entity-type]])

     ;; Aspects Section (separated from type)
     [:div {:style {:marginBottom "20px"}}
      [:h3 {:style {:fontSize "13px"
                    :color "#6b7280"
                    :textTransform "uppercase"
                    :letterSpacing "0.05em"
                    :marginBottom "8px"}}
       (str "Aspects" (when (seq aspects) (str " (" (count aspects) ")")))]
      (if (seq aspects)
        [:div {:style {:display "flex"
                       :flexWrap "wrap"}}
         (for [aspect (sort aspects)]
           ^{:key (str aspect)}
           [aspect-tag aspect])]
        [:div {:style {:fontSize "12px"
                       :color "#9ca3af"
                       :fontStyle "italic"}}
         "No aspects"])]

     ;; Properties Section
     (when (seq entries)
       (for [[k v] entries]
         ^{:key (str k)}
         [:div {:style {:marginBottom "18px"}}
         [:h3 {:style {:fontSize "13px"
                        :color "#6b7280"
                        :textTransform "uppercase"
                        :letterSpacing "0.05em"
                        :marginBottom "8px"}}
           (str k)]
          [render-value v on-click clickable?]]))]))

(defn sidebar-component
  "Sidebar showing details of entity (only shows for entities, not aspects)"
  [entity graph-data registry on-close on-toggle-aspect on-select-entity]
  [:div.sidebar
   {:style {:width "380px"
            :height "100%"
            :backgroundColor "#ffffff"
            :borderLeft "1px solid #e5e7eb"
            :overflowY "auto"
            :padding "24px"
            :boxShadow "-2px 0 8px rgba(0, 0, 0, 0.05)"}}

   [:div
    ;; Header with close button
    [:div {:style {:display "flex"
                   :justifyContent "space-between"
                   :alignItems "flex-start"
                   :marginBottom "24px"
                   :paddingBottom "16px"
                   :borderBottom "2px solid #e5e7eb"}}
     [:div {:style {:flex "1"}}
      [:h2 {:style {:fontSize "18px"
                    :fontWeight "600"
                    :color "#111827"
                    :margin "0"
                    :wordBreak "break-all"}}
       (str (:atlas-ui.graph.node/id entity))]
      [:div {:style {:fontSize "11px"
                     :color "#9ca3af"
                     :marginTop "4px"
                     :textTransform "uppercase"
                     :letterSpacing "0.05em"}}
       (name (:atlas-ui.graph.node/type entity))]]

     [:button
      {:on-click on-close
       :style {:padding "6px 10px"
               :border "1px solid #e5e7eb"
               :borderRadius "6px"
               :backgroundColor "white"
               :color "#6b7280"
               :fontSize "16px"
               :cursor "pointer"
               :transition "all 0.2s"
               :lineHeight "1"}
       :on-mouse-over #(set! (-> % .-target .-style .-backgroundColor) "#f9fafb")
       :on-mouse-out #(set! (-> % .-target .-style .-backgroundColor) "white")}
      "×"]]

    [entity-detail entity graph-data registry on-toggle-aspect on-select-entity]]])
