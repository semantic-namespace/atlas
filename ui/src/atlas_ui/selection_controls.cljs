(ns atlas-ui.selection-controls
  (:require [atlas-ui.graph-data :as gd]
            [atlas-ui.colors-v2 :as colors]))

(defn mode-button [current-mode mode label on-click]
  [:button.mode-btn
   {:on-click #(on-click mode)
    :style {:padding "6px 12px"
            :margin "0 4px 0 0"
            :border "1px solid"
            :borderColor (if (= current-mode mode) "#7c3aed" "#e5e7eb")
            :borderRadius "6px"
            :fontSize "11px"
            :fontWeight "500"
            :cursor "pointer"
            :backgroundColor (if (= current-mode mode) "#7c3aed" "white")
            :color (if (= current-mode mode) "white" "#6b7280")
            :transition "all 0.2s"}}
   label])

(defn aspect-badge
  [aspect {:keys [kind on-remove]}]
  [:span.aspect-badge
   {:key (str aspect)
    :style {:display "inline-flex"
            :alignItems "center"
            :gap "6px"
            :padding "6px 10px"
            :margin "3px"
            :backgroundColor (case kind
                               :negated "#ef4444"
                               "#7c3aed")
            :color "white"
            :borderRadius "6px"
            :fontSize "11px"
            :fontFamily "ui-monospace, monospace"
            :boxShadow "0 1px 2px rgba(0,0,0,0.1)"}}
   [:span (str aspect)]
   [:span.remove-btn
    {:on-click #(do (.stopPropagation %)
                    (on-remove aspect))
     :style {:cursor "pointer"
             :fontWeight "700"
             :fontSize "14px"
             :lineHeight "1"
             :opacity "0.8"
             :transition "opacity 0.2s"}
     :on-mouse-over #(set! (-> % .-target .-style .-opacity) "1")
     :on-mouse-out #(set! (-> % .-target .-style .-opacity) "0.8")}
    "×"]])

(defn mode-description [mode]
  (case mode
    :and "Show entities with ALL selected aspects"
    :or "Show entities with ANY selected aspect"
    :count "Color by number of matching aspects"))

(defn selection-controls
  [selections negated-aspects query-mode min-score on-toggle-aspect on-set-mode on-clear on-toggle-visibility hide-unmatched? on-set-min-score]
  [:div.selection-controls
   {:style {:position "absolute"
            :top "20px"
            :left "20px"
            :background "rgba(255, 255, 255, 0.95)"
            :backdropFilter "blur(8px)"
            :padding "16px"
            :borderRadius "8px"
            :boxShadow "0 4px 12px rgba(0, 0, 0, 0.15)"
            :zIndex 1001
            :maxWidth "400px"
            :border "2px solid #7c3aed"}}

   ;; Header with clear and visibility toggle
   [:div {:style {:display "flex"
                  :justifyContent "space-between"
                  :alignItems "center"
                  :marginBottom "12px"}}
    [:div {:style {:fontSize "13px"
                   :fontWeight "600"
                   :color "#374151"}}
     "Active Query (+" (count selections) " / -" (count negated-aspects) ")"]
    [:div {:style {:display "flex"
                   :gap "8px"}}
     [:button
      {:on-click on-toggle-visibility
       :title (if hide-unmatched? "Show all nodes" "Hide unmatched nodes")
       :style {:padding "4px 10px"
               :border "1px solid"
               :borderColor (if hide-unmatched? "#7c3aed" "#e5e7eb")
               :borderRadius "4px"
               :fontSize "11px"
               :cursor "pointer"
               :backgroundColor (if hide-unmatched? "#7c3aed" "white")
               :color (if hide-unmatched? "white" "#6b7280")
               :transition "all 0.2s"}}
      (if hide-unmatched? "Hide ✓" "Show All")]
     [:button
      {:on-click on-clear
       :style {:padding "4px 10px"
               :border "1px solid #e5e7eb"
               :borderRadius "4px"
               :fontSize "11px"
               :cursor "pointer"
               :backgroundColor "white"
               :color "#6b7280"
               :transition "all 0.2s"}
       :on-mouse-over #(set! (-> % .-target .-style .-backgroundColor) "#f9fafb")
       :on-mouse-out #(set! (-> % .-target .-style .-backgroundColor) "white")}
      "Clear All"]]]

   ;; Selected aspects (+)
   [:div.selected-aspects
    {:style {:display "flex"
             :flexWrap "wrap"
             :marginBottom "12px"
             :padding "8px"
             :backgroundColor "#f9fafb"
             :borderRadius "6px"
             :minHeight "40px"}}
    (if (seq selections)
      (for [sel selections]
        ^{:key (str sel)}
        [aspect-badge sel {:kind :selected
                           :on-remove #(on-toggle-aspect % false)}])
      [:div {:style {:fontSize "11px"
                     :color "#9ca3af"
                     :fontStyle "italic"}}
       "Click aspects to add them to query"])]

   ;; Negated aspects (-)
   [:div.negated-aspects
    {:style {:display "flex"
             :flexWrap "wrap"
             :marginBottom "12px"
             :padding "8px"
             :backgroundColor "#fef2f2"
             :borderRadius "6px"
             :minHeight "40px"
             :border "1px solid #fee2e2"}}
    (if (seq negated-aspects)
      (for [neg negated-aspects]
        ^{:key (str neg)}
        [aspect-badge neg {:kind :negated
                           :on-remove #(on-toggle-aspect % true)}])
      [:div {:style {:fontSize "11px"
                     :color "#9ca3af"
                     :fontStyle "italic"}}
       "Shift+click an aspect to negate (NOT)"])]

   ;; Query mode selector
   [:div.query-mode
    {:style {:marginBottom "8px"}}
    [:div {:style {:fontSize "11px"
                   :color "#6b7280"
                   :marginBottom "6px"
                   :textTransform "uppercase"
                   :letterSpacing "0.05em"}}
     "Query Mode"]
    [:div {:style {:display "flex"}}
     [mode-button query-mode :and "AND (âˆ©)" on-set-mode]
     [mode-button query-mode :or "OR (âˆª)" on-set-mode]
     [mode-button query-mode :count "COUNT (Î£)" on-set-mode]]]

   ;; Min score (threshold)
   (let [threshold-enabled? (and hide-unmatched? (= query-mode :count) (seq selections))
         slider-disabled? (not threshold-enabled?)]
     [:div.min-score
      {:style {:marginTop "10px"
               :padding "10px"
               :backgroundColor (if slider-disabled? "#f9fafb" "#ffffff")
               :border "1px solid #e5e7eb"
               :borderRadius "6px"
               :opacity (if slider-disabled? 0.6 1.0)}}
      [:div {:style {:display "flex"
                     :justifyContent "space-between"
                     :alignItems "center"
                     :marginBottom "6px"}}
       [:div {:style {:fontSize "11px"
                      :color "#6b7280"
                      :textTransform "uppercase"
                      :letterSpacing "0.05em"}}
        "Min Score"]
       [:div {:style {:fontSize "11px"
                      :fontFamily "ui-monospace, monospace"
                      :color "#374151"}}
        (str (int (* 100 (or min-score 0.0))) "%")]]
      [:input {:type "range"
               :min 0
               :max 1
               :step 0.05
               :value (or min-score 0.0)
               :disabled slider-disabled?
               :style {:width "100%"}
               :on-change #(on-set-min-score (js/parseFloat (.. % -target -value)))}]
      (when slider-disabled?
        [:div {:style {:marginTop "6px"
                       :fontSize "10px"
                       :color "#9ca3af"
                       :fontStyle "italic"}}
         "Enable Hide ✓, select aspects, and use COUNT mode"])])

   ;; Mode description
   [:div {:style {:fontSize "10px"
                  :color "#9ca3af"
                  :fontStyle "italic"
                  :marginTop "8px"
                  :padding "8px"
                  :backgroundColor "#f9fafb"
                  :borderRadius "4px"}}
    (mode-description query-mode)]])

;; =============================================================================
;; Entity Type Filter Component (Phase 2)
;; =============================================================================

(defn type-filter-badge
  "Display a single entity type as a toggleable filter badge"
  [entity-type count active? on-toggle]
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
    [:button
     {:on-click #(on-toggle entity-type)
      :style {:display "flex"
              :alignItems "center"
              :justifyContent "space-between"
              :width "100%"
              :padding "8px 12px"
              :margin "4px 0"
              :border (if active? "2px solid #7c3aed" "1px solid #e5e7eb")
              :borderRadius "6px"
              :backgroundColor (if active? type-color "#ffffff")
              :cursor "pointer"
              :transition "all 0.2s"
              :opacity (if active? 1.0 0.7)}
      :on-mouse-over #(when-not active?
                        (set! (-> % .-target .-style .-opacity) "0.9"))
      :on-mouse-out #(when-not active?
                       (set! (-> % .-target .-style .-opacity) "0.7"))}
     [:div {:style {:display "flex"
                    :alignItems "center"
                    :gap "8px"}}
      [:div {:style {:width "12px"
                     :height "12px"
                     :borderRadius "3px"
                     :backgroundColor type-color
                     :boxShadow "0 1px 2px rgba(0,0,0,0.1)"}}]
      [:span {:style {:fontSize "12px"
                      :fontWeight (if active? "600" "500")
                      :color (if active? "#111827" "#6b7280")}}
       formatted-name]]
     [:span {:style {:fontSize "11px"
                     :fontFamily "ui-monospace, monospace"
                     :color (if active? "#6b7280" "#9ca3af")
                     :fontWeight "600"}}
      count]]))

(defn type-filter-panel
  "Panel for filtering entities by type.
   Shows all available entity types with counts, allows multi-select."
  [graph-data selected-types on-toggle-type]
  (let [type-counts (gd/type-counts graph-data)
        sorted-types (sort-by (fn [[type _]] (name type)) type-counts)]
    [:div.type-filter-panel
     {:style {:position "absolute"
              :top "20px"
              :right "20px"
              :background "rgba(255, 255, 255, 0.95)"
              :backdropFilter "blur(8px)"
              :padding "16px"
              :borderRadius "8px"
              :boxShadow "0 4px 12px rgba(0, 0, 0, 0.15)"
              :zIndex 1001
              :width "280px"
              :border "2px solid #10b981"}}

     ;; Header
     [:div {:style {:marginBottom "12px"
                    :paddingBottom "12px"
                    :borderBottom "2px solid #e5e7eb"}}
      [:div {:style {:fontSize "13px"
                     :fontWeight "600"
                     :color "#374151"
                     :marginBottom "4px"}}
       "Filter by Type"]
      [:div {:style {:fontSize "10px"
                     :color "#9ca3af"}}
       (if (seq selected-types)
         (str "Showing " (count selected-types) " of " (count type-counts) " types")
         (str "All " (count type-counts) " types visible"))]]

     ;; Type list
     [:div {:style {:maxHeight "400px"
                    :overflowY "auto"}}
      (if (seq sorted-types)
        (for [[entity-type count] sorted-types]
          ^{:key entity-type}
          [type-filter-badge
           entity-type
           count
           (contains? (set selected-types) entity-type)
           on-toggle-type])
        [:div {:style {:fontSize "12px"
                       :color "#9ca3af"
                       :fontStyle "italic"
                       :textAlign "center"
                       :padding "20px"}}
         "No entity types found"])]

     ;; Clear button
     (when (seq selected-types)
       [:button
        {:on-click #(doseq [type selected-types]
                      (on-toggle-type type))
         :style {:width "100%"
                 :marginTop "12px"
                 :padding "8px"
                 :border "1px solid #e5e7eb"
                 :borderRadius "6px"
                 :fontSize "11px"
                 :fontWeight "500"
                 :cursor "pointer"
                 :backgroundColor "white"
                 :color "#6b7280"
                 :transition "all 0.2s"}
         :on-mouse-over #(set! (-> % .-target .-style .-backgroundColor) "#f9fafb")
         :on-mouse-out #(set! (-> % .-target .-style .-backgroundColor) "white")}
        "Clear Type Filter"])]))
