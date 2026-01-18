(ns atlas-ui-v2.aspects-map
  "Aspects Map component - displays namespace -> aspect names hierarchy.

   Click on an aspect to filter the entities map.")

(defn aspect-chip
  "Render a single aspect as a clickable chip"
  [aspect-name ns-key {:keys [aspects-and aspects-or highlight-aspects filter-mode on-click]}]
  (let [full-aspect (keyword (name ns-key) (name aspect-name))
        in-and? (contains? aspects-and full-aspect)
        in-or? (contains? aspects-or full-aspect)
        highlighted? (and highlight-aspects (contains? highlight-aspects full-aspect))
        dimmed? (and highlight-aspects
                     (not highlighted?)
                     (= filter-mode :highlight))]
    [:span {:on-click #(on-click full-aspect)
            :style {:display "inline-block"
                    :padding "0.25rem 0.5rem"
                    :margin "0.2rem"
                    :border-radius "4px"
                    :cursor "pointer"
                    :font-size "0.8rem"
                    :background (cond
                                  in-and? "#4a9eff"      ; Blue for AND
                                  in-or? "#4aef7a"       ; Green for OR
                                  highlighted? "#3a7a5a"
                                  :else "#2a2a4a")
                    :color (cond
                             in-and? "#fff"
                             in-or? "#000"
                             dimmed? "#555"
                             :else "#ccc")
                    :opacity (if dimmed? 0.4 1)
                    :border (cond
                              in-and? "1px solid #6ab4ff"
                              in-or? "1px solid #6aff9a"
                              :else "1px solid #3a3a5a")
                    :transition "all 0.2s ease"}}
     (name aspect-name)]))

(defn namespace-section
  "Render a namespace with its aspects"
  [ns-key aspect-names opts]
  (let [{:keys [aspects-and aspects-or highlight-aspects filter-mode]} opts
        ;; Check if any aspect in this namespace is selected (AND or OR)
        ns-has-selection? (some (fn [a-name]
                                  (let [full-aspect (keyword (name ns-key) (name a-name))]
                                    (or (contains? aspects-and full-aspect)
                                        (contains? aspects-or full-aspect))))
                                aspect-names)
        ;; Check if any aspect in this namespace is highlighted
        ns-has-highlight? (and highlight-aspects
                               (some (fn [a-name]
                                       (contains? highlight-aspects
                                                  (keyword (name ns-key) (name a-name))))
                                     aspect-names))
        dimmed-ns? (and highlight-aspects
                        (not ns-has-highlight?)
                        (= filter-mode :highlight))
        ;; In hide mode, hide namespace only if it has no selections AND no highlights
        should-hide-ns? (and (= filter-mode :hide)
                             highlight-aspects
                             (not ns-has-highlight?)
                             (not ns-has-selection?))]
    (when-not should-hide-ns?
      [:div {:style {:margin-bottom "1rem"
                     :opacity (if dimmed-ns? 0.4 1)
                     :transition "opacity 0.2s ease"}}
       [:div {:style {:font-weight "bold"
                      :color "#8a8aaa"
                      :font-size "0.9rem"
                      :margin-bottom "0.3rem"
                      :padding-left "0.5rem"
                      :border-left "3px solid #4a4a6a"}}
        (name ns-key)
        [:span {:style {:color "#5a5a7a"
                        :font-weight "normal"
                        :margin-left "0.5rem"}}
         (str "(" (count aspect-names) ")")]]
       [:div {:style {:padding-left "0.5rem"}}
        (for [aspect-name (sort aspect-names)]
          ^{:key aspect-name}
          [aspect-chip aspect-name ns-key opts])]])))

(defn aspects-map-view
  "Main aspects map component"
  [aspects-map opts]
  [:div {:style {:padding "1rem"
                 :color "#eee"}}
   [:h2 {:style {:font-size "1.1rem"
                 :margin-bottom "1rem"
                 :color "#aaa"}}
    "Aspects by Namespace"]
   (if (empty? aspects-map)
     [:div {:style {:color "#666"}} "No aspects found"]
     (for [[ns-key aspect-names] aspects-map]
       ^{:key ns-key}
       [namespace-section ns-key aspect-names opts]))])
