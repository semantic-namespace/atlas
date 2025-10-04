(ns atlas-ui.search-bar)

(defn search-bar [search-term on-change]
  [:div.search-bar
   {:style {:position "absolute"
            :top "80px"
            :right "20px"
            :background "rgba(255, 255, 255, 0.95)"
            :backdropFilter "blur(8px)"
            :padding "12px"
            :borderRadius "8px"
            :boxShadow "0 2px 8px rgba(0, 0, 0, 0.1)"
            :zIndex 1001
            :minWidth "300px"}}

   [:input
    {:type "text"
     :placeholder "Search entities..."
     :value search-term
     :on-change #(on-change (-> % .-target .-value))
     :style {:width "100%"
             :padding "8px 12px"
             :border "1px solid #e5e7eb"
             :borderRadius "6px"
             :fontSize "13px"
             :fontFamily "system-ui, -apple-system, sans-serif"
             :outline "none"
             :transition "border-color 0.2s"}
     :on-focus #(set! (-> % .-target .-style .-borderColor) "#7c3aed")
     :on-blur #(set! (-> % .-target .-style .-borderColor) "#e5e7eb")}]

   (when (seq search-term)
     [:button
      {:on-click #(on-change "")
       :style {:position "absolute"
               :right "20px"
               :top "50%"
               :transform "translateY(-50%)"
               :border "none"
               :background "none"
               :color "#9ca3af"
               :cursor "pointer"
               :fontSize "18px"
               :padding "4px"
               :lineHeight "1"}}
      "Ã—"])])
