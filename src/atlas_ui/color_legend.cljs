(ns atlas-ui.color-legend
  (:require [atlas-ui.colors-v2 :as colors]))

(defn legend-color-box [color label]
  [:div {:style {:display "flex"
                 :alignItems "center"
                 :gap "8px"
                 :marginBottom "6px"}}
   [:div {:style {:width "20px"
                  :height "20px"
                  :backgroundColor color
                  :borderRadius "4px"
                  :border "1px solid #e5e7eb"}}]
   [:span {:style {:fontSize "11px"
                   :color "#6b7280"}}
    label]])

(defn legend-shape-item [{:keys [shape color label active? on-click]}]
  (let [radius (if (= shape :circle) "999px" "6px")
        size (if (= shape :circle) 16 20)
        height (if (= shape :circle) 16 14)]
    [:div {:style {:display "flex"
                   :alignItems "center"
                   :gap "8px"
                   :marginBottom "6px"
                   :cursor (when on-click "pointer")
                   :opacity (if active? "1.0" "0.9")}
           :on-click (when on-click #(on-click))}
     [:div {:style {:width (str size "px")
                    :height (str height "px")
                    :backgroundColor color
                    :borderRadius radius
                    :border (if active? "2px solid #6366f1" "1px solid #e5e7eb")
                    :boxShadow (when active? "0 0 0 2px rgba(99, 102, 241, 0.15)")}}]
     [:span {:style {:fontSize "11px"
                     :color "#6b7280"}}
      label]]))

(defn semantic-legend [legend-focus on-toggle-focus]
  (let [entity-items [{:label "Execution Function"
                       :color (:atlas/execution-function colors/entity-type-colors)
                       :shape :rect
                       :focus {:kind :entity-type :value :atlas/execution-function}}
                      {:label "Structure Component"
                       :color (:atlas/structure-component colors/entity-type-colors)
                       :shape :rect
                       :focus {:kind :entity-type :value :atlas/structure-component}}
                      {:label "Interface Endpoint"
                       :color (:atlas/interface-endpoint colors/entity-type-colors)
                       :shape :rect
                       :focus {:kind :entity-type :value :atlas/interface-endpoint}}
                      {:label "Interface Protocol"
                       :color (:atlas/interface-protocol colors/entity-type-colors)
                       :shape :rect
                       :focus {:kind :entity-type :value :atlas/interface-protocol}}
                      {:label "Governance Constraint"
                       :color (:atlas/governance-constraint colors/entity-type-colors)
                       :shape :rect
                       :focus {:kind :entity-type :value :atlas/governance-constraint}}
                      {:label "Business Pattern"
                       :color (:atlas/business-pattern colors/entity-type-colors)
                       :shape :rect
                       :focus {:kind :entity-type :value :atlas/business-pattern}}
                      {:label "Value Proposition"
                       :color (:atlas/value-proposition colors/entity-type-colors)
                       :shape :rect
                       :focus {:kind :entity-type :value :atlas/value-proposition}}
                      {:label "Data Schema"
                       :color (:atlas/data-schema colors/entity-type-colors)
                       :shape :rect
                       :focus {:kind :entity-type :value :atlas/data-schema}}
                      {:label "Identity Role"
                       :color (:atlas/identity-role colors/entity-type-colors)
                       :shape :rect
                       :focus {:kind :entity-type :value :atlas/identity-role}}
                      {:label "Experience Journey"
                       :color (:atlas/experience-journey colors/entity-type-colors)
                       :shape :rect
                       :focus {:kind :entity-type :value :atlas/experience-journey}}
                      {:label "Risk/Failure Mode"
                       :color (:atlas/risk-failure-mode colors/entity-type-colors)
                       :shape :rect
                       :focus {:kind :entity-type :value :atlas/risk-failure-mode}}
                      {:label "Interaction Intent"
                       :color (:semantic-namespace/interaction-intent colors/entity-type-colors)
                       :shape :rect
                       :focus {:kind :entity-type :value :semantic-namespace/interaction-intent}}]
        aspect-items [{:label "Aspect: Tier"
                       :color (:aspect-tier colors/base-colors)
                       :shape :circle
                       :focus {:kind :aspect-namespace :value "tier"}}
                      {:label "Aspect: Domain"
                       :color (:aspect-domain colors/base-colors)
                       :shape :circle
                       :focus {:kind :aspect-namespace :value "domain"}}
                      {:label "Aspect: Protocol"
                       :color (:aspect-protocol colors/base-colors)
                       :shape :circle
                       :focus {:kind :aspect-namespace :value "protocol"}}
                      {:label "Aspect: Operation"
                       :color (:aspect-operation colors/base-colors)
                       :shape :circle
                       :focus {:kind :aspect-namespace :value "operation"}}
                      {:label "Aspect: Constraint"
                       :color (:aspect-constraint colors/base-colors)
                       :shape :circle
                       :focus {:kind :aspect-namespace :value "constraint"}}
                      {:label "Aspect: Lens Type"
                       :color (:aspect-lens-type colors/base-colors)
                       :shape :circle
                       :focus {:kind :aspect-namespace :value "lens-type"}}
                      {:label "Aspect: Feature Type"
                       :color (:aspect-feature-type colors/base-colors)
                       :shape :circle
                       :focus {:kind :aspect-namespace :value "feature-type"}}
                      {:label "Aspect: Effect"
                       :color (:aspect-effect colors/base-colors)
                       :shape :circle
                       :focus {:kind :aspect-namespace :value "effect"}}
                      {:label "Aspect: Other"
                       :color (:aspect-other colors/base-colors)
                       :shape :circle}]]
    [:div.semantic-legend
     {:style {:position "absolute"
              :top "160px"
              :right "20px"
              :background "rgba(255, 255, 255, 0.95)"
              :backdropFilter "blur(8px)"
              :padding "16px"
              :borderRadius "8px"
              :boxShadow "0 4px 6px rgba(0, 0, 0, 0.1)"
              :fontSize "12px"
              :zIndex 1000
              :minWidth "240px"
              :maxWidth "320px"}}
     [:div {:style {:marginBottom "12px"
                    :fontWeight "600"
                    :color "#374151"}}
      "Legend"]
     [:div {:style {:marginBottom "8px"
                    :fontSize "11px"
                    :fontWeight "600"
                    :color "#6b7280"
                    :textTransform "uppercase"
                    :letterSpacing "0.04em"}}
      "Entities"]
     [:div {:style {:display "grid"
                    :gridTemplateColumns "repeat(2, minmax(0, 1fr))"
                    :columnGap "12px"
                    :rowGap "2px"
                    :marginBottom "12px"}}
      (for [item entity-items]
        ^{:key (:label item)}
        [legend-shape-item (assoc item
                                  :active? (= legend-focus (:focus item))
                                  :on-click (when (and on-toggle-focus (:focus item))
                                              #(on-toggle-focus (:focus item))))])]
     [:div {:style {:marginBottom "8px"
                    :fontSize "11px"
                    :fontWeight "600"
                    :color "#6b7280"
                    :textTransform "uppercase"
                    :letterSpacing "0.04em"}}
      "Aspects"]
     [:div {:style {:display "grid"
                    :gridTemplateColumns "repeat(2, minmax(0, 1fr))"
                    :columnGap "12px"
                    :rowGap "2px"}}
      (for [item aspect-items]
        ^{:key (:label item)}
        [legend-shape-item (assoc item
                                  :active? (and (:focus item)
                                                (= legend-focus (:focus item)))
                                  :on-click (when (and on-toggle-focus (:focus item))
                                              #(on-toggle-focus (:focus item))))])]]))

(defn color-legend [selections query-mode]
  (when (seq selections)
    [:div.color-legend
     {:style {:position "absolute"
              :bottom "20px"
              :right "20px"
              :background "rgba(255, 255, 255, 0.95)"
              :backdropFilter "blur(8px)"
              :padding "16px"
              :borderRadius "8px"
              :boxShadow "0 4px 6px rgba(0, 0, 0, 0.1)"
              :fontSize "12px"
              :zIndex 1000
              :minWidth "200px"}}

     [:div {:style {:marginBottom "12px"
                    :fontWeight "600"
                    :color "#374151"}}
      "Color Legend"]

     (case query-mode
       :and
       [:div
        [legend-color-box "#7c3aed" "Has ALL aspects"]
        [legend-color-box "#e5e7eb" "Missing some"]]

       :or
       [:div
        [legend-color-box "#7c3aed" "Has ANY aspect"]
        [legend-color-box "#e5e7eb" "No match"]]

       :count
       [:div
        [legend-color-box "#7c3aed" "100% match"]
        [legend-color-box "#b8a3d9" "66% match"]
        [legend-color-box "#9c7dce" "33% match"]
        [legend-color-box "#e5e7eb" "0% match"]
        [:div {:style {:marginTop "8px"
                       :paddingTop "8px"
                       :borderTop "1px solid #e5e7eb"
                       :fontSize "10px"
                       :color "#9ca3af"
                       :fontStyle "italic"}}
         "Intensity = % of aspects matched"]])

     ;; Show category colors if present
     (let [categories (group-by namespace selections)
           has-tier? (contains? categories "tier")
           has-domain? (contains? categories "domain")
           has-operation? (contains? categories "operation")]
       (when (or has-tier? has-domain? has-operation?)
         [:div {:style {:marginTop "12px"
                        :paddingTop "12px"
                        :borderTop "1px solid #e5e7eb"}}
          [:div {:style {:marginBottom "8px"
                         :fontSize "11px"
                         :fontWeight "600"
                         :color "#6b7280"}}
           "Category Colors"]
          (when has-tier?
            [:div
             [legend-color-box "#ef4444" "Foundation"]
             [legend-color-box "#f59e0b" "Service"]
             [legend-color-box "#10b981" "API"]])
          (when has-domain?
            [:div
             [legend-color-box "#8b5cf6" "Users"]
             [legend-color-box "#ec4899" "Auth"]
             [legend-color-box "#06b6d4" "Scheduling"]])]))]))
