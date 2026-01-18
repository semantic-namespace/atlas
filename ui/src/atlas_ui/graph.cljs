(ns atlas-ui.graph
  (:require [reagent.core :as r]
            [atlas.query :as q]
            [atlas-ui.colors-v2 :as colors]
            [atlas-ui.graph-data :as gd]
            [atlas-ui.id :as id]
            [clojure.string :as str]
            ["cytoscape" :as cytoscape]
            ["cytoscape-navigator" :as navigator])) ;; ENTROPY REDUCTION: Minimap

(defn truncate-label
  "Truncate label to max-chars, adding ellipsis if needed.
  Preserves newlines for badge text."
  [label max-chars]
  (if (str/includes? label "\n")
    ;; Has badges - truncate only the first line
    (let [[first-line & rest-lines] (str/split label #"\n")
          truncated-first (if (> (count first-line) max-chars)
                            (str (subs first-line 0 max-chars) "...")
                            first-line)]
      (str/join "\n" (cons truncated-first rest-lines)))
    ;; No badges - simple truncation
    (if (> (count label) max-chars)
      (str (subs label 0 max-chars) "...")
      label)))

(defn build-cytoscape-elements
  [graph-data selections negated-aspects query-mode min-score highlighted-entity-id highlighted-entity-aspects registry hide-unmatched? search-term]
  (let [query {::q/selected selections
               ::q/negated negated-aspects
               ::q/mode query-mode
               ::q/min-score (if hide-unmatched? min-score 0.0)}
        search-lower (str/lower-case (or search-term ""))
        node-matches-search? (fn [node]
                               (if (seq search-term)
                                 (str/includes?
                                  (str/lower-case (:atlas-ui.graph.node/label node))
                                  search-lower)
                                 true))
        should-show-node? (fn [node]
                            (if hide-unmatched?
                              (case (:atlas-ui.graph.node/type node)
                                :entity (q/query-matches? (:atlas-ui.graph.node/identity node) query)
                                :aspect (or (contains? selections (:atlas-ui.graph.node/id node))
                                            (contains? negated-aspects (:atlas-ui.graph.node/id node)))
                                true)
                              true))

        ;; All entities use rectangle shape (color differentiates type)
        entity-shape (fn [identity] "round-rectangle")

        nodes (for [node (:atlas-ui.graph.data/all-nodes graph-data)
                    :when (should-show-node? node)]
                (let [color (colors/node-color node query highlighted-entity-id highlighted-entity-aspects)
                      ;; Use new :aspects field for better performance, fall back to :identity
                      opacity (if (= (:atlas-ui.graph.node/type node) :entity)
                                (colors/calculate-opacity
                                 (or (:atlas-ui.graph.node/aspects node)
                                     (:atlas-ui.graph.node/identity node))
                                 query)
                                1.0)
                      is-selected (contains? selections (:atlas-ui.graph.node/id node))
                      is-negated (contains? negated-aspects (:atlas-ui.graph.node/id node))
                      matches-search (node-matches-search? node)
                      shape (if (= (:atlas-ui.graph.node/type node) :entity)
                              (entity-shape (:atlas-ui.graph.node/identity node))
                              "ellipse") ; aspects stay as circles

                      ;; Build badge text for lens mode
                      badges (:atlas-ui.graph.node/aspect-badges node)
                      badge-text (when (seq badges)
                                   (str "\n" (clojure.string/join " "
                                                                  (map #(str "🏷️" (name %)) (take 3 badges)))))

                      ;; Combine label with badges
                      full-label (str (:atlas-ui.graph.node/label node) (or badge-text ""))

                      ;; ENTROPY REDUCTION: Truncate label for display
                      truncated-label (truncate-label full-label 18)]
                  {:data {:id (id/id->string (:atlas-ui.graph.node/id node))
                          :type (name (:atlas-ui.graph.node/type node))
                          :label truncated-label
                          :fullLabel full-label ;; Store full label for tooltip
                          :identity (:atlas-ui.graph.node/identity node)
                          :color color
                          :opacity (if (and (seq search-term) (not matches-search))
                                     (* opacity 0.3)
                                     opacity)
                          :selected (str is-selected)
                          :negated (str is-negated)
                          :highlighted (str (= (:atlas-ui.graph.node/id node) highlighted-entity-id))
                          :search-match (str matches-search)
                          :shape shape}}))
        visible-node-ids (set (map #(get-in % [:data :id]) nodes))
        edges (for [edge (:atlas-ui.graph.data/edges graph-data)
                    :let [source-id (id/id->string (:atlas-ui.graph.edge/source edge))
                          target-id (id/id->string (:atlas-ui.graph.edge/target edge))
                          edge-type (:atlas-ui.graph.edge/type edge)]
                    :when (and (contains? visible-node-ids source-id)
                               (contains? visible-node-ids target-id)
                               (not= source-id target-id))]
                {:data {:source source-id
                        :target target-id
                        :edge-type (name edge-type)
                        :edge-label (case edge-type
                                      :membership "has"
                                      :dependency "uses"
                                      "")
                        :color (colors/edge-color edge-type)}})
        _ (js/console.log "📊 Edges:" (count edges) "visible ("
                          (count (filter #(= "dependency" (get-in % [:data :edge-type])) edges)) "dependency,"
                          (count (filter #(= "membership" (get-in % [:data :edge-type])) edges)) "membership)")]
    (clj->js {:nodes nodes :edges edges})))

(defn- legend-focus-node-ids
  [graph-data legend-focus]
  (case (:kind legend-focus)
    :entity-type
    (->> (:atlas-ui.graph.data/entities graph-data)
         (filter (fn [entity]
                   (contains? (:atlas-ui.graph.node/identity entity)
                              (:value legend-focus))))
         (map :atlas-ui.graph.node/id)
         set)
    :aspect-namespace
    (->> (:atlas-ui.graph.data/aspects graph-data)
         (filter (fn [aspect]
                   (= (namespace (:atlas-ui.graph.node/id aspect))
                      (:value legend-focus))))
         (map :atlas-ui.graph.node/id)
         set)
    #{}))

(defn- apply-pinned!
  [cy graph-data highlighted-entity-id highlighted-aspect-id legend-focus]
  (let [all-elements (.elements cy)
        focus-ids (cond
                    highlighted-entity-id #{highlighted-entity-id}
                    highlighted-aspect-id #{highlighted-aspect-id}
                    (seq legend-focus) (legend-focus-node-ids graph-data legend-focus)
                    :else #{})
        focus-set (set focus-ids)]
    (.removeClass all-elements "pinned")
    (.removeClass all-elements "pinned-dim")
    (when (seq focus-set)
      (let [focus-nodes (.filter (.nodes cy)
                                 (fn [ele]
                                   (contains? focus-set
                                              (id/string->id (.data ele "id")))))
            connected-edges (.connectedEdges focus-nodes)
            connected-nodes (.connectedNodes connected-edges)
            pinned (.union (.union focus-nodes connected-edges) connected-nodes)
            to-dim (.difference all-elements pinned)]
        (.addClass pinned "pinned")
        (.addClass to-dim "pinned-dim")))))

(defn cytoscape-style []
  "Cytoscape stylesheet for nodes and edges"
  (clj->js
   [{:selector "node"
     :style {:label "data(label)"
             :text-valign "center"
             :text-halign "center"
             :font-size "12px"
             :font-family "system-ui, -apple-system, sans-serif"
             :font-weight "500"
             :color "#1f2937"
             :text-wrap "wrap"
             :text-max-width "100px"
             :background-color "data(color)"
             :shape "data(shape)"
             :width "70px"
             :height "70px"
             :opacity "data(opacity)"
             :border-width "2px"
             :border-color "#9ca3af"
             :border-opacity "0.3"
             :transition-property "background-color, opacity, border-color, border-width"
             :transition-duration "0.3s"}}

    {:selector "node[type='aspect']"
     :style {:width "60px"
             :height "60px"
             :shape "ellipse"
             :font-size "11px"
             :border-width "2px"}}

    {:selector "node[type='entity']"
     :style {:width "80px"
             :height "70px"
             :font-size "12px"
             :border-width "2px"}}

    ;; Selected aspects (in query) - Purple border
    {:selector "node[type='aspect'][selected='true']"
     :style {:border-width "4px"
             :border-color "#8b5cf6"
             :border-opacity "1.0"
             :border-style "solid"}}

    ;; Negated aspects (NOT) - Red border
    {:selector "node[type='aspect'][negated='true']"
     :style {:border-width "4px"
             :border-color "#ef4444"
             :border-opacity "1.0"
             :border-style "solid"}}

    ;; Highlighted entity (clicked) - Amber border
    {:selector "node[type='entity'][highlighted='true']"
     :style {:border-width "4px"
             :border-color "#f59e0b"
             :border-opacity "1.0"
             :border-style "solid"}}

    ;; Pinned relations (selected entity neighborhood)
    {:selector "node.pinned"
     :style {:border-width "3px"
             :border-color "#f59e0b"
             :border-opacity "0.9"}}

    ;; Pinned dim (persistent fade outside selection)
    {:selector "node.pinned-dim"
     :style {:opacity "0.15"
             :text-opacity "0.15"}}

    ;; ENTROPY REDUCTION: Dimmed nodes (path highlighting)
    {:selector "node.dimmed"
     :style {:opacity "0.15"
             :text-opacity "0.15"}}

    ;; ENTROPY REDUCTION: Highlighted nodes (path highlighting)
    {:selector "node.highlighted"
     :style {:opacity "1.0"
             :text-opacity "1.0"
             :border-width "3px"
             :border-color "#3b82f6"
             :border-opacity "0.8"}}

    ;; Membership edges (entity â†’ aspect)
    {:selector "edge[edge-type='membership']"
     :style {:width "2px"
             :line-color "#d1d5db"
             :line-style "solid"
             :curve-style "bezier"
             :target-arrow-shape "triangle"
             :target-arrow-color "#d1d5db"
             :arrow-scale "1.0"
             :opacity "0.5"
             :label "data(edge-label)"
             :font-size "11px"
             :color "#6b7280"
             :text-opacity "0"
             :text-background-color "#ffffff"
             :text-background-opacity "0.8"
             :text-background-padding "3px"
             :transition-property "line-color, opacity, text-opacity"
             :transition-duration "0.3s"}}

    ;; Dependency edges (entity â†’ entity)
    {:selector "edge[edge-type='dependency']"
     :style {:width "3px"
             :line-color "#94a3b8"
             :line-style "solid"
             :curve-style "bezier"
             :target-arrow-shape "triangle"
             :target-arrow-color "#94a3b8"
             :arrow-scale "1.2"
             :opacity "0.7"
             :label "data(edge-label)"
             :font-size "12px"
             :color "#475569"
             :font-weight "600"
             :text-opacity "0"
             :text-background-color "#ffffff"
             :text-background-opacity "0.9"
             :text-background-padding "4px"
             :transition-property "line-color, opacity, text-opacity"
             :transition-duration "0.3s"}}

    ;; ENTROPY REDUCTION: Show edge labels on hover
    {:selector "edge:selected"
     :style {:text-opacity "1.0"}}

    ;; ENTROPY REDUCTION: Highlighted edges (path highlighting)
    {:selector "edge.highlighted"
     :style {:width "4px"
             :opacity "1.0"
             :text-opacity "1.0"
             :line-color "#3b82f6"
             :target-arrow-color "#3b82f6"
             :z-index "999"}}

    ;; Pinned relations (selected entity neighborhood)
    {:selector "edge.pinned"
     :style {:width "4px"
             :opacity "1.0"
             :text-opacity "1.0"
             :line-color "#f59e0b"
             :target-arrow-color "#f59e0b"
             :z-index "998"}}

    ;; Pinned dim (persistent fade outside selection)
    {:selector "edge.pinned-dim"
     :style {:opacity "0.1"}}

    ;; ENTROPY REDUCTION: Dimmed edges (path highlighting)
    {:selector "edge.dimmed"
     :style {:opacity "0.1"}}]))
(declare entities-per-ring)
(declare num-rings)
(defn circular-aspect-layout
  "Creates a circular layout with aspects on the perimeter and entities in center.
  Aspects are grouped by type (tier, domain, protocol, etc.) for visual organization."
  [cy]
  (let [nodes (.nodes cy)
        all-nodes (js->clj (.jsons nodes) :keywordize-keys true)

        ;; Separate aspects and entities
        aspect-nodes (filter #(= "aspect" (get-in % [:data :type])) all-nodes)
        entity-nodes (filter #(= "entity" (get-in % [:data :type])) all-nodes)

        ;; Group aspects by their namespace (tier, domain, protocol, etc.)
        aspect-groups (group-by (fn [node]
                                  (let [id (get-in node [:data :id])]
                                    (first (str/split id #"/"))))
                                aspect-nodes)

        ;; Get viewport dimensions for responsive centering
        extent (.extent cy)
        viewport-width (- (.-x2 extent) (.-x1 extent))
        viewport-height (- (.-y2 extent) (.-y1 extent))

        ;; Calculate center based on viewport (responsive, not fixed!)
        center-x (/ viewport-width 2)
        center-y (/ viewport-height 2)

        ;; Calculate radii based on viewport size (percentage-based)
        min-dimension (min viewport-width viewport-height)
        outer-radius (* 2.42 min-dimension) ;; 42% of viewport for aspects
        inner-radius (* 0.35 min-dimension) ;; 35% of viewport for entities - MUCH BIGGER!

        ;; Position aspects in circular groups
        aspect-positions
        (let [group-order (sort (keys aspect-groups))
              total-aspects (count aspect-nodes)
              angle-step (/ (* 2 Math/PI) total-aspects)]
          (loop [groups group-order
                 current-angle 0
                 positions {}]
            (if (empty? groups)
              positions
              (let [group (first groups)
                    group-aspects (get aspect-groups group)
                    group-positions
                    (into {}
                          (map-indexed
                           (fn [idx aspect]
                             (let [angle (+ current-angle (* idx angle-step))
                                   x (+ center-x (* outer-radius (Math/cos angle)))
                                   y (+ center-y (* outer-radius (Math/sin angle)))]
                               [(get-in aspect [:data :id]) {:x x :y y}]))
                           group-aspects))]
                (recur (rest groups)
                       (+ current-angle (* (count group-aspects) angle-step))
                       (merge positions group-positions))))))

        ;; Position entities in center using concentric circles - FORCE SPREAD!
        entity-positions
        (let [num-entities (count entity-nodes)
              entities-per-ring 4 ;; EVEN FEWER per ring!
              num-rings (Math/ceil (/ num-entities entities-per-ring))]
          (into {}
                (map-indexed
                 (fn [idx entity]
                   (let [ring (Math/floor (/ idx entities-per-ring))
                         pos-in-ring (mod idx entities-per-ring)
                         total-in-ring (min entities-per-ring
                                            (- num-entities (* ring entities-per-ring)))
                         ;; FORCE entities to spread across FULL inner area
                         ring-radius (if (= num-rings 1)
                                       (* inner-radius 0.3) ;; Single ring
                                       ;; Spread from 20% to 100% of inner radius
                                       (+ (* inner-radius 0.2)
                                          (* inner-radius 3.8 (/ ring (dec num-rings)))))
                         ;; Angle with rotation per ring
                         angle (+ (* 2 Math/PI (/ pos-in-ring total-in-ring))
                                  (* ring Math/PI 0.2))
                         x (+ center-x (* ring-radius (Math/cos angle)))
                         y (+ center-y (* ring-radius (Math/sin angle)))]
                     [(get-in entity [:data :id]) {:x x :y y}]))
                 entity-nodes)))]

    ;; Apply positions to nodes
    (doseq [[node-id pos] (merge aspect-positions entity-positions)]
      (when-let [node (.getElementById cy node-id)]
        (.position node (clj->js pos))))

    ;; Fit to viewport after positioning - with delay to let layout settle
    (js/setTimeout #(.fit cy (clj->js {:padding 60})) 200)

    (js/console.log "ðŸŽ¯ Circular layout applied:"
                    "\n  Viewport:" (int viewport-width) "x" (int viewport-height)
                    "\n  Center: (" (int center-x) "," (int center-y) ")"
                    "\n  Outer radius:" (int outer-radius) "px"
                    "\n  Inner radius:" (int inner-radius) "px (EXPANDED!)"
                    "\n  Aspects:" (count aspect-nodes) "in" (count aspect-groups) "groups"
                    "\n  Entities:" (count entity-nodes) "in" num-rings "rings (" entities-per-ring "per ring)")))

(defn init-cytoscape! [container elements graph-data lens on-toggle-aspect on-click-entity on-click-aspect]
  (let [;; Use lens-specific layout if lens is active, otherwise default cose
        layout-config (if lens
                        (merge {:name "grid"
                                :animate true
                                :animationDuration 500
                                :avoidOverlap true
                                :avoidOverlapPadding 100
                                :condense false}
                               (:layout-config lens))
                        ;; CIRCULAR TARGET LAYOUT: Aspects outside, entities inside
                        {:name "preset" ;; Use preset so we can position manually
                         :animate false
                         :fit true
                         :padding 50})
        cy (cytoscape
            (clj->js
             {:container container
              :elements elements
              :style (cytoscape-style)
              :layout layout-config
              :minZoom 0.2
              :maxZoom 3
              :wheelSensitivity 0.2}))]
    (js/console.log "🎨 Using layout:" (if lens "grid (lens mode)" "circular (target view)"))

    ;; Apply circular layout for normal mode (not lens)
    (when-not lens
      (js/setTimeout #(circular-aspect-layout cy) 100))

    ;; Click handlers
    (.on cy "tap" "node"
         (fn [evt]
           (try
             (let [node (.-target evt)
                   node-data (.data node)
                   node-id (.-id node-data)
                   node-type (.-type node-data)
                   node-label (.-label node-data)
                   shift-key? (.-shiftKey (.-originalEvent evt))]
               (js/console.log "Clicked node:" node-id "type:" node-type "shift:" shift-key?)

               (if (= node-type "aspect")
                 ;; Aspect clicked - toggle in query and pin relations
                 (on-click-aspect (id/string->id node-id) shift-key?)

                 ;; Entity clicked - show details
                 (let [entity-id (id/string->id node-id)
                       full-entity (->> (:atlas-ui.graph.data/all-nodes graph-data)
                                        (filter #(= (:atlas-ui.graph.node/id %) entity-id))
                                        first)]
                   (on-click-entity full-entity))))
             (catch js/Error e
               (js/console.error "ERROR:" e)))))

    ;; Click on background - clear entity highlight (but keep query)
    (.on cy "tap"
         (fn [evt]
           (when (identical? (.-target evt) cy)
             (js/console.log "Clicked background")
             (on-click-entity nil)
             (on-click-aspect nil))))

    ;; PHASE 4: Double-click to focus on node and neighborhood
    (.on cy "dbltap" "node"
         (fn [evt]
           (let [node (.-target evt)
                 connected-edges (.connectedEdges node)
                 connected-nodes (.connectedNodes connected-edges)
                 focus-collection (.union (.union node connected-edges) connected-nodes)]
             (js/console.log "ðŸŽ¯ Double-click focus on node")
             ;; Fit viewport to show focused nodes
             (.fit cy focus-collection 50)
             ;; Highlight the focused collection
             (.addClass focus-collection "highlighted")
             ;; Dim everything else
             (.addClass (.difference (.elements cy) focus-collection) "dimmed")
             ;; Auto-clear after 3 seconds
             (js/setTimeout
              (fn []
                (.removeClass (.elements cy) "highlighted")
                (.removeClass (.elements cy) "dimmed"))
              3000))))

    ;; ENTROPY REDUCTION: Path highlighting on hover
    (.on cy "mouseover" "node"
         (fn [evt]
           (let [node (.-target evt)
                 connected-edges (.connectedEdges node)
                 connected-nodes (.connectedNodes connected-edges)
                 all-elements (.elements cy)
                 pinned (.elements cy ".pinned")
                 focus-collection (.union (.union node connected-edges) connected-nodes)
                 keep-visible (.union focus-collection pinned)]
             ;; Highlight the hovered node, its edges, and connected nodes
             (.addClass node "highlighted")
             (.addClass connected-edges "highlighted")
             (.addClass connected-nodes "highlighted")
             ;; Dim everything else
             (let [to-dim (.difference all-elements keep-visible)]
               (.addClass to-dim "dimmed")))))

    (.on cy "mouseout" "node"
         (fn [evt]
           ;; Remove all highlighting/dimming
           (.removeClass (.elements cy) "highlighted")
           (.removeClass (.elements cy) "dimmed")))

    ;; ENTROPY REDUCTION: Tooltip showing full label on hover
    (.on cy "mouseover" "node"
         (fn [evt]
           (let [node (.-target evt)
                 full-label (.data node "fullLabel")
                 node-id (.data node "id")
                 node-type (.data node "type")
                 identity (.data node "identity")]
             (when full-label
               ;; PHASE 4: Enhanced tooltip with more context
               (let [identity-set (js->clj identity :keywordize-keys true)
                     aspects (filter keyword? identity-set)
                     tooltip-html (str "<div style='padding:8px;background:white;border-radius:6px;box-shadow:0 2px 8px rgba(0,0,0,0.2);max-width:250px;'>"
                                       "<div style='font-weight:600;margin-bottom:6px;color:#1f2937;'>" full-label "</div>"
                                       "<div style='font-size:11px;color:#6b7280;margin-bottom:4px;'>" node-type ": " node-id "</div>"
                                       (when (seq aspects)
                                         (str "<div style='font-size:10px;color:#9ca3af;margin-top:6px;'>"
                                              (str/join ", " (map name (take 5 aspects)))
                                              "</div>"))
                                       "</div>")]
                 ;; Create tooltip element
                 (when-let [existing-tooltip (js/document.getElementById "cy-tooltip")]
                   (.remove existing-tooltip))
                 (let [tooltip (js/document.createElement "div")]
                   (set! (.-id tooltip) "cy-tooltip")
                   (set! (.-innerHTML tooltip) tooltip-html)
                   (set! (.-position (.-style tooltip)) "absolute")
                   (set! (.-zIndex (.-style tooltip)) "10000")
                   (set! (.-pointerEvents (.-style tooltip)) "none")
                   (.appendChild js/document.body tooltip)
                   ;; Position near mouse
                   (.on cy "mousemove" "node"
                        (fn [move-evt]
                          (let [browser-evt (.-originalEvent move-evt)]
                            (set! (.-left (.-style tooltip)) (str (+ (.-pageX browser-evt) 15) "px"))
                            (set! (.-top (.-style tooltip)) (str (+ (.-pageY browser-evt) 15) "px")))))))))))

    (.on cy "mouseout" "node"
         (fn [evt]
           ;; Remove tooltip
           (when-let [tooltip (js/document.getElementById "cy-tooltip")]
             (.remove tooltip))
           (.off cy "mousemove" "node")))

    ;; ENTROPY REDUCTION: Semantic zoom levels
    (let [update-zoom-labels!
          (fn []
            (let [zoom (.zoom cy)
                  nodes (.nodes cy)]
              (cond
                ;; Zoom < 0.5: Hide all labels (icon-only mode)
                (< zoom 0.5)
                (do
                  (.style nodes "font-size" "0px")
                  (.style nodes "text-opacity" "0"))

                ;; Zoom 0.5-1.0: Show abbreviated labels
                (< zoom 1.0)
                (do
                  (.style nodes "font-size" "10px")
                  (.style nodes "text-opacity" "0.8"))

                ;; Zoom > 1.0: Full labels + full opacity
                :else
                (do
                  (.style nodes "font-size" "12px")
                  (.style nodes "text-opacity" "1.0")))))]

      ;; Apply initial zoom state
      (update-zoom-labels!)

      ;; Update on zoom
      (.on cy "zoom" update-zoom-labels!))

    ;; ENTROPY REDUCTION: Minimap navigator - TEMPORARILY DISABLED FOR DEBUG
    (try
      (js/console.log "âš ï¸� Minimap disabled for debugging edge overlap issue")
      ;; Register navigator extension if available
      #_(when navigator
          (navigator cytoscape))

      ;; Initialize minimap
      #_(let [nav-options (clj->js {:container (js/document.createElement "div")
                                    :viewLiveFramerate 0
                                    :thumbnailEventFramerate 30
                                    :thumbnailLiveFramerate false
                                    :dblClickDelay 200
                                    :removeCustomContainer false
                                    :rerenderDelay 100})]
          (when (.-navigator cy)
            (let [nav (.navigator cy nav-options)
                  nav-container (.-container nav)]
            ;; Style the minimap container
              (set! (.-position (.-style nav-container)) "absolute")
              (set! (.-right (.-style nav-container)) "20px")
              (set! (.-bottom (.-style nav-container)) "20px")
              (set! (.-width (.-style nav-container)) "280px")
              (set! (.-height (.-style nav-container)) "280px")
              (set! (.-zIndex (.-style nav-container)) "1000")
              (set! (.-border (.-style nav-container)) "4px solid #3b82f6")
              (set! (.-borderRadius (.-style nav-container)) "8px")
              (set! (.-boxShadow (.-style nav-container)) "0 8px 24px rgba(0,0,0,0.4)")
              (set! (.-backgroundColor (.-style nav-container)) "rgba(255,255,255,0.98)")

            ;; Append to graph container
              (.appendChild container nav-container)

              (js/console.log "ðŸ—ºï¸� Minimap navigator initialized"))))
      (catch js/Error e
        (js/console.warn "âš ï¸� Navigator extension not available:" (.-message e))))

    ;; PHASE 4: Keyboard shortcuts
    (let [keyboard-handler
          (fn [evt]
            (let [key (.-key evt)
                  ctrl? (.-ctrlKey evt)
                  shift? (.-shiftKey evt)]
              (cond
                ;; F - Fit graph to viewport
                (and (= key "f") (not ctrl?) (not shift?))
                (do
                  (.preventDefault evt)
                  (.fit cy)
                  (js/console.log "Ã¢Å¡Â¡ Keyboard: Fit to viewport"))

                ;; R - Reset zoom to 1.0
                (and (= key "r") (not ctrl?) (not shift?))
                (do
                  (.preventDefault evt)
                  (.zoom cy 1.0)
                  (.center cy)
                  (js/console.log "Ã¢Å¡Â¡ Keyboard: Reset zoom"))

                ;; C - Center on selected node
                (and (= key "c") (not ctrl?) (not shift?))
                (do
                  (.preventDefault evt)
                  (let [selected (.nodes cy ":selected")]
                    (when (> (.-length selected) 0)
                      (.center cy selected)
                      (js/console.log "âš¡ Keyboard: Center on selection"))))

                ;; + or = - Zoom in
                (or (= key "+") (= key "="))
                (do
                  (.preventDefault evt)
                  (let [current-zoom (.zoom cy)]
                    (.zoom cy (* current-zoom 1.2))
                    (js/console.log "Ã¢Å¡Â¡ Keyboard: Zoom in")))

                ;; - - Zoom out
                (= key "-")
                (do
                  (.preventDefault evt)
                  (let [current-zoom (.zoom cy)]
                    (.zoom cy (* current-zoom 0.8))
                    (js/console.log "âš¡ Keyboard: Zoom out")))

                ;; H - Toggle highlight mode
                (and (= key "h") (not ctrl?) (not shift?))
                (do
                  (.preventDefault evt)
                  (if (.hasClass (.elements cy) "dimmed")
                    (do
                      (.removeClass (.elements cy) "highlighted")
                      (.removeClass (.elements cy) "dimmed")
                      (js/console.log "âš¡ Keyboard: Clear highlighting"))
                    (js/console.log "âš¡ Keyboard: Hover a node to highlight"))))))]

      ;; Add keyboard event listener
      (js/document.addEventListener "keydown" keyboard-handler)

      ;; Store handler for cleanup
      (.data cy "keyboardHandler" keyboard-handler)

      (js/console.log "âŒ¨ï¸�  Keyboard shortcuts enabled: F=fit, R=reset, C=center, +/-=zoom, H=toggle"))

    cy))

(defn update-colors!
  [cy graph-data selections negated-aspects query-mode min-score highlighted-entity-id highlighted-entity-aspects highlighted-aspect-id registry hide-unmatched? search-term legend-focus]
  (js/console.log "ðŸŽ¨ UPDATE COLORS! selections:" (clj->js selections) "mode:" query-mode "highlighted:" highlighted-entity-id)
  (try
    (let [elements (build-cytoscape-elements graph-data selections negated-aspects query-mode min-score highlighted-entity-id highlighted-entity-aspects registry hide-unmatched? search-term)
          nodes-array (.-nodes elements)
          edges-array (.-edges elements)]
      (js/console.log "   Colored" (.-length nodes-array) "nodes")

      ;; Update node colors, opacity, and highlight state
      (doseq [node-data (js->clj nodes-array :keywordize-keys true)]
        (let [node-id (get-in node-data [:data :id])
              color (get-in node-data [:data :color])
              opacity (get-in node-data [:data :opacity])
              selected (get-in node-data [:data :selected])
              negated (get-in node-data [:data :negated])
              highlighted (get-in node-data [:data :highlighted])
              search-match (get-in node-data [:data :search-match])
              node (.getElementById cy node-id)]
          (when node
            (.data node "color" color)
            (.data node "opacity" opacity)
            (.data node "selected" selected)
            (.data node "negated" negated)
            (.data node "highlighted" highlighted)
            (.data node "search-match" search-match)
            (when-let [shape (get-in node-data [:data :shape])]
              (.data node "shape" shape)))))

      ;; Update edge colors
      (doseq [edge-data (js->clj edges-array :keywordize-keys true)]
        (let [source (get-in edge-data [:data :source])
              target (get-in edge-data [:data :target])
              color (get-in edge-data [:data :color])
              edges (.edges cy (str "[source='" source "'][target='" target "']"))]
          (when (> (.-length edges) 0)
            (.forEach edges (fn [edge]
                              (.data edge "color" color)))))))

      (apply-pinned! cy graph-data highlighted-entity-id highlighted-aspect-id legend-focus)
    (catch js/Error e
      (js/console.error "â�Œ ERROR updating colors:" e))))

(defn graph-component
  [graph-data selections negated-aspects query-mode min-score registry highlighted-entity-id highlighted-entity-aspects highlighted-aspect-id hide-unmatched? search-term lens on-toggle-aspect on-click-entity on-click-aspect legend-focus]
  (let [cy-instance (atom nil)
        container-id "cy-container"]
    (r/create-class
     {:component-did-mount
      (fn [this]
        (let [container (js/document.getElementById container-id)
              elements (build-cytoscape-elements graph-data selections negated-aspects query-mode min-score highlighted-entity-id highlighted-entity-aspects registry hide-unmatched? search-term)]
          (js/console.log "ðŸ“Š Graph mounted with multi-selection support")
          (reset! cy-instance (init-cytoscape! container elements graph-data lens on-toggle-aspect on-click-entity on-click-aspect))
          (when (or (seq selections) (seq negated-aspects) highlighted-entity-id highlighted-aspect-id legend-focus)
            (update-colors! @cy-instance graph-data selections negated-aspects query-mode min-score highlighted-entity-id highlighted-entity-aspects highlighted-aspect-id registry hide-unmatched? search-term legend-focus))))

      :component-did-update
      (fn [this prev-argv]
        (let [curr-argv (r/argv this)
              curr-graph-data (nth curr-argv 1)
              curr-selections (nth curr-argv 2)
              curr-negated-aspects (nth curr-argv 3)
              curr-query-mode (nth curr-argv 4)
              curr-min-score (nth curr-argv 5)
              curr-registry (nth curr-argv 6)
              curr-highlighted-entity-id (nth curr-argv 7)
              curr-highlighted-entity-aspects (nth curr-argv 8)
              curr-highlighted-aspect-id (nth curr-argv 9)
              curr-hide-unmatched? (nth curr-argv 10)
              curr-search-term (nth curr-argv 11)
              curr-lens (nth curr-argv 12)
              curr-legend-focus (nth curr-argv 16)
              prev-graph-data (nth prev-argv 1)
              prev-selections (nth prev-argv 2)
              prev-negated-aspects (nth prev-argv 3)
              prev-query-mode (nth prev-argv 4)
              prev-min-score (nth prev-argv 5)
              prev-highlighted-entity-id (nth prev-argv 7)
              prev-highlighted-entity-aspects (nth prev-argv 8)
              prev-highlighted-aspect-id (nth prev-argv 9)
              prev-hide-unmatched? (nth prev-argv 10)
              prev-search-term (nth prev-argv 11)
              prev-lens (nth prev-argv 12)
              prev-legend-focus (nth prev-argv 16)]
          (let [selections-changed? (not= prev-selections curr-selections)
                negations-changed? (not= prev-negated-aspects curr-negated-aspects)
                mode-changed? (not= prev-query-mode curr-query-mode)
                min-score-changed? (not= prev-min-score curr-min-score)
                graph-changed? (not= prev-graph-data curr-graph-data)
                entity-highlight-changed? (not= prev-highlighted-entity-id curr-highlighted-entity-id)
                aspect-highlight-changed? (not= prev-highlighted-entity-aspects curr-highlighted-entity-aspects)
                highlighted-aspect-changed? (not= prev-highlighted-aspect-id curr-highlighted-aspect-id)
                hide-changed? (not= prev-hide-unmatched? curr-hide-unmatched?)
                search-changed? (not= prev-search-term curr-search-term)
                lens-changed? (not= prev-lens curr-lens)
                legend-focus-changed? (not= prev-legend-focus curr-legend-focus)]
            (when (and @cy-instance
                       (or selections-changed?
                           negations-changed?
                           mode-changed?
                           min-score-changed?
                           graph-changed?
                           entity-highlight-changed?
                           aspect-highlight-changed?
                           highlighted-aspect-changed?
                           hide-changed?
                           search-changed?
                           lens-changed?
                           legend-focus-changed?))
              (let [needs-rebuild?
                    (or graph-changed?
                        hide-changed?
                        search-changed?
                        lens-changed? ;; Lens changes require full rebuild
                        (and curr-hide-unmatched?
                             (or selections-changed?
                                 negations-changed?
                                 mode-changed?
                                 min-score-changed?)))]
                (js/console.log "âœ¨ State changed, updating" (if needs-rebuild? "graph structure" "colors"))
                (if needs-rebuild?
                  (let [container (js/document.getElementById container-id)
                        elements (build-cytoscape-elements curr-graph-data curr-selections curr-negated-aspects curr-query-mode curr-min-score curr-highlighted-entity-id curr-highlighted-entity-aspects curr-registry curr-hide-unmatched? curr-search-term)]
                    (.destroy @cy-instance)
                    (reset! cy-instance (init-cytoscape! container elements curr-graph-data curr-lens on-toggle-aspect on-click-entity on-click-aspect))
                    (when (or curr-highlighted-entity-id curr-highlighted-aspect-id curr-legend-focus)
                      (apply-pinned! @cy-instance curr-graph-data curr-highlighted-entity-id curr-highlighted-aspect-id curr-legend-focus)))
                  (update-colors! @cy-instance curr-graph-data curr-selections curr-negated-aspects curr-query-mode curr-min-score curr-highlighted-entity-id curr-highlighted-entity-aspects curr-highlighted-aspect-id curr-registry curr-hide-unmatched? curr-search-term curr-legend-focus)))))))

      :reagent-render
      (fn [graph-data selections negated-aspects query-mode min-score registry highlighted-entity-id highlighted-entity-aspects highlighted-aspect-id hide-unmatched? search-term lens on-toggle-aspect on-click-entity on-click-aspect legend-focus]
        [:div {:id container-id
               :style {:width "100%"
                       :height "100%"
                       :background-color "#f9fafb"}}])})))
