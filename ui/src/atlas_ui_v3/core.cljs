(ns atlas-ui-v3.core
  "Atlas UI v3 — Two-momentum layout viewer driven by LLM via SSE.

   The server holds view-state (mode + target). This browser renders it.
   Claude Code POSTs to /api/v3/state; we receive it here via SSE and render.

   Modes:
     home          — registry overview (waiting for navigation)
     entity-focus  — A-pane: entity details + B-pane: data-flow or dependents
     blast-radius  — recursive dependents list
     domain-survey — all entities carrying an aspect
     dataflow-focus — producers + consumers of a data key"
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [clojure.string :as str]
            [atlas-ui-v3.api :as api]))

;; =============================================================================
;; App state
;; =============================================================================

(defonce app-state
  (r/atom {:view-state  {:mode "home" :entity nil :aspect nil :data-key nil}
           :registry    nil
           :loading?    true
           :error       nil
           :nav-history []
           :nav-future  []}))

(defonce pane-split (r/atom 50))         ; left pane % of two-pane container
(defonce narrative-height (r/atom 60))   ; pixels — drag handle resizes this

;; =============================================================================
;; Registry helpers
;; =============================================================================

(defn- find-entity
  "Find registry entry [compound-id props] where :atlas/dev-id matches dev-id-str."
  [registry dev-id-str]
  (->> registry
       (filter (fn [[_id props]] (= (str (:atlas/dev-id props)) dev-id-str)))
       first))

(defn- entity-aspects
  "Return aspects set — compound identity minus the entity-type keyword."
  [compound-id etype-str]
  (let [etype-kw (when etype-str (keyword (subs etype-str 1)))]
    (cond-> (set compound-id)
      etype-kw (disj etype-kw))))

(defn- prop-by-name
  "Find the value of the first prop whose key *name* is `prop-name`, regardless
  of namespace — so :execution-function/context, :interface-endpoint/context,
  and any custom ontology's :myapp-endpoint/context all match. Mirrors the
  ontology-driven dataflow-keys convention without needing type-refs client-side."
  [props prop-name]
  (some (fn [[k v]]
          (when (and (keyword? k) (= (name k) prop-name))
            v))
        props))

(defn- has-dataflow?
  "True if entity props declare context or response (execution-fn, endpoint, etc.)."
  [props]
  (boolean (or (prop-by-name props "context")
               (prop-by-name props "response"))))

(defn- dataflow-data
  "Extract context / response / deps from props (whichever keys exist)."
  [props]
  {:context  (prop-by-name props "context")
   :response (prop-by-name props "response")
   :deps     (prop-by-name props "deps")})

(defn- unique-by-dev-id
  "Deduplicate entity props by :atlas/dev-id (same dev-id can appear under
  multiple compound-id keys when registered more than once)."
  [coll]
  (->> coll (group-by :atlas/dev-id) vals (map first)))

(defn- find-dependents
  "Return props of all entities whose deps include dev-id-str."
  [registry dev-id-str]
  (let [kw (keyword (subs dev-id-str 1))]
    (->> registry
         vals
         (filter #(contains? (set (or (prop-by-name % "deps") #{})) kw))
         unique-by-dev-id)))

(defn- entities-by-aspect
  "Return props of all entities carrying aspect-str."
  [registry aspect-str]
  (let [kw (keyword (subs aspect-str 1))]
    (->> registry
         (filter (fn [[compound-id _]] (contains? compound-id kw)))
         (map second)
         unique-by-dev-id)))

(defn- count-by-type
  "Return [{:type \":atlas/execution-function\" :count N} ...] sorted by count desc."
  [registry]
  (->> (vals registry)
       unique-by-dev-id
       (group-by #(str (:atlas/type %)))
       (map (fn [[t ps]] {:type t :count (count ps)}))
       (sort-by :count >)))

;; =============================================================================
;; Navigation
;; =============================================================================

(defn- capture-scroll []
  (when-let [el (js/document.getElementById "v3-scroll-area")]
    (.-scrollTop el)))

(defn- restore-scroll! [n]
  (when n
    (js/requestAnimationFrame
     (fn []
       (when-let [el (js/document.getElementById "v3-scroll-area")]
         (set! (.-scrollTop el) n))))))

(defn- navigate-to!
  "POST new view state to the server — browser updates via SSE."
  [mode entity-str aspect-str]
  (-> (js/fetch "/api/v3/state"
                (clj->js {:method  "POST"
                          :headers {"Content-Type" "application/json"}
                          :body    (js/JSON.stringify
                                    (clj->js {:mode   mode
                                              :entity entity-str
                                              :aspect aspect-str}))}))
      (.catch (fn [e] (js/console.error "navigate-to! error" e)))))

(defn- nav-snapshot [s]
  {:view-state     (:view-state s)
   :scroll-top     (capture-scroll)
   :expanded-lists (or (:expanded-lists s) {})
   :filter-text    (or (:filter-text s) "")})

(defn- apply-entry! [s entry]
  (-> s
      (assoc :view-state     (:view-state entry))
      (assoc :expanded-lists (or (:expanded-lists entry) {}))
      (assoc :filter-text    (or (:filter-text entry) ""))
      (assoc :search-text    "")))

(defn- back!
  "Go back one step; push current state onto nav-future."
  []
  (let [entry (last (:nav-history @app-state))]
    (when entry
      (swap! app-state
             (fn [s]
               (-> (apply-entry! s entry)
                   (update :nav-history pop)
                   (update :nav-future conj (nav-snapshot s)))))
      (restore-scroll! (:scroll-top entry)))))

(defn- forward!
  "Go forward one step; push current state onto nav-history."
  []
  (let [entry (last (:nav-future @app-state))]
    (when entry
      (swap! app-state
             (fn [s]
               (-> (apply-entry! s entry)
                   (update :nav-future pop)
                   (update :nav-history conj (nav-snapshot s)))))
      (restore-scroll! (:scroll-top entry)))))

;; =============================================================================
;; Styles (shared)
;; =============================================================================

(def ^:private dark "#0f0f1a")
(def ^:private border "#2a2a4a")
(def ^:private text-main "#e0e0e0")
(def ^:private text-dim "#888")
(def ^:private accent-blue "#3b82f6")
(def ^:private accent-green "#22c55e")
(def ^:private accent-amber "#f59e0b")
(def ^:private accent-purple "#a855f7")

(defn- s [& kvs] (apply hash-map kvs))

;; =============================================================================
;; Shared components
;; =============================================================================

(defn- kw-chip
  ([kw-str] (kw-chip kw-str accent-blue))
  ([kw-str color]
   [:span {:style (s :font-family "monospace"
                     :font-size "0.8rem"
                     :background (str color "22")
                     :color color
                     :padding "2px 6px"
                     :border-radius "3px"
                     :margin "2px"
                     :display "inline-block")}
    kw-str]))

(defn- collapsible-list
  "list-key is a keyword used to persist expanded state in app-state.
   n is closed over so re-renders never receive nil for it."
  [list-key items render-fn & [n]]
  (let [n (or n 6)]
    (fn [list-key items render-fn & _]
      (let [expanded? (get-in @app-state [:expanded-lists list-key] false)
            total     (count items)
            visible   (if (or expanded? (<= total n)) items (take n items))
            hidden    (- total (count visible))]
        [:<>
         (for [[i item] (map-indexed vector visible)]
           ^{:key i} [render-fn item])
         (when (pos? hidden)
           [:span {:style    (s :font-size "0.78rem" :color text-dim
                                :cursor "pointer" :padding "2px 6px"
                                :margin "2px" :display "inline-block")
                   :on-click #(swap! app-state assoc-in [:expanded-lists list-key] true)
                   :title    "Show all"}
            (str "… " hidden " more")])
         (when (and expanded? (> total n))
           [:span {:style    (s :font-size "0.78rem" :color text-dim
                                :cursor "pointer" :padding "2px 6px"
                                :margin "2px" :display "inline-block")
                   :on-click #(swap! app-state assoc-in [:expanded-lists list-key] false)}
            "↑ collapse"])]))))

(defn- section-header [title]
  [:div {:style (s :font-size "0.75rem"
                   :font-weight "bold"
                   :letter-spacing "0.08em"
                   :color text-dim
                   :text-transform "uppercase"
                   :margin-bottom "8px"
                   :margin-top "16px")}
   title])

(defn- dev-id-display [dev-id-str]
  [:span {:style (s :font-family "monospace"
                    :font-size "1rem"
                    :color accent-blue
                    :font-weight "bold")}
   dev-id-str])

(defn- entity-link
  "Clickable entity row — navigates to entity-focus on click."
  ([dev-id-str] (entity-link dev-id-str nil accent-blue))
  ([dev-id-str type-str] (entity-link dev-id-str type-str accent-blue))
  ([dev-id-str type-str color]
   [:div {:style    (s :cursor "pointer" :padding "1px 4px" :margin "0 -4px"
                       :border-radius "3px" :display "inline-flex"
                       :align-items "center" :gap "8px")
          :on-click #(navigate-to! "entity-focus" dev-id-str nil)}
    [:span {:style (s :font-family "monospace" :font-size "0.85rem" :color color)}
     dev-id-str]
    (when type-str
      [:span {:style (s :font-size "0.8rem" :color text-dim)} type-str])]))

(defn- aspect-chip
  "Clickable aspect chip — navigates to domain-survey for that aspect."
  [aspect-str]
  [:span {:style    (s :font-family "monospace" :font-size "0.8rem"
                       :background (str accent-green "22") :color accent-green
                       :padding "2px 6px" :border-radius "3px" :margin "2px"
                       :display "inline-block" :cursor "pointer")
          :on-click #(navigate-to! "domain-survey" nil aspect-str)
          :title    (str "Survey " aspect-str)}
   aspect-str])

(defn- data-key-chip
  "Clickable data-key chip — navigates to dataflow-focus for that key."
  [kw-str color]
  [:span {:style    (s :font-family "monospace" :font-size "0.8rem"
                       :background (str color "22") :color color
                       :padding "2px 6px" :border-radius "3px" :margin "2px"
                       :display "inline-block" :cursor "pointer")
          :on-click #(navigate-to! "dataflow-focus" kw-str nil)
          :title    (str "Data flow: " kw-str)}
   kw-str])

(defn- classify-kw
  "Classify a keyword by registry lookup. Returns :entity-ref, :aspect, or :data-key."
  [registry kw]
  (let [kw-str (str kw)]
    (cond
      (find-entity registry kw-str)            :entity-ref
      (some #(contains? % kw) (keys registry)) :aspect
      :else                                    :data-key)))

(defn- smart-chip
  "Chip whose color and navigation are derived from registry classification."
  [registry kw]
  (let [kw-str (str kw)]
    (case (classify-kw registry kw)
      :entity-ref [entity-link kw-str]
      :aspect     [aspect-chip kw-str]
      :data-key   [:span {:style (s :font-family "monospace" :font-size "0.8rem"
                                    :color accent-purple :padding "2px 4px")}
                   kw-str])))

;; =============================================================================
;; Panel: entity info (Momentum A)
;; =============================================================================

(defn- render-prop-value
  "Render a property value. Keywords go through smart-chip; keyword collections
   render as a chip row; anything else is plain text.
   list-key must be provided for collapsible collections."
  [registry v list-key]
  (cond
    (keyword? v)
    [smart-chip registry v]

    (and (coll? v) (seq v) (every? keyword? v))
    [collapsible-list list-key (sort-by str v) #(do [smart-chip registry %]) 5]

    :else
    [:span {:style (s :color text-main)} (pr-str v)]))

(defn- entity-info-panel [registry dev-id-str]
  (let [[compound-id props] (find-entity registry dev-id-str)
        etype-str  (str (:atlas/type props))
        aspects    (entity-aspects compound-id etype-str)
        ;; All props except atlas/* and type-specific structural ones
        extra-keys (remove #(str/starts-with? (str %) ":atlas/") (keys props))]
    [:div {:id "v3-scroll-area" :style (s :padding "16px" :overflow-y "auto" :height "100%")}
     ;; Dev-id
     [:div {:style (s :margin-bottom "12px")}
      [dev-id-display dev-id-str]]
     ;; Type — clickable, navigates to domain-survey for that type
     [:div {:style (s :margin-bottom "16px")}
      [:span {:style    (s :font-family "monospace" :font-size "0.8rem"
                           :background (str accent-amber "22") :color accent-amber
                           :padding "2px 6px" :border-radius "3px" :margin "2px"
                           :display "inline-block" :cursor "pointer")
              :on-click #(navigate-to! "domain-survey" nil etype-str)
              :title    (str "Browse all " etype-str)}
       etype-str]]
     ;; Aspects
     (when (seq aspects)
       [:<>
        [section-header "Aspects"]
        [:div {:style (s :display "flex" :flex-wrap "wrap" :gap "4px")}
         [collapsible-list :aspects (sort-by str aspects) #(do [aspect-chip (str %)])]]])
     ;; Properties
     (when (seq extra-keys)
       [:<>
        [section-header "Properties"]
        [:div {:style (s :font-family "monospace" :font-size "0.8rem" :line-height "1.8")}
         (for [k (sort-by str extra-keys)]
           (let [v (get props k)]
             ^{:key (str k)}
             [:div {:style (s :display "flex" :flex-wrap "wrap" :align-items "baseline"
                              :gap "4px" :margin-bottom "2px")}
              [:span {:style (s :color text-dim :flex-shrink 0)} (str k) " "]
              [render-prop-value registry v k]]))]])]))
;; =============================================================================
;; Panel: B-pane — data-flow or dependents (Momentum B)
;; =============================================================================

(defn- data-flow-panel [registry props]
  (let [{:keys [context response deps]} (dataflow-data props)]
    [:div {:id "v3-scroll-area" :style (s :padding "16px" :overflow-y "auto" :height "100%")}
     (when (seq context)
       [:<>
        [section-header "Consumes"]
        [:div {:style (s :display "flex" :flex-wrap "wrap" :gap "4px")}
         (for [k (if (set? context) (sort-by str context) context)]
           ^{:key (str k)}
           [smart-chip registry k])]])
     (when (seq response)
       [:<>
        [section-header "Produces"]
        [:div {:style (s :display "flex" :flex-wrap "wrap" :gap "4px")}
         (for [k (if (set? response) (sort-by str response) response)]
           ^{:key (str k)}
           [smart-chip registry k])]])
     (when (seq deps)
       [:<>
        [section-header "Depends on"]
        [:div {:style (s :font-family "monospace" :font-size "0.85rem" :line-height "1.8")}
         (for [d (sort-by str deps)]
           ^{:key (str d)}
           [:div [entity-link (str d)]])]])]))

(defn- dependents-panel [registry dev-id-str]
  (let [deps (find-dependents registry dev-id-str)]
    [:div {:id "v3-scroll-area" :style (s :padding "16px" :overflow-y "auto" :height "100%")}
     [section-header (str "Depended on by (" (count deps) ")")]
     (if (seq deps)
       [:div {:style (s :font-family "monospace" :font-size "0.85rem" :line-height "1.8")}
        (for [p (sort-by #(str (:atlas/dev-id %)) deps)]
          ^{:key (str (:atlas/dev-id p))}
          [:div [entity-link (str (:atlas/dev-id p)) (str (:atlas/type p))]])]
       [:div {:style (s :color text-dim :font-size "0.9rem")}
        "No dependents found."])]))

;; =============================================================================
;; Layout views
;; =============================================================================

(defn- resize-handle []
  [:div {:style    (s :width "5px" :cursor "col-resize" :flex-shrink 0
                      :background "transparent"
                      :border-left (str "1px solid " border)
                      :user-select "none")
         :on-mouse-down
         (fn [e]
           (.preventDefault e)
           (let [rect   (.. e -target -parentElement getBoundingClientRect)
                 left-x (.-left rect)
                 total-w (.-width rect)
                 on-move (fn [me]
                           (let [pct (max 15 (min 85 (* 100 (/ (- (.-clientX me) left-x) total-w))))]
                             (reset! pane-split pct)))
                 on-up   (fn on-up []
                           (.removeEventListener js/window "mousemove" on-move)
                           (.removeEventListener js/window "mouseup" on-up))]
             (.addEventListener js/window "mousemove" on-move)
             (.addEventListener js/window "mouseup" on-up)))}])

(defn- two-pane [left-content right-content]
  (let [split @pane-split]
    [:div {:style (s :display "flex" :height "100%" :overflow "hidden")}
     [:div {:style (s :width (str split "%") :flex-shrink 0 :overflow "hidden")}
      left-content]
     [resize-handle]
     [:div {:style (s :flex 1 :overflow "hidden")}
      right-content]]))

(defn- entity-focus-view [registry dev-id-str]
  (let [[_id props] (find-entity registry dev-id-str)]
    (if-not props
      [:div {:style (s :padding "24px" :color "#ef4444")}
       (str "Entity not found: " dev-id-str)]
      [two-pane
       [entity-info-panel registry dev-id-str]
       (if (has-dataflow? props)
         [data-flow-panel registry props]
         [dependents-panel registry dev-id-str])])))

(defn- blast-radius-view [registry dev-id-str]
  (let [deps (find-dependents registry dev-id-str)
        by-type (->> deps
                     (group-by #(str (:atlas/type %)))
                     (sort-by first))]
    [:div {:id "v3-scroll-area" :style (s :padding "16px" :overflow-y "auto" :height "100%")}
     [section-header (str "Blast radius of " dev-id-str " — " (count deps) " dependents")]
     (for [[etype entities] by-type]
       ^{:key etype}
       [:div {:style (s :margin-bottom "16px")}
        [:div {:style (s :color accent-amber :font-size "0.8rem"
                         :font-weight "bold" :margin-bottom "4px")}
         etype " (" (count entities) ")"]
        [:div {:style (s :font-family "monospace" :font-size "0.85rem" :line-height "1.8")}
         [collapsible-list (keyword etype) (sort-by #(str (:atlas/dev-id %)) entities)
          (fn [p] [:div [entity-link (str (:atlas/dev-id p))]])
          10]]])]))

(defn- domain-survey-view [registry aspect-str]
  (let [entities (entities-by-aspect registry aspect-str)
        query    (str/lower-case (get @app-state :filter-text ""))
        filtered (if (seq query)
                   (filter #(str/includes? (str/lower-case (str (:atlas/dev-id %))) query)
                           entities)
                   entities)
        by-type  (->> filtered (group-by #(str (:atlas/type %))) (sort-by first))]
    [:div {:id "v3-scroll-area" :style (s :padding "16px" :overflow-y "auto" :height "100%")}
     [:div {:style (s :display "flex" :align-items "baseline" :gap "12px" :margin-bottom "8px")}
      [:div {:style (s :font-size "0.75rem" :font-weight "bold" :letter-spacing "0.08em"
                       :color text-dim :text-transform "uppercase")}
       (str aspect-str " — " (count filtered)
            (when (< (count filtered) (count entities)) (str "/" (count entities))))]
      [:input {:type        "text"
               :placeholder "filter…"
               :value       (get @app-state :filter-text "")
               :on-change   #(swap! app-state assoc :filter-text (.. % -target -value))
               :style       (s :background "#1a1a2e"
                               :border (str "1px solid " border)
                               :border-radius "4px"
                               :color text-main
                               :font-size "0.8rem"
                               :padding "3px 8px"
                               :outline "none"
                               :width "180px")}]]
     (for [[etype eps] by-type]
       ^{:key etype}
       [:div {:style (s :margin-bottom "16px")}
        [:div {:style (s :color accent-amber :font-size "0.8rem"
                         :font-weight "bold" :margin-bottom "4px")}
         etype " (" (count eps) ")"]
        [:div {:style (s :font-family "monospace" :font-size "0.85rem" :line-height "1.8")}
         [collapsible-list (keyword etype) (sort-by #(str (:atlas/dev-id %)) eps)
          (fn [p] [:div [entity-link (str (:atlas/dev-id p))]])
          10]]])]))

(defn- dataflow-focus-view [registry data-key-str]
  (let [kw         (keyword (subs data-key-str 1))
        producers  (->> (vals registry)
                        (filter (fn [p]
                                  (let [r (prop-by-name p "response")]
                                    (and r (contains? (set r) kw))))))
        consumers  (->> (vals registry)
                        (filter (fn [p]
                                  (let [c (prop-by-name p "context")]
                                    (and c (contains? (set c) kw))))))]
    [:div {:style (s :display "flex" :height "100%" :overflow "hidden")}
     [:div {:id "v3-scroll-area"
            :style (s :flex 1 :border-right (str "1px solid " border)
                      :padding "16px" :overflow-y "auto")}
      [section-header (str "Producers of " data-key-str " (" (count producers) ")")]
      [:div {:style (s :font-family "monospace" :font-size "0.85rem" :line-height "1.8")}
       [collapsible-list :df-producers (sort-by #(str (:atlas/dev-id %)) producers)
        (fn [p] [:div [entity-link (str (:atlas/dev-id p)) nil accent-green]])
        10]]]
     [:div {:style (s :flex 1 :padding "16px" :overflow-y "auto")}
      [section-header (str "Consumers of " data-key-str " (" (count consumers) ")")]
      [:div {:style (s :font-family "monospace" :font-size "0.85rem" :line-height "1.8")}
       [collapsible-list :df-consumers (sort-by #(str (:atlas/dev-id %)) consumers)
        (fn [p] [:div [entity-link (str (:atlas/dev-id p)) nil accent-blue]])
        10]]]]))

(defn- home-view [registry]
  (let [by-type (count-by-type registry)
        total   (count registry)]
    [:div {:id "v3-scroll-area" :style (s :padding "24px" :overflow-y "auto" :height "100%"
                                          :display "flex" :flex-direction "column" :gap "24px")}
     [:div
      [:div {:style (s :font-size "2rem" :color text-dim :margin-bottom "8px")} "Atlas v3"]
      [:div {:style (s :color text-dim :font-size "0.9rem")}
       total " entities — waiting for navigation"]]
     [:div
      [section-header "Registry by type"]
      [:div {:style (s :display "flex" :flex-direction "column" :gap "6px")}
       [collapsible-list :home-types by-type
        (fn [{:keys [type count]}]
          [:div {:style    (s :display "flex" :align-items "center" :gap "12px"
                              :cursor "pointer" :padding "2px 4px" :margin "0 -4px"
                              :border-radius "4px")
                 :on-click #(navigate-to! "domain-survey" nil type)
                 :title    (str "Browse " type)}
           [:span {:style (s :font-family "monospace" :font-size "0.85rem" :color accent-amber
                             :min-width "300px")} type]
           [:div {:style (s :height "6px" :border-radius "3px" :background accent-blue
                            :width (str (max 4 (* 300 (/ count total))) "px"))}]
           [:span {:style (s :color text-dim :font-size "0.8rem")} count]])
        8]]]
     [:div {:style (s :color text-dim :font-size "0.8rem" :font-style "italic")}
      "Use Claude Code: /atlas-emacs <intent> — or POST to /api/v3/state"]]))

;; =============================================================================
;; App header
;; =============================================================================

(defn- mode-label [mode]
  (case mode
    "entity-focus"  "entity-focus"
    "blast-radius"  "blast-radius"
    "domain-survey" "domain-survey"
    "dataflow-focus" "dataflow-focus"
    "home"))

(defn- search-result-item [m]
  [:div {:style    (s :padding "5px 10px" :font-size "0.8rem"
                      :font-family "monospace" :color text-main
                      :cursor "pointer")
         :on-click #(do (swap! app-state assoc :search-text "")
                        (navigate-to! "entity-focus" (str (:atlas/dev-id m)) nil))
         :on-mouse-over #(set! (.. % -target -style -background) "#1a1a2e")
         :on-mouse-out  #(set! (.. % -target -style -background) "transparent")}
   (str (:atlas/dev-id m))])

(defn- search-box [registry]
  (let [q       (str/lower-case (get @app-state :search-text ""))
        matches (when (>= (count q) 2)
                  (->> (vals registry)
                       unique-by-dev-id
                       (filter #(str/includes? (str/lower-case (str (:atlas/dev-id %))) q))
                       (sort-by #(str (:atlas/dev-id %)))
                       (take 10)))]
    [:div {:style (s :position "relative")}
     [:input {:type        "text"
              :placeholder "search entities…"
              :value       (get @app-state :search-text "")
              :on-change   #(swap! app-state assoc :search-text (.. % -target -value))
              :on-blur     #(js/setTimeout (fn [] (swap! app-state assoc :search-text "")) 200)
              :style       (s :background "#1a1a2e"
                              :border (str "1px solid " border)
                              :border-radius "4px"
                              :color text-main
                              :font-size "0.75rem"
                              :padding "3px 8px"
                              :outline "none"
                              :width "160px")}]
     (when (seq matches)
       [:div {:style (s :position "absolute" :top "110%" :right 0
                        :background "#0f0f1a"
                        :border (str "1px solid " border)
                        :border-radius "4px"
                        :z-index 200
                        :min-width "240px"
                        :box-shadow "0 4px 12px rgba(0,0,0,0.5)")}
        (for [m matches]
          ^{:key (str (:atlas/dev-id m))}
          [search-result-item m])])]))

(defn- clear-scenario! []
  (-> (js/fetch "/api/v3/state"
                (clj->js {:method  "POST"
                          :headers {"Content-Type" "application/json"}
                          :body    (js/JSON.stringify
                                    (clj->js {:scenario nil :step nil :step-total nil
                                              :narrative nil}))}))
      (.catch (fn [e] (js/console.error "clear-scenario! error" e)))))

(defn- scenario-bar [scenario step step-total]
  [:div {:style (s :padding "4px 16px"
                   :background "#08081a"
                   :border-bottom (str "1px solid " border)
                   :display "flex" :align-items "center" :gap "10px"
                   :flex-shrink 0)}
   [:span {:style (s :font-size "0.75rem" :color accent-green)} "▶ investigating"]
   [:span {:style (s :font-size "0.78rem" :color text-main :font-weight "bold"
                     :font-family "monospace")}
    scenario]
   (when (and step (pos? step))
     [:span {:style (s :font-size "0.72rem" :color text-dim
                       :background (str accent-green "18")
                       :padding "1px 7px" :border-radius "8px")}
      (str "step " step (when step-total (str "/" step-total)))])
   [:span {:style    (s :margin-left "auto" :font-size "0.72rem" :color "#555"
                        :cursor "pointer" :padding "1px 6px")
           :on-click clear-scenario!
           :title    "Clear scenario"}
    "✕"]])

(defn- header [view-state nav-count nav-future-count registry]
  (let [{:keys [mode entity aspect data-key]} (:view-state @app-state)
        target (or entity aspect data-key)]
    [:div {:style (s :padding "10px 16px"
                     :background "#0a0a1a"
                     :border-bottom (str "1px solid " border)
                     :display "flex" :align-items "center" :gap "12px"
                     :flex-shrink 0)}
     (if (pos? nav-count)
       [:span {:style    (s :font-size "1rem" :color text-dim :cursor "pointer"
                            :padding "0 4px" :border-radius "3px")
               :on-click back!
               :title    "Back"}
        "←"]
       [:span {:style (s :font-size "1rem" :color "#333" :padding "0 4px")} "←"])
     (if (pos? nav-future-count)
       [:span {:style    (s :font-size "1rem" :color text-dim :cursor "pointer"
                            :padding "0 4px" :border-radius "3px")
               :on-click forward!
               :title    "Forward"}
        "→"]
       [:span {:style (s :font-size "1rem" :color "#333" :padding "0 4px")} "→"])
     [:span {:style (s :font-size "0.9rem" :font-weight "bold" :color accent-blue)}
      "Atlas v3"]
     [:span {:style (s :font-size "0.75rem" :background (str accent-blue "22")
                       :color accent-blue :padding "2px 8px" :border-radius "8px")}
      (mode-label mode)]
     (when target
       [:span {:style (s :font-family "monospace" :font-size "0.85rem" :color text-main)}
        target])
     [:div {:style (s :margin-left "auto" :display "flex" :align-items "center" :gap "8px")}
      (for [[m handler] [["home"           #(navigate-to! "home" nil nil)]
                         ["entity-focus"   (when entity   #(navigate-to! "entity-focus" entity nil))]
                         ["blast-radius"   (when entity   #(navigate-to! "blast-radius" entity nil))]
                         ["domain-survey"  (when aspect   #(navigate-to! "domain-survey" nil aspect))]
                         ["dataflow-focus" (when data-key #(navigate-to! "dataflow-focus" data-key nil))]]]
        ^{:key m}
        [:span {:style    (s :font-size "0.75rem"
                             :color (if (= m mode) text-main (if handler text-dim "#555"))
                             :cursor (if handler "pointer" "default")
                             :padding "2px 6px")
                :on-click handler
                :title    (when-not handler "No context")}
         m])
      [search-box registry]]]))


;; =============================================================================
;; Narrative pane (Momentum B — LLM annotation)
;; =============================================================================

(defn- parse-narrative
  "Split '[tool-name] target\\nresult…' into [tool-line rest-text].
   Returns [nil full-text] for plain text."
  [text]
  (if (str/starts-with? text "[")
    (let [idx (str/index-of text "\n")]
      (if idx [(subs text 0 idx) (subs text (inc idx))] [text nil]))
    [nil text]))

(defn- parse-with-kws
  "Tokenize text into [:text str] / [:kw str] segments for keyword detection."
  [text]
  (let [kw-re #":[a-zA-Z][a-zA-Z0-9.*+!_?-]*/[a-zA-Z][a-zA-Z0-9.*+!_?-]*"]
    (loop [s text result []]
      (if-let [m (re-find kw-re s)]
        (let [idx    (str/index-of s m)
              before (subs s 0 idx)
              after  (subs s (+ idx (count m)))]
          (recur after
                 (cond-> result
                   (seq before) (conj [:text before])
                   true         (conj [:kw m]))))
        (if (seq s) (conj result [:text s]) result)))))

(defn- render-with-kws
  "Render text with :[ns]/name keywords as clickable chips."
  [text registry]
  (let [tokens (parse-with-kws text)]
    (into [:<>]
          (map-indexed
           (fn [i [kind seg]]                         ; `seg` avoids shadowing the `s` style fn
             ^{:key i}
             (if (= kind :kw)
               [smart-chip registry (keyword (subs seg 1))]
               [:span {:style (s :color "#c8d4de" :font-size "0.8rem" :line-height "1.75"
                                 :white-space "pre-wrap" :word-break "break-word")}
                seg]))
           tokens))))

(defn- narrative-panel [text registry]
  (when (seq text)
    (let [[tool-line result-text] (parse-narrative text)]
      [:div {:style (s :flex-shrink 0)}
       ;; Drag handle — grab and drag UP to expand, DOWN to shrink
       [:div {:style (s :height "5px" :cursor "ns-resize"
                        :background "transparent"
                        :border-top (str "1px solid " border)
                        :user-select "none")
              :on-mouse-down
              (fn [e]
                (.preventDefault e)
                (let [start-y (.-clientY e)
                      start-h @narrative-height
                      on-move (fn [me]
                                (let [dy    (- start-y (.-clientY me))
                                      new-h (max 30 (min 400 (+ start-h dy)))]
                                  (reset! narrative-height new-h)))
                      on-up   (fn on-up []
                                (.removeEventListener js/window "mousemove" on-move)
                                (.removeEventListener js/window "mouseup" on-up))]
                  (.addEventListener js/window "mousemove" on-move)
                  (.addEventListener js/window "mouseup" on-up)))}]
       ;; Content area — height controlled by narrative-height atom
       [:div {:style (s :height (str @narrative-height "px")
                        :background "#0c0c18"
                        :overflow-y "auto"
                        :padding "6px 14px")}
        (when tool-line
          [:div {:style (s :font-size "0.76rem" :color "#6a9fc8"
                           :font-family "monospace" :font-weight "bold"
                           :margin-bottom "4px")}
           tool-line])
        (when (seq result-text)
          [render-with-kws result-text registry])]])))


;; =============================================================================
;; Root app
;; =============================================================================

(defn- main-content [view-state registry]
  (let [{:keys [mode entity aspect data-key]} view-state]
    (cond
      (= mode "entity-focus")
      (if entity
        [entity-focus-view registry entity]
        [:div {:style (s :padding "24px" :color text-dim)} "No entity specified."])

      (= mode "blast-radius")
      (if entity
        [blast-radius-view registry entity]
        [:div {:style (s :padding "24px" :color text-dim)} "No entity specified."])

      (= mode "domain-survey")
      (if aspect
        [domain-survey-view registry aspect]
        [:div {:style (s :padding "24px" :color text-dim)} "No aspect specified."])

      (= mode "dataflow-focus")
      (if data-key
        [dataflow-focus-view registry data-key]
        [:div {:style (s :padding "24px" :color text-dim)} "No data-key specified."])

      :else
      [home-view registry])))

(defn app []
  (let [{:keys [loading? error view-state registry nav-history nav-future]} @app-state
        {:keys [narrative scenario step step-total]} view-state]
    [:div {:style (s :height "100vh" :display "flex" :flex-direction "column"
                     :background dark :color text-main
                     :font-family "-apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif")}
     [header view-state (count nav-history) (count nav-future) registry]
     (when (seq scenario)
       [scenario-bar scenario step step-total])
     [:div {:style (s :flex 1 :overflow "hidden")}
      (cond
        loading?  [:div {:style (s :display "flex" :align-items "center"
                                   :justify-content "center" :height "100%"
                                   :color text-dim)} "Loading registry…"]
        error     [:div {:style (s :display "flex" :align-items "center"
                                   :justify-content "center" :height "100%"
                                   :color "#ef4444")} (str error)]
        registry  [main-content view-state registry]
        :else     [:div {:style (s :color text-dim :padding "24px")} "Waiting…"])]
     [narrative-panel narrative registry]]))

;; =============================================================================
;; Init
;; =============================================================================

(defn mount-root []
  (when-let [el (.getElementById js/document "app")]
    (rdom/render [app] el)))

(defn init []
  (js/console.log "Atlas UI v3 initializing…")
  ;; Subscribe to SSE state updates
  (api/subscribe-sse!
   (fn [new-view-state]
     (js/console.log "SSE state:" (clj->js new-view-state))
     (swap! app-state
            (fn [s]
              (-> s
                  (update :nav-history
                          (fn [h] (vec (take-last 50 (conj h (nav-snapshot s))))))
                  (assoc :nav-future [])          ; new tool call clears forward stack
                  (assoc :view-state new-view-state)
                  (assoc :expanded-lists {})
                  (assoc :filter-text "")
                  (assoc :search-text "")))))
   (fn [err]
     (js/console.warn "SSE error:" err)))
  ;; Fetch registry once
  (api/fetch-registry!
   (fn [data]
     (let [registry (or (:atlas-ui.api.response/registry data) data)]
       (swap! app-state assoc :registry registry :loading? false)))
   (fn [err]
     (swap! app-state assoc :loading? false :error (str "Registry load failed: " (pr-str err)))))
  (mount-root))
