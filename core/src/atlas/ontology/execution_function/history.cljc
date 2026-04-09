(ns atlas.ontology.execution-function.history
  "Execution history data layer for atlas execution-functions.

   Bounded in-process ring buffer that records (filtered) execution-function
   calls. Environment-agnostic: the same record!/query/summary primitives
   serve dev introspection, prod telemetry, or anything else.

   The LLM-IDE tool surface that wraps these primitives lives separately
   so each environment can ship its own (dev-tools, future cloud, …).

   Pipeline:
     1. configure! once on startup (capacity, capture predicate, aspect
        filter, optional sink and trace-id-fn).
     2. enable! to start recording.
     3. The user's exec-fn calls record! after each invocation (success
        or failure).
     4. Consumers query via query/summary, or via the LLM-IDE tools
        registered by a sibling namespace
        (e.g. atlas.ontology.execution-function.history.llm-ide).

   See docs/exec-history.md for the full design."
  (:require [atlas.core :as atlas]
            [atlas.registry :as registry]
            [clojure.set :as set]
            [clojure.spec.alpha :as s]))

;; ============================================================================
;; SPECS — context (input) keys
;; ============================================================================
;;
;; :entity/dev-id and :query/aspect are already defined in atlas.llm-ide;
;; defining them again here is harmless (last wins) and keeps this namespace
;; loadable in isolation. Same predicate either way.

(s/def :entity/dev-id    qualified-keyword?)
(s/def :query/aspect     qualified-keyword?)
(s/def :query/cursor     nat-int?)
(s/def :query/limit      pos-int?)
(s/def :query/status     #{:ok :error})
(s/def :query/since      inst?)
(s/def :query/order      #{:asc :desc})

;; ============================================================================
;; SPECS — response (output) keys
;; ============================================================================

(s/def :exec/id             nat-int?)
(s/def :exec/at             inst?)
(s/def :exec/dev-id         qualified-keyword?)
(s/def :exec/duration-ms    nat-int?)
(s/def :exec/status         #{:ok :error})
(s/def :exec/args           map?)
(s/def :exec/response       (s/nilable map?))
(s/def :exec/error          (s/nilable map?))
(s/def :exec/atlas          (s/nilable map?))

(s/def :exec/entry          (s/keys :req [:exec/id :exec/at :exec/dev-id
                                          :exec/duration-ms :exec/status]
                                    :opt [:exec/args :exec/response
                                          :exec/error :exec/atlas]))

(s/def :exec/entries        (s/coll-of :exec/entry))
(s/def :exec/next-cursor    nat-int?)
(s/def :exec/returned       nat-int?)
(s/def :exec/has-more?      boolean?)
(s/def :exec/total-buffered nat-int?)
(s/def :exec/evicted-before nat-int?)
(s/def :exec/total          nat-int?)
(s/def :exec/by-dev-id      (s/map-of qualified-keyword? map?))
(s/def :exec/by-status      (s/map-of keyword? nat-int?))
(s/def :exec/by-domain      (s/map-of qualified-keyword? nat-int?))
(s/def :exec/time-range     (s/nilable (s/keys :req-un [::from ::to])))
(s/def :exec/cleared?       boolean?)

;; ============================================================================
;; STATE
;; ============================================================================

(def ^:private default-capacity 1000)

(def ^:private state
  (atom {:enabled?       false
         :capacity       default-capacity
         :match-aspects  nil          ;; #{aspects} or nil for no filter
         :predicate      nil          ;; (fn [entry props]) or nil
         :next-id        0
         :evicted-before 0
         :entries        #?(:clj clojure.lang.PersistentQueue/EMPTY
                            :cljs #queue [])}))

;; ============================================================================
;; CONFIGURATION
;; ============================================================================

(defn configure!
  "Set or update history config. All keys optional.

   Options:
     :enabled?       boolean - master switch (default false)
     :capacity       int     - ring buffer size (default 1000)
     :match-aspects  set     - capture only entries whose entity's
                                declared∪derived aspects intersect this set.
                                nil = no aspect filter.
     :predicate      fn      - (fn [entry props]) - last word; capture only
                                if truthy. Use this to gate on thread-bound
                                state, request id, environment, etc."
  [opts]
  (swap! state merge (select-keys opts [:enabled? :capacity
                                        :match-aspects :predicate]))
  ;; if shrinking capacity, drop overflow now
  (let [{:keys [capacity entries]} @state]
    (when (> (count entries) capacity)
      (swap! state update :entries
             (fn [q] (loop [q q]
                       (if (> (count q) capacity) (recur (pop q)) q))))))
  @state)

(defn enable!  [] (swap! state assoc :enabled? true) nil)
(defn disable! [] (swap! state assoc :enabled? false) nil)
(defn enabled? [] (:enabled? @state))

(defn clear!
  "Drop all entries. Resets cursor to 0."
  []
  (swap! state assoc
         :entries        #?(:clj clojure.lang.PersistentQueue/EMPTY
                            :cljs #queue [])
         :next-id        0
         :evicted-before 0)
  nil)

;; ============================================================================
;; CAPTURE GATE
;; ============================================================================

(defn- aspects-of [dev-id]
  (set/union (or (registry/declared-aspects dev-id) #{})
             (or (registry/derived-aspects  dev-id) #{})))

(defn- should-capture?
  [{:keys [enabled? match-aspects predicate]} entry props]
  (and enabled?
       (or (nil? match-aspects)
           (seq (set/intersection (aspects-of (:exec/dev-id entry))
                                  match-aspects)))
       (or (nil? predicate)
           (try (boolean (predicate entry props))
                (catch #?(:clj Throwable :cljs :default) _ false)))))

;; ============================================================================
;; RING BUFFER PUSH
;; ============================================================================

(defn- push [{:keys [entries capacity evicted-before] :as s} entry]
  (let [q (conj entries entry)
        [q evicted-before'] (if (> (count q) capacity)
                              [(pop q) (inc evicted-before)]
                              [q evicted-before])]
    (assoc s
           :entries        q
           :next-id        (inc (:next-id s))
           :evicted-before evicted-before')))

(defn record!
  "Append one execution entry. Safe no-op when disabled or filtered out.
   Never throws — wrapped in try/catch so executor failures cannot mask
   the original call result.

   Entry shape (caller is responsible for pre-filtering args/response to
   declared keys):
     {:exec/dev-id      :fn/foo
      :exec/duration-ms 12
      :exec/status      :ok            ;; or :error
      :exec/args        {...}           ;; pre-filtered context-keys
      :exec/response    {...}           ;; pre-filtered response-keys, or nil
      :exec/error       {:class \"...\"
                         :message \"...\"
                         :ex-data {...}}}  ;; or nil

   `props` is the entity props the executor already fetched, used to
   enrich :exec/atlas without a redundant registry lookup."
  [entry props]
  (try
    (when (should-capture? @state entry props)
      (let [snapshot (atlas/entity-snapshot-fast (:exec/dev-id entry) props)
            id       (:next-id @state)
            full     (assoc entry
                            :exec/id    id
                            :exec/at    #?(:clj (java.util.Date.)
                                           :cljs (js/Date.))
                            :exec/atlas snapshot)]
        (swap! state push full)))
    (catch #?(:clj Throwable :cljs :default) _ nil)))

;; ============================================================================
;; QUERY
;; ============================================================================

(def ^:private max-limit     200)
(def ^:private default-limit 50)

(defn- entry-aspects [e]
  (let [a (:exec/atlas e)]
    (set/union (or (:atlas/declared a) #{})
               (or (:atlas/derived  a) #{}))))

(defn query
  "Cursor-paginated history fetch. The single primitive every tool builds on.

   Opts:
     :query/cursor    - id to start at (inclusive). Default 0.
     :query/limit     - max entries to return. Default 50, capped at 200.
     :entity/dev-id   - filter to a single execution-function dev-id
     :query/status    - :ok or :error
     :query/aspect    - keyword; entries whose :exec/atlas declared∪derived
                        aspects include this
     :query/since     - inst lower bound on :exec/at
     :query/order     - :asc (default) or :desc

   Returns:
     {:exec/entries        [...]
      :exec/returned       n
      :exec/next-cursor    nat-int      ;; pass back to fetch the next page
      :exec/has-more?      bool
      :exec/total-buffered n
      :exec/evicted-before nat-int}     ;; oldest id still in buffer; cursors
                                        ;; below this are stale"
  ([] (query {}))
  ([{:keys [:query/cursor :query/limit
            :entity/dev-id
            :query/status :query/aspect :query/since :query/order]
     :or   {cursor 0 limit default-limit order :asc}}]
   (let [{:keys [entries evicted-before]} @state
         limit    (min (max 1 limit) max-limit)
         filtered (cond->> (seq entries)
                    true            (filter #(>= (:exec/id %) cursor))
                    dev-id          (filter #(= (:exec/dev-id %) dev-id))
                    status          (filter #(= (:exec/status %) status))
                    aspect          (filter #(contains? (entry-aspects %) aspect))
                    since           (filter #(>= (compare (:exec/at %) since) 0))
                    (= :desc order) reverse)
         taken     (take (inc limit) filtered)   ;; +1 to detect has-more?
         page      (vec (take limit taken))
         has-more? (> (count taken) limit)
         next-cursor (if (seq page)
                       (inc (:exec/id (last page)))
                       cursor)]
     {:exec/entries        page
      :exec/returned       (count page)
      :exec/next-cursor    next-cursor
      :exec/has-more?      has-more?
      :exec/total-buffered (count entries)
      :exec/evicted-before evicted-before})))

;; ============================================================================
;; AGGREGATE SUMMARY
;; ============================================================================

(defn- p95 [sorted-durs]
  (let [n (count sorted-durs)]
    (when (pos? n)
      (nth sorted-durs (min (dec n) (int (* 0.95 n)))))))

(defn summary
  "Aggregate stats over the buffer (or a filtered slice).
   Bounded payload — never returns raw entries.

   Opts: same filter keys as query (:entity/dev-id, :query/status,
   :query/aspect, :query/since), no cursor."
  ([] (summary {}))
  ([{:keys [:entity/dev-id :query/status :query/aspect :query/since]}]
   (let [{:keys [entries evicted-before]} @state
         filtered (cond->> (seq entries)
                    dev-id (filter #(= (:exec/dev-id %) dev-id))
                    status (filter #(= (:exec/status %) status))
                    aspect (filter #(contains? (entry-aspects %) aspect))
                    since  (filter #(>= (compare (:exec/at %) since) 0)))
         es        (vec filtered)
         total     (count es)
         by-dev-id (->> es
                        (group-by :exec/dev-id)
                        (reduce-kv
                          (fn [m k v]
                            (let [errs   (count (filter #(= :error (:exec/status %)) v))
                                  durs   (vec (sort (keep :exec/duration-ms v)))
                                  n      (count durs)
                                  sum    (reduce + 0 durs)]
                              (assoc m k {:count  n
                                          :errors errs
                                          :avg-ms (when (pos? n) (double (/ sum n)))
                                          :p95-ms (p95 durs)})))
                          {}))
         by-status (frequencies (map :exec/status es))
         by-domain (->> es
                        (mapcat entry-aspects)
                        (filter #(= "domain" (namespace %)))
                        frequencies)
         time-range (when (seq es)
                      {:from (:exec/at (first es))
                       :to   (:exec/at (last  es))})]
     {:exec/total          total
      :exec/total-buffered (count (:entries @state))
      :exec/evicted-before evicted-before
      :exec/by-dev-id      by-dev-id
      :exec/by-status      by-status
      :exec/by-domain      by-domain
      :exec/time-range     time-range})))

;; LLM-IDE tool registrations live in a sibling namespace, not here.
;; Each environment ships its own tool surface against the same data layer:
;;   - dev-tools : atlas.ontology.execution-function.history.llm-ide
;;   - cloud     : (future)
