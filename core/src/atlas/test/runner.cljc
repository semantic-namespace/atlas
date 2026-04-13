(ns atlas.test.runner
  "Test runner for :atlas/test-case entities.

   Composes existing machinery — does not re-implement execution, tracing, or
   burst capture. The only new logic is expectation checking.

   Usage:
     (require '[atlas.test.runner :as test-runner])
     (test-runner/run-test {:test/case-dev-id :test/my-case
                            :test/ig-config   ig-config})

   See docs/testing-design.md for the full design."
  (:require [atlas.registry :as registry]
            [atlas.registry.lookup :as entity]
            [atlas.ontology :as o]
            [atlas.ontology.execution-function.executor :as executor]
            [atlas.ontology.execution-function.history :as h]
            [atlas.adapter.integrant :as ig-adapter]
            [clojure.spec.alpha :as s]))

;; ============================================================================
;; RESOLVE COMPONENTS — mocks override, rest from subsystem
;; ============================================================================

(defn- wrap-with-record
  "Wrap an impl fn so it calls h/record! with the execution entry.
   This ensures trace expectations work inside with-trace."
  [dev-id impl-fn]
  (fn [ctx]
    (let [props   (entity/props-for dev-id)
          started #?(:clj (System/currentTimeMillis)
                     :cljs (.getTime (js/Date.)))
          result  (try
                    (let [r (impl-fn ctx)]
                      (h/record! {:exec/dev-id      dev-id
                                  :exec/duration-ms (- #?(:clj (System/currentTimeMillis)
                                                          :cljs (.getTime (js/Date.)))
                                                       started)
                                  :exec/status      :ok
                                  :exec/args        (select-keys ctx (o/context-for dev-id))
                                  :exec/response    (select-keys r (o/response-for dev-id))}
                                 props)
                      r)
                    (catch #?(:clj Throwable :cljs :default) e
                      (h/record! {:exec/dev-id      dev-id
                                  :exec/duration-ms (- #?(:clj (System/currentTimeMillis)
                                                          :cljs (.getTime (js/Date.)))
                                                       started)
                                  :exec/status      :error
                                  :exec/error       {:class   #?(:clj (.getName (class e))
                                                                 :cljs "Error")
                                                     :message #?(:clj (.getMessage e)
                                                                 :cljs (.-message e))}}
                                 props)
                      (throw e)))]
      result)))

(defn- impl-map-for
  "Build impl-map from :atlas/impl in registry for target and its direct deps.
   Each impl is wrapped with h/record! so trace capture works."
  [target-id]
  (let [deps    (o/deps-for target-id)
        all-ids (conj (vec deps) target-id)]
    (into {}
          (keep (fn [dev-id]
                  (when-let [impl (:atlas/impl (entity/props-for dev-id))]
                    [dev-id (wrap-with-record dev-id impl)])))
          all-ids)))

(defn- resolve-components
  "Merge real subsystem with mock overrides."
  [system mocks]
  (merge system (or mocks {})))

;; ============================================================================
;; EXPECTATION CHECKING
;; ============================================================================

(defmulti check-expectation
  "Check a single expectation against the test result and/or burst.
   Returns {:check/pass? bool :check/kind kw :check/message str ...}."
  (fn [expectation _result _burst] (:expectation/kind expectation)))

(defmethod check-expectation :result
  [{:expectation/keys [spec pred docs]} result _burst]
  (cond
    ;; Spec-based check
    spec
    (let [valid? (s/valid? spec result)]
      (cond-> {:check/pass?   valid?
               :check/kind    :result
               :check/expected (str "spec " spec)}
        (not valid?) (assoc :check/message (s/explain-str spec result)
                            :check/actual  result)
        docs         (assoc :check/docs docs)))

    ;; Predicate-based check
    pred
    (let [pass? (try (boolean (pred result))
                     (catch #?(:clj Throwable :cljs :default) e
                       false))]
      (cond-> {:check/pass?   pass?
               :check/kind    :result
               :check/expected (or docs "<predicate>")}
        (not pass?) (assoc :check/message (or docs "Predicate returned false")
                           :check/actual  result)
        docs        (assoc :check/docs docs)))

    :else
    {:check/pass?  false
     :check/kind   :result
     :check/message "Expectation has neither :expectation/spec nor :expectation/pred"}))

(defmethod check-expectation :trace
  [{:expectation/keys [must-contain pred docs]} _result burst]
  (cond
    ;; must-contain: check if a dev-id appears in the burst
    must-contain
    (let [burst-dev-ids (set (map :exec/dev-id burst))
          found?        (contains? burst-dev-ids must-contain)]
      (cond-> {:check/pass?   found?
               :check/kind    :trace
               :check/expected (str "burst must contain " must-contain)}
        (not found?) (assoc :check/message (str "not found in burst. "
                                                "Burst contains: "
                                                (vec (sort burst-dev-ids)))
                            :check/actual (vec (sort burst-dev-ids)))
        docs         (assoc :check/docs docs)))

    ;; Predicate over burst
    pred
    (let [pass? (try (boolean (pred burst))
                     (catch #?(:clj Throwable :cljs :default) _ false))]
      (cond-> {:check/pass?   pass?
               :check/kind    :trace
               :check/expected (or docs "<trace predicate>")}
        (not pass?) (assoc :check/message (or docs "Trace predicate returned false"))
        docs        (assoc :check/docs docs)))

    :else
    {:check/pass?  false
     :check/kind   :trace
     :check/message "Trace expectation has neither :expectation/must-contain nor :expectation/pred"}))

(defmethod check-expectation :invariant
  [{:expectation/keys [invariant-id docs]} _result _burst]
  (if-let [inv-fn (:invariant/fn (entity/props-for invariant-id))]
    (let [violation (inv-fn)]
      (cond-> {:check/pass?   (nil? violation)
               :check/kind    :invariant
               :check/expected (str "invariant " invariant-id " holds")}
        violation (assoc :check/message (:message violation)
                         :check/actual  violation)
        docs      (assoc :check/docs docs)))
    {:check/pass?  false
     :check/kind   :invariant
     :check/message (str "Invariant " invariant-id " not found in registry")}))

(defmethod check-expectation :default
  [expectation _result _burst]
  {:check/pass?  false
   :check/kind   (:expectation/kind expectation)
   :check/message (str "Unknown expectation kind: " (:expectation/kind expectation))})

;; ============================================================================
;; RUNNER
;; ============================================================================

(defn run-test
  "Run a single :atlas/test-case.

   Args map:
     :test/case-dev-id — the test-case's dev-id (required)
     :test/ig-config   — integrant config map (optional, needed for :from-system)
     :test/init-key-fn — integrant init-key function (optional, default identity)
     :test/halt-key-fn — integrant halt-key function (optional, default identity)

   Starts a minimal subsystem (only the components the target needs, minus
   mocked ones), runs the pipeline inside with-trace, captures the burst,
   checks expectations, tears down the subsystem.

   Returns a report map — see docs/testing-design.md for the shape."
  [{:test/keys [case-dev-id ig-config init-key-fn halt-key-fn]}]
  ;; Ensure history is enabled so trace expectations work
  (when-not (h/enabled?) (h/enable!))
  (let [{:test-case/keys [target fixture mocks expectations]}
        (entity/props-for case-dev-id)

        mock-keys  (set (keys (or mocks {})))
        sub-config (when ig-config
                     (ig-adapter/subsystem-config ig-config target mock-keys))
        init-fn    (or init-key-fn identity)
        halt-fn    (or halt-key-fn (fn [_ _]))
        system     (when sub-config
                     (ig-adapter/start-system sub-config init-fn))]
    (try
      (let [ctx      (merge (resolve-components system mocks)
                            (or fixture {}))
            impl     (impl-map-for target)
            pipeline (executor/build-pipeline target impl)
            started  #?(:clj (System/currentTimeMillis)
                        :cljs (.getTime (js/Date.)))
            ;; with-trace binds *exec-trace-id* — capture it inside
            trace-id-atom (atom nil)
            result   (h/with-trace
                       (reset! trace-id-atom h/*exec-trace-id*)
                       (pipeline ctx))
            elapsed  (- #?(:clj (System/currentTimeMillis)
                           :cljs (.getTime (js/Date.)))
                        started)
            trace-id @trace-id-atom
            burst    (:exec/entries
                      (h/query {:query/trace-id trace-id
                                :query/compact  true
                                :query/limit    200}))
            checks   (mapv #(check-expectation % result (or burst []))
                           (or expectations []))
            passed   (filter :check/pass? checks)
            failed   (remove :check/pass? checks)]
        (cond-> {:test/case          case-dev-id
                 :test/target        target
                 :test/pass?         (empty? failed)
                 :test/trace-id      trace-id
                 :test/duration-ms   elapsed
                 :test/burst-size    (count (or burst []))
                 :test/checks-passed (count passed)
                 :test/checks-failed (count failed)}
          (seq failed) (assoc :test/failed-checks (vec failed))))
      (finally
        (when (and sub-config system)
          (ig-adapter/stop-system sub-config system halt-fn))))))

(defn- user-test-cases
  "All test-case dev-ids, excluding the ontology meta-entity."
  []
  (->> (entity/all-with-aspect :atlas/test-case)
       (remove #(entity/has-aspect? % :atlas/ontology))))

(defn run-all
  "Run all test-cases matching an optional aspect filter.

   Args map:
     :test/aspect     — aspect keyword to filter by (optional, runs all if nil)
     :test/ig-config  — integrant config map (optional)
     :test/init-key-fn — integrant init-key function (optional)
     :test/halt-key-fn — integrant halt-key function (optional)

   Returns {:test/total n :test/passed n :test/failed n :test/reports [...]}"
  [{:test/keys [aspect ig-config init-key-fn halt-key-fn]}]
  (let [all-cases  (user-test-cases)
        cases      (if aspect
                     (filter #(entity/has-aspect? % aspect) all-cases)
                     all-cases)
        reports    (mapv (fn [case-dev-id]
                           (run-test {:test/case-dev-id case-dev-id
                                      :test/ig-config   ig-config
                                      :test/init-key-fn init-key-fn
                                      :test/halt-key-fn halt-key-fn}))
                         cases)
        passed     (filter :test/pass? reports)
        failed     (remove :test/pass? reports)]
    {:test/total   (count reports)
     :test/passed  (count passed)
     :test/failed  (count failed)
     :test/reports reports}))

;; ============================================================================
;; HISTORY LIFT — generate test-case from a trace
;; ============================================================================

(defn test-case-from-trace
  "Given an exec-history trace-id, generate a candidate test-case registration.

   Inspects the burst to find the entry-point call, lifts its args into a
   fixture, and suggests trace expectations based on the calls observed.

   Returns a map with:
     :test-case/target       — the entry-point dev-id
     :test-case/fixture      — args from the first call
     :test-case/expectations — suggested expectations
     :test/registration-edn  — printable EDN string for register! call

   The developer reviews and registers the result."
  [{:keys [trace-id test-dev-id]}]
  (when-not (h/enabled?) (h/enable!))
  (let [burst   (:exec/entries
                  (h/query {:query/trace-id trace-id
                            :query/compact  false
                            :query/limit    200}))
        _       (when (empty? burst)
                  (throw (ex-info "No entries found for trace-id"
                                  {:trace-id trace-id})))
        ;; Entry point: the dev-id in the burst that is not a dep of any other.
        ;; Falls back to last-by-id if ontology deps don't resolve.
        burst-ids (set (map :exec/dev-id burst))
        dep-ids   (into #{} (mapcat o/deps-for) burst-ids)
        root-ids  (remove dep-ids burst-ids)
        target    (or (first root-ids)
                      (:exec/dev-id (last (sort-by :exec/id burst))))
        entry     (first (filter #(= target (:exec/dev-id %)) burst))
        fixture (or (:exec/args entry) {})
        ;; Suggest trace expectations for each call in the burst
        trace-expectations
        (mapv (fn [e]
                {:expectation/kind :trace
                 :expectation/must-contain (:exec/dev-id e)
                 :expectation/docs (str "Observed call to " (:exec/dev-id e))})
              (remove #(= target (:exec/dev-id %)) burst))
        ;; Suggest a result expectation if the entry succeeded
        result-expectations
        (when (= :ok (:exec/status entry))
          [{:expectation/kind :result
            :expectation/pred 'identity
            :expectation/docs "TODO: replace with meaningful predicate"}])
        all-expectations (vec (concat result-expectations trace-expectations))
        dev-id  (or test-dev-id
                    (keyword "test" (name target)))
        edn-str (pr-str
                  (list 'registry/register!
                        dev-id
                        :atlas/test-case
                        #{:atlas/test-case}
                        {:test-case/target       target
                         :test-case/fixture      fixture
                         :test-case/expectations all-expectations}))]
    {:test-case/target       target
     :test-case/fixture      fixture
     :test-case/expectations all-expectations
     :test/registration-edn  edn-str}))
