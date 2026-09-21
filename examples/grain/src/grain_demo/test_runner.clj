(ns grain-demo.test-runner
  "Generic runner for fully-declarative grain test-cases.

   On an event-sourced substrate, arrange/act/assert are data:
     Given = events to append        (:fixture :given-events)
     When  = a command map           (:fixture :command)
     Then  = expected event types or an anomaly category (:expectations)

   So a test-case is a registered :atlas/test-case entity with no code, and
   this one runner executes them all: fresh in-memory event store + cache +
   a random tenant per case (isolating grain's global L1 read-model cache),
   append Given, process When, compare Then.

   Requires materialize! to have run (commands must be live in grain's
   registries)."
  (:require
   [atlas.registry :as registry]
   [ai.obney.grain.event-store-v3.interface :as es :refer [->event]]
   [ai.obney.grain.command-processor-v2.interface :as cp]
   [ai.obney.grain.time.interface :as gtime]
   [grain-demo.mem-kv :as mem-kv]))

(defn- check-expectation
  [{:expect/keys [anomaly events] :as _expectation} result]
  (cond
    anomaly
    (let [actual (:cognitect.anomalies/category result)]
      {:pass? (= (name anomaly) (some-> actual name))
       :expected {:anomaly anomaly}
       :actual {:anomaly actual}})

    events
    (let [actual (mapv :event/type (:command-result/events result))]
      {:pass? (= (vec events) actual)
       :expected {:events (vec events)}
       :actual {:events actual}})

    :else {:pass? false :error "expectation has neither :expect/anomaly nor :expect/events"}))

(defn run-test-case
  "Execute one declarative test-case props map. Returns
   {:test dev-id :pass? bool :checks [...]}."
  [{:keys [:atlas/dev-id :test-case/fixture :test-case/expectations]}]
  (let [tenant-id (random-uuid)
        event-store (es/start {:conn {:type :in-memory}})]
    (try
      (let [{:keys [given-events command]} fixture
            context {:event-store event-store
                     :cache (mem-kv/->mem-kv)
                     :tenant-id tenant-id}]
        (when (seq given-events)
          (es/append event-store
                     {:tenant-id tenant-id
                      :events (mapv ->event given-events)}))
        (let [result (cp/process-command
                      (assoc context
                             :command (merge {:command/id (random-uuid)
                                              :command/timestamp (gtime/now)}
                                             command)))
              checks (mapv #(check-expectation % result) expectations)]
          {:test dev-id
           :pass? (every? :pass? checks)
           :checks checks}))
      (finally (es/stop event-store)))))

(defn run-all!
  "Run every registered :atlas/test-case. Prints a report, returns results."
  []
  (let [cases (->> @registry/registry
                   (keep (fn [[cid props]]
                           ;; actual test-cases, not the ontology/type meta
                           ;; entities whose compound-ids also carry the aspect
                           (when (and (contains? cid :atlas/test-case)
                                      (:test-case/fixture props))
                             props)))
                   (sort-by :atlas/dev-id))
        results (mapv run-test-case cases)]
    (doseq [{:keys [test pass? checks]} results]
      (println (format "  %s %s" (if pass? "✓" "✗") test))
      (when-not pass?
        (doseq [c (remove :pass? checks)]
          (println "     expected" (:expected c) "— actual" (:actual c)))))
    (println (format "  %d/%d passed"
                     (count (filter :pass? results)) (count results)))
    results))
