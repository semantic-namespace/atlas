(ns grain-demo.demo
  "End-to-end demo: atlas defines, grain executes.

   1. Register the counter app as atlas entities   (grain-demo.counter)
   2. Run atlas invariants                          (the report grain can't produce)
   3. Materialize into grain's registries           (atlas.adapter.grain.materialize)
   4. Verify grain's catalog matches atlas          (closed loop)
   5. Run it live: in-memory event store, commands, projections, and the
      async processor chain (increment → processor → average-calculated).
   6. Verify behavior: declarative test-cases (one per declared
      :grain/outcomes verdict) executed by the generic runner.

   Run:  cd examples/grain && clojure -M:demo"
  (:require
   [grain-demo.counter :as counter]
   [grain-demo.counter-tests :as counter-tests]
   [grain-demo.test-runner :as test-runner]
   [atlas.adapter.grain :as adapter]
   [atlas.invariant :as inv]
   [ai.obney.grain.command-processor-v2.interface :as cp]
   [ai.obney.grain.query-processor.interface :as qp]
   [ai.obney.grain.event-store-v3.interface :as es]
   [grain-demo.mem-kv :as mem-kv]
   [ai.obney.grain.todo-processor-v2.interface :as tp]
   [ai.obney.grain.time.interface :as time]
   [clojure.pprint :as pp])
  (:gen-class))

(def tenant-id #uuid "11111111-1111-1111-1111-111111111111")

(defn- cmd [context command-map]
  (cp/process-command
   (assoc context :command (merge {:command/id (random-uuid)
                                   :command/timestamp (time/now)}
                                  command-map))))

(defn- qry [context query-map]
  (qp/process-query
   (assoc context :query (merge {:query/id (random-uuid)
                                 :query/timestamp (time/now)}
                                query-map))))

(defn run-demo!
  []
  (println "════ 1. ATLAS DEFINES ═══════════════════════════════════")
  (counter/init-registry!)
  (println "Counter app registered as atlas entities.\n")

  (println "════ 2. ATLAS REASONS (invariants grain cannot express) ═")
  (inv/report)

  (println "\n════ 3. MATERIALIZE INTO GRAIN ══════════════════════════")
  (pp/pprint (adapter/materialize!))

  (println "\n════ 4. VERIFY (grain catalog vs atlas declarations) ════")
  (let [{:keys [in-sync? kinds]} (adapter/verify!)]
    (doseq [[kind {:keys [declared match?]}] kinds]
      (println (format "  %-15s %s %s" (name kind) (if match? "✓" "✗") (vec (sort declared))))
      (when (seq (:unmanaged (get kinds kind)))
        (println (format "  %-15s   unmanaged (framework): %s" ""
                         (vec (sort (:unmanaged (get kinds kind)))))))
      (when-not match?
        (println (format "  %-15s   MISSING from grain: %s" ""
                         (vec (sort (:missing (get kinds kind))))))))
    (println (if in-sync? "  → registries in sync" "  → DRIFT DETECTED")))

  (println "\n════ 5. GRAIN EXECUTES ══════════════════════════════════")
  (let [event-store (es/start {:conn {:type :in-memory}})
        cache (mem-kv/->mem-kv)
        context {:event-store event-store :cache cache :tenant-id tenant-id}
        poller (tp/start-tenant-poller {:event-store event-store
                                        :tenant-ids #{tenant-id}
                                        :context {:cache cache}
                                        :poll-interval-ms 100})]
    (try
      (let [created (cmd context {:command/name :example/create-counter
                                  :name "Counter A"})
            counter-id (get-in created [:command/result :counter-id])]
        (println "create-counter →" (:command/result created))
        (cmd context {:command/name :example/increment-counter :counter-id counter-id})
        (cmd context {:command/name :example/increment-counter :counter-id counter-id})
        (cmd context {:command/name :example/decrement-counter :counter-id counter-id})

        ;; Give the todo processor a moment to react (increment/decrement →
        ;; processor → calculate-average command → average-calculated event).
        (Thread/sleep 1500)

        (println "query counters →")
        (pp/pprint (:query/result (qry context {:query/name :example/counters})))

        (println "event stream (the async chain's tail should be :example/average-calculated):")
        (doseq [e (into [] (es/read event-store {:tenant-id tenant-id}))]
          (println "  " (:event/type e))))
      (finally
        (tp/stop-tenant-poller poller)
        (es/stop event-store))))

  (println "\n════ 6. VERIFY BEHAVIOR (declarative test-cases) ════════")
  (counter-tests/init-tests!)
  (test-runner/run-all!)

  (println "\n════ 7. AUDIT (observed vs declared — the store is truth) ")
  (doseq [{:keys [command undeclared-events unexercised-events observed]}
          (adapter/observed-vs-declared)]
    (println (format "  %s %s — %dx, emitted %s"
                     (if (or (seq undeclared-events) (seq unexercised-events)) "✗" "✓")
                     command (:invocations observed) (vec (sort (:events observed)))))
    (when (seq (:anomalies observed))
      (println "      anomalies observed:" (vec (sort (:anomalies observed)))
               "— declared alphabet:" (vec (sort (:declared-outcomes observed)))))
    (when (seq undeclared-events)
      (println "      UNDECLARED emissions:" (vec undeclared-events)))
    (when (seq unexercised-events)
      (println "      declared but never observed:" (vec unexercised-events))))

  (println "\nDone: same runtime as grain's own example — defined by atlas."))

(defn -main [& _]
  (run-demo!)
  (shutdown-agents)
  (System/exit 0))
