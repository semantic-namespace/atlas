(ns atlas.invariant.datalog
  "DATALOG-BASED INVARIANT CHECKING"
  (:require [atlas.datalog :as graph.datalog]))

(def ^:private invariant-messages
  {:invariant/no-missing-producers "Consumed keys must have a producer or be endpoint/external input."
   :invariant/no-orphan-outputs "Produced keys must be consumed or be terminal outputs."
   :invariant/deps-exist "Dependencies must reference existing entities."
   :invariant/no-circular-deps "Dependency graph must be acyclic."
   :invariant/all-fns-reachable "Functions should be reachable from endpoints."
   :invariant/protocol-exists "Protocols referenced by components must be defined."})

(defn check-invariant-datalog
  "Check an invariant using Datalog queries.
   This is an alternative implementation to the DSL-based checker."
  [db invariant-fn invariant-id level]
  (let [message (get invariant-messages invariant-id)
        violations (->> (invariant-fn db)
                        (map (fn [violation]
                               (cond-> (assoc violation
                                              :invariant invariant-id
                                              :level level
                                              :severity level)
                                 message (assoc :message message))))
                        vec)]
    (when (seq violations)
      {:invariant invariant-id
       :level level
       :severity level
       :message message
       :violations violations})))

(defn datalog-invariants
  "Standard invariants implemented using Datalog queries."
  [db]
  (remove nil?
          [(check-invariant-datalog
            db
            (fn [db] (map (fn [[ctx consumer]] {:entity consumer :missing-key ctx})
                          (graph.datalog/query-missing-producers db)))
            :invariant/no-missing-producers
            :error)

           (check-invariant-datalog
            db
            (fn [db] (map (fn [[resp producer]] {:entity producer :orphan-key resp})
                          (graph.datalog/query-orphan-outputs db)))
            :invariant/no-orphan-outputs
            :warning)

           (check-invariant-datalog
            db
            (fn [db] (map (fn [[dev missing]] {:entity dev :missing-dep missing})
                          (graph.datalog/query-missing-dependencies db)))
            :invariant/deps-exist
            :error)

           (check-invariant-datalog
            db
            (fn [db] (when-let [cycle (graph.datalog/query-acyclic-deps db)]
                       [{:cycle cycle}]))
            :invariant/no-circular-deps
            :error)

           (check-invariant-datalog
            db
            (fn [db] (map (fn [fn-id] {:entity fn-id})
                          (graph.datalog/query-unreachable-functions db)))
            :invariant/all-fns-reachable
            :warning)

           ;; Protocol invariants
           (check-invariant-datalog
            db
            (fn [db] (map (fn [protocol-aspect] {:missing-protocol protocol-aspect})
                          (graph.datalog/query-undefined-protocols db)))
            :invariant/protocol-exists
            :error)]))

(defn check-all-datalog
  "Check all invariants using Datalog backend."
  []
  (let [db (graph.datalog/create-db)
        violations (vec (datalog-invariants db))
        flat-violations (vec (mapcat :violations violations))
        errors (filterv #(= :error (:level %)) violations)
        warnings (filterv #(= :warning (:level %)) violations)
        errors-flat (filterv #(= :error (:level %)) flat-violations)
        warnings-flat (filterv #(= :warning (:level %)) flat-violations)]
    {:violations violations
     :errors errors
     :warnings warnings
     :violations-flat flat-violations
     :errors-flat errors-flat
     :warnings-flat warnings-flat
     :valid? (empty? errors)}))
