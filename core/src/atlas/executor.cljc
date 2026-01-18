(ns atlas.executor
  "Execute functions with semantic-driven wrappers."
  (:require [atlas.entity :as rt]
            [atlas.graph :as graph]
            [atlas.cljc.platform :as platform]
            [clojure.set :as set]))

(def ^:dynamic *timeout-ms* 5000)
(def ^:dynamic *trace* false)

(defn wrap-timeout [f timeout-ms]
  (fn [ctx]
    (let [result (platform/call-with-timeout timeout-ms #(f ctx))]
      (if (platform/timeout? result)
        {:error :timeout :timeout-ms timeout-ms}
        result))))

(defn wrap-trace [f dev-id]
  (fn [ctx]
    (when *trace*
      (println "[TRACE]" dev-id "input:" (keys ctx)))
    (let [result (f ctx)]
      (when *trace*
        (println "[TRACE]" dev-id "output:" (keys result)))
      result)))

(defn wrap-validate-context [f dev-id]
  (fn [ctx]
    (let [required (set (rt/context-for dev-id))
          provided (set (keys ctx))
          missing (set/difference required provided)]
      (if (seq missing)
        {:error :missing-context :missing missing :dev-id dev-id}
        (f ctx)))))

(defn wrap-merge-response [f]
  (fn [ctx]
    (let [result (f ctx)]
      (if (:error result)
        result
        (merge ctx result)))))

(defn build-executor
  "Build executor for a function, applying semantic wrappers."
  [dev-id impl-fn]
  (let [id (rt/identity-for dev-id)]
    (cond-> impl-fn
      true (wrap-merge-response)
      true (wrap-validate-context dev-id)
      (contains? id :integration/external) (wrap-timeout *timeout-ms*) ;; instead of :integration/external :atlas/integration-external or :atlas/external ?
      *trace* (wrap-trace dev-id))))

(defn execute
  "Execute a function by dev-id with given context."
  [dev-id impl-fn ctx]
  ((build-executor dev-id impl-fn) ctx))

(defn build-pipeline
  "Build execution pipeline from endpoint through its deps, ordered by data flow."
  [endpoint-id impl-map]
  (let [all-fns (conj (rt/deps-for endpoint-id) endpoint-id)
        order (graph/topo-sort-by-data all-fns)]
    (fn [initial-ctx]
      (reduce (fn [ctx dev-id]
                (if (:error ctx)
                  (reduced ctx)
                  (if-let [impl (get impl-map dev-id)]
                    (execute dev-id impl ctx)
                    ctx)))
              initial-ctx
              order))))
