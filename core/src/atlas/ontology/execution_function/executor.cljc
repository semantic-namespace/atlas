(ns atlas.ontology.execution-function.executor
  "Execute functions with semantic-driven wrappers.

   This namespace provides execution utilities for :atlas/execution-function
   entities. It requires the execution-function ontology to be loaded.

   Usage:
     (require '[atlas.ontology.execution-function :as ef])
     (ef/load!)

     (require '[atlas.ontology.execution-function.executor :as executor])
     (executor/execute :fn/my-function impl-fn {:input/key \"value\"})"
  (:require [atlas.registry.lookup :as entity]
            [atlas.ontology :as o]
            [atlas.registry.graph :as graph]
            [atlas.cljc.platform :as platform]
            [clojure.set :as set]))

(def ^:dynamic *timeout-ms*
  "Timeout in milliseconds for external integration calls.
   Default: 5000ms"
  5000)

(def ^:dynamic *trace*
  "Enable execution tracing when true.
   Prints input/output keys for debugging."
  false)

;; =============================================================================
;; WRAPPERS
;; =============================================================================

(defn wrap-timeout
  "Wrap function with timeout for external integrations.

   Returns a function that will timeout after timeout-ms milliseconds,
   returning {:error :timeout :timeout-ms ...} if exceeded."
  [f timeout-ms]
  (fn [ctx]
    (let [result (platform/call-with-timeout timeout-ms #(f ctx))]
      (if (platform/timeout? result)
        {:error :timeout :timeout-ms timeout-ms}
        result))))

(defn wrap-trace
  "Wrap function with tracing for debugging.

   When *trace* is true, prints input and output keys to stdout."
  [f dev-id]
  (fn [ctx]
    (when *trace*
      (println "[TRACE]" dev-id "input:" (keys ctx)))
    (let [result (f ctx)]
      (when *trace*
        (println "[TRACE]" dev-id "output:" (keys result)))
      result)))

(defn wrap-validate-context
  "Wrap function with context validation.

   Returns error if required context keys are missing:
   {:error :missing-context :missing #{...} :dev-id ...}"
  [f dev-id]
  (fn [ctx]
    (let [required (set (o/context-for dev-id))
          provided (set (keys ctx))
          missing (set/difference required provided)]
      (if (seq missing)
        {:error :missing-context :missing missing :dev-id dev-id}
        (f ctx)))))

(defn wrap-merge-response
  "Wrap function to merge response into context.

   If function returns an error, passes it through unchanged.
   Otherwise merges the response map into the input context."
  [f]
  (fn [ctx]
    (let [result (f ctx)]
      (if (:error result)
        result
        (merge ctx result)))))

;; =============================================================================
;; EXECUTOR BUILDING
;; =============================================================================

(defn build-executor
  "Build executor for a function, applying semantic wrappers.

   Applies wrappers based on entity aspects:
   - Always: wrap-merge-response, wrap-validate-context
   - If :integration/external: wrap-timeout
   - If *trace* is true: wrap-trace

   Returns a function: (fn [ctx] -> ctx')"
  [dev-id impl-fn]
  (let [id (entity/identity-for dev-id)]
    (cond-> impl-fn
      true (wrap-merge-response)
      true (wrap-validate-context dev-id)
      (contains? id :integration/external) (wrap-timeout *timeout-ms*)
      *trace* (wrap-trace dev-id))))

(defn execute
  "Execute a function by dev-id with given context.

   Builds an executor with semantic wrappers and executes it.

   Returns the updated context map, or an error map if validation fails."
  [dev-id impl-fn ctx]
  ((build-executor dev-id impl-fn) ctx))

;; =============================================================================
;; PIPELINE BUILDING
;; =============================================================================

(defn build-pipeline
  "Build execution pipeline from endpoint through its deps, ordered by data flow.

   Takes an endpoint dev-id and a map of dev-id -> impl-fn.
   Returns a function that executes all functions in topological order,
   passing context through each one.

   Short-circuits on error: if any function returns {:error ...},
   stops execution and returns the error."
  [endpoint-id impl-map]
  (let [all-fns (conj (o/deps-for endpoint-id) endpoint-id)
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
