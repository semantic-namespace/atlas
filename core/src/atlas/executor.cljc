(ns atlas.executor
  "DEPRECATED: This namespace has been moved to atlas.ontology.execution-function.executor

   Migration:
     ;; Old code:
     (require '[atlas.executor :as executor])
     (executor/execute :fn/my-function impl-fn ctx)

     ;; New code:
     (require '[atlas.ontology.execution-function :as ef])
     (ef/load!)  ; Must load the ontology first!

     (require '[atlas.ontology.execution-function.executor :as executor])
     (executor/execute :fn/my-function impl-fn ctx)

   This namespace re-exports functions from the new location for backward
   compatibility, but will be removed in a future version."
  {:deprecated "0.2.0"}
  (:require [atlas.ontology.execution-function.executor :as new-executor]))

;; Re-export for backward compatibility
(def ^{:deprecated "0.2.0"
       :doc "DEPRECATED: Use atlas.ontology.execution-function.executor/*timeout-ms*"}
  ^:dynamic *timeout-ms* new-executor/*timeout-ms*)

(def ^{:deprecated "0.2.0"
       :doc "DEPRECATED: Use atlas.ontology.execution-function.executor/*trace*"}
  ^:dynamic *trace* new-executor/*trace*)

(def ^{:deprecated "0.2.0"
       :doc "DEPRECATED: Use atlas.ontology.execution-function.executor/wrap-timeout"}
  wrap-timeout new-executor/wrap-timeout)

(def ^{:deprecated "0.2.0"
       :doc "DEPRECATED: Use atlas.ontology.execution-function.executor/wrap-trace"}
  wrap-trace new-executor/wrap-trace)

(def ^{:deprecated "0.2.0"
       :doc "DEPRECATED: Use atlas.ontology.execution-function.executor/wrap-validate-context"}
  wrap-validate-context new-executor/wrap-validate-context)

(def ^{:deprecated "0.2.0"
       :doc "DEPRECATED: Use atlas.ontology.execution-function.executor/wrap-merge-response"}
  wrap-merge-response new-executor/wrap-merge-response)

(def ^{:deprecated "0.2.0"
       :doc "DEPRECATED: Use atlas.ontology.execution-function.executor/build-executor"}
  build-executor new-executor/build-executor)

(def ^{:deprecated "0.2.0"
       :doc "DEPRECATED: Use atlas.ontology.execution-function.executor/execute"}
  execute new-executor/execute)

(def ^{:deprecated "0.2.0"
       :doc "DEPRECATED: Use atlas.ontology.execution-function.executor/build-pipeline"}
  build-pipeline new-executor/build-pipeline)
