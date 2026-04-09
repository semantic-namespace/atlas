(ns atlas.ontology.execution-function.history.llm-ide
  "LLM-IDE tool surface over the execution-history data layer.

   Wraps atlas.ontology.execution-function.history with five MCP-discoverable
   tools registered under :domain/llm-ide / :intent/diagnose:

     :atlas.llm-ide/exec-history-summary
     :atlas.llm-ide/exec-history-page
     :atlas.llm-ide/exec-history-by-dev-id
     :atlas.llm-ide/exec-history-errors
     :atlas.llm-ide/exec-history-clear

   This namespace lives in dev-tools because the tool surface is environment-
   specific. A future cloud module will ship a sibling namespace with a
   different (read-only, sink-backed) tool set against the same data layer.

   Tools self-register on namespace load — require this namespace from your
   dev REPL/system startup to make them discoverable via the registry."
  (:require [atlas.ontology.execution-function.history :as h]
            [atlas.registry :as registry]))

;; ============================================================================
;; HELPERS — coerce string-encoded keyword values from MCP/JSON
;; ============================================================================

(defn- ensure-keyword
  [x]
  (cond
    (keyword? x) x
    (string? x)  (keyword (cond-> x (= \: (first x)) (subs 1)))
    :else        x))

(defn- normalize-args
  "Coerce string-encoded keyword values from MCP/JSON into keywords."
  [args]
  (cond-> args
    (contains? args :entity/dev-id) (update :entity/dev-id ensure-keyword)
    (contains? args :query/status)  (update :query/status  ensure-keyword)
    (contains? args :query/aspect)  (update :query/aspect  ensure-keyword)
    (contains? args :query/order)   (update :query/order   ensure-keyword)))

;; ============================================================================
;; TOOL REGISTRATIONS
;; ============================================================================
;;
;; Conventions match atlas.llm-ide:
;;   dev-id namespace : :atlas.llm-ide/exec-history-*
;;   aspects          : #{:tier/tooling :domain/llm-ide :intent/diagnose
;;                        :tool/<name>}
;;   context/response : qualified keywords spec'd in the data-layer ns

(registry/register!
 :atlas.llm-ide/exec-history-summary
 :atlas/execution-function
 #{:tier/tooling :domain/llm-ide :intent/diagnose :tool/exec-history-summary}
 {:execution-function/context  [:entity/dev-id :query/status
                                :query/aspect :query/since]
  :execution-function/response [:exec/total :exec/total-buffered
                                :exec/evicted-before
                                :exec/by-dev-id :exec/by-status
                                :exec/by-domain :exec/time-range]
  :execution-function/deps     #{}
  :atlas/docs "Aggregate stats over the local exec history buffer: counts/errors/avg-ms/p95-ms by dev-id, counts by status, counts by domain. Small bounded payload — start here to find what's interesting, then drill in via :atlas.llm-ide/exec-history-page. Filters: :entity/dev-id, :query/status, :query/aspect, :query/since."
  :atlas/impl (fn [args] (h/summary (normalize-args args)))})

(registry/register!
 :atlas.llm-ide/exec-history-page
 :atlas/execution-function
 #{:tier/tooling :domain/llm-ide :intent/diagnose :tool/exec-history-page}
 {:execution-function/context  [:query/cursor :query/limit
                                :entity/dev-id :query/status
                                :query/aspect :query/since :query/order]
  :execution-function/response [:exec/entries :exec/returned
                                :exec/next-cursor :exec/has-more?
                                :exec/total-buffered :exec/evicted-before]
  :execution-function/deps     #{}
  :atlas/docs "Paginated raw execution entries from the local history ring buffer. Returns up to :query/limit entries (default 50, max 200) starting at :query/cursor. Pass :exec/next-cursor from the response back into the next call. If :exec/evicted-before > your :query/cursor, the buffer wrapped and you missed entries — restart from :exec/evicted-before. Same filters as :atlas.llm-ide/exec-history-summary."
  :atlas/impl (fn [args] (h/query (normalize-args args)))})

(registry/register!
 :atlas.llm-ide/exec-history-by-dev-id
 :atlas/execution-function
 #{:tier/tooling :domain/llm-ide :intent/diagnose :tool/exec-history-by-dev-id}
 {:execution-function/context  [:entity/dev-id :query/cursor
                                :query/limit :query/order]
  :execution-function/response [:exec/entries :exec/returned
                                :exec/next-cursor :exec/has-more?
                                :exec/total-buffered :exec/evicted-before]
  :execution-function/deps     #{}
  :atlas/docs "Paginated history entries for a single execution-function dev-id. Convenience wrapper over :atlas.llm-ide/exec-history-page with :entity/dev-id required."
  :atlas/impl (fn [args] (h/query (normalize-args args)))})

(registry/register!
 :atlas.llm-ide/exec-history-errors
 :atlas/execution-function
 #{:tier/tooling :domain/llm-ide :intent/diagnose :tool/exec-history-errors}
 {:execution-function/context  [:query/cursor :query/limit
                                :entity/dev-id :query/aspect
                                :query/since :query/order]
  :execution-function/response [:exec/entries :exec/returned
                                :exec/next-cursor :exec/has-more?
                                :exec/total-buffered :exec/evicted-before]
  :execution-function/deps     #{}
  :atlas/docs "Paginated history entries restricted to :exec/status :error. Same pagination contract as :atlas.llm-ide/exec-history-page. Use to triage what's been failing in this dev session."
  :atlas/impl (fn [args]
                (h/query (assoc (normalize-args args) :query/status :error)))})

(registry/register!
 :atlas.llm-ide/exec-history-clear
 :atlas/execution-function
 #{:tier/tooling :domain/llm-ide :intent/diagnose :tool/exec-history-clear}
 {:execution-function/context  []
  :execution-function/response [:exec/cleared?]
  :execution-function/deps     #{}
  :atlas/docs "Drop all entries from the local history ring buffer. Dev-only — irreversible. Returns {:exec/cleared? true}."
  :atlas/impl (fn [_] (h/clear!) {:exec/cleared? true})})
