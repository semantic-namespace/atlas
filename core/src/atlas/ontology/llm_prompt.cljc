(ns atlas.ontology.llm-prompt
  "LLM prompt ontology — registers :atlas/llm-prompt as a first-class entity type.

   A prompt is a conversational workflow definition: it declares what domain it
   operates in, what intent it has, which MCP tools it depends on, and (for write
   prompts) what entity type it produces.

   Compound-id = semantic identity (domain, intent, role).
   Props       = immutable definition (file, mcp-deps, produces).

   Usage:
     (require '[atlas.ontology.llm-prompt])"
  (:require [atlas.registry :as registry]
            [atlas.registry.lookup :as entity]
            [atlas.ontology.type-ref :as type-ref]
            [clojure.spec.alpha :as s]))

;; ---------------------------------------------------------------------------
;; Specs
;; ---------------------------------------------------------------------------

(s/def :llm-prompt/summary string?)
(s/def :llm-prompt/file string?)
(s/def :llm-prompt/mcp-deps (s/coll-of qualified-keyword?))
(s/def :llm-prompt/produces qualified-keyword?)

;; ---------------------------------------------------------------------------
;; :atlas/llm-prompt
;; ---------------------------------------------------------------------------

(registry/register!
 :atlas/llm-prompt
 :atlas/ontology
 #{:atlas/llm-prompt}
 {:ontology/for  :atlas/llm-prompt
  :ontology/keys [:llm-prompt/summary
                  :llm-prompt/file
                  :llm-prompt/mcp-deps
                  :llm-prompt/produces]})

;; Type-ref: prompt → mcp tools (many)
(registry/register!
 :type-ref/llm-prompt-mcp-deps
 :atlas/type-ref
 #{:meta/ref-llm-prompt-mcp-deps}
 {:type-ref/source       :atlas/llm-prompt
  :type-ref/property     :llm-prompt/mcp-deps
  :type-ref/datalog-verb :llm-prompt/uses
  :type-ref/cardinality  :db.cardinality/many})

;; Type-ref: prompt → produced entity type (one)
(registry/register!
 :type-ref/llm-prompt-produces
 :atlas/type-ref
 #{:meta/ref-llm-prompt-produces}
 {:type-ref/source       :atlas/llm-prompt
  :type-ref/property     :llm-prompt/produces
  :type-ref/datalog-verb :llm-prompt/produces
  :type-ref/cardinality  :db.cardinality/one})

;; Datalog extractor
(registry/register!
 :datalog-extractor/llm-prompt
 :atlas/datalog-extractor
 #{:meta/llm-prompt-extractor}
 {:datalog-extractor/fn
  (fn [compound-id props]
    (when (contains? compound-id :atlas/llm-prompt)
      (type-ref/extract-reference-facts :atlas/llm-prompt compound-id props)))
  :datalog-extractor/schema
  {:llm-prompt/uses     {:db/cardinality :db.cardinality/many}
   :llm-prompt/produces {:db/cardinality :db.cardinality/one}}})

;; ---------------------------------------------------------------------------
;; Invariants
;; ---------------------------------------------------------------------------

(registry/register!
 :invariant/llm-prompt-intent-required
 :atlas/invariant
 #{:meta/llm-prompt-intent-check}
 {:invariant/fn
  (fn []
    (let [violations (for [id (entity/all-with-aspect :atlas/llm-prompt)
                           :when (empty? (filter #(= "intent" (namespace %)) id))]
                       {:entity id})]
      (when (seq violations)
        {:invariant :llm-prompt-intent-required
         :violation :prompt-missing-intent
         :details   (vec violations)
         :severity  :error
         :message   "Every llm-prompt must have at least one :intent/x aspect — without it the prompt is undiscoverable by purpose"})))})

(registry/register!
 :invariant/llm-prompt-write-requires-produces
 :atlas/invariant
 #{:meta/llm-prompt-write-produces-check}
 {:invariant/fn
  (fn []
    (let [violations (for [id (entity/all-with-aspect :atlas/llm-prompt)
                           :when (contains? id :intent/write)
                           :when (nil? (:llm-prompt/produces (entity/props-for id)))]
                       {:entity id})]
      (when (seq violations)
        {:invariant :llm-prompt-write-requires-produces
         :violation :write-prompt-missing-produces
         :details   (vec violations)
         :severity  :error
         :message   "Write prompts must declare :llm-prompt/produces — a typed function must declare its output"})))})

(registry/register!
 :invariant/llm-prompt-deps-exist
 :atlas/invariant
 #{:meta/llm-prompt-deps-check}
 {:invariant/fn
  (fn []
    (let [violations (for [id  (entity/all-with-aspect :atlas/llm-prompt)
                           dep (:llm-prompt/mcp-deps (entity/props-for id))
                           :when (nil? (entity/identity-for dep))]
                       {:prompt id :missing-dep dep})]
      (when (seq violations)
        {:invariant :llm-prompt-deps-exist
         :violation :prompt-dep-not-found
         :details   (vec violations)
         :severity  :warning
         :message   "Prompt mcp-deps reference tools not found in the registry"})))})
