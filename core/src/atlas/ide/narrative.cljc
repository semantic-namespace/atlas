(ns atlas.ide.narrative
  "Per-entity-type narrative context builders.

   Each builder takes a dev-id (keyword) and returns a flat string map
   consumable by atlas-render-specs :narrative templates via ${variable}
   substitution.

   Graph traversal is done via atlas.ide.trace (data-key cache + reverse-deps).
   No direct datascript calls — everything goes through existing IDE functions.

   Usage from Emacs:
     (require '[atlas.ide.narrative])
     (atlas.ide.narrative/narrative-contexts [:fn.ide/entity-info :fn.ide/data-flow])"
  (:require [atlas.registry :as registry]
            [atlas.registry.lookup :as lookup]
            [atlas.ontology :as ontology]
            [atlas.ide.trace :as trace]
            [clojure.string :as str]))

;; =============================================================================
;; Humanization helpers
;; =============================================================================

(defn- kw->str
  "Keyword to fully-qualified string without leading colon."
  [kw]
  (if (keyword? kw)
    (if (namespace kw)
      (str (namespace kw) "/" (name kw))
      (name kw))
    (str kw)))

(defn- humanize-list
  "Oxford-comma prose from a sequence of strings."
  [items]
  (let [v (vec items)]
    (case (count v)
      0 ""
      1 (first v)
      2 (str (first v) " and " (second v))
      (str (str/join ", " (butlast v)) ", and " (last v)))))

(defn- count-word
  "Spell out number for 0–12, then use digit. Returns 'N singular/plural'."
  [n singular plural]
  (let [words ["zero" "one" "two" "three" "four" "five"
               "six"  "seven" "eight" "nine" "ten"
               "eleven" "twelve"]]
    (str (if (< n (count words)) (get words n) (str n))
         " "
         (if (= n 1) singular plural))))

(defn- first-aspect-of-ns
  "Name of first aspect whose namespace is ns-str, or nil."
  [aspects ns-str]
  (some-> (first (filter #(= ns-str (namespace %)) aspects)) name))

;; =============================================================================
;; Graph-derived prose fragments
;; =============================================================================

(defn- producers-prose
  "Prose clause: who produces each context key."
  [ctx-keys]
  (if (empty? ctx-keys)
    "it requires no context"
    (let [producers (->> ctx-keys
                         (mapcat #(trace/producers-of %))
                         distinct sort (map kw->str))]
      (case (count producers)
        0 "no producer is registered for these keys"
        1 (str "produced by " (first producers))
        (str "produced by " (humanize-list producers))))))

(defn- consumers-prose
  "Prose clause: who consumes each response key."
  [resp-keys]
  (if (empty? resp-keys)
    "it produces no output"
    (let [consumers (->> resp-keys
                         (mapcat #(trace/consumers-of %))
                         distinct sort (map kw->str))]
      (case (count consumers)
        0 "with no known consumer in the registry"
        1 (str "consumed by " (first consumers))
        (str "consumed by " (humanize-list consumers))))))

(defn- deps-prose
  "Full sentence about direct dependencies, or nil when empty."
  [dep-ids]
  (when (seq dep-ids)
    (str "It depends on "
         (humanize-list (map kw->str (sort dep-ids))) ".")))

(defn- dependents-prose
  "Prose clause: who depends on this entity."
  [dev-id]
  (let [deps (trace/dependents-of dev-id)]
    (case (count deps)
      0 "nothing in the registry depends on it directly"
      1 (str "`" (kw->str (first deps)) "` depends on it directly")
      (str (count-word (count deps) "entity depends" "entities depend")
           " on it directly"))))

;; =============================================================================
;; Multimethod
;; =============================================================================

(defmulti narrative-context
  "Build narrative variable map for dev-id (keyword).
   Returns a flat map of string values for ${variable} substitution.
   Dispatches on the :atlas/* keyword in the compound identity directly,
   so it works even when entity types are not registered via
   ont/register-entity-types!."
  (fn [dev-id]
    (when-let [cid (lookup/identity-for dev-id)]
      (first (filter #(= "atlas" (namespace %)) cid)))))

;; =============================================================================
;; :atlas/execution-function
;; =============================================================================

(defmethod narrative-context :atlas/execution-function [dev-id]
  (let [aspects   (registry/declared-aspects dev-id)
        ctx-keys  (ontology/context-for dev-id)
        resp-keys (ontology/response-for dev-id)
        dep-ids   (ontology/deps-for dev-id)
        domain    (first-aspect-of-ns aspects "domain")
        intent    (first-aspect-of-ns aspects "intent")
        operation (first-aspect-of-ns aspects "operation")]
    (cond-> {:dev-id                   (kw->str dev-id)
             :entity-type              ":atlas/execution-function"
             :domain                   (or domain "")
             :intent                   (or intent "")
             :operation                (or operation "")
             :context-count            (count-word (count ctx-keys) "key" "keys")
             :context-list             (if (seq ctx-keys)
                                         (str/join " · " (map kw->str ctx-keys))
                                         "nothing")
             :context-producers-prose  (producers-prose ctx-keys)
             :response-count           (count-word (count resp-keys) "key" "keys")
             :response-list            (if (seq resp-keys)
                                         (str/join " · " (map kw->str resp-keys))
                                         "nothing")
             :consumers-prose          (consumers-prose resp-keys)}
      (seq dep-ids) (assoc :deps-prose (deps-prose dep-ids)))))

;; =============================================================================
;; :atlas/interface-endpoint
;; =============================================================================

(defmethod narrative-context :atlas/interface-endpoint [dev-id]
  (let [aspects   (registry/declared-aspects dev-id)
        ctx-keys  (ontology/context-for dev-id)
        resp-keys (ontology/response-for dev-id)
        dep-ids   (ontology/deps-for dev-id)
        domain    (first-aspect-of-ns aspects "domain")
        operation (first-aspect-of-ns aspects "operation")]
    (cond-> {:dev-id                   (kw->str dev-id)
             :entity-type              ":atlas/interface-endpoint"
             :domain                   (or domain "")
             :operation                (or operation "")
             :context-count            (count-word (count ctx-keys) "key" "keys")
             :context-list             (if (seq ctx-keys)
                                         (str/join " · " (map kw->str ctx-keys))
                                         "nothing")
             :context-producers-prose  (producers-prose ctx-keys)
             :response-count           (count-word (count resp-keys) "key" "keys")
             :response-list            (if (seq resp-keys)
                                         (str/join " · " (map kw->str resp-keys))
                                         "nothing")
             :consumers-prose          (consumers-prose resp-keys)
             :dependents-prose         (dependents-prose dev-id)}
      (seq dep-ids) (assoc :deps-prose (deps-prose dep-ids)))))

;; =============================================================================
;; :atlas/structure-component
;; =============================================================================

(defmethod narrative-context :atlas/structure-component [dev-id]
  (let [aspects  (registry/declared-aspects dev-id)
        dep-ids  (ontology/deps-for dev-id)
        domain   (first-aspect-of-ns aspects "domain")]
    (cond-> {:dev-id           (kw->str dev-id)
             :entity-type      ":atlas/structure-component"
             :domain           (or domain "")
             :deps-count       (count-word (count dep-ids) "dependency" "dependencies")
             :dependents-prose (dependents-prose dev-id)}
      (seq dep-ids) (assoc :deps-prose (deps-prose dep-ids)))))

;; =============================================================================
;; :atlas/llm-prompt
;; =============================================================================

(defmethod narrative-context :atlas/llm-prompt [dev-id]
  (let [aspects   (registry/declared-aspects dev-id)
        props     (lookup/props-for dev-id)
        mcp-deps  (vec (:llm-prompt/mcp-deps props))
        domain    (first-aspect-of-ns aspects "domain")
        intent    (first-aspect-of-ns aspects "intent")
        operation (first-aspect-of-ns aspects "operation")]
    {:dev-id          (kw->str dev-id)
     :entity-type     ":atlas/llm-prompt"
     :domain          (or domain "")
     :intent          (or intent "")
     :operation       (or operation "")
     :summary         (or (:llm-prompt/summary props) "")
     :file            (or (:llm-prompt/file props) "")
     :mcp-deps-count  (count-word (count mcp-deps) "tool" "tools")
     :mcp-deps-list   (str/join " · " (map kw->str mcp-deps))}))

;; =============================================================================
;; :default — minimal fallback for unknown entity types
;; =============================================================================

(defmethod narrative-context :default [dev-id]
  (let [aspects (registry/declared-aspects dev-id)
        cid     (lookup/identity-for dev-id)
        type    (when cid (registry/entity-type cid))]
    {:dev-id      (kw->str dev-id)
     :entity-type (str (or type ""))
     :aspects     (str/join " · " (map kw->str (sort aspects)))}))

;; =============================================================================
;; Batch API — matches shape of atlas.ide/entities-info
;; =============================================================================

(defn- coerce-kw [x]
  (cond
    (keyword? x) x
    (string? x)  (keyword (cond-> x (= \: (first x)) (subs 1)))
    :else         x))

(defn narrative-contexts
  "Build narrative context maps for a sequence of dev-ids.
   Returns {dev-id-kw -> context-map}, same shape as entities-info."
  [dev-ids]
  (into {}
        (map (fn [id]
               (let [kw (coerce-kw id)]
                 [kw (narrative-context kw)])))
        dev-ids))
