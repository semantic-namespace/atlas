(ns atlas.adapter.overarch
  "EXPERIMENTAL — Export an Atlas registry into an Overarch model + views (EDN).

   Atlas binds identity to *semantics* (a compound identity — a set of aspects)
   and is strong at reasoning/lifecycle (invariants, data-flow, versioning) but
   has no diagram generation. Overarch (github.com/soulspace-org/overarch) is the
   mirror image: a data-driven C4/UML model with rich PlantUML/GraphViz/Markdown
   generators, but no invariant engine or lifecycle.

   Both are Clojure + EDN + qualified-keyword graphs, so the bridge is a pure
   data projection (this namespace requires nothing beyond clojure core):

     Atlas                                 Overarch
     -------------------------------       ------------------------------
     compound-id (the map KEY)          →  :tags      (semantic aspects)
     :atlas/dev-id                      →  :id        (stable identity)
     :atlas/type                        →  :el        (element category)
     type-refs, verb :entity/depends    →  :rel       (dependency edges)
     type-refs, verbs produces/consumes →  :dataflow  (producer → consumer)

   The registry is the Atlas map shape `{compound-id-set props-map}` — exactly
   what a registry snapshot (e.g. atlas-cloud `cloud-pull`) returns.

   Usage:
     (def reg (clojure.edn/read-string (slurp \"snapshot.edn\")))
     (emit! reg \"/path/to/overarch/models/orders\"
            {:select      (aspect-selector :domain/orders)
             :system-id   :myapp/orders
             :system-name \"MyApp — Orders\"
             :view-title  \"Orders subsystem and its dependencies\"})

   Then render with Overarch:
     java -jar overarch.jar -m models/orders -r plantuml   ; C4 .puml
     java -jar overarch.jar -m models/orders -r graphviz   ; concept .dot"
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.pprint :as pp]))

;; ---------------------------------------------------------------------------
;; Mapping: Atlas entity-type -> Overarch element category (:el)
;; ---------------------------------------------------------------------------

(def default-type->el
  "Maps built-in Atlas entity types to Overarch C4 element categories. Anything
   not listed falls back to :container so unknown/custom types still render.
   Extend per-call for custom entity types:
     {:type->el (assoc default-type->el :myapp/event :component)}"
  {:atlas/structure-component :container
   :atlas/execution-function  :component
   :atlas/interface-endpoint  :component
   :atlas/interface-protocol  :component
   :atlas/data-schema         :component})

;; ---------------------------------------------------------------------------
;; Small accessors over the Atlas entity value map
;; ---------------------------------------------------------------------------

(defn- dev-id  [props] (or (:atlas/dev-id props) (:entity/dev-id props)))
(defn- etype   [props] (:atlas/type props))

;; ---------------------------------------------------------------------------
;; Type-refs — the registry's own meta-model of what a relation is.
;;
;; A `:atlas/type-ref` entity declares: for entities of `:type-ref/source`, the
;; property `:type-ref/property` holds references that form `:type-ref/datalog-verb`
;; edges. We drive ALL edge extraction from these, rather than hardcoding
;; `execution-function/deps` — that is why endpoints (whose edges live under
;; `endpoint/deps`, `endpoint/input`, `mcp-tool/req-input-args`, …) were invisible
;; before.
;; ---------------------------------------------------------------------------

(def ^:private depends-verbs  #{:entity/depends})
(def ^:private consumes-verbs #{:entity/consumes})
(def ^:private produces-verbs #{:entity/produces})

(defn type-ref-index
  "Build `{source-type [{:property :verb :cardinality} …]}` from the type-ref
   entities in `registry`. Empty when a registry declares no type-refs."
  [registry]
  (reduce (fn [m e]
            (if (= :atlas/type-ref (:atlas/type e))
              (update m (:type-ref/source e) (fnil conj [])
                      {:property (:type-ref/property e)
                       :verb     (:type-ref/datalog-verb e)
                       :cardinality (:type-ref/cardinality e)})
              m))
          {} (vals registry)))

(defn- refs-for
  "All reference targets of `props` whose declaring verb is in `verbs`, per the
   type-ref meta-model `tri`. Handles both cardinality/one and /many properties."
  [tri verbs props]
  (for [tr    (get tri (etype props))
        :when (contains? verbs (:verb tr))
        :let  [v (get props (:property tr))]
        t     (if (coll? v) v (when v [v]))]
    t))

(defn- humanize
  "A qualified display name from a dev-id: keeps the namespace so entities that
   share a short name stay distinct — :fn.gmail-pending/logins -> \"fn.gmail-pending/logins\",
   not the ambiguous \"logins\". Atlas identity is namespaced; the label reflects that."
  [kw]
  (when kw (subs (str kw) 1)))          ; (str :ns/name) => \":ns/name\"; drop the colon

(defn- safe-id
  "Make a dev-id usable as an Overarch/GraphViz identifier by stripping chars
   the renderers don't escape (`! ? * ' + = < > & %`). Overarch already handles
   `- . /`, so those are preserved. The original id is kept on the node as
   `:atlas/dev-id`, so identity is recoverable and the change is display-only.
   Returns the keyword unchanged when it is already safe."
  [kw]
  (if (and (keyword? kw) (re-find #"[^a-zA-Z0-9.\-/]" (or (name kw) "")))
    (keyword (some-> (namespace kw) (str/replace #"[^a-zA-Z0-9.\-/]" ""))
             (str/replace (name kw) #"[^a-zA-Z0-9.\-/]" ""))
    kw))

(defn- tech-of
  "Derive an Overarch :tech string from :services/* and :tier/* aspects."
  [aspects]
  (let [svc  (->> aspects (filter #(= "services" (namespace %))) (map name) sort)
        tier (->> aspects (filter #(= "tier" (namespace %))) (map name) sort)]
    (some->> (concat svc tier) seq (str/join ", "))))

(defn- meta-aspect?
  "Drop framework-internal aspects from the tag set; keep domain semantics."
  [a]
  (contains? #{"atlas"} (namespace a)))

;; ---------------------------------------------------------------------------
;; Selection
;; ---------------------------------------------------------------------------

(defn aspect-selector
  "Returns a `(fn [compound-id props] -> boolean)` selecting entities whose
   compound identity contains `aspect`. Optionally restrict to `types`."
  ([aspect] (aspect-selector aspect nil))
  ([aspect types]
   (fn [compound-id props]
     (and (contains? compound-id aspect)
          (or (nil? types) (contains? types (etype props)))))))

(defn aspects-selector
  "Like `aspect-selector` but matches entities carrying ANY of `aspects` (a set) —
   e.g. span several domains in one view. Optionally restrict to `types`."
  ([aspects] (aspects-selector aspects nil))
  ([aspects types]
   (fn [compound-id props]
     (and (some compound-id aspects)
          (or (nil? types) (contains? types (etype props)))))))

;; ---------------------------------------------------------------------------
;; Node / relation construction
;; ---------------------------------------------------------------------------

(defn- ->node
  [compound-id props type->el {:keys [external?]}]
  (let [id      (dev-id props)
        oid     (safe-id id)
        aspects (remove meta-aspect? compound-id)
        el      (get type->el (etype props) :container)]
    (cond-> {:el   el
             :id   oid
             :name (humanize id)}
      (not= oid id)         (assoc :atlas/dev-id id)
      (seq aspects)         (assoc :tags (into (sorted-set) aspects))
      (tech-of compound-id) (assoc :tech (tech-of compound-id))
      external?             (assoc :external true))))

(defn- rel-id
  [from to kind]
  (keyword (str (namespace from) "." (name from))
           (str (name kind) "->" (namespace to) "." (name to))))

(defn- dep-relations
  "One :rel per `:entity/depends`-verb reference (per the type-ref meta-model)
   whose target resolves to a known node. Spans every dependency property the
   registry declares — execution-function/deps, endpoint/deps,
   endpoint/serialisation, structure-component/deps, cache/serialisation, …"
  [tri selected-props node-ids]
  (for [props selected-props
        d     (refs-for tri depends-verbs props)
        :let  [from (dev-id props)]
        :when (contains? node-ids d)]
    {:el   :rel
     :id   (rel-id (safe-id from) (safe-id d) :depends)
     :from (safe-id from)
     :to   (safe-id d)
     :name "depends on"}))

(defn- dataflow-relations
  "One :dataflow per (producer -> consumer) pair sharing a data key, using the
   type-ref meta-model to decide which properties produce (`:entity/produces`)
   and which consume (`:entity/consumes`). Producers are indexed over the whole
   registry so cross-domain flows resolve; both ends must be selected nodes."
  [tri all-props selected node-ids]
  (let [producers (reduce (fn [m props]
                            (reduce #(update %1 %2 (fnil conj #{}) (dev-id props))
                                    m (refs-for tri produces-verbs props)))
                          {} all-props)]
    (->> (for [props selected
               k     (refs-for tri consumes-verbs props)
               p     (get producers k)
               :let  [c (dev-id props)]
               :when (and (not= p c) (contains? node-ids p))]
           {:el :dataflow :id (rel-id (safe-id p) (safe-id c) :flow)
            :from (safe-id p) :to (safe-id c) :name (name k)})
         (distinct))))

;; ---------------------------------------------------------------------------
;; Model + views
;; ---------------------------------------------------------------------------

(defn registry->overarch
  "Project an Atlas `registry` map into `{:model #{…} :views #{…}}`.

   opts:
     :select        (fn [compound-id props] -> bool)  required
     :type->el      map, defaults to `default-type->el`
     :system-id     keyword, wraps in-selection nodes as a C4 system boundary
     :system-name   string
     :view-title    string
     :relations     set of #{:deps :dataflow}, default #{:deps}
     :external-deps? include out-of-selection dep targets as external nodes
                     (default true) so edges resolve."
  [registry {:keys [select type->el system-id system-name view-title
                    relations external-deps?]
             :or   {type->el        default-type->el
                    system-id       :atlas/system
                    system-name     "System"
                    view-title      "Atlas export"
                    relations       #{:deps}
                    external-deps?  true}}]
  (let [tri        (type-ref-index registry)
        selected   (for [[cid props] registry :when (select cid props)]
                     [cid props])
        sel-props  (map second selected)
        sel-ids    (set (map dev-id sel-props))
        all-ids    (set (map dev-id (vals registry)))
        ;; external depends-targets that ARE real entities but not selected
        ext-ids    (when external-deps?
                     (set (for [props sel-props
                                d     (refs-for tri depends-verbs props)
                                :when (and (not (contains? sel-ids d))
                                           (contains? all-ids d))]
                            d)))
        node-ids   (into sel-ids ext-ids)
        in-nodes   (for [[cid props] selected]
                     (->node cid props type->el {}))
        ext-nodes  (for [d ext-ids]
                     (cond-> {:el :system :id (safe-id d) :name (humanize d)
                              :external true}
                       (not= (safe-id d) d) (assoc :atlas/dev-id d)))
        rels       (concat
                    (when (:deps relations)
                      (dep-relations tri sel-props node-ids))
                    (when (:dataflow relations)
                      (dataflow-relations tri sel-props sel-props sel-ids)))
        ;; wrap in-selection nodes inside a C4 system boundary via :ct nesting
        system     {:el :system :id system-id :name system-name
                    :ct (set in-nodes)}
        model      (into #{system} (concat ext-nodes rels))
        refs       (vec (concat (map (fn [n] {:ref (:id n)})
                                     (concat in-nodes ext-nodes))
                                (map (fn [r] {:ref (:id r)}) rels)))
        ;; C4 container view — rendered by Overarch's PlantUML backend
        container-view {:el    :container-view
                        :id    (keyword (namespace system-id)
                                        (str (name system-id) "-container-view"))
                        :title view-title
                        :ct    refs}
        ;; concept view — rendered by Overarch's GraphViz backend (offline, via dot)
        concept-view   {:el    :concept-view
                        :id    (keyword (namespace system-id)
                                        (str (name system-id) "-concept-view"))
                        :title view-title
                        :ct    refs}]
    {:model  model
     :views  #{container-view concept-view}
     :stats  {:nodes    (count in-nodes)
              :external (count ext-nodes)
              :rels     (count rels)}}))

;; ---------------------------------------------------------------------------
;; Emit to an Overarch model directory
;; ---------------------------------------------------------------------------

(defn- pp-str [x] (with-out-str (pp/pprint x)))

(defn emit!
  "Project `registry` and write `model.edn` + `views.edn` into `dir` (an
   Overarch `--model-dir`). Returns the :stats map."
  [registry dir opts]
  (let [{:keys [model views stats]} (registry->overarch registry opts)]
    (io/make-parents (io/file dir "model.edn"))
    (spit (io/file dir "model.edn")
          (str ";; Generated by atlas.adapter.overarch from an Atlas registry\n"
               (pp-str model)))
    (spit (io/file dir "views.edn")
          (str ";; Generated by atlas.adapter.overarch\n"
               (pp-str views)))
    stats))
