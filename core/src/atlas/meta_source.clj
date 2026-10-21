(ns atlas.meta-source
  "EXPERIMENTAL (see EXPERIMENTAL.md) — atlas's registry read by
   meta-source (https://github.com/semantic-namespace/meta-source), the
   cross-reference of the registries a project already has: atlas's next to
   integrant's, spec's, Pathom's, re-frame's...

   `pack` is a meta-source pack. A project names it in its meta-source.edn,
   with atlas on the classpath:

     {:registry [\"registry\"] :src [\"src\"]
      :packs {atlas.meta-source/pack {}}
      :deps {io.github.semantic-namespace/atlas
             {:git/url \"https://github.com/semantic-namespace/atlas\"
              :git/sha \"...\" :deps/root \"core\"}}}

   What it adds to what meta-source reads by itself (EDN registry files):

   - :atlas.source/id is the meta key linking a defn to its entity, with
     no :id-key to configure;
   - each atlas.registry/register! (or atlas.core/register!) call whose
     dev-id, type and aspects are written as data is a source of that
     entity, as an EDN entry is; a 3-argument call gets the dev-id register!
     would generate. Props written as code (an inline fn) are left out, and
     listed as dropped;
   - :atlas/aspects is the union of every source's aspects;
   - an atlas entity under another library's id (an integrant component's
     key, a resolver's) describes that thing: it is one entity, its primary
     definition the library's, not one id in two packs;
   - atlas's context and response keys are dataflow: what an execution
     function, an endpoint or a workflow consumes and produces, and what the
     project's own ontologies declare as theirs (:dataflow/context-key,
     :dataflow/response-key), with no :dataflow line in meta-source.edn;
   - an invariant's :invariant/rule, its check written as a meta-source
     rule, is checked with the project's own rules, at the entity it fails
     on;
   - check: an entity no source gives an :atlas/type, an execution
     function from EDN alone with no var carrying :atlas.source/id, and a
     var carrying :atlas.source/id for an entity no source defines (errors):
     atlas.source/load! would refuse all three; and two dev-ids sharing one
     compound identity (error), which the registry would let one replace
     the other under;
   - its runtime half: `watch!` keeps the snapshot meta-source reads, the
     registry as this process has it, so entities registered by a generator,
     a loop or a computed call are seen, and where the code and the running
     registry disagree on an entity's type or aspects is reported."
  (:require [atlas.registry :as registry]
            [atlas.source :as source]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def kind :atlas)

(def dataflow
  "The entry keys listing the data keys atlas's built-in entity types
   consume and produce. A project's own ontologies add theirs by declaring
   :dataflow/context-key and :dataflow/response-key, read by
   `ontology-dataflow`."
  {:consumes [:execution-function/context :interface-endpoint/context :dataflow/context :workflow/context]
   :produces [:execution-function/response :interface-endpoint/response :dataflow/response]})

;; =============================================================================
;; register! calls
;; =============================================================================

(defn- ontology-dataflow
  "The context and response keys the ontologies read declare (an
   :atlas/ontology's :dataflow/context-key and :dataflow/response-key, a
   keyword or a vector), as `dataflow` names the built-in ones."
  [entries]
  (let [ks (fn [k] (vec (distinct (for [e (vals entries)
                                        :let [v (get e k)]
                                        x (if (sequential? v) v [v])
                                        :when (qualified-keyword? x)]
                                    x))))]
    {:consumes (ks :dataflow/context-key) :produces (ks :dataflow/response-key)}))

(declare registration)

(def builtin-rule-files
  "atlas's own ontologies whose invariants carry an :invariant/rule, read
   from the classpath: a project's source doesn't include them."
  ["atlas/ontology/allium_rule.cljc"
   "atlas/ontology/business_pattern.cljc"
   "atlas/ontology/decision.cljc"
   "atlas/ontology/deployment.cljc"
   "atlas/ontology/execution_function.cljc"
   "atlas/ontology/interface_endpoint.cljc"
   "atlas/ontology/llm_prompt.cljc"
   "atlas/ontology/shapeup.cljc"
   "atlas/ontology/structure_component.cljc"
   "atlas/ontology/system.cljc"
   "atlas/ontology/test_case.cljc"
   "atlas/ontology/value_proposition.cljc"
   "atlas/ontology/workflow.cljc"
   "atlas/ontology/workflow_producer.cljc"])

(defn- rules-in
  "{invariant-id rule}: the :invariant/rule of each :atlas/invariant in `entries`."
  [entries]
  (into {} (for [[id e] entries
                 :when (and (= :atlas/invariant (:atlas/type e)) (map? (:invariant/rule e)))]
             [id (:invariant/rule e)])))

(def ^:private builtin-rules
  (delay
    ;; edamame is babashka's, where meta-source runs the pack
    (let [parse (requiring-resolve 'edamame.core/parse-string-all)]
      (rules-in (into {} (for [f builtin-rule-files
                               :let [r (io/resource f)]
                               :when r
                               form (parse (slurp r) {:all true :read-cond :allow :features #{:clj}
                                                      :auto-resolve (fn [a] (if (= :current a) 'user a))})
                               :when (and (seq? form) (= 'register! (some-> (first form) name symbol)))
                               :let [{:keys [dev-id entry]} (registration form)]
                               :when dev-id]
                           [dev-id entry]))))))

(defn- invariant-rules
  "The rules invariants carry: an :atlas/invariant's :invariant/rule is its
   check as a meta-source rule, checked from the files where its
   :invariant/fn needs the running registry. atlas's own (see
   `builtin-rule-files`), and those of the invariants read."
  [entries]
  (merge @builtin-rules (rules-in entries)))

(defn- literal?
  "True when x is plain data: no symbols or lists (var references, calls,
   fns), no regexes or other host objects."
  [x]
  (cond
    (or (symbol? x) (seq? x)) false
    (map? x) (every? (fn [[k v]] (and (literal? k) (literal? v))) x)
    (coll? x) (every? literal? x)
    :else (or (nil? x) (keyword? x) (string? x) (number? x) (boolean? x) (char? x))))

(defn registration
  "What a register! call says, when its dev-id, type and aspects are written
   as data: {:dev-id :entry :dropped}, :dropped being the props whose values
   aren't data (an inline fn, a var reference). Otherwise {:skipped reason}.
   A 3-argument call gets the dev-id register! would generate."
  [form]
  (let [args (when (seq? form) (rest form))
        [id type aspects props] (case (count args) 4 args 3 (cons ::generated args) nil)]
    (cond
      (= form :unreadable) {:skipped "not readable as data"}
      (nil? type) {:skipped "not a 3- or 4-argument call"}
      (not (and (literal? type) (literal? aspects) (or (= id ::generated) (literal? id))))
      {:skipped "dev-id, type or aspects not written as data"}
      (not (and (qualified-keyword? type) (set? aspects) (every? qualified-keyword? aspects)
                (or (= id ::generated) (qualified-keyword? id))))
      {:skipped "type, aspects or dev-id of the wrong shape"}
      (not (map? props)) {:skipped "props not written as a map"}
      :else (let [kept (into {} (filter (fn [[k v]] (and (keyword? k) (literal? v)))) props)]
              {:dev-id (if (= id ::generated) (registry/generate-dev-id (conj aspects type)) id)
               :entry (assoc (dissoc kept :atlas/dev-id) :atlas/type type :atlas/aspects aspects)
               :dropped (vec (sort (remove (set (keys kept)) (keys props))))}))))

(defn- find-forms
  "Each atlas.registry/register! and atlas.core/register! call."
  [analysis _opts]
  (for [u (:var-usages analysis)
        :when (and ('#{atlas.registry atlas.core} (:to u)) (= 'register! (:name u)))]
    (select-keys u [:filename :row :name-row :name-col])))

(defn- read-forms [sites {:keys [form-at]} _opts]
  (vec (for [{:keys [row name-row name-col]} sites
             :let [{:keys [dropped] :as r} (registration (form-at name-row name-col))]]
         (cond-> (assoc (dissoc r :dropped) :row row :kind kind)
           (seq dropped) (assoc :dropped dropped)))))

;; =============================================================================
;; Checks, the hover, the runtime half
;; =============================================================================

(defn- checks
  "- :entity-without-type (error): an entity from registry files or
     register! calls that no source gives an :atlas/type. A data key, and
     an entity another pack declares, are in their library's vocabulary
     and aren't atlas entities;
   - :entity-without-impl (error): an entity of one of atlas.source's
     impl-types (an execution function), or of a type whose ontology declares
     :ontology/impl-key, given only by registry files, with no var carrying
     the meta key. A register! call gives its impl inline, the running
     process at runtime;
   - :var-without-entity (error): a var carrying :atlas.source/id for an
     id no source defines;
   - :compound-identity-shared (error): two dev-ids with the same compound
     identity (type and aspects). The registry keys entities by it, so one
     silently replaces the other, and calling the replaced one fails."
  [{:keys [entities data-key? loc impl linked-by]}]
  (let [atlas-only? (fn [e] (every? #(#{:edn :binding kind} (:kind %)) (:definitions e)))
        ;; the types whose ontology, read here, keeps an impl under its own key
        keyed (set (for [[_ e] entities :when (get-in e [:entry :ontology/impl-key])] (get-in e [:entry :ontology/for])))]
    (concat
     (for [[id e] (sort-by key entities)
           :when (and (atlas-only? e) (not (data-key? e)) (not (get-in e [:entry :atlas/type]))
                      (not (every? #(= :binding (:kind %)) (:definitions e))))]
       {:severity :error :problem :entity-without-type :dev-id id :definition (loc e)})
     (for [[id e] (sort-by key entities)
           :let [type (get-in e [:entry :atlas/type])]
           :when (and (or (source/impl-types type) (keyed type)) (every? #(= :edn (:kind %)) (:definitions e))
                      (not (impl id)))]
       {:severity :error :problem :entity-without-impl :dev-id id :type type :definition (loc e)
        :message (str "no var carries :atlas.source/id " id ": " type " keeps one under "
                      (or (some #(when (= type (get-in (val %) [:entry :ontology/for]))
                                   (get-in (val %) [:entry :ontology/impl-key]))
                                entities)
                          :atlas/impl))})
     (for [[id e] (sort-by key entities)
           :when (and (every? #(= :binding (:kind %)) (:definitions e)) (= :atlas (linked-by id)))]
       {:severity :error :problem :var-without-entity :dev-id id :impl (impl id)})
     (for [[identity ids] (sort-by (comp pr-str key)
                                   (group-by #(let [{:atlas/keys [type aspects]} (get-in entities [% :entry])]
                                                (when (and type (seq aspects)) (conj (set aspects) type)))
                                             (keys entities)))
           :when (and identity (> (count ids) 1))
           :let [ids (vec (sort ids))]
           id ids]
       (let [others (vec (remove #{id} ids))]
         {:severity :error :problem :compound-identity-shared :dev-id id
          :shared-with others :identity (vec (sort identity))
          :message (str "same type and aspects as " (str/join ", " others)
                        ": the registry keeps one entity for both")
          :definition (loc (get entities id))})))))

(defn- describe [entry]
  (when-let [t (:atlas/type entry)]
    (str (name t) (when-let [as (seq (:atlas/aspects entry))] (str "  " (str/join " " (sort as)))))))

(defn- runtime-fill
  "The entry with what the running registry has and the code doesn't say:
   props written as code, or no source gave."
  [entry runtime]
  (merge runtime entry))

(defn- runtime-diff
  "Where the code and the running registry disagree on type or aspects."
  [entry runtime]
  (let [d (cond-> {}
            (and (:atlas/type runtime) (not= (:atlas/type entry) (:atlas/type runtime)))
            (assoc :atlas/type {:code (:atlas/type entry) :runtime (:atlas/type runtime)})
            (and (:atlas/aspects runtime) (not= (set (:atlas/aspects entry)) (set (:atlas/aspects runtime))))
            (assoc :atlas/aspects {:only-in-code (vec (sort (remove (set (:atlas/aspects runtime)) (:atlas/aspects entry))))
                                   :only-in-runtime (vec (sort (remove (set (:atlas/aspects entry)) (:atlas/aspects runtime))))}))]
    (not-empty d)))

(def pack
  {:pack/id :atlas
   :pack/kinds #{kind}
   :pack/id-key :atlas.source/id
   :pack/vocabulary #{"atlas"}
   :pack/data-keys? false
   ;; atlas describes what other libraries declare: an entity under an
   ;; integrant component's key, a resolver's, a spec's
   :pack/describes? true
   :pack/merge {:atlas/aspects :union}
   :pack/dataflow dataflow
   :pack/dataflow-of ontology-dataflow
   :pack/rules-of invariant-rules
   :pack/find find-forms
   :pack/read read-forms
   :pack/checks checks
   :pack/describe describe
   ;; its registry files are its declarations too
   :pack/runtime-kinds #{kind :edn}
   :pack/runtime-fill runtime-fill
   :pack/runtime-diff runtime-diff})

(def default-snapshot ".meta-source/packs/atlas.edn")

(defn snapshot!
  "Writes the registry as this process has it where meta-source reads the
   atlas pack's runtime half: {:pack :atlas :written ms :entities {dev-id
   entry}}, each entry the last registration's type, aspects and the props
   that are data. Returns `path`."
  ([] (snapshot! default-snapshot))
  ([path]
   (let [f (io/file path)
         tmp (io/file (str path ".tmp"))]
     (io/make-parents f)
     (binding [*print-namespace-maps* false]
       (spit tmp (pr-str {:pack :atlas
                          :written (System/currentTimeMillis)
                          :entities (:entities (source/snapshot))})))
     (.renameTo tmp f)
     path)))

(defn watch!
  "Keeps `path` holding `snapshot!`: written now, and again a moment after
   each change to the registry. Returns `path`."
  ([] (watch! default-snapshot))
  ([path]
   (let [pending (atom false)]
     (snapshot! path)
     (add-watch registry/registry ::snapshot
                (fn [_ _ old new]
                  (when (and (not= old new) (compare-and-set! pending false true))
                    (future
                      (Thread/sleep 300)
                      (reset! pending false)
                      (try (snapshot! path) (catch Throwable _ nil))))))
     path)))

(defn unwatch!
  "Stops `watch!`."
  []
  (remove-watch registry/registry ::snapshot))
