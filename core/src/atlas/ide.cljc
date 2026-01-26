(ns atlas.ide
  "IDE integration API - clean interface for editor tooling.
   Returns EDN that editors can parse and display."
  (:require [atlas.registry :as cid]
            [atlas.registry.lookup :as rt]
            [atlas.registry.graph :as graph]
            [atlas.invariant :as ax]
            [atlas.docs :as docs]
            [atlas.ontology :as ot]
            [atlas.query :as query]
            [atlas.cljc.platform :as platform]
            [clojure.string :as str]
            [clojure.set :as set]))

;; =============================================================================
;; HELPERS
;; =============================================================================

(defn- ensure-keyword 
  "Convert string to keyword, or return as-is if already a keyword."
  [x]
  (if (string? x) (keyword x) x))

(defn- sorted-vec
  "Return a sorted vector from a collection (defaults to ascending compare)."
  [coll]
  (->> (or coll [])
       sort
       vec))

(defn- definition-value
  "Normalize registry definition values for editor display."
  [value]
  (cond
    (set? value) (->> value (sort-by pr-str) vec)
    (map? value) (reduce-kv (fn [acc k v]
                              (assoc acc k (definition-value v)))
                            (empty value)
                            value)
    (sequential? value) (mapv definition-value value)
    :else value))

(defn- to-string 
  "Convert keyword to string while preserving its namespace and dropping the leading ':'."
  [x]
  (if (keyword? x)
    (if-let [ns (namespace x)]
      (str ns "/" (name x))
      (name x))
    (str x)))

(defn- fields-for [props]
  (or (:data-schema/fields props)
      (:atlas/fields props)
      []))

;; Cache for reverse dependency index (for performance optimization)
(def ^:private reverse-deps-cache (atom {:time 0 :data {}}))
(def ^:private data-key-cache (atom {:time 0 :entity/produces {} :entity/consumes {}}))
(def ^:private cache-ttl-ms 5000)

(defn- build-reverse-deps []
  "Build reverse dependency index: maps dev-id to set of dependents."
  (let [all-ids (ax/all-dev-ids)]
    (reduce (fn [acc dev-id]
              (let [deps (ot/deps-for dev-id)]
                (reduce (fn [inner dep]
                          (update inner dep (fnil conj #{}) dev-id))
                        acc
                        deps)))
            {}
            all-ids)))

(defn- get-reverse-deps []
  "Get reverse deps cache, rebuilding if stale."
  (let [now (platform/now-ms)
        {:keys [time data]} @reverse-deps-cache]
    (if (and (pos? time) (< (- now time) cache-ttl-ms))
      data
      (let [new-data (build-reverse-deps)]
        (swap! reverse-deps-cache assoc :time now :data new-data)
        new-data))))

(defn- build-data-key-cache []
  "Build indexes for producers/consumers keyed by data-key."
  (reduce (fn [acc [_ props]]
            (let [id (:atlas/dev-id props)
                  context (ot/context-for id)
                  response (ot/response-for id)
                  produces (:entity/produces acc)
                  consumes (:entity/consumes acc)
                  producer-updated (reduce (fn [m k] (update m k (fnil conj #{}) id)) produces response)
                  consumer-updated (reduce (fn [m k] (update m k (fnil conj #{}) id)) consumes context)]
              (assoc acc :entity/produces producer-updated :entity/consumes consumer-updated)))
          {:entity/produces {} :entity/consumes {}}
          @cid/registry))

(defn- get-data-key-cache []
  "Get cached producers/consumers by data-key, rebuilding when stale."
  (let [now (platform/now-ms)
        cache @data-key-cache
        time (:time cache)
        produces (:entity/produces cache)
        consumes (:entity/consumes cache)]
    (if (and (pos? time) (< (- now time) cache-ttl-ms))
      {:entity/produces produces :entity/consumes consumes}
      (let [new-cache (build-data-key-cache)
            new-produces (:entity/produces new-cache)
            new-consumes (:entity/consumes new-cache)]
        (swap! data-key-cache assoc :time now :entity/produces new-produces :entity/consumes new-consumes)
        {:entity/produces new-produces :entity/consumes new-consumes}))))

;; =============================================================================
;; QUERY API
;; =============================================================================

(defn list-entity-types
  "List all registered entity types in the registry."
  []
  (vec (sort (cid/registered-types))))

(defn entity-type-ontology-keys
  "Get ontology keys for a given entity type."
  [entity-type]
  (vec (or (ot/ontology-keys-for (ensure-keyword entity-type)) [])))

(defn list-all-entities
  "List all registered entities with their type classification.
   Types: endpoint, function, component, schema, protocol,
          business-pattern, constraint, failure-mode,
          value-proposition, user-role, user-experience, other"
  []
  (vec (sort-by str
                (for [[id props] @cid/registry]
                  {:entity/dev-id (:atlas/dev-id props)
                   :entity/type (cond
                                  (contains? id :atlas/interface-endpoint) :endpoint
                                  (contains? id :atlas/execution-function) :function
                                  (contains? id :atlas/structure-component) :component
                                  (contains? id :atlas/schema) :schema
                                  (contains? id :atlas/interface-protocol) :protocol
                                  (contains? id :atlas/business-pattern) :business-pattern
                                  (contains? id :atlas/governance-constraint) :constraint
                                  (contains? id :atlas/risk-failure-mode) :failure-mode
                                  (contains? id :atlas/value-proposition) :value-proposition
                                  (contains? id :atlas/identity-role) :user-role
                                  (contains? id :atlas/experience-journey) :user-experience
                                  :else :other)}))))

(defn list-aspects
  "List all unique aspects used in the registry.

   Args:
     registry - (Optional) Registry map. Defaults to global registry."
  ([]
   (list-aspects @cid/registry))
  ([registry]
   (let [;; Get registered entity types dynamically
         entity-types (cid/registered-types)
         ;; Count aspect usage, excluding entity types
         counts (reduce (fn [acc identity]
                          (let [aspects (cid/aspects identity)]
                            (reduce (fn [m aspect]
                                      (update m aspect (fnil inc 0)))
                                    acc
                                    aspects)))
                        {}
                        (keys registry))
         ;; Namespace ordering for display
         ns-order {"authorization" 0
                   "capacity" 1
                   "compliance" 2
                   "domain" 3
                   "protocol" 4
                   "semantic-namespace" 5
                   "tier" 6
                   "temporal" 7}]
     (->> counts
          (map (fn [[aspect count]]
                 {:aspect/aspect aspect
                  :aspect/count count}))
          (sort-by (fn [{:aspect/keys [aspect]}]
                     (let [ns (namespace aspect)]
                       [(get ns-order ns 100) ns (name aspect)])))
          vec))))

(defn list-aspect-namespaces
  "List all aspect namespaces with usage counts.
   Dynamically excludes entity types using the registry."
  ([]
   (list-aspect-namespaces @cid/registry))
  ([registry]
   (let [;; Count aspects by namespace
         ns-counts (reduce (fn [acc identity]
                             (let [aspects (cid/aspects identity)]
                               (reduce (fn [m aspect]
                                         (when-let [ns (namespace aspect)]
                                           (update m ns (fnil inc 0))))
                                       acc
                                       aspects)))
                           {}
                           (keys registry))]
     (->> ns-counts
          (map (fn [[ns count]]
                 {:namespace/name ns
                  :namespace/count count}))
          (sort-by :namespace/name)
          vec))))

(defn list-aspect-names-in-namespace
  "List all aspect names (without namespace prefix) in a given namespace.
   Returns only aspects, not entity types."
  ([ns-name]
   (list-aspect-names-in-namespace @cid/registry ns-name))
  ([registry ns-name]
   (let [;; Collect all unique names in the namespace
         names (reduce (fn [acc identity]
                         (let [aspects (cid/aspects identity)]
                           (reduce (fn [s aspect]
                                     (if (= (namespace aspect) ns-name)
                                       (conj s (name aspect))
                                       s))
                                   acc
                                   aspects)))
                       #{}
                       (keys registry))]
     (vec (sort names)))))

(defn entities-with-aspect [aspect]
  "Find all entities with given aspect."
  (->> (rt/all-with-aspect (ensure-keyword aspect))
       (remove nil?)
       sort
       vec))

(defn entity-info
  [dev-id]
  (let [dev-id-kw (ensure-keyword dev-id)
        id (rt/identity-for dev-id-kw)
        props (rt/props-for dev-id-kw)]
    (when id
      (let [definition-keys (ot/definition-keys-for-identity id)
            definition-values (when (seq definition-keys)
                             (reduce (fn [acc k]
                                       (if (contains? props k)
                                         (assoc acc k (definition-value (get props k)))
                                         acc))
                                     (array-map)
                                     definition-keys))]
        {:entity/dev-id dev-id-kw
         :entity/identity (vec (sort id))
         :entity/aspects (vec (sort (disj id :atlas/execution-function
                                          :atlas/structure-component
                                          :atlas/interface-endpoint
                                          :atlas/schema)))
         :entity/definition-keys definition-keys
         :entity/definition-values definition-values
         :interface-endpoint/context (vec (sort (ot/context-for dev-id-kw)))
         :interface-endpoint/response (vec (sort (ot/response-for dev-id-kw)))
         :execution-function/deps (vec (sort (ot/deps-for dev-id-kw)))
         :atlas/fields (vec (sort (fields-for props)))}))))

(defn data-flow [dev-id]
  "Get data flow info for a function."
  (->> (ot/trace-data-flow (ensure-keyword dev-id))
       (map (fn [{:keys [needs produced-by satisfied?]}]
              {:dataflow/needs needs
               :dataflow/produced-by (sorted-vec produced-by)
               :dataflow/satisfied? (boolean satisfied?)}))
       vec))

(defn execution-order []
  "Get topologically sorted execution order."
  (graph/topo-sort-by-data (rt/all-with-aspect :atlas/execution-function)))

;; =============================================================================
;; VALIDATION API
;; =============================================================================

(defn check-invariants []
  "Run all invariant checks and return results."
  (let [{:keys [valid? errors warnings]} (ax/check-all)]
    {:invariant/valid? valid?
     :invariant/error-count (count errors)
     :invariant/warning-count (count warnings)
     :invariant/errors (mapv #(select-keys % [:invariant :message]) errors)
     :invariant/warnings (mapv #(select-keys % [:invariant :message]) warnings)}))

(defn validate-entity [dev-id]
  "Check invariants relevant to a specific entity."
  (let [dev-id-kw (ensure-keyword dev-id)
        {:keys [errors warnings]} (ax/check-all)
        dev-id-str (str dev-id-kw)
        dev-id-name (name dev-id-kw)
        relevant (fn [v]
                   (let [text (str (:message v) (:details v))]
                     (or (str/includes? text dev-id-str)
                         (str/includes? text dev-id-name))))]
    {:validation/dev-id dev-id-kw
     :validation/errors (filterv relevant errors)
     :validation/warnings (filterv relevant warnings)}))

;; =============================================================================
;; DOCUMENTATION API
;; =============================================================================

(defn entity-doc [dev-id]
  "Get documentation for an entity."
  (docs/enrich-with-descriptions (ensure-keyword dev-id)))

(defn system-summary []
  "Get system overview."
  (docs/system-overview))

(defn generate-markdown []
  "Generate full markdown documentation."
  (docs/generate-markdown))

;; =============================================================================
;; NAVIGATION API
;; =============================================================================

(defn dependents-of
    "Find what depends on this entity. Uses cached reverse dependency index for O(1) lookup."
  [dev-id]

  (let [dev-id-kw (ensure-keyword dev-id)
        reverse-deps (get-reverse-deps)
        dependents (get reverse-deps dev-id-kw #{})]
    (vec (sort dependents))))

(defn dependencies-of 
  "Find what this entity depends on."
  [dev-id]
  (vec (sort (ot/deps-for (ensure-keyword dev-id)))))

(defn producers-of
  "Find functions that produce a data key."
  [data-key]
  (let [data-key-kw (ensure-keyword data-key)
        cache (get-data-key-cache)
        produces (:entity/produces cache)]
    (sorted-vec (get produces data-key-kw #{}))))

(defn consumers-of
  "Find functions that consume a data key."
  [data-key]
  (let [data-key-kw (ensure-keyword data-key)
        cache (get-data-key-cache)
        consumes (:entity/consumes cache)]
    (sorted-vec (get consumes data-key-kw #{}))))

;; =============================================================================
;; COMPLETION API
;; =============================================================================

(defn complete-dev-id
  "Autocomplete dev-id from prefix. Returns vector of strings without ':' prefix."
  [prefix]
  (let [prefix-str (to-string prefix)
        all-ids (map to-string (ax/all-dev-ids))]
    (->> all-ids
         (filter #(str/starts-with? % prefix-str))
         sort
         vec)))

(defn complete-aspect 
  "Autocomplete aspect from prefix. Returns vector of strings without ':' prefix."
  [prefix]
  (let [prefix-str (to-string prefix)
        all-aspects (map (comp to-string :aspect/aspect) (list-aspects))]
    (->> all-aspects
         (filter #(str/starts-with? % prefix-str))
         sort
         vec)))

(defn complete-data-key 
  "Autocomplete data key from prefix. Returns vector of strings without ':' prefix."
  [prefix]
  (let [prefix-str (to-string prefix)
        all-keys (->> @cid/registry
                      vals
                      (mapcat #(concat (:interface-endpoint/context %)
                                       (:interface-endpoint/response %)))
                      (remove nil?)
                      (map to-string)
                      distinct
                      sort)]
    (->> all-keys
         (filter #(str/starts-with? % prefix-str))
         sort
         vec)))

;; =============================================================================
;; ADVANCED QUERIES API
;; =============================================================================

(defn trace-data-flow
  "Trace how a data key flows through the system."
  [data-key]
  (let [data-key-kw (ensure-keyword data-key)
        produced-by (producers-of data-key-kw)
        consumed-by (consumers-of data-key-kw)]
    {:dataflow/data-key data-key-kw
     :dataflow/produced-by produced-by
     :dataflow/consumed-by consumed-by
     :dataflow/connected? (boolean (and (seq produced-by) (seq consumed-by)))}))

(defn impact-of-change
  "Show what would be affected if an entity changes."
  [entity-id]
  (let [result (query/impact-of-change @cid/registry (ensure-keyword entity-id) :atlas/dev-id :execution-function/deps :interface-endpoint/response)
        entity (:entity result)
        produces (:entity/produces result)
        direct-dependents (:direct-dependents result)]
    {:impact/entity entity
     :impact/produces (sorted-vec produces)
     :impact/direct-dependents (sorted-vec direct-dependents)}))

(defn domain-coupling
  "Analyze inter-domain dependencies."
  []
  (->> (query/domain-coupling @cid/registry :atlas/dev-id :execution-function/deps)
       (map (fn [{:keys [domain depends-on entity-count]}]
              {:coupling/domain domain
               :coupling/depends-on (sorted-vec depends-on)
               :coupling/entity-count entity-count}))
       (sort-by :coupling/domain)
       vec))

(defn pii-surface
  "Find all entities handling PII."
  []
  (->> (query/pii-surface @cid/registry :atlas/dev-id :interface-endpoint/context :interface-endpoint/response)
       (map (fn [{:keys [id audited? context response]}]
              {:pii/id id
               :pii/audited? audited?
               :pii/context (sorted-vec context)
               :pii/response (sorted-vec response)}))
       (sort-by :pii/id)
       vec))

(defn error-handler-coverage
  "Check error handler coverage."
  []
  (let [{:keys [handlers coverage]} (query/error-handler-coverage @cid/registry :atlas/dev-id)]
    {:error-handler/handlers (->> handlers
                                  (map (fn [{:keys [id handles]}]
                                         {:error-handler/id id
                                          :error-handler/handles (sorted-vec handles)}))
                                  (sort-by :error-handler/id)
                                  vec)
     :error-handler/coverage (->> coverage
                                  (map (fn [{:keys [entity concern has-handler?]}]
                                         {:error-handler/entity entity
                                          :error-handler/concern concern
                                          :error-handler/has-handler? has-handler?}))
                                  (sort-by (juxt :error-handler/concern :error-handler/entity))
                                  vec)}))

;; =============================================================================
;; ONTOLOGY TOOLS API (from ontology-tools)
;; =============================================================================

(defn suggest-aspects
  "Suggest aspects for an entity based on similar entries."
  [dev-id]
  (let [id (rt/identity-for (keyword dev-id))]
    (let [{:keys [similar-entries suggested-aspects] :as raw} (ot/suggest-aspects id)]
      (-> raw
          (assoc :ontology/similar-entries (->> similar-entries
                                                (map sorted-vec)
                                                vec))
          (assoc :ontology/suggested-aspects (->> suggested-aspects sort vec))
          (dissoc :similar-entries :suggested-aspects)))))

(defn inspect-entity
  "Quick inspection of an entity."
  [dev-id]
  (let [dev-id-kw (ensure-keyword dev-id)]
    (let [{:keys [semantic-identity value]} (ot/inspect dev-id-kw)
          namespaced-value (when value
                             (cond-> value
                               (:execution-function/deps value)
                               (update :execution-function/deps sorted-vec)))]
      {:inspection/semantic-identity (sorted-vec semantic-identity)
       :inspection/value namespaced-value})))

(defn aspect-catalog
  "Show all aspects with usage stats."
  []
  (->> (ot/aspect-catalog)
       (map (fn [[ns items]]
              {:aspect-catalog/namespace ns
               :aspect-catalog/items (->> items
                                          (map (fn [{:keys [aspect usage-count examples]}]
                                                 {:aspect-catalog/aspect aspect
                                                  :aspect-catalog/usage-count usage-count
                                                  :aspect-catalog/examples (sorted-vec examples)}))
                                          (sort-by :aspect-catalog/aspect)
                                          vec)}))
       (sort-by :aspect-catalog/namespace)
       vec))

(defn validate-identity
  "Check if an identity is valid before registering."
  [identity-set]
  (ot/validate-before-register (set (map keyword identity-set))))

(defn list-templates
  "List available templates."
  []
  [{:template/name "foundation-component"
    :template/params [:domain :external? :async? :traced?]
    :template/description "Infrastructure components"}
   {:template/name "service-function"
    :template/params [:domain :operation :external? :pure? :pii?]
    :template/description "Business logic functions"}
   {:template/name "api-endpoint"
    :template/params [:domain :operation :auth-required? :rate-limited? :pii?]
    :template/description "HTTP API endpoints"}])

;; =============================================================================
;; SEMANTIC SIMILARITY API
;; =============================================================================

(defn semantic-similarity
  "Find entities with similar semantic profiles to given entity."
  [dev-id]
  (let [dev-id-kw (ensure-keyword dev-id)
        [target-identity _] (query/find-by-dev-id @cid/registry dev-id-kw)
        results (query/semantic-similarity @cid/registry target-identity 0.0)]
    (->> results
         (map (fn [{:keys [identity similarity shared]}]
                (let [dev-id-for-identity (get-in @cid/registry [identity :atlas/dev-id])]
                  {:similarity/entity dev-id-for-identity
                   :similarity/score (double similarity)
                   :similarity/shared-aspects (sorted-vec shared)})))
         vec)))

;; =============================================================================
;; ARCHITECTURE VIEWS API
;; =============================================================================

(defn by-tier
  "Group all entities by architectural tier."
  []
  (let [result (query/by-tier @cid/registry :atlas/dev-id)]
    (->> result
         (map (fn [[tier entities]]
                {:tier/name tier
                 :tier/entities (sorted-vec entities)
                 :tier/count (count entities)}))
         (sort-by :tier/name)
         vec)))

(defn architecture-view
  "Get architecture documentation: entities grouped by tier and domain."
  []
  (let [arch (docs/architecture-documentation)]
    {:architecture/title (:title arch)
     :architecture/tiers (->> (:tiers arch)
                              (map (fn [[tier data]]
                                     {:tier/name tier
                                      :tier/entities (sorted-vec (:entities data))
                                      :tier/count (:count data)}))
                              (sort-by :tier/name)
                              vec)
     :architecture/domains (->> (:domains arch)
                                (map (fn [[domain data]]
                                       {:domain/name domain
                                        :domain/entities (sorted-vec (:entities data))
                                        :domain/count (:count data)}))
                                (sort-by :domain/name)
                                vec)}))

(defn operations-view
  "Get operations documentation: external integrations, pure functions, OAuth deps."
  []
  (let [ops (docs/operations-documentation)]
    {:operations/title (:title ops)
     :operations/external-integrations
     {:entities (sorted-vec (get-in ops [:external-integrations :entities]))
      :description (get-in ops [:external-integrations :description])
      :concerns (vec (get-in ops [:external-integrations :operational-concerns]))}
     :operations/pure-functions
     {:entities (sorted-vec (get-in ops [:pure-functions :entities]))
      :description (get-in ops [:pure-functions :description])
      :concerns (vec (get-in ops [:pure-functions :operational-concerns]))}
     :operations/oauth-dependencies
     {:entities (sorted-vec (get-in ops [:oauth-dependencies :entities]))
      :description (get-in ops [:oauth-dependencies :description])
      :concerns (vec (get-in ops [:oauth-dependencies :operational-concerns]))}}))

;; =============================================================================
;; ASPECT IMPACT API
;; =============================================================================

(defn aspect-impact
  "Analyze what would be affected by changes to an aspect."
  [aspect]
  (let [aspect-kw (ensure-keyword aspect)
        result (ot/impact-analysis aspect-kw)]
    {:aspect-impact/aspect (:aspect result)
     :aspect-impact/total (or (:total-impact result) 0)
     :aspect-impact/by-tier (:by-tier result)
     :aspect-impact/by-type (:by-type result)
     :aspect-impact/examples (sorted-vec (:examples result))}))

;; =============================================================================
;; REFACTORING API
;; =============================================================================

(defn preview-refactor-aspect
  "Preview what would happen if an aspect is renamed (dry-run only)."
  [old-aspect new-aspect]
  (let [old-kw (ensure-keyword old-aspect)
        new-kw (ensure-keyword new-aspect)
        result (ot/refactor-aspect old-kw new-kw :dry-run? true)]
    {:refactor/old-aspect old-kw
     :refactor/new-aspect new-kw
     :refactor/affected-count (:affected-count result)
     :refactor/conflicts (vec (:conflicts result))
     :refactor/migrations (->> (:migrations result)
                               (map (fn [{:keys [dev-id old-id new-id will-collide?]}]
                                      {:refactor/dev-id dev-id
                                       :refactor/old-identity (sorted-vec old-id)
                                       :refactor/new-identity (sorted-vec new-id)
                                       :refactor/will-collide? will-collide?}))
                               vec)}))

;; =============================================================================
;; PROTOCOL API
;; =============================================================================

(defn list-protocols
  "List all registered protocols."
  []
  (->> @cid/registry
       (filter (fn [[id _]] (contains? id :atlas/interface-protocol)))
       (map (fn [[_ props]]
              (let [dev-id (:atlas/dev-id props)]
                {:protocol/id dev-id
                 :interface-protocol/functions (vec (sort (:interface-protocol/functions props)))})))
       (sort-by :protocol/id)
       vec))

(defn protocol-info
  "Get detailed information about a protocol."
  [protocol-id]
  (let [protocol-id-kw (ensure-keyword protocol-id)
        props (rt/props-for protocol-id-kw)]
    (when props
      {:protocol/id protocol-id-kw
       :interface-protocol/functions (vec (sort (:interface-protocol/functions props)))
       :protocol/implementers (->> @cid/registry
                                   (filter (fn [[id _]]
                                             (and (contains? id :atlas/structure-component)
                                                  (contains? id protocol-id-kw))))
                                   (map (fn [[_ v]] (:atlas/dev-id v)))
                                   sort
                                   vec)})))

(defn component-protocols
  "Get all protocols implemented by a component."
  [component-id]
  (let [component-id-kw (ensure-keyword component-id)
        compound-id (rt/identity-for component-id-kw)
        protocol-aspects (filter #(= "protocol" (namespace %)) compound-id)]
    (vec (sort protocol-aspects))))

(defn components-implementing
  "Find all components implementing a specific protocol."
  [protocol-id]
  (let [protocol-id-kw (ensure-keyword protocol-id)]
    (->> @cid/registry
         (filter (fn [[id _]]
                   (and (contains? id :atlas/structure-component)
                        (contains? id protocol-id-kw))))
         (map (fn [[_ v]] (:atlas/dev-id v)))
         sort
         vec)))

;; =============================================================================
;; BUSINESS SEMANTICS API
;; =============================================================================

(defn- business-entity-type 
  "Detect the business entity type from compound identity."
  [id]
  (cond
    (contains? id :atlas/business-pattern) :business-pattern
    (contains? id :atlas/governance-constraint) :constraint
    (contains? id :atlas/risk-failure-mode) :failure-mode
    (contains? id :atlas/value-proposition) :value-proposition
    (contains? id :atlas/identity-role) :user-role
    (contains? id :atlas/experience-journey) :user-experience
    :else nil))

(defn list-business-entities
  "List all business-layer entities grouped by type."
  []
  (let [all-entities (list-all-entities)
        business-types [:business-pattern :constraint :failure-mode
                        :value-proposition :user-role :user-experience]
        grouped (reduce (fn [acc entity]
                          (let [type (:entity/type entity)]
                            (if (some #{type} business-types)
                              (update acc type (fnil conj []) entity)
                              acc)))
                        {}
                        all-entities)]
    {:business/patterns (sorted-vec (map :entity/dev-id (get grouped :business-pattern)))
     :business/constraints (sorted-vec (map :entity/dev-id (get grouped :constraint)))
     :business/failure-modes (sorted-vec (map :entity/dev-id (get grouped :failure-mode)))
     :business/values (sorted-vec (map :entity/dev-id (get grouped :value-proposition)))
     :business/roles (sorted-vec (map :entity/dev-id (get grouped :user-role)))
     :business/experiences (sorted-vec (map :entity/dev-id (get grouped :user-experience)))}))

(defn- extract-business-metadata
  "Extract type-specific metadata from business entity properties."
  [props biz-type]
  (case biz-type
    :business-pattern
    {:principle (:semantic-namespace/principle props)
     :justification (:semantic-namespace/justification props)
     :user-experience (:atlas/experience-journey props)
     :business-value (:semantic-namespace/business-value props)
     :alternative-rejected (:semantic-namespace/alternative-rejected props)
     :why-rejected (:semantic-namespace/why-rejected props)}

    :constraint
    {:enforced-by (:semantic-namespace/enforced-by props)
     :rationale (:semantic-namespace/rationale props)
     :compliance-requirement (:semantic-namespace/compliance-requirement props)
     :violation-response (:semantic-namespace/violation-response props)
     :user-sees (:semantic-namespace/user-sees props)
     :business-impact (:semantic-namespace/business-impact props)}

    :failure-mode
    {:triggered-by (:semantic-namespace/triggered-by props)
     :detection (:semantic-namespace/detection props)
     :user-experiences (:semantic-namespace/user-experiences props)
     :recovery-path (:semantic-namespace/recovery-path props)
     :recovery-steps (:semantic-namespace/recovery-steps props)
     :data-loss (:semantic-namespace/data-loss props)
     :business-impact (:semantic-namespace/business-impact props)
     :frequency (:semantic-namespace/frequency props)
     :preventable (:semantic-namespace/preventable props)}

    :value-proposition
    {:business-problem (:semantic-namespace/business-problem props)
     :before-state (:semantic-namespace/before-state props)
     :after-state (:semantic-namespace/after-state props)
     :solution (:semantic-namespace/solution props)
     :user-segment (:semantic-namespace/user-segment props)
     :business-value (:semantic-namespace/business-value props)
     :competitive-advantage (:semantic-namespace/competitive-advantage props)}

    :user-role
    {:description (:semantic-namespace/description props)
     :cannot-access (:semantic-namespace/cannot-access props)
     :responsibilities (:semantic-namespace/responsibilities props)
     :expectations (:semantic-namespace/expectations props)
     :data-access (:semantic-namespace/data-access props)
     :granted-by (:semantic-namespace/granted-by props)
     :security-requirement (:semantic-namespace/security-requirement props)}

    :user-experience
    {:user-journey (:semantic-namespace/user-journey props)
     :time-to-complete (:semantic-namespace/time-to-complete props)
     :friction-points (:semantic-namespace/friction-points props)
     :user-sentiment (:semantic-namespace/user-sentiment props)
     :failure-mode (:atlas/risk-failure-mode props)
     :why-designed-this-way (:semantic-namespace/why-designed-this-way props)
     :delivers-value (:semantic-namespace/delivers-value props)}

    {}))

(defn business-entity-info
  "Get detailed information about a business entity including metadata and implementations."
  [dev-id]
  (let [dev-id-kw (ensure-keyword dev-id)
        [id props] (query/find-by-dev-id @cid/registry dev-id-kw)]
    (when id
      (let [biz-type (business-entity-type id)]
        (cond-> {:entity/dev-id dev-id-kw
                 :entity/type biz-type
                 :entity/identity (vec (sort id))}
          biz-type (assoc :business/metadata (extract-business-metadata props biz-type))
          biz-type (assoc :business/implements-in
                         (sorted-vec (entities-with-aspect dev-id-kw))))))))

(defn entities-implementing
  "Find all technical entities that implement/have a given business aspect."
  [business-aspect]
  (let [aspect-kw (ensure-keyword business-aspect)]
    (vec (sort (entities-with-aspect aspect-kw)))))

(defn business-aspects-of
  "Get all business aspects applied to a technical entity, grouped by type."
  [dev-id]
  (let [dev-id-kw (ensure-keyword dev-id)
        id (rt/identity-for dev-id-kw)
        ;; Filter aspects by business namespaces (pattern/, constraint/, etc.)
        business-namespaces #{"pattern" "constraint" "failure-mode"
                             "value" "role" "experience"}
        business-aspects (filter (fn [aspect]
                                   (when (keyword? aspect)
                                     (some #{(namespace aspect)} business-namespaces)))
                                 id)
        ;; Group by their type based on namespace
        grouped (reduce (fn [acc aspect]
                          (let [ns (namespace aspect)]
                            (case ns
                              "pattern" (update acc :patterns (fnil conj []) aspect)
                              "constraint" (update acc :constraints (fnil conj []) aspect)
                              "failure-mode" (update acc :failure-modes (fnil conj []) aspect)
                              "value" (update acc :values (fnil conj []) aspect)
                              "role" (update acc :roles (fnil conj []) aspect)
                              "experience" (update acc :experiences (fnil conj []) aspect)
                              acc)))
                        {}
                        business-aspects)]
    (cond-> {:entity/dev-id dev-id-kw}
      (:patterns grouped) (assoc :business/patterns (sorted-vec (:patterns grouped)))
      (:constraints grouped) (assoc :business/constraints (sorted-vec (:constraints grouped)))
      (:failure-modes grouped) (assoc :business/failure-modes (sorted-vec (:failure-modes grouped)))
      (:values grouped) (assoc :business/values (sorted-vec (:values grouped)))
      (:roles grouped) (assoc :business/roles (sorted-vec (:roles grouped)))
      (:experiences grouped) (assoc :business/experiences (sorted-vec (:experiences grouped))))))

;; =============================================================================
;; EXPLORER API (v2 dual-map filtering)
;; =============================================================================

(defn explorer-filter-entities
  "Filter entities matching AND/OR aspect criteria, sorted by semantic distance.
   - Entity matches if it has ALL aspects from aspects-and
   - OR entity matches if it has ANY aspect from aspects-or
   - Results sorted by Jaccard similarity to the query aspects (closest first)
   Returns vector of {:dev-id :type :similarity :shared :unique} maps."
  [aspects-and aspects-or]
  (let [registry @cid/registry
        query-aspects (set/union (set aspects-and) (set aspects-or))]
    (when (seq query-aspects)
      (->> registry
           (filter (fn [[identity _props]]
                     (let [matches-and? (and (seq aspects-and)
                                             (every? #(contains? identity %) aspects-and))
                           matches-or? (and (seq aspects-or)
                                            (some #(contains? identity %) aspects-or))]
                       (or matches-and? matches-or?))))
           (map (fn [[identity props]]
                  (let [entity-aspects (cid/aspects identity)
                        shared (set/intersection query-aspects entity-aspects)
                        unique-to-entity (set/difference entity-aspects query-aspects)
                        union (set/union query-aspects entity-aspects)
                        similarity (if (seq union)
                                     (/ (count shared) (count union))
                                     0.0)]
                    {:dev-id (:atlas/dev-id props)
                     :type (to-string (:atlas/type props))
                     :similarity (double similarity)
                     :shared (sorted-vec shared)
                     :unique (sorted-vec unique-to-entity)})))
           (filter :dev-id)
           ;; Sort by similarity (descending), then by dev-id for stability
           (sort-by (juxt (comp - :similarity) :dev-id))
           vec))))

(defn similar-with-diff
  "Find similar entities to a compound identity, showing aspect differences.
   Returns entities sorted by similarity with shared and unique aspects highlighted."
  [compound-identity]
  (let [compound-identity-set (if (set? compound-identity)
                                 compound-identity
                                 (set compound-identity))
        query-aspects (cid/aspects compound-identity-set)
        registry @cid/registry]
    (->> registry
         (keep (fn [[identity props]]
                 (when (not= identity compound-identity-set)
                   (let [entity-aspects (cid/aspects identity)
                         shared (set/intersection query-aspects entity-aspects)
                         unique-to-query (set/difference query-aspects entity-aspects)
                         unique-to-entity (set/difference entity-aspects query-aspects)
                         union (set/union query-aspects entity-aspects)
                         similarity (if (seq union)
                                      (/ (count shared) (count union))
                                      0.0)]
                     (when (pos? similarity)  ; Only include entities with some overlap
                       {:dev-id (:atlas/dev-id props)
                        :similarity (double similarity)
                        :shared (sorted-vec shared)
                        :unique-to-query (sorted-vec unique-to-query)
                        :unique-to-entity (sorted-vec unique-to-entity)})))))
         (sort-by (juxt (comp - :similarity) :dev-id))
         vec)))

;; =============================================================================
;; LLM EXPORT API
;; =============================================================================

(defn llm-context
  "Get full documentation context optimized for LLM consumption."
  []
  (docs/llm-documentation-context))
