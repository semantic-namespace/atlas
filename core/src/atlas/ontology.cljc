(ns atlas.ontology
  "Comprehensive tooling for effortless semantic ontology development"
  (:require [atlas.registry :as registry]
            [atlas.query :as q]
            [atlas.registry.lookup :as entity]
            [clojure.set :as set]
            [clojure.string :as str]))

;; =============================================================================
;; ONTOLOGY QUERIES - Query ontologies from registry
;; =============================================================================

(defn all-ontologies
  "Return all registered ontologies from the registry."
  []
  (q/find-by-aspect @registry/registry :atlas/ontology))

(defn ontology-for
  "Return the ontology definition for a given entity type from the registry.
   Returns nil if not found."
  [entity-type]
  (let [results (q/find-by-aspect @registry/registry #{:atlas/ontology entity-type})]
    (when (seq results)
      (val (first results)))))

(defn ontology-keys-for
  "Return the ontology keys for a given entity type from the registry."
  [entity-type]
  (:ontology/keys (ontology-for entity-type)))

;; =============================================================================
;; DEFINITION HELPERS - For backward compatibility
;; =============================================================================

(def ^:private common-registry-keys
  [:atlas/dev-id])

(defn registry-definition-for
  "Return registry definition for a semantic-namespace aspect.
   Queries from the registry (requires register-ontologies! to be called first)."
  [aspect]
  (when-let [ont (ontology-for aspect)]
    {:registry-definition/for (:ontology/for ont)
     :registry-definition/keys (:ontology/keys ont)}))

(defn- ordered-distinct
  [coll]
  (reduce (fn [acc item]
            (if (some #{item} acc)
              acc
              (conj acc item)))
          []
          coll))

(defn definition-keys-for-identity
  "Return ordered definition keys for a compound identity set.
   Queries from the registry (requires register-ontologies! to be called first)."
  [identity]
  (let [entity-types (filter #(= "atlas" (namespace %)) identity)
        ontologies (keep ontology-for entity-types)
        keys (ordered-distinct (concat common-registry-keys
                                       (mapcat :ontology/keys ontologies)))]
    (when (seq ontologies)
      (vec keys))))

;; =============================================================================
;; DISCOVERY TOOLS - Understanding what exists
;; =============================================================================

(defn aspect-catalog
  "Show all aspects with usage stats and examples"
  []
  (let [freq (q/aspect-frequency @registry/registry)]
    (->> freq
         (map (fn [[aspect count]]
                (let [examples (take 2 (filter #(contains? % aspect)
                                               (q/all-identities @registry/registry)))]
                  {:aspect aspect
                   :usage-count count
                   :examples (map #(:atlas/dev-id (registry/fetch %)) examples)})))
         (group-by #(namespace (:aspect %)))
         (sort-by key))))

(defn suggest-aspects
  "Given partial identity, suggest aspects based on similar entries"
  [partial-identity]
  (let [similar (q/semantic-similarity @registry/registry partial-identity 0.0)
        all-aspects (->> similar
                         (take 5)
                         (mapcat :identity)
                         frequencies
                         (remove (fn [[asp _]] (contains? partial-identity asp)))
                         (sort-by val >)
                         (take 10))]
    {:similar-entries (take 3 (map :identity similar))
     :suggested-aspects (map first all-aspects)
     :rationale "Based on 5 most similar entries"}))

(defn impact-analysis
  "Analyze what would be affected by changes to an aspect"
  [aspect]
  (let [direct-users (q/find-by-aspect @registry/registry aspect)
        by-tier (group-by (fn [[id _]]
                            (cond
                              (contains? id :tier/foundation) :foundation
                              (contains? id :tier/service) :service
                              (contains? id :tier/feature) :feature
                              (contains? id :tier/api) :api
                              :else :other))
                          direct-users)
        by-type (group-by (fn [[id _]]
                            (cond
                              (contains? id :atlas/structure-component) :component
                              (contains? id :atlas/execution-function) :function
                              (contains? id :atlas/interface-endpoint) :endpoint
                              (contains? id :semantic-namespace/error-handler) :error-handler
                              (contains? id :semantic-namespace/architectural-decision) :decision
                              :else :other))
                          direct-users)]
    {:aspect aspect
     :total-impact (count direct-users)
     :by-tier (into {} (map (fn [[k v]] [k (count v)]) by-tier))
     :by-type (into {} (map (fn [[k v]] [k (count v)]) by-type))
     :examples (take 3 (map (fn [[_ v]] (:atlas/dev-id v)) direct-users))}))

;; =============================================================================
;; DATA FLOW TOOLS - Tracing context/response relationships
;; =============================================================================
;;
;; These functions trace data flow based on context/response patterns
;; defined by ontologies via :dataflow/* keys.


;; =============================================================================
;; DATAFLOW LOOKUPS
;; =============================================================================
;;
;; These functions query ontologies for :dataflow/* keys to find context,
;; response, and deps properties for entities.

;; Default dataflow keys - empty by default.
;; All dataflow keys come from ontology modules:
;; - (ef/load!) for execution-function
;; - (sc/load!) for structure-component
;; - (ie/load!) for interface-endpoint
(def ^:private default-dataflow-keys
  {:dataflow/context-key []
   :dataflow/response-key []
   :dataflow/deps-key []})

(defn- dataflow-keys
  "Find all dataflow keys of given type from registered ontologies.
   key-type is one of: :dataflow/context-key, :dataflow/response-key, :dataflow/deps-key
   Merges defaults with ontology-defined keys."
  [key-type]
  (let [from-ontologies (->> (all-ontologies)
                              (keep (fn [[_ ont]] (get ont key-type)))
                              vec)
        defaults (get default-dataflow-keys key-type [])]
    (vec (distinct (concat defaults from-ontologies)))))

(defn- first-present
  [props keys]
  (some (fn [k]
          (when (contains? props k)
            (get props k)))
        keys))

(defn deps-for [dev-id]
  (let [deps-keys (dataflow-keys :dataflow/deps-key)
        deps (first-present (entity/props-for dev-id) deps-keys)]
    (cond
      (set? deps) deps
      (coll? deps) (set deps)
      :else #{})))

(defn context-for [dev-id]
  (let [context-keys (dataflow-keys :dataflow/context-key)]
    (or (first-present (entity/props-for dev-id) context-keys) [])))

(defn response-for [dev-id]
  (let [response-keys (dataflow-keys :dataflow/response-key)]
    (or (first-present (entity/props-for dev-id) response-keys) [])))

(defn trace-data-flow
  "Trace what produces each context key for a given entity.

   Queries ontologies for :dataflow/response-key to find which response keys
   from other entities can satisfy context requirements.

   Returns a sequence of maps:
     {:needs <key>
      :produced-by [<dev-ids>]
      :satisfied? <boolean>}"
  [dev-id]
  (let [context (context-for dev-id)
        response-keys (dataflow-keys :dataflow/response-key)]
    (for [ctx-key context
          :let [producers-map (reduce (fn [acc key]
                                        (merge acc (q/find-producers @registry/registry ctx-key key)))
                                      {}
                                      response-keys)
                function-producers (q/find-by-aspect @registry/registry :atlas/execution-function)
                producer-ids (->> producers-map
                                  (keep (fn [[id v]]
                                          (when (contains? function-producers id)
                                            (:atlas/dev-id v))))
                                  vec)]]
      {:needs ctx-key
       :produced-by producer-ids
       :satisfied? (seq producer-ids)})))

(defn compute-data-deps
 "Compute dependencies based on context/response data flow.

   Finds all entities whose response keys can satisfy the context
   requirements of the given entity.

   Returns a set of dev-ids that produce data consumed by dev-id."
  [dev-id]
  (let [context (set (context-for dev-id))
        all-fns (entity/all-with-aspect :atlas/execution-function)
        response-keys (dataflow-keys :dataflow/response-key)]
    (->> context
         (mapcat (fn [ctx-key]
                   (let [producers (reduce (fn [acc key]
                                             (merge acc (q/find-producers @registry/registry ctx-key key)))
                                           {}
                                           response-keys)]
                     (->> producers
                          (keep (fn [[_ v]] (:atlas/dev-id v)))
                          (filter all-fns)))))
         set)))

;; =============================================================================
;; VALIDATION TOOLS - Preventing errors before registration
;; =============================================================================

(defn validate-before-register
  "Check if identity is valid before registering"
  [new-identity]
  (let [issues []
        symmetric-diff (fn [a b]
                         (set/union
                          (set/difference a b)
                          (set/difference b a)))]
    (-> issues
        (into (when (contains? @registry/registry new-identity)
                [{:level :error
                  :type :collision
                  :message "Identity already exists"
                  :existing-dev-id (:atlas/dev-id (registry/fetch new-identity))}]))
        (into (keep (fn [existing-id]
                      (let [diff (symmetric-diff new-identity existing-id)]
                        (when (= 2 (count diff))
                          {:level :warning
                           :type :near-collision
                           :message "Differs by only one aspect from existing"
                           :existing-dev-id (:atlas/dev-id (registry/fetch existing-id))
                           :difference diff})))
                    (take 100 (q/all-identities @registry/registry))))
        (into (when (and (contains? new-identity :integration/external)
                         (not (contains? new-identity :temporal/async)))
                [{:level :error
                  :type :invariant-violation
                  :message "External integrations must be async"}]))
        (into (when (and (contains? new-identity :compliance/pii)
                         (not (contains? new-identity :compliance/audited)))
                [{:level :warning
                  :type :compliance-gap
                  :message "PII handling should be audited"}])))))

;; =============================================================================
;; QUERY TOOLS - Fluent querying
;; =============================================================================

(defn where->
  "Fluent query builder"
  [initial-aspects]
  {:aspects (if (set? initial-aspects) initial-aspects #{initial-aspects})
   :ops []})

(defn with-aspect [query aspect]
  (update query :aspects conj aspect))

(defn without-aspect [query aspect]
  (update query :ops conj [:exclude aspect]))

(defn in-tier [query tier]
  (with-aspect query tier))

(defn execute-query [query]
  (let [{:keys [aspects ops]} query
        base-results (q/find-by-aspect @registry/registry aspects)
        filtered (reduce (fn [results [op arg]]
                           (case op
                             :exclude (into {} (remove (fn [[id _]] (contains? id arg)) results))
                             results))
                         base-results
                         ops)]
    (keys filtered)))

;; =============================================================================
;; TEMPLATE TOOLS - Reducing boilerplate
;; =============================================================================

;; NOTE: Templates have been moved to their respective ontology modules:
;;
;; For execution-function templates:
;;   (require '[atlas.ontology.execution-function :as ef])
;;   (ef/load!)
;;   (ef/template:service-function :domain/x :operation/y)
;;   (ef/template:api-endpoint :domain/x :operation/y)
;;
;; For structure-component templates:
;;   (require '[atlas.ontology.structure-component :as sc])
;;   (sc/load!)
;;   (sc/template:foundation-component :domain/x :external? true)

;; =============================================================================
;; REFACTORING TOOLS - Safe evolution
;; =============================================================================

(defn refactor-aspect
  "Safely rename/replace an aspect across the registry"
  [old-aspect new-aspect & {:keys [dry-run?] :or {dry-run? true}}]
  (let [affected (filter #(contains? % old-aspect) (q/all-identities @registry/registry))
        migrations (map (fn [old-id]
                          (let [new-id (-> old-id (disj old-aspect) (conj new-aspect))
                                value (registry/fetch old-id)]
                            {:old-id old-id
                             :new-id new-id
                             :dev-id (:atlas/dev-id value)
                             :will-collide? (contains? @registry/registry new-id)}))
                        affected)]
    (if dry-run?
      {:dry-run true
       :affected-count (count migrations)
       :migrations migrations
       :conflicts (filter :will-collide? migrations)}
      (do
        (doseq [{:keys [old-id new-id]} migrations
                :when (not (:will-collide? (first migrations)))]
          (let [value (registry/fetch old-id)]
            (registry/remove old-id)
            (swap! registry/registry assoc new-id value)))
        {:migrated (count migrations)}))))

;; =============================================================================
;; DOCUMENTATION TOOLS - Auto-generation
;; =============================================================================

(defn generate-ontology-docs
  "Generate markdown documentation of the ontology"
  []
  (let [catalog (aspect-catalog)
        by-category (group-by (fn [[ns _]]
                                (cond
                                  (= ns "tier") "Architecture Layers"
                                  (= ns "domain") "Business Domains"
                                  (= ns "operation") "Operations"
                                  (= ns "compliance") "Compliance"
                                  (= ns "temporal") "Temporal Concerns"
                                  (= ns "observability") "Observability"
                                  (= ns "integration") "Integration"
                                  (= ns "protocol") "Protocols"
                                  :else "Other"))
                              catalog)]
    (apply str
           "# Semantic Ontology Documentation\n\n"
           "Generated from live registry.\n\n"
           (for [[category aspects] (sort-by key by-category)]
             (str "## " category "\n\n"
                  (apply str
                         (for [[_ aspect-list] aspects
                               {:keys [aspect usage-count examples]} aspect-list]
                           (str "### " aspect "\n"
                                "- **Usage**: " usage-count " entries\n"
                                "- **Examples**: " (str/join ", " examples) "\n\n"))))))))

;; =============================================================================
;; REPL TOOLS - Interactive exploration
;; =============================================================================

(defn help-ontology
  "Interactive help system"
  []
  (println "=== Semantic Ontology Tooling ===\n")
  (println "Discovery:")
  (println "  (aspect-catalog)              - Browse all aspects with usage")
  (println "  (suggest-aspects #{...})      - Get aspect suggestions")
  (println "  (impact-analysis :aspect/x)   - See impact of changes")
  (println)
  (println "Validation:")
  (println "  (validate-before-register #{...}) - Check before registering")
  (println)
  (println "Querying:")
  (println "  (where-> :domain/x)           - Fluent query builder")
  (println "  (-> (where-> :domain/x) (with-aspect :tier/service) execute-query)")
  (println)
  (println "Templates:")
  (println "  (template:foundation-component :domain/x :external? true)")
  (println "  ; For execution-function templates, load the EF ontology:")
  (println "  (require '[atlas.ontology.execution-function :as ef])")
  (println "  (ef/load!)")
  (println "  (ef/template:service-function :domain/x :operation/y :pii? true)")
  (println "  (ef/template:api-endpoint :domain/x :operation/y :rate-limited? true)")
  (println)
  (println "Refactoring:")
  (println "  (refactor-aspect :old :new :dry-run? true)")
  (println)
  (println "Documentation:")
  (println "  (generate-ontology-docs)      - Generate markdown docs")
  nil)

(defn inspect
  "Quick inspection of a dev-id"
  [dev-id]
  (let [matches (filter #(= dev-id (:atlas/dev-id (registry/fetch %))) (q/all-identities @registry/registry))]
    (if (seq matches)
      (let [id (first matches)
            val (registry/fetch id)]
        {:dev-id dev-id
         :semantic-identity id
         :aspect-count (count id)
         :value val
         :docs (:docs/content val)})
      {:error "Not found" :dev-id dev-id})))

;; =============================================================================
;; Type Registration
;; =============================================================================

(defn register-entity-types!
  "Register all entity types in the registry with :atlas/type.

  This makes entity types discoverable and enables runtime type validation.
  Call this once during initialization, after register-ontologies!."
  []
  (let [ontologies (all-ontologies)]
    (doseq [[_ ont] ontologies
            :let [entity-type (:ontology/for ont)]]
      (registry/register!
       entity-type
       :atlas/type
       #{}
       {:registry-definition/keys (vec (cons :atlas/dev-id (:ontology/keys ont)))}))
    (count ontologies)))

;; =============================================================================
;; Ontology Registration - Ontologies as semantic entities
;; =============================================================================

;; loading this namespace already register a enitity
(registry/register!
   :ontology/ontology
   :atlas/ontology
   #{:atlas/ontology}
   {:ontology/for :atlas/ontology
    :ontology/keys [:ontology/for :ontology/keys]})

(defn load-ontology
  "Load a built-in ontology by type.

   For :atlas/execution-function, use atlas.ontology.execution-function/load! instead.
   This provides a cleaner separation and allows the EF ontology to be truly optional."
  [ontology-type]

  (condp = ontology-type
    ;; NOTE: :atlas/execution-function is now loaded via
    ;; (require '[atlas.ontology.execution-function :as ef]) (ef/load!)
    ;; This allows execution-function to be truly optional.

    ;; NOTE: :atlas/structure-component is now loaded via
    ;; (require '[atlas.ontology.structure-component :as sc]) (sc/load!)
    ;; This allows structure-component to be truly optional.

    ;; NOTE: :atlas/interface-endpoint is now loaded via
    ;; (require '[atlas.ontology.interface-endpoint :as ie]) (ie/load!)
    ;; This allows interface-endpoint to be truly optional.

    :atlas/interface-protocol  (registry/register!
                                   :atlas/interface-protocol
                                   :atlas/ontology
                                   #{:atlas/interface-protocol}
                                   {:ontology/for :atlas/interface-protocol
                                    :ontology/keys [:interface-protocol/functions]})

    :atlas/data-schema  (registry/register!
                            :atlas/data-schema
                            :atlas/ontology
                            #{:atlas/data-schema}
                            {:ontology/for :atlas/data-schema
                             :ontology/keys [:data-schema/fields]})

    :atlas/business-pattern  (registry/register!
                                 :atlas/business-pattern
                                 :atlas/ontology
                                 #{:atlas/business-pattern}
                                 {:ontology/for :atlas/business-pattern
                                  :ontology/keys [:business-pattern/principle
                                                  :business-pattern/justification
                                                  :business-pattern/experience-journey
                                                  :business-pattern/failure-recovery
                                                  :business-pattern/alternative-rejected
                                                  :business-pattern/why-rejected
                                                  :business-pattern/business-value
                                                  :business-pattern/metrics-improved]})

    :atlas/governance-constraint  (registry/register!
                                      :atlas/governance-constraint
                                      :atlas/ontology
                                      #{:atlas/governance-constraint}
                                      {:ontology/for :atlas/governance-constraint
                                       :ontology/keys [:governance-constraint/enforced-by
                                                       :governance-constraint/rationale
                                                       :governance-constraint/compliance-requirement
                                                       :governance-constraint/violation-response
                                                       :governance-constraint/user-sees
                                                       :governance-constraint/business-impact
                                                       :governance-constraint/google-oauth-scope
                                                       :governance-constraint/user-benefit
                                                       :governance-constraint/alternative-rejected
                                                       :governance-constraint/why-rejected
                                                       :governance-constraint/user-experiences
                                                       :governance-constraint/recovery-path
                                                       :governance-constraint/revocable
                                                       :governance-constraint/revocation-path]})

    :atlas/risk-failure-mode  (registry/register!
                                  :atlas/risk-failure-mode
                                  :atlas/ontology
                                  #{:atlas/risk-failure-mode}
                                  {:ontology/for :atlas/risk-failure-mode
                                   :ontology/keys [:risk-failure-mode/triggered-by
                                                   :risk-failure-mode/detection
                                                   :risk-failure-mode/user-experiences
                                                   :risk-failure-mode/recovery-path
                                                   :risk-failure-mode/recovery-steps
                                                   :risk-failure-mode/data-loss
                                                   :risk-failure-mode/business-impact
                                                   :risk-failure-mode/frequency
                                                   :risk-failure-mode/preventable
                                                   :risk-failure-mode/why-not-preventable
                                                   :risk-failure-mode/prevention-strategy
                                                   :risk-failure-mode/security-event
                                                   :risk-failure-mode/logged
                                                   :risk-failure-mode/log-details]})

    :atlas/value-proposition  (registry/register!
                                  :atlas/value-proposition
                                  :atlas/ontology
                                  #{:atlas/value-proposition}
                                  {:ontology/for :atlas/value-proposition
                                   :ontology/keys [:value-proposition/business-problem
                                                   :value-proposition/before-state
                                                   :value-proposition/after-state
                                                   :value-proposition/time-saved
                                                   :value-proposition/solution
                                                   :value-proposition/metrics-improved
                                                   :value-proposition/user-segment
                                                   :value-proposition/business-value-quantified
                                                   :value-proposition/business-value
                                                   :value-proposition/competitive-advantage
                                                   :value-proposition/implements-pattern
                                                   :value-proposition/trust-factors
                                                   :value-proposition/risk-mitigation
                                                   :value-proposition/compliance-benefit]})

    :atlas/identity-role  (registry/register!
                              :atlas/identity-role
                              :atlas/ontology
                              #{:atlas/identity-role}
                              {:ontology/for :atlas/identity-role
                               :ontology/keys [:identity-role/description
                                               :identity-role/cannot-access
                                               :identity-role/responsibilities
                                               :identity-role/expectations
                                               :identity-role/data-access
                                               :identity-role/granted-by
                                               :identity-role/security-requirement
                                               :identity-role/audit-logged
                                               :identity-role/typical-users
                                               :identity-role/privacy-constraint]})

    :atlas/experience-journey  (registry/register!
                                   :atlas/experience-journey
                                   :atlas/ontology
                                   #{:atlas/experience-journey}
                                   {:ontology/for :atlas/experience-journey
                                    :ontology/keys [:experience-journey/user-journey
                                                    :experience-journey/time-to-complete
                                                    :experience-journey/friction-points
                                                    :experience-journey/why-designed-this-way
                                                    :experience-journey/user-sentiment
                                                    :experience-journey/risk-failure-mode
                                                    :experience-journey/recovery-time
                                                    :experience-journey/delivers-value
                                                    :experience-journey/replaces]})))


