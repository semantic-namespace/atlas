(ns atlas.datalog.facts
  "Datascript-free fact and schema builders for the registry's datalog view."
  (:require [atlas.registry :as registry]
            [atlas.query :as q]))

;; =============================================================================
;; FACT GENERATION
;; =============================================================================

(defn core-registry-facts
  "Extract core Atlas facts (always included).
   These are universal facts that apply to all entities."
  ([] (core-registry-facts (registry/current-registry)))
  ([reg]
   (let [facts (atom [])]
     (doseq [[compound-id props] reg
             :let [dev-id (:atlas/dev-id props)]]
       (when dev-id
         ;; Entity existence
         (swap! facts conj [:db/add dev-id :atlas/dev-id dev-id])

         ;; Aspects (universal - all entities have aspects)
         (doseq [aspect compound-id]
           (swap! facts conj [:db/add dev-id :entity/aspect aspect]))

         ;; TODO this should be removed
         ;; Dataflow terminal markers (core markers)
         (when (contains? compound-id :dataflow/external-input)
           (swap! facts conj [:db/add dev-id :dataflow/external-input dev-id]))
         (when (contains? compound-id :dataflow/display-output)
           (swap! facts conj [:db/add dev-id :dataflow/display-output dev-id]))))
     @facts)))

(defn custom-registry-facts
  "Extract facts using extractors discovered from the registry.
   Finds all :atlas/datalog-extractor entities and calls their :datalog-extractor/fn."
  ([] (custom-registry-facts (registry/current-registry)))
  ([reg]
   (let [extractors (->> (q/find-by-aspect reg :atlas/datalog-extractor)
                         vals
                         (map :datalog-extractor/fn)
                         (remove nil?))
         facts (atom [])]
     (doseq [[compound-id props] reg
             extractor extractors]
       (when-let [extracted (seq (extractor compound-id props))]
         (swap! facts concat extracted)))
     @facts)))

(defn registry-facts
  "Convert registry into Datascript facts (core + custom).
   Returns vector of facts: [:db/add entity-id attribute value]"
  ([] (registry-facts (registry/current-registry)))
  ([reg]
   (concat (core-registry-facts reg)
           (custom-registry-facts reg))))

;; =============================================================================
;; GENERIC FACT EXTRACTION (from registry metadata, no functions needed)
;; =============================================================================
;;
;; These functions derive datalog facts entirely from declarative metadata
;; (type-refs and ontology verb declarations). They work on serialised
;; snapshots where :datalog-extractor/fn is not available.

(defn- ontology-entries
  "Find all ontology entries in a registry map."
  [reg]
  (->> reg
       (filter (fn [[compound-id _]] (contains? compound-id :atlas/ontology)))
       (map (fn [[_ props]] props))))

(defn- type-ref-entries
  "Find all type-ref entries in a registry map."
  [reg]
  (->> reg
       (filter (fn [[compound-id _]] (contains? compound-id :atlas/type-ref)))
       (map (fn [[_ props]] props))))

(def ^:private many {:db/cardinality :db.cardinality/many})

(defn- merge-schema
  [& schemas]
  (apply merge-with
         (fn [a b]
           (cond-> (merge a b)
             (some #(= many (select-keys % [:db/cardinality])) [a b]) (merge many)))
         schemas))

(defn- type-ref-verb-schema
  "Datascript schema contributed by type-refs: each type-ref's datalog-verb ->
   its cardinality, many winning when two type-refs share a verb."
  [reg]
  (->> (type-ref-entries reg)
       (map (fn [tr]
              {(:type-ref/datalog-verb tr)
               {:db/cardinality (:type-ref/cardinality tr)}}))
       (apply merge-schema)))

(defn- as-values [v]
  (if (coll? v) v [v]))

(defn- key-list [raw-key]
  (cond (nil? raw-key) []
        (and (vector? raw-key) (every? keyword? raw-key)) raw-key
        :else [raw-key]))

(defn- deps-key-values [v]
  (cond (coll? v) v
        (qualified-keyword? v) [v]
        :else []))

(defn- extraction-index [reg]
  {:ontology-for (reduce (fn [m ont] (if (contains? m (:ontology/for ont)) m (assoc m (:ontology/for ont) ont)))
                         {}
                         (ontology-entries reg))
   :type-refs-for (group-by :type-ref/source (type-ref-entries reg))})

(defn- extract-facts
  [{:keys [ontology-for type-refs-for]} props]
  (let [dev-id (:atlas/dev-id props)
        entity-type (:atlas/type props)
        ont (get ontology-for entity-type)
        add (fn [verb vs] (map (fn [v] [:db/add dev-id verb v]) vs))
        dataflow (fn [raw-key verb]
                   (when verb
                     (mapcat #(when-let [vs (get props %)] (add verb (as-values vs)))
                             (key-list raw-key))))]
    (when (and dev-id entity-type)
      (concat
       (mapcat (fn [{:type-ref/keys [property datalog-verb cardinality]}]
                 (when-let [value (get props property)]
                   (if (= cardinality :db.cardinality/many)
                     (add datalog-verb (as-values value))
                     [[:db/add dev-id datalog-verb value]])))
               (get type-refs-for entity-type))
       (when ont
         (concat
          (dataflow (:dataflow/context-key ont) (:dataflow/context-verb ont))
          (dataflow (:dataflow/response-key ont) (:dataflow/response-verb ont))
          (mapcat #(add :entity/depends (deps-key-values (get props %)))
                  (key-list (:dataflow/deps-key ont)))
          (mapcat (fn [[source-key target-verb]]
                    (when-let [vs (get props source-key)]
                      (add target-verb (as-values vs))))
                  (:dataflow/extra-verbs ont))))))))

(defn generic-extract-facts
  "Extract datalog facts from a single entity using only registry metadata:
   type-refs (property -> verb), and the entity's ontology (context -> consumes,
   response -> produces, deps-key -> :entity/depends, extra verbs)."
  [reg _compound-id props]
  (extract-facts (extraction-index reg) props))

(defn generic-all-facts
  "Extract all datalog facts from a registry using only metadata.
   No extractor functions needed — works on cloud snapshots."
  [reg]
  (let [idx (extraction-index reg)]
    (into [] (mapcat (fn [[_ props]] (extract-facts idx props))) reg)))

(defn core-schema
  "Core Atlas schema (always included).
   These attributes are fundamental to all Atlas entities."
  []
  {:atlas/dev-id {:db/unique :db.unique/identity}
   :entity/aspect {:db/cardinality :db.cardinality/many}
   ;; Dataflow markers (core markers for terminal nodes)
   :dataflow/external-input {:db/cardinality :db.cardinality/many}
   :dataflow/display-output {:db/cardinality :db.cardinality/many}})

(defn- extractor-schemas [reg]
  (->> (q/find-by-aspect reg :atlas/datalog-extractor)
       vals
       (map :datalog-extractor/schema)
       (remove nil?)))

(defn build-schema
  "Build complete schema (core + contributions from ontology extractors)."
  ([] (build-schema (registry/current-registry)))
  ([reg]
   (apply merge-schema (core-schema) (type-ref-verb-schema reg) (extractor-schemas reg))))

(defn derive-schema
  "Derive datascript schema from type-ref and ontology metadata in a registry.
   No :datalog-extractor/schema needed — works on cloud snapshots."
  [reg]
  (merge-schema
   (core-schema)
   (type-ref-verb-schema reg)
   (->> (ontology-entries reg)
        (mapcat (fn [ont]
                  (concat
                   (when-let [v (:dataflow/context-verb ont)] [{v many}])
                   (when-let [v (:dataflow/response-verb ont)] [{v many}])
                   (when (:dataflow/deps-key ont) [{:entity/depends many}])
                   (for [[_ verb] (:dataflow/extra-verbs ont)] {verb many}))))
        (apply merge-schema))))

(defn schema
  ([] (schema (registry/current-registry)))
  ([reg] (merge-schema (derive-schema reg) (build-schema reg))))

(defn snapshot-facts
  [reg]
  (concat (core-registry-facts reg)
          (generic-all-facts reg)))

(defn all-facts
  ([] (all-facts (registry/current-registry)))
  ([reg]
   (concat (core-registry-facts reg)
           (custom-registry-facts reg)
           (generic-all-facts reg))))
