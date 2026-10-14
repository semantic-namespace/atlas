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

(defn- type-refs-for-source-in
  "Find type-refs for a given source ontology in a registry map."
  [reg source-ontology]
  (->> (type-ref-entries reg)
       (filter #(= source-ontology (:type-ref/source %)))))

(defn- type-ref-verb-schema
  "Datascript schema contributed by type-refs: each type-ref's datalog-verb →
   its cardinality. Lets any domain introduce a NEW verb just by registering a
   type-ref — no extractor-schema needed. Both the live path (build-schema) and
   the snapshot path (derive-schema) read verb cardinality from here, so an
   extension works identically in-process and on a pulled snapshot."
  [reg]
  (->> (type-ref-entries reg)
       (map (fn [tr]
              {(:type-ref/datalog-verb tr)
               {:db/cardinality (:type-ref/cardinality tr)}}))
       (apply merge)))

(defn generic-extract-facts
  "Extract datalog facts from a single entity using only registry metadata.
   Works on serialised snapshots — no extractor functions needed.

   Uses type-refs for property→verb mappings and ontology declarations
   for dataflow verb mappings (context→consumes, response→produces)."
  [reg compound-id props]
  (let [dev-id (:atlas/dev-id props)
        ;; Use :atlas/type from props as the authoritative entity type.
        ;; Compound-ids can contain inherited types (e.g. VPs inherit :atlas/business-pattern),
        ;; so scanning compound-id with `first` risks picking the wrong ontology.
        ontologies (ontology-entries reg)
        entity-type (:atlas/type props)
        ont-props (first (filter #(= entity-type (:ontology/for %)) ontologies))]
    (when (and dev-id entity-type)
      (concat
       ;; 1. Type-ref facts: property → verb mapping from type-ref declarations
       (let [refs (type-refs-for-source-in reg entity-type)]
         (mapcat
          (fn [type-ref]
            (let [property (:type-ref/property type-ref)
                  verb     (:type-ref/datalog-verb type-ref)
                  card     (:type-ref/cardinality type-ref)
                  value    (get props property)]
              (when value
                (if (= card :db.cardinality/many)
                  (map (fn [v] [:db/add dev-id verb v])
                       (if (coll? value) value [value]))
                  [[:db/add dev-id verb value]]))))
          refs))

       ;; 2. Dataflow facts: context→consumes, response→produces
       ;; context-key/response-key can be a single key or a vector of keys
       (when ont-props
         (letfn [(extract-dataflow [raw-key verb]
                   (when (and raw-key verb)
                     (let [keys (if (and (vector? raw-key)
                                         (every? keyword? raw-key))
                                  raw-key [raw-key])]
                       (mapcat (fn [k]
                                 (when-let [values (get props k)]
                                   (map (fn [v] [:db/add dev-id verb v])
                                        (if (coll? values) values [values]))))
                               keys))))]
           (concat
            (extract-dataflow (:dataflow/context-key ont-props)
                              (:dataflow/context-verb ont-props))
            (extract-dataflow (:dataflow/response-key ont-props)
                              (:dataflow/response-verb ont-props)))))

       ;; 3. Extra verbs (endpoint-specific etc.)
       (when-let [extras (:dataflow/extra-verbs ont-props)]
         (mapcat
          (fn [[source-key target-verb]]
            (when-let [values (get props source-key)]
              (map (fn [v] [:db/add dev-id target-verb v])
                   (if (coll? values) values [values]))))
          extras))))))

(defn generic-all-facts
  "Extract all datalog facts from a registry using only metadata.
   No extractor functions needed — works on cloud snapshots."
  [reg]
  (let [facts (atom [])]
    (doseq [[compound-id props] reg]
      (when-let [extracted (seq (generic-extract-facts reg compound-id props))]
        (swap! facts concat extracted)))
    @facts))

(defn core-schema
  "Core Atlas schema (always included).
   These attributes are fundamental to all Atlas entities."
  []
  {:atlas/dev-id {:db/unique :db.unique/identity}
   :entity/aspect {:db/cardinality :db.cardinality/many}
   ;; Dataflow markers (core markers for terminal nodes)
   :dataflow/external-input {:db/cardinality :db.cardinality/many}
   :dataflow/display-output {:db/cardinality :db.cardinality/many}})

(defn build-schema
  "Build complete schema (core + contributions from ontology extractors)."
  ([] (build-schema (registry/current-registry)))
  ([reg]
   (let [extractor-schemas (->> (q/find-by-aspect reg :atlas/datalog-extractor)
                                vals
                                (map :datalog-extractor/schema)
                                (remove nil?))]
     ;; type-ref-derived verb schema first, extractor schemas last (authoritative
     ;; on conflict). This is what lets a domain add a new type-ref verb (e.g.
     ;; :invariant/governs) and have it schema'd in the live path with no core edit.
     (apply merge (core-schema) (type-ref-verb-schema reg) extractor-schemas))))

(defn derive-schema
  "Derive datascript schema from type-ref and ontology metadata in a registry.
   No :datalog-extractor/schema needed — works on cloud snapshots."
  [reg]
  (merge
   (core-schema)
   ;; From type-refs
   (type-ref-verb-schema reg)
   ;; From dataflow verbs
   (->> (ontology-entries reg)
        (mapcat (fn [ont]
                  (concat
                   (when-let [v (:dataflow/context-verb ont)]
                     [{v {:db/cardinality :db.cardinality/many}}])
                   (when-let [v (:dataflow/response-verb ont)]
                     [{v {:db/cardinality :db.cardinality/many}}])
                   (for [[_ verb] (:dataflow/extra-verbs ont)]
                     {verb {:db/cardinality :db.cardinality/many}}))))
        (apply merge))))


(defn snapshot-facts
  [reg]
  (concat (core-registry-facts reg)
          (generic-all-facts reg)))
