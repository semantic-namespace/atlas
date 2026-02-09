(ns atlas.ontology.type-ref
  "Type-ref ontology - declares references between entity types.

   A type-ref describes how one ontology references another, including:
   - Which property holds the reference
   - How to map it to datalog facts

   This makes references discoverable and enables automatic fact extraction.

   Auto-registers on require."
  (:require [atlas.registry :as registry]
            [atlas.registry.lookup :as entity]))

;; =============================================================================
;; ONTOLOGY DEFINITION
;; =============================================================================

(def ontology-definition
  {:ontology/for :atlas/type-ref
   :ontology/keys [:type-ref/source        ; Source ontology (who has the ref)
                   :type-ref/property      ; Property key in source entity
                   :type-ref/datalog-verb  ; Datalog attribute name
                   :type-ref/cardinality]}) ; :db.cardinality/one or /many

;; =============================================================================
;; HELPER FUNCTIONS
;; =============================================================================

(defn all-type-refs
  "Return all registered type-ref dev-ids."
  []
  (->> (entity/all-with-aspect :atlas/type-ref)
       (remove #(entity/has-aspect? % :atlas/ontology))
       set))

(defn type-refs-for-source
  "Find all type-refs where source matches the given ontology.

   Example:
     (type-refs-for-source :atlas/yorba-cache)
     ;; => #{:type-ref/cache-serialisation}"
  [source-ontology]
  (->> (all-type-refs)
       (filter (fn [ref-id]
                 (= source-ontology
                    (:type-ref/source (entity/props-for ref-id)))))
       vec))


;; =============================================================================
;; GENERIC REFERENCE EXTRACTOR
;; =============================================================================

(defn extract-reference-facts
  "Extract datalog facts for all type-refs of a given source ontology.

   This is a GENERIC extractor that automatically generates facts based on
   registered type-ref metadata. Use this in your ontology's datalog extractor.

   Example usage in yorba-cache extractor:
     (type-ref/extract-reference-facts :atlas/yorba-cache compound-id props)

   Returns a collection of [:db/add ...] facts."
  [source-ontology compound-id props]
  (when (contains? compound-id source-ontology)
    (let [dev-id (:atlas/dev-id props)
          type-refs (type-refs-for-source source-ontology)]
      (mapcat
       (fn [type-ref-id]
         (let [type-ref-props (entity/props-for type-ref-id)
               property (:type-ref/property type-ref-props)
               datalog-verb (:type-ref/datalog-verb type-ref-props)
               cardinality (:type-ref/cardinality type-ref-props)
               value (get props property)]
           (when value
             (if (= cardinality :db.cardinality/many)
               ;; Many cardinality: map over collection
               (map (fn [v] [:db/add dev-id datalog-verb v])
                    (if (coll? value) value [value]))
               ;; One cardinality: single fact
               [[:db/add dev-id datalog-verb value]]))))
       type-refs))))

;; =============================================================================
;; DATALOG INTEGRATION
;; =============================================================================

(def datalog-schema
  "Datascript schema for type-ref properties."
  {:type-ref/source {:db/cardinality :db.cardinality/one}
   :type-ref/property {:db/cardinality :db.cardinality/one}
   :type-ref/datalog-verb {:db/cardinality :db.cardinality/one}
   :type-ref/cardinality {:db/cardinality :db.cardinality/one}})

;; =============================================================================
;; INVARIANTS
;; =============================================================================

(def invariants
  [])

;; =============================================================================
;; AUTO-REGISTRATION (top-level, like clojure.spec)
;; =============================================================================

;; Ontology
(registry/register!
 :atlas/type-ref
 :atlas/ontology
 #{:atlas/type-ref}
 ontology-definition)

;; Datalog extractor
(registry/register!
 :datalog-extractor/type-ref
 :atlas/datalog-extractor
 #{:meta/type-ref-extractor}
 {:datalog-extractor/fn (fn [compound-id props]
                          (when (contains? compound-id :atlas/type-ref)
                            (let [dev-id (:atlas/dev-id props)
                                  source (:type-ref/source props)
                                  property (:type-ref/property props)
                                  datalog-verb (:type-ref/datalog-verb props)
                                  cardinality (:type-ref/cardinality props)]
                              (cond-> []
                                source
                                (conj [:db/add dev-id :type-ref/source source])

                                property
                                (conj [:db/add dev-id :type-ref/property property])

                                datalog-verb
                                (conj [:db/add dev-id :type-ref/datalog-verb datalog-verb])

                                cardinality
                                (conj [:db/add dev-id :type-ref/cardinality cardinality])))))
  :datalog-extractor/schema datalog-schema})

