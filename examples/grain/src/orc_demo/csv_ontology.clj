(ns orc-demo.csv-ontology
  "PoC part 3 — a SECOND real ORC workflow: `csv-to-ontology`
   (components/ontology/src/ai/obney/orc/ontology/sheets/csv_ontology.clj), a
   real 24-node pipeline that extracts an OWL ontology from a CSV. Rebuilt here
   as a COMPLETE, faithful transcription — every :bt.csv/* dev-id, :reads, and
   :writes below is copied verbatim from the sheet's `sheet/code`/`sheet/llm`/
   `sheet/map-each` DSL calls.

   Audit finding (2026-07-13): the prior version of this file claimed an
   8-node 'faithful slice' whose headline reuse-case was a `validate-schema`
   leaf mitigating :failure.orc/hallucination — the SAME concept the eval
   grounding-judge guards, in a different workflow. Verified against the real
   source: NO `validate-schema` node exists anywhere in the real pipeline. The
   only real quality-check leaf is `detect-ambiguity`, which maps onto
   :failure.orc/ambiguity — not hallucination. The cross-workflow
   hallucination join this file used to advertise was fabricated; it has been
   removed. What's real and still demonstrates cross-workflow knowledge reuse:
   :failure.orc/ambiguity itself, and the shared :atlas/risk-failure-mode /
   :concept/broader / :node/mitigates machinery, applied faithfully this time.

   Naming fix (2026-07-13, second pass): the first collision-free rebuild
   disambiguated same-workflow leaves by minting a bespoke, ultra-specific
   :operation/* verb per leaf (:operation/prepare-entity-context,
   :operation/build-tbox, …) — copying the literal ORC node name into the
   aspect instead of decomposing it. That's semantically empty: the verb
   doesn't generalise or compose with anything. Fixed by splitting onto TWO
   axes, the same generic-verb-plus-subject pattern used everywhere else in
   this ontology (:domain/counter :operation/increment): :operation/* stays a
   small, reusable verb (prepare/collect/build/…), and the new :artifact/*
   aspect names what it acts on (:artifact/entity-context,
   :artifact/tbox, :artifact/hierarchies, …). Disambiguation is now carried by
   a genuinely queryable dimension ('every step that builds a tbox') instead
   of a dev-id wearing an aspect costume — verified still collision-free (the
   one pair sharing both :operation/build and :artifact/hierarchies,
   `build-hierarchies`/`collect-hierarchies`, differ by :behavior-tree/map-each vs
   :behavior-tree/leaf, same as before)."
  (:require [atlas.registry :as registry]
            [atlas.ontology :as ontology]
            [atlas.ontology.behavior-tree]))

(defn init-csv-ontology!
  []
  ;; failure concept under the shared taxonomy — the one real cross-concept
  ;; link this pipeline has (detect-ambiguity, below)
  (registry/register!
   :failure.orc/ambiguity :atlas/risk-failure-mode
   #{:domain/orc :failure/clarity}
   {:risk-failure-mode/detection "extracted terms are ambiguous / not crisply defined"
    :concept/broader #{:failure.orc/output-quality}})

  ;; ROOT — sequence "csv-main-pipeline". :behavior-tree/inputs are the blackboard's
  ;; declared === Inputs === (csv-path/csv-data/entity-column/entity-type/base-uri).
  (registry/register!
   :bt.csv/root :atlas/execution-function
   #{:behavior-tree/sequence :domain/orc :workflow/csv-to-ontology}
   {:behavior-tree/inputs #{:bb/csv-path :bb/csv-data :bb/entity-column :bb/entity-type :bb/base-uri}
    :behavior-tree/children [:bt.csv/load-csv :bt.csv/analyze-structure
                  :bt.csv/detect-temporal-columns :bt.csv/extract-temporal-metadata
                  :bt.csv/build-context-strings :bt.csv/analyze-csv-schema
                  :bt.csv/generate-definitions :bt.csv/collect-definitions
                  :bt.csv/identify-categorical-columns :bt.csv/build-hierarchies
                  :bt.csv/collect-hierarchies :bt.csv/prepare-relationship-context
                  :bt.csv/discover-implicit-relationships :bt.csv/prepare-validation-context
                  :bt.csv/detect-ambiguity :bt.csv/collect-mapping-issues
                  :bt.csv/build-tbox :bt.csv/build-abox :bt.csv/serialize-to-owl
                  :bt.csv/compute-statistics]})

  ;; PHASE 1: STRUCTURE ANALYSIS (code, rule-based)
  (registry/register!
   :bt.csv/load-csv :atlas/execution-function
   #{:behavior-tree/leaf :behavior-tree/code :domain/orc :workflow/csv-to-ontology :operation/load}
   ;; read-modify-write: falls back to the pre-supplied :bb/csv-data input if
   ;; :bb/csv-path isn't set — a node may read a key it itself writes.
   {:execution-function/context [:bb/csv-path :bb/csv-data]
    :execution-function/response [:bb/csv-data]})

  (registry/register!
   :bt.csv/analyze-structure :atlas/execution-function
   #{:behavior-tree/leaf :behavior-tree/code :domain/orc :workflow/csv-to-ontology :operation/analyze}
   {:execution-function/context [:bb/csv-data :bb/entity-column]
    :execution-function/response [:bb/column-analysis :bb/detected-classes
                                  :bb/detected-properties :bb/detected-hierarchies
                                  :bb/detected-foreign-keys]})

  ;; PHASE 1.5: TEMPORAL COLUMN DETECTION
  (registry/register!
   :bt.csv/detect-temporal-columns :atlas/execution-function
   #{:behavior-tree/leaf :behavior-tree/code :domain/orc :workflow/csv-to-ontology :operation/detect}
   {:execution-function/context [:bb/column-analysis]
    :execution-function/response [:bb/date-columns :bb/has-temporal-data]})

  (registry/register!
   :bt.csv/extract-temporal-metadata :atlas/execution-function
   #{:behavior-tree/leaf :behavior-tree/code :domain/orc :workflow/csv-to-ontology :operation/extract}
   {:execution-function/context [:bb/csv-data :bb/date-columns :bb/entity-column]
    :execution-function/response [:bb/temporal-entities]})

  (registry/register!
   :bt.csv/build-context-strings :atlas/execution-function
   #{:behavior-tree/leaf :behavior-tree/code :domain/orc :workflow/csv-to-ontology :operation/build :artifact/prompt-context}
   {:execution-function/context [:bb/column-analysis :bb/csv-data :bb/detected-classes
                                 :bb/detected-hierarchies :bb/detected-foreign-keys]
    :execution-function/response [:bb/column-summary :bb/sample-rows :bb/detected-patterns]})

  ;; PHASE 2: SCHEMA ANALYSIS (LLM, ChainOfThought)
  (registry/register!
   :bt.csv/analyze-csv-schema :atlas/execution-function
   #{:behavior-tree/leaf :behavior-tree/llm :domain/orc :workflow/csv-to-ontology :operation/reason}
   {:execution-function/context [:bb/column-summary :bb/sample-rows :bb/detected-patterns]
    :execution-function/response [:bb/schema-reasoning :bb/domain :bb/domain-description
                                  :bb/entities :bb/relationships :bb/property-mappings]})

  ;; PHASE 3: DEFINITION GENERATION (LLM per class) — map-each over :entities
  (registry/register!
   :bt.csv/generate-definitions :atlas/execution-function
   #{:behavior-tree/map-each :domain/orc :workflow/csv-to-ontology :operation/generate}
   {:behavior-tree/as :bb/current-entity
    :execution-function/context [:bb/entities]
    :execution-function/response [:bb/definition-results]
    :behavior-tree/children [:bt.csv/prepare-entity-context :bt.csv/enrich-entity-definition]})

  (registry/register!
   :bt.csv/prepare-entity-context :atlas/execution-function
   #{:behavior-tree/leaf :behavior-tree/code :domain/orc :workflow/csv-to-ontology :operation/prepare :artifact/entity-context}
   {:execution-function/context [:bb/entities]
    :execution-function/response [:bb/entity-name :bb/source-columns
                                  :bb/sample-instances :bb/related-entities]})

  (registry/register!
   :bt.csv/enrich-entity-definition :atlas/execution-function
   #{:behavior-tree/leaf :behavior-tree/llm :domain/orc :workflow/csv-to-ontology :operation/enrich}
   {:execution-function/context [:bb/domain]
    :execution-function/response [:bb/definition-reasoning :bb/definition
                                  :bb/scope-note :bb/external-alignments]})

  (registry/register!
   :bt.csv/collect-definitions :atlas/execution-function
   #{:behavior-tree/leaf :behavior-tree/code :domain/orc :workflow/csv-to-ontology :operation/collect :artifact/entity-definitions}
   {:execution-function/context [:bb/entities :bb/definition-results]
    :execution-function/response [:bb/entity-definitions]})

  ;; PHASE 4: HIERARCHY ENRICHMENT (LLM per categorical column) — map-each
  (registry/register!
   :bt.csv/identify-categorical-columns :atlas/execution-function
   #{:behavior-tree/leaf :behavior-tree/code :domain/orc :workflow/csv-to-ontology :operation/identify}
   {:execution-function/context [:bb/column-analysis]
    :execution-function/response [:bb/categorical-columns]})

  (registry/register!
   :bt.csv/build-hierarchies :atlas/execution-function
   #{:behavior-tree/map-each :domain/orc :workflow/csv-to-ontology :operation/build :artifact/hierarchies}
   {:behavior-tree/as :bb/current-column
    :execution-function/context [:bb/categorical-columns]
    :execution-function/response [:bb/hierarchy-results]
    :behavior-tree/children [:bt.csv/prepare-hierarchy-context :bt.csv/suggest-hierarchy]})

  (registry/register!
   :bt.csv/prepare-hierarchy-context :atlas/execution-function
   #{:behavior-tree/leaf :behavior-tree/code :domain/orc :workflow/csv-to-ontology :operation/prepare :artifact/hierarchy-context}
   {:execution-function/context [:bb/csv-data]
    :execution-function/response [:bb/column-name :bb/unique-values :bb/value-counts]})

  (registry/register!
   :bt.csv/suggest-hierarchy :atlas/execution-function
   #{:behavior-tree/leaf :behavior-tree/llm :domain/orc :workflow/csv-to-ontology :operation/suggest}
   {:execution-function/context [:bb/value-counts]
    :execution-function/response [:bb/has-hierarchy :bb/hierarchy-type
                                  :bb/hierarchy-relationships :bb/top-level :bb/reasoning]})

  (registry/register!
   :bt.csv/collect-hierarchies :atlas/execution-function
   #{:behavior-tree/leaf :behavior-tree/code :domain/orc :workflow/csv-to-ontology :operation/collect :artifact/hierarchies}
   {:execution-function/context [:bb/categorical-columns :bb/hierarchy-results]
    :execution-function/response [:bb/hierarchies]})

  ;; PHASE 5: RELATIONSHIP DISCOVERY (LLM)
  (registry/register!
   :bt.csv/prepare-relationship-context :atlas/execution-function
   #{:behavior-tree/leaf :behavior-tree/code :domain/orc :workflow/csv-to-ontology :operation/prepare :artifact/relationship-context}
   {:execution-function/context [:bb/relationships]
    :execution-function/response [:bb/entities-info :bb/sample-data :bb/existing-relationships]})

  (registry/register!
   :bt.csv/discover-implicit-relationships :atlas/execution-function
   #{:behavior-tree/leaf :behavior-tree/llm :domain/orc :workflow/csv-to-ontology :operation/discover}
   {:execution-function/context [:bb/existing-relationships]
    :execution-function/response [:bb/relationship-reasoning :bb/discovered-relationships
                                  :bb/inverse-relationships]})

  ;; PHASE 6: QUALITY VALIDATION (LLM) — the ONE real quality-check leaf.
  ;; Note: the real pipeline has NO schema/hallucination guard — only ambiguity.
  (registry/register!
   :bt.csv/prepare-validation-context :atlas/execution-function
   #{:behavior-tree/leaf :behavior-tree/code :domain/orc :workflow/csv-to-ontology :operation/prepare :artifact/validation-context}
   {:execution-function/context [:bb/property-mappings]
    :execution-function/response [:bb/terms-to-validate :bb/existing-definitions
                                  :bb/mappings-to-validate]})

  (registry/register!
   :bt.csv/detect-ambiguity :atlas/execution-function
   #{:behavior-tree/leaf :behavior-tree/llm :domain/orc :workflow/csv-to-ontology :operation/validate}
   {:execution-function/context [:bb/existing-definitions]
    :execution-function/response [:bb/ambiguity-reasoning :bb/ambiguous-terms]
    :node/mitigates #{:failure.orc/ambiguity}})

  (registry/register!
   :bt.csv/collect-mapping-issues :atlas/execution-function
   #{:behavior-tree/leaf :behavior-tree/code :domain/orc :workflow/csv-to-ontology :operation/collect :artifact/mapping-issues}
   {:execution-function/context [:bb/mappings-to-validate]
    :execution-function/response [:bb/mapping-issues]})

  ;; PHASE 7-9: TBOX/ABOX CONSTRUCTION + SERIALIZATION (code)
  (registry/register!
   :bt.csv/build-tbox :atlas/execution-function
   #{:behavior-tree/leaf :behavior-tree/code :domain/orc :workflow/csv-to-ontology :operation/build :artifact/tbox}
   {:execution-function/context [:bb/entities :bb/entity-definitions :bb/relationships
                                 :bb/discovered-relationships :bb/property-mappings
                                 :bb/mapping-issues :bb/hierarchies :bb/base-uri]
    :execution-function/response [:bb/tbox]})

  (registry/register!
   :bt.csv/build-abox :atlas/execution-function
   #{:behavior-tree/leaf :behavior-tree/code :domain/orc :workflow/csv-to-ontology :operation/build :artifact/abox}
   {:execution-function/context [:bb/tbox :bb/csv-data :bb/column-analysis
                                 :bb/entity-column :bb/entity-type :bb/hierarchies :bb/base-uri]
    :execution-function/response [:bb/abox]})

  (registry/register!
   :bt.csv/serialize-to-owl :atlas/execution-function
   #{:behavior-tree/leaf :behavior-tree/code :domain/orc :workflow/csv-to-ontology :operation/emit}
   {:execution-function/context [:bb/tbox :bb/abox :bb/temporal-entities :bb/base-uri]
    :execution-function/response [:bb/owl-output]})

  (registry/register!
   :bt.csv/compute-statistics :atlas/execution-function
   #{:behavior-tree/leaf :behavior-tree/code :domain/orc :workflow/csv-to-ontology :operation/compute}
   {:execution-function/context [:bb/csv-data :bb/tbox :bb/abox
                                 :bb/temporal-entities :bb/date-columns]
    :execution-function/response [:bb/statistics]})

  (ontology/register-entity-types!)
  :done)
