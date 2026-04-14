(ns atlas.ontology.workflow
  "Workflow ontology module.

   Defines the `:atlas/workflow` entity type. Auto-registers on require.

   A workflow is an FSM (tilakone) that ties workflow-producers together
   with per-producer transition maps. Each producer is an FSM state node;
   signals emitted by producers drive transitions to the next producer.

   Transitions are per-producer so the same signal can route differently
   depending on which producer emitted it. This also allows the same
   producer to be reused across different workflows with different wiring.

   Usage:
     (require '[atlas.ontology.workflow])

   Example registration:
     (registry/register!
       :wf/order-fulfillment
       :atlas/workflow
       #{:domain/warehouse :workflow/fulfillment}
       {:workflow/producers #{:wp/fetch-inventory :wp/reserve-stock :wp/notify-shipping}
        :workflow/initial-producer :wp/fetch-inventory
        :workflow/transitions {:wp/fetch-inventory {:signal/in-stock :wp/reserve-stock
                                                    :signal/out-of-stock :wp/notify-shipping}
                               :wp/reserve-stock   {:signal/reserved :wp/notify-shipping}}})"
  (:require [atlas.registry :as registry]
            [atlas.registry.lookup :as entity]
            [atlas.ontology.type-ref :as type-ref]
            [clojure.set :as set]
            [clojure.spec.alpha :as s]))

;; =============================================================================
;; SPECS
;; =============================================================================

;; Set of workflow-producer dev-ids
(s/def :workflow/producers (s/coll-of :atlas/dev-id :kind set? :min-count 1))

;; Dev-id of the starting producer
(s/def :workflow/initial-producer :atlas/dev-id)

;; Per-producer signal→target transition map
;; {producer-dev-id {signal-keyword target-producer-dev-id}}
(s/def :workflow/producer-transitions
  (s/map-of qualified-keyword? :atlas/dev-id :min-count 1))

(s/def :workflow/transitions
  (s/map-of :atlas/dev-id :workflow/producer-transitions :min-count 1))

;; Full workflow props spec
(s/def :atlas/workflow-props
  (s/keys :req [:workflow/producers
                :workflow/initial-producer
                :workflow/transitions]))

;; =============================================================================
;; ONTOLOGY
;; =============================================================================

(registry/register!
 :atlas/workflow
 :atlas/ontology
 #{:atlas/workflow}
 {:ontology/for :atlas/workflow
  :ontology/keys [:workflow/producers
                  :workflow/initial-producer
                  :workflow/transitions]
  :dataflow/deps-key :workflow/producers})

;; Type-ref: workflow → workflow-producer (producers)
(registry/register!
 :type-ref/workflow-producers
 :atlas/type-ref
 #{:meta/ref-workflow-producers}
 {:type-ref/source :atlas/workflow
  :type-ref/property :workflow/producers
  :type-ref/datalog-verb :workflow/has-producer
  :type-ref/cardinality :db.cardinality/many})

;; =============================================================================
;; DATALOG EXTRACTOR
;; =============================================================================

(defn- all-transition-targets
  "Extract all target producer dev-ids from nested transitions map."
  [transitions]
  (->> (vals transitions)
       (mapcat vals)
       set))

(defn- all-transition-signals
  "Extract all signal keywords from nested transitions map."
  [transitions]
  (->> (vals transitions)
       (mapcat keys)
       set))

(registry/register!
 :datalog-extractor/workflow
 :atlas/datalog-extractor
 #{:meta/workflow-extractor}
 {:datalog-extractor/fn
  (fn [compound-id props]
    (when (contains? compound-id :atlas/workflow)
      (let [dev-id (:atlas/dev-id props)
            transitions (:workflow/transitions props)
            initial (:workflow/initial-producer props)
            producers (:workflow/producers props)
            ;; Aggregate context/output from producers: workflow produces what
            ;; any of its producers produce; consumes what producers consume
            ;; but isn't satisfied internally by another producer's output.
            producer-propsets (map entity/props-for producers)
            producer-outputs (->> producer-propsets
                                  (mapcat :workflow-producer/output)
                                  set)
            producer-contexts (->> producer-propsets
                                   (mapcat :workflow-producer/context)
                                   set)
            external-context (set/difference producer-contexts producer-outputs)]
        (vec
         (concat
          ;; Automatic reference extraction via type-ref
          (type-ref/extract-reference-facts
           :atlas/workflow
           compound-id
           props)

          ;; Initial producer
          (when initial
            [[:db/add dev-id :workflow/starts-at initial]])

          ;; Dependency edges: workflow depends on all its producers
          (when producers
            (map (fn [p] [:db/add dev-id :entity/depends p])
                 producers))

          ;; Aggregated produces (union of producer outputs)
          (map (fn [k] [:db/add dev-id :entity/produces k])
               producer-outputs)

          ;; Aggregated external consumes (producer contexts not satisfied internally)
          (map (fn [k] [:db/add dev-id :entity/consumes k])
               external-context)

          ;; Transition signals and targets (flattened from nested map)
          (when transitions
            (concat
             (map (fn [sig] [:db/add dev-id :workflow/transition-signal sig])
                  (all-transition-signals transitions))
             (map (fn [target] [:db/add dev-id :workflow/transition-target target])
                  (all-transition-targets transitions)))))))))

  :datalog-extractor/schema {:workflow/has-producer {:db/cardinality :db.cardinality/many}
                             :workflow/starts-at {:db/cardinality :db.cardinality/one}
                             :workflow/transition-signal {:db/cardinality :db.cardinality/many}
                             :workflow/transition-target {:db/cardinality :db.cardinality/many}
                             :entity/depends {:db/cardinality :db.cardinality/many}
                             :entity/consumes {:db/cardinality :db.cardinality/many}
                             :entity/produces {:db/cardinality :db.cardinality/many}}})

;; =============================================================================
;; INVARIANTS
;; =============================================================================

(registry/register!
 :invariant/workflow-producers-exist
 :atlas/invariant
 #{:meta/workflow-producers-exist}
 {:invariant/fn
  (fn []
    (let [workflows (->> (entity/all-with-aspect :atlas/workflow)
                         (remove #(entity/has-aspect? % :atlas/ontology)))
          violations (for [wf-id workflows
                           :let [props (entity/props-for wf-id)
                                 producers (:workflow/producers props)]
                           :when producers
                           producer producers
                           :when (nil? (entity/identity-for producer))]
                       {:workflow wf-id :missing-producer producer})]
      (when (seq violations)
        {:invariant :workflow-producers-exist
         :violation :producer-not-found
         :details (vec violations)
         :severity :error
         :message (str "Workflow references non-existent producers: "
                       (set (map :missing-producer violations)))})))})

(registry/register!
 :invariant/workflow-initial-in-producers
 :atlas/invariant
 #{:meta/workflow-initial-in-producers}
 {:invariant/fn
  (fn []
    (let [workflows (->> (entity/all-with-aspect :atlas/workflow)
                         (remove #(entity/has-aspect? % :atlas/ontology)))
          violations (for [wf-id workflows
                           :let [props (entity/props-for wf-id)
                                 initial (:workflow/initial-producer props)
                                 producers (:workflow/producers props)]
                           :when (and initial producers
                                      (not (contains? producers initial)))]
                       {:workflow wf-id
                        :initial-producer initial
                        :producers producers})]
      (when (seq violations)
        {:invariant :workflow-initial-in-producers
         :violation :initial-not-in-producers
         :details (vec violations)
         :severity :error
         :message "Workflow initial-producer must be a member of :workflow/producers"})))})

(registry/register!
 :invariant/workflow-transition-sources-in-producers
 :atlas/invariant
 #{:meta/workflow-transition-sources}
 {:invariant/fn
  (fn []
    "Every key in the transitions map must be a member of :workflow/producers."
    (let [workflows (->> (entity/all-with-aspect :atlas/workflow)
                         (remove #(entity/has-aspect? % :atlas/ontology)))
          violations (for [wf-id workflows
                           :let [props (entity/props-for wf-id)
                                 producers (:workflow/producers props)
                                 transitions (:workflow/transitions props)]
                           :when (and producers transitions)
                           :let [sources (set (keys transitions))
                                 unknown (set/difference sources producers)]
                           :when (seq unknown)]
                       {:workflow wf-id
                        :unknown-sources unknown})]
      (when (seq violations)
        {:invariant :workflow-transition-sources-in-producers
         :violation :transition-source-not-in-producers
         :details (vec violations)
         :severity :error
         :message "All transition source producers must be members of :workflow/producers"})))})

(registry/register!
 :invariant/workflow-transition-targets-in-producers
 :atlas/invariant
 #{:meta/workflow-transition-targets}
 {:invariant/fn
  (fn []
    (let [workflows (->> (entity/all-with-aspect :atlas/workflow)
                         (remove #(entity/has-aspect? % :atlas/ontology)))
          violations (for [wf-id workflows
                           :let [props (entity/props-for wf-id)
                                 producers (:workflow/producers props)
                                 transitions (:workflow/transitions props)]
                           :when (and producers transitions)
                           :let [targets (all-transition-targets transitions)
                                 unknown (set/difference targets producers)]
                           :when (seq unknown)]
                       {:workflow wf-id
                        :unknown-targets unknown})]
      (when (seq violations)
        {:invariant :workflow-transition-targets-in-producers
         :violation :transition-target-not-in-producers
         :details (vec violations)
         :severity :error
         :message "All transition targets must be members of :workflow/producers"})))})

(registry/register!
 :invariant/workflow-signals-covered
 :atlas/invariant
 #{:meta/workflow-signals-covered}
 {:invariant/fn
  (fn []
    "For each producer in the workflow, every signal it declares should have
     a corresponding transition in that producer's entry in the transitions map."
    (let [workflows (->> (entity/all-with-aspect :atlas/workflow)
                         (remove #(entity/has-aspect? % :atlas/ontology)))
          violations (for [wf-id workflows
                           :let [props (entity/props-for wf-id)
                                 producers (:workflow/producers props)
                                 transitions (:workflow/transitions props)]
                           :when (and producers transitions)
                           producer producers
                           :let [producer-props (entity/props-for producer)
                                 declared-signals (set (:workflow-producer/signals producer-props))
                                 handled-signals (set (keys (get transitions producer)))
                                 unhandled (set/difference declared-signals handled-signals)]
                           :when (seq unhandled)]
                       {:workflow wf-id
                        :producer producer
                        :unhandled-signals unhandled})]
      (when (seq violations)
        {:invariant :workflow-signals-covered
         :violation :unhandled-signals
         :details (vec violations)
         :severity :warning
         :message "Some producer signals have no transition defined in this workflow"})))})

(registry/register!
 :invariant/workflow-props-valid
 :atlas/invariant
 #{:meta/workflow-props-valid}
 {:invariant/fn
  (fn []
    "All workflow entities must conform to :atlas/workflow-props spec."
    (let [workflows (->> (entity/all-with-aspect :atlas/workflow)
                         (remove #(entity/has-aspect? % :atlas/ontology)))
          violations (for [wf-id workflows
                           :let [props (entity/props-for wf-id)]
                           :when (not (s/valid? :atlas/workflow-props props))]
                       {:workflow wf-id
                        :explain (s/explain-str :atlas/workflow-props props)})]
      (when (seq violations)
        {:invariant :workflow-props-valid
         :violation :invalid-props
         :details (vec violations)
         :severity :error
         :message "Workflow props do not conform to spec"})))})
