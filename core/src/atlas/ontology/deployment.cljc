(ns atlas.ontology.deployment
  "Deployment ontology — immutable Cloud Run deployment snapshots linked to :atlas/system.

   Each deploy event registers a new :atlas/deployment entity capturing the gcloud run
   configuration active at that git-sha. Allows correlating exec-fn behaviour with the
   deployment parameters that were live at the time.

   Relationship: deployment → system (many deployments per system, one snapshot per deploy).
   Query pattern: exec-fn → system → deployments for that system.

   Fields wrap the main `gcloud run deploy` options plus app-level resource config
   (worker threads, DB pool) that affect throughput but are not part of the Cloud Run spec.

   Auto-registers on require."
  (:require [atlas.registry :as registry]
            [atlas.registry.lookup :as entity]
            [atlas.ontology.type-ref :as type-ref]
            [clojure.spec.alpha :as s]))

;; ---------------------------------------------------------------------------
;; Specs
;; ---------------------------------------------------------------------------

(s/def :deployment/system          qualified-keyword?)
(s/def :deployment/git-sha         string?)
;; gcloud run deploy main options
(s/def :deployment/min-instances   nat-int?)
(s/def :deployment/max-instances   pos-int?)
(s/def :deployment/cpu             string?)    ;; e.g. "1", "2", "4"
(s/def :deployment/memory          string?)    ;; e.g. "512Mi", "1Gi", "2Gi"
(s/def :deployment/concurrency     pos-int?)   ;; max concurrent requests per instance
;; App-level resource config (not in Cloud Run spec)
(s/def :deployment/worker-threads  pos-int?)
(s/def :deployment/db-threads      pos-int?)

;; ---------------------------------------------------------------------------
;; Entity type
;; ---------------------------------------------------------------------------

(registry/register!
 :atlas/deployment
 :atlas/ontology
 #{:atlas/deployment}
 {:ontology/for  :atlas/deployment
  :ontology/keys [:deployment/system
                  :deployment/git-sha
                  :deployment/min-instances
                  :deployment/max-instances
                  :deployment/cpu
                  :deployment/memory
                  :deployment/concurrency
                  :deployment/worker-threads
                  :deployment/db-threads]
  :dataflow/deps-key :deployment/system})

;; Type-ref: deployment/system → :atlas/system (cardinality one)
(registry/register!
 :type-ref/deployment-system
 :atlas/type-ref
 #{:meta/ref-deployment-system}
 {:type-ref/source       :atlas/deployment
  :type-ref/property     :deployment/system
  :type-ref/datalog-verb :entity/depends
  :type-ref/cardinality  :db.cardinality/one})

;; Datalog extractor
(registry/register!
 :datalog-extractor/deployment
 :atlas/datalog-extractor
 #{:meta/deployment-extractor}
 {:datalog-extractor/fn
  (fn [compound-id props]
    (when (contains? compound-id :atlas/deployment)
      (type-ref/extract-reference-facts :atlas/deployment compound-id props)))
  :datalog-extractor/schema
  {:entity/depends {:db/cardinality :db.cardinality/one}}})

;; ---------------------------------------------------------------------------
;; Invariants
;; ---------------------------------------------------------------------------

(defn- all-deployments []
  (->> (entity/all-with-aspect :atlas/deployment)
       (filter #(= :atlas/deployment (:atlas/type (entity/props-for %))))))

(registry/register!
 :invariant/deployment-git-sha-required
 :atlas/invariant
 #{:meta/deployment-git-sha-required}
 {:invariant/fn
  (fn []
    (let [violations (for [id    (all-deployments)
                           :let  [props (entity/props-for id)]
                           :when (not (seq (:deployment/git-sha props)))]
                       {:deployment id})]
      (when (seq violations)
        {:invariant :deployment-git-sha-required
         :violation :deployment-missing-git-sha
         :details   violations
         :severity  :error
         :message   (str "Deployment entities missing :deployment/git-sha: "
                         (mapv :deployment violations))})))})

(registry/register!
 :invariant/deployment-references-valid-system
 :atlas/invariant
 #{:meta/deployment-references-valid-system}
 {:invariant/fn
  (fn []
    (let [violations (for [id    (all-deployments)
                           :let  [props  (entity/props-for id)
                                  sys-id (:deployment/system props)]
                           :when (nil? (entity/props-for sys-id))]
                       {:deployment id :missing-system sys-id})]
      (when (seq violations)
        {:invariant :deployment-references-valid-system
         :violation :deployment-references-missing-system
         :details   violations
         :severity  :error
         :message   (str "Deployment entities reference non-existent systems: "
                         (mapv :missing-system violations))})))})

(registry/register!
 :invariant/deployment-instance-bounds
 :atlas/invariant
 #{:meta/deployment-instance-bounds}
 {:invariant/fn
  (fn []
    (let [violations (for [id    (all-deployments)
                           :let  [props    (entity/props-for id)
                                  min-inst (:deployment/min-instances props)
                                  max-inst (:deployment/max-instances props)]
                           :when (and min-inst max-inst (> min-inst max-inst))]
                       {:deployment id :min min-inst :max max-inst})]
      (when (seq violations)
        {:invariant :deployment-instance-bounds
         :violation :min-instances-exceeds-max
         :details   violations
         :severity  :error
         :message   (str "Deployment min-instances > max-instances: "
                         (mapv #(select-keys % [:deployment :min :max]) violations))})))})
