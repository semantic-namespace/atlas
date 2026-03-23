(ns atlas.aggregation
  "Semantic aggregation analysis — detect redundant aspects.

   After compile!, compound-ids already include aggregated aspects.
   This module provides analysis tools to find explicit aspects
   that are derivable from deps (informational, not required).

   See atlas.registry/compile! for the aggregation implementation."
  (:require [atlas.registry :as registry]
            [clojure.set :as set]))

;; =============================================================================
;; REDUNDANCY DETECTION
;; =============================================================================

(defn redundant-aspects
  "Find explicit aspects that are already derivable from deps via type-refs.
   Returns nil or a vector of {:dev-id :redundant [...]} maps.

   Uses the log to compare declared vs compiled aspects."
  []
  (let [violations
        (->> @registry/registrations
             (reduce (fn [acc e] (assoc acc (:dev-id e) e)) {})
             vals
             (keep
              (fn [{:keys [dev-id aspects]}]
                (let [derived (registry/derived-aspects dev-id)
                      redundant (when (seq derived)
                                  (set/intersection aspects derived))]
                  (when (seq redundant)
                    {:dev-id    dev-id
                     :redundant (vec (sort redundant))}))))
             vec)]
    (when (seq violations) violations)))

;; =============================================================================
;; INVARIANT (auto-registered)
;; =============================================================================

(registry/register!
 :invariant/redundant-explicit-aspects
 :atlas/invariant
 #{:meta/redundant-aspects-check}
 {:invariant/fn (fn []
                  "Explicit aspects that are already derivable via type-ref dependencies.

                   These aspects can be removed from the compound-id since they are
                   implied by the entity's deps. Keeping them is not wrong but adds
                   noise — the aggregation system derives them automatically."
                  (when-let [violations (redundant-aspects)]
                    {:invariant  :redundant-explicit-aspects
                     :violation  :redundant-aspects
                     :details    violations
                     :severity   :info
                     :message    "Some entities have explicit aspects already derivable from their deps"}))})
