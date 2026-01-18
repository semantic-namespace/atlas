(ns atlas.entity
  "Runtime that drives behavior from semantic registry."
  (:require [atlas.registry :as registry]
            [atlas.query :as query]
            [clojure.set :as set]))
;; that is more related to ontology ???

;; TODO this namespace is almost everything as API dev-id  except all-with-aspect
(defn- fetch-by-dev-id
  "Find the [identity value] pair for a given dev-id.
   Delegates to atlas.query/find-by-dev-id."
  [dev-id]
  (query/find-by-dev-id @registry/registry dev-id))

(defn identity-for [dev-id]
  (first (fetch-by-dev-id dev-id)))

(defn props-for [dev-id]
  (second (fetch-by-dev-id dev-id)))

(def ^:private context-keys
  [:interface-endpoint/context
   :execution-function/context
   :semantic-namespace/context])

(def ^:private response-keys
  [:execution-function/response
   :interface-endpoint/response
   :semantic-namespace/response])

(def ^:private deps-keys
  [:interface-endpoint/deps
   :execution-function/deps
   :structure-component/deps
   :semantic-namespace/deps])

(defn- first-present
  [props keys]
  (some (fn [k]
          (when (contains? props k)
            (get props k)))
        keys))

(defn context-for [dev-id]
  (or (first-present (props-for dev-id) context-keys) []))

(defn response-for [dev-id]
  (or (first-present (props-for dev-id) response-keys) []))

(defn has-aspect? [dev-id aspect]
  (contains? (identity-for dev-id) aspect))

(defn all-with-aspect
  "Find all dev-ids with given aspect.
   Delegates to atlas.query/find-dev-ids-with-aspect."
  [aspect]
  (query/find-dev-ids-with-aspect @registry/registry aspect))

(defn deps-for [dev-id]
  (let [deps (first-present (props-for dev-id) deps-keys)]
    (cond
      (set? deps) deps
      (coll? deps) (set deps)
      :else #{})))

(defn trace-data-flow
  "Trace what produces each context key for a function.
   Uses atlas.query for finding producers."
  [dev-id]
  (let [context (context-for dev-id)]
    (for [ctx-key context
          :let [producers-map (reduce (fn [acc key]
                                        (merge acc (query/find-producers @registry/registry ctx-key key)))
                                      {}
                                      response-keys)
                function-producers (query/find-by-aspect @registry/registry :atlas/execution-function)
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
   Uses atlas.query for finding data flow connections."
  [dev-id]
  (let [context (set (context-for dev-id))
        all-fns (all-with-aspect :atlas/execution-function)]
    (->> context
         (mapcat (fn [ctx-key]
                   (let [producers (reduce (fn [acc key]
                                             (merge acc (query/find-producers @registry/registry ctx-key key)))
                                           {}
                                           response-keys)]
                     (->> producers
                          (keep (fn [[_ v]] (:atlas/dev-id v)))
                          (filter all-fns)))))
         set)))

