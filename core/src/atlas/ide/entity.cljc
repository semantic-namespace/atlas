(ns atlas.ide.entity
  "Entity and aspect queries over the current registry. Pure, so they load in
   babashka and answer against a bound snapshot."
  (:require [atlas.registry :as cid]
            [atlas.registry.lookup :as rt]
            [atlas.registry.serialise :as ser]
            [atlas.ontology :as ot]))

(defn- ensure-keyword
  [x]
  (cond
    (keyword? x) x
    (string? x)  (keyword (cond-> x (= \: (first x)) (subs 1)))
    :else        x))

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

(defn- fields-for [props]
  (or (:data-schema/fields props)
      (:atlas/fields props)
      []))

(defn list-aspects
  "List all unique aspects used in the registry.

   Args:
     registry - (Optional) Registry map. Defaults to global registry."
  ([]
   (list-aspects (cid/current-registry)))
  ([registry]
   (let [;; Count aspect usage (cid/aspects strips the entity type per identity)
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

(defn entities-with-aspect
  "Find all entities with given aspect."
  [aspect]
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
            not-serialisable-keys (ot/not-serialisable-keys-for-identity id)
            ;; Filter to only include ontology-defined keys, then remove non-serialisable ones
            definition-values (when (seq definition-keys)
                             (reduce (fn [acc k]
                                       ;; Only include if: (1) in props, (2) not in non-serialisable list
                                       (if (and (contains? props k)
                                               (not (contains? not-serialisable-keys k)))
                                         (assoc acc k (definition-value (get props k)))
                                         acc))
                                     (array-map)
                                     definition-keys))
            ;; Props the entity actually carries that no ontology DECLARES.
            ;; Undeclared props are legitimate by design — :ontology/keys is the
            ;; REQUIRED-key contract (see registry/validate-ontology-specs), not an
            ;; exhaustive list of what an entity may hold, and serialise keeps them
            ;; on purpose (:grain/outcomes, :data-schema/field-specs, …). Reporting
            ;; only the declared subset made real, pushed properties invisible to
            ;; entity-detail and every consumer built on it.
            declared    (set definition-keys)
            extra-props (reduce (fn [acc [k v]]
                                  (if (or (contains? declared k)
                                          (contains? not-serialisable-keys k)
                                          (not (ser/serialisable-value? v)))
                                    acc
                                    (assoc acc k (definition-value v))))
                                (array-map)
                                (sort-by key props))]
        {:entity/dev-id dev-id-kw
         :entity/identity (vec (sort id))
         :entity/aspects (vec (sort (disj id :atlas/execution-function
                                          :atlas/structure-component
                                          :atlas/interface-endpoint
                                          :atlas/data-schema)))
         :entity/definition-keys definition-keys
         :entity/definition-values definition-values
         :entity/extra-props extra-props
         :interface-endpoint/context (vec (sort (ot/context-for dev-id-kw)))
         :interface-endpoint/response (vec (sort (ot/response-for dev-id-kw)))
         :execution-function/deps (vec (sort (ot/deps-for dev-id-kw)))
         :atlas/fields (vec (sort (fields-for props)))}))))
