(ns atlas.query.dataflow
  "Data flow queries: find producers and consumers of data keys,
   trace data flow through the system.
   Takes arbitrary property keys as parameters for ontology-agnostic querying.")

(defn find-producers
  "Find entities producing a data key.
   property-key is the key to check (e.g., :interface-endpoint/response).
   Returns map of identities->values."
  [registry data-key property-key]
  (->> registry
       (filter (fn [[_ v]]
                 (some #{data-key} (get v property-key))))
       (into {})))

(defn find-consumers
  "Find entities consuming a data key.
   property-key is the key to check (e.g., :execution-function/context).
   Returns map of identities->values."
  [registry data-key property-key]
  (->> registry
       (filter (fn [[_ v]]
                 (some #{data-key} (get v property-key))))
       (into {})))

(defn trace-data-flow
  "Trace data flow for a key.
   producer-key and consumer-key specify which properties to check.
   Returns {:data-key :producers :consumers :connected?}."
  [registry data-key producer-key consumer-key]
  (let [producers (find-producers registry data-key producer-key)
        consumers (find-consumers registry data-key consumer-key)]
    {:data-key data-key
     :producers producers
     :consumers consumers
     :connected? (and (seq producers) (seq consumers))}))
