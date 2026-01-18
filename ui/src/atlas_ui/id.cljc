(ns atlas-ui.id
  "Shared, pure helpers for converting between keywords/ids and Cytoscape-friendly strings.")

(defn id->string [id]
  (cond
    (keyword? id) (if-let [ns (namespace id)]
                    (str ns "/" (name id))
                    (name id))
    :else (str id)))

(defn string->id [s]
  (if (string? s)
    (keyword s)
    s))

