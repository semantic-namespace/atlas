(ns atlas.registry.serialise
  "Sanitising a registry for serialisation (cloud push, snapshot export).

   The LIVE registry holds non-EDN values: function values under :atlas/impl
   (execution-function handlers), :invariant/fn, :datalog-extractor/fn, and any
   other opaque host object an ontology might attach. Those cannot round-trip
   through EDN, so they must be stripped before a snapshot is pushed.

   `sanitize` is a BLACKLIST BY VALUE: it keeps every prop whose value is
   EDN-serialisable and drops the rest. Contrast the whitelist in atlas.cloud
   (`:ontology/keys` minus `:ontology/not-serialisable-keys`), which needs every
   prop declared in an ontology and throws on entities whose type has no
   descriptor. Sanitizing by value needs no declaration, never throws, and keeps
   legitimate undeclared props (e.g. :grain/outcomes, :test-case/target) that a
   whitelist would silently drop.

   The live registry is untouched — fns stay resident so handle-tool can invoke
   :atlas/impl and check-all can run :invariant/fn. Only the exported copy is
   sanitised.")

(defn serialisable-value?
  "True when v round-trips through EDN — no fns, no opaque host objects.
   Recurses into maps and collections; a container is serialisable only when all
   of its keys and values are."
  [v]
  (cond
    (fn? v)      false
    (nil? v)     true
    (keyword? v) true
    (symbol? v)  true
    (string? v)  true
    (number? v)  true
    (boolean? v) true
    (uuid? v)    true
    (inst? v)    true
    (map? v)     (every? (fn [[k val]]
                           (and (serialisable-value? k)
                                (serialisable-value? val)))
                         v)
    (coll? v)    (every? serialisable-value? v)
    :else        false))

(defn sanitize-props
  "Entity props with every non-serialisable value dropped — :atlas/impl,
   :invariant/fn, :datalog-extractor/fn, and anything else that won't serialise."
  [props]
  (into {} (filter (fn [[_ v]] (serialisable-value? v)) props)))

(defn sanitize
  "Registry map with every non-serialisable prop value stripped, so the result
   serialises to EDN. Entity keys (compound-ids) are preserved as-is; only
   values inside each entity's props are filtered."
  [registry]
  (into {} (map (fn [[cid props]] [cid (sanitize-props props)])) registry))
