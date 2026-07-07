(ns atlas.adapter.yaml-ld
  "EXPERIMENTAL — Export an Atlas registry to JSON-LD / YAML-LD (W3C linked data).

   Atlas already *is* a linked-data graph: qualified-keyword identity, a typed
   subject per entity, and namespaced predicates. JSON-LD (and its YAML
   serialization, YAML-LD — https://w3c.github.io/yaml-ld/) is the standard name
   for that model, so this adapter is a near-1:1 projection:

     Atlas                                Linked data
     -----------------------------------  --------------------------------------
     :atlas/dev-id                        @id            (a CURIE / IRI)
     :atlas/type                          @type          (rdf:type)
     compound-id aspects                  atlas:aspect   (each an @id resource)
     qualified-keyword namespaces         @context prefix -> IRI mappings
     type-refs (property + datalog-verb)  @context term defs: property CURIE ->
                                          the verb IRI, {@type @id, @container @set}
     ref edges (deps / consumes / …)      triples on the verb predicate

   The type-ref meta-model does double duty: it both selects the edge properties
   AND generates the @context term definitions, so e.g. execution-function/deps
   and endpoint/deps both compact to the predicate `entity:depends`.

   The registry is the Atlas map shape `{compound-id-set props-map}` — exactly
   what a registry snapshot (atlas-cloud cloud-pull) returns.

   Requires clojure.data.json (already a core dep). JSON is a subset of YAML, so
   the emitted JSON-LD is itself valid YAML-LD; `->yaml` additionally renders the
   friendlier YAML block form.

   Usage:
     (def reg (clojure.edn/read-string (slurp \"snapshot.edn\")))
     (emit! reg \"/tmp/out\" {:select (aspect-selector :domain/orders)})
     ;; => writes out.jsonld and out.yaml"
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.data.json :as json]))

;; ---------------------------------------------------------------------------
;; Config
;; ---------------------------------------------------------------------------

(def default-base
  "Base IRI under which every Atlas keyword namespace becomes a prefix."
  "https://semantic-namespace.org/atlas/")

;; ---------------------------------------------------------------------------
;; Accessors + type-ref meta-model (same convention as atlas.adapter.overarch)
;; ---------------------------------------------------------------------------

(defn- dev-id [props] (or (:atlas/dev-id props) (:entity/dev-id props)))
(defn- etype  [props] (:atlas/type props))

(def ^:private ref-verbs #{:entity/depends :entity/consumes :entity/produces})

(defn type-ref-index
  "Build `{source-type [{:property :verb} …]}` from the type-ref entities in
   `registry` (only reference-forming verbs are kept)."
  [registry]
  (reduce (fn [m e]
            (if (and (= :atlas/type-ref (:atlas/type e))
                     (contains? ref-verbs (:type-ref/datalog-verb e)))
              (update m (:type-ref/source e) (fnil conj [])
                      {:property (:type-ref/property e)
                       :verb     (:type-ref/datalog-verb e)})
              m))
          {} (vals registry)))

;; ---------------------------------------------------------------------------
;; CURIEs
;; ---------------------------------------------------------------------------

(defn- curie
  "Qualified keyword -> \"ns:name\" compact IRI. Non-namespaced -> name."
  [kw]
  (if-let [ns (namespace kw)] (str ns ":" (name kw)) (name kw)))

(defn- meta-aspect? [a] (= "atlas" (namespace a)))

;; ---------------------------------------------------------------------------
;; Node construction
;; ---------------------------------------------------------------------------

(defn- entity-refs
  "Seq of [verb target-kw] for all reference properties of `props` per `tri`."
  [tri props]
  (for [tr    (get tri (etype props))
        :let  [v (get props (:property tr))]
        t     (if (coll? v) v (when v [v]))
        :when t]
    [(:verb tr) t]))

(defn- ->node
  "One JSON-LD node object for an entity. Edges are grouped by verb predicate."
  [compound-id props tri]
  (let [id      (dev-id props)
        aspects (->> compound-id (remove meta-aspect?) (remove #(= % (etype props))))
        by-verb (reduce (fn [m [verb t]]
                          (update m (curie verb) (fnil conj []) (curie t)))
                        {} (entity-refs tri props))]
    (cond-> {"@id"   (curie id)
             "@type" (curie (etype props))}
      (seq aspects) (assoc "atlas:aspect" (mapv curie (sort aspects)))
      true          (merge (into {} (map (fn [[k vs]] [k (vec (distinct vs))]) by-verb))))))

;; ---------------------------------------------------------------------------
;; @context
;; ---------------------------------------------------------------------------

(defn- prefix-of
  "Prefix of a CURIE string (\"domain:auth\" -> \"domain\"); nil for @-keywords
   and non-CURIEs."
  [s]
  (when (and (string? s) (not (str/starts-with? s "@")) (str/includes? s ":"))
    (subs s 0 (str/index-of s ":"))))

(defn- collect-namespaces
  "All keyword namespaces appearing across the projected nodes (subjects, types,
   aspects, edge targets, verbs) — one @context prefix each."
  [nodes]
  (->> nodes
       (mapcat (fn [n]
                 (for [[k v] n
                       x (cons k (if (sequential? v) v [v]))
                       :let [p (prefix-of x)]
                       :when p]
                   p)))
       (into (sorted-set))))

(defn- build-context
  "The @context: a prefix per namespace + a term def per reference property
   (property CURIE -> verb IRI, typed as an @id set)."
  [base namespaces tri]
  (let [prefixes (into {} (map (fn [ns] [ns (str base ns "/")]) namespaces))
        ref-terms (into {}
                        (for [[_src refs] tri
                              {:keys [property verb]} refs]
                          [(curie property) {"@id"        (curie verb)
                                             "@type"      "@id"
                                             "@container" "@set"}]))]
    (merge prefixes
           {"atlas:aspect" {"@type" "@id" "@container" "@set"}}
           ref-terms)))

;; ---------------------------------------------------------------------------
;; Selection (same helpers as atlas.adapter.overarch)
;; ---------------------------------------------------------------------------

(defn aspect-selector
  ([aspect] (aspect-selector aspect nil))
  ([aspect types]
   (fn [cid props]
     (and (contains? cid aspect)
          (or (nil? types) (contains? types (etype props)))))))

(defn aspects-selector
  ([aspects] (aspects-selector aspects nil))
  ([aspects types]
   (fn [cid props]
     (and (some cid aspects)
          (or (nil? types) (contains? types (etype props)))))))

;; ---------------------------------------------------------------------------
;; Projection
;; ---------------------------------------------------------------------------

(defn registry->jsonld
  "Project an Atlas `registry` into a JSON-LD document (Clojure data):
   {\"@context\" {…} \"@graph\" [nodes…]}.

   opts:
     :select   (fn [compound-id props] -> bool)   required
     :base     base IRI (default `default-base`)"
  [registry {:keys [select base] :or {base default-base}}]
  (let [tri      (type-ref-index registry)
        selected (for [[cid props] registry :when (select cid props)] [cid props])
        nodes    (mapv (fn [[cid props]] (->node cid props tri)) selected)
        namespaces (collect-namespaces nodes)]
    {"@context" (build-context base namespaces tri)
     "@graph"   (vec (sort-by #(get % "@id") nodes))}))

;; ---------------------------------------------------------------------------
;; Serialization
;; ---------------------------------------------------------------------------

(defn ->jsonld [doc] (json/write-str doc {:indent true}))

(declare ^:private yaml-lines)

(defn- scalar->yaml
  "Render a scalar as YAML. Plain (unquoted) only when safe: must start with an
   alphanumeric/_/./- (NOT a YAML indicator like @ ! ? : - etc.) and contain no
   `: ` or ` #`. Otherwise double-quote via JSON (valid YAML double-quoted form)
   — this is what keeps JSON-LD's @-keywords valid in YAML-LD."
  [x]
  (let [s (str x)]
    (if (and (seq s)
             (re-matches #"[A-Za-z0-9_./][A-Za-z0-9_.:/@!?#\-]*" s)
             (not (str/includes? s ": "))
             (not (str/includes? s " #")))
      s
      (json/write-str s))))

(defn- yaml-lines [x indent]
  (let [pad (apply str (repeat indent "  "))]
    (cond
      (map? x)
      (mapcat (fn [[k v]]
                (if (coll? v)
                  (cons (str pad (scalar->yaml k) ":") (yaml-lines v (inc indent)))
                  [(str pad (scalar->yaml k) ": " (scalar->yaml v))]))
              x)
      (sequential? x)
      (mapcat (fn [v]
                (if (coll? v)
                  ;; render children one level deeper (aligned under "- "), then
                  ;; rewrite the first line's indent into the "- " bullet
                  (let [ls   (yaml-lines v (inc indent))
                        head (subs (first ls) (count (str pad "  ")))]
                    (cons (str pad "- " head) (rest ls)))
                  [(str pad "- " (scalar->yaml v))]))
              x)
      :else [(str pad (scalar->yaml x))])))

(defn ->yaml
  "Render the JSON-LD doc as YAML-LD block text (minimal emitter for the
   map/vector/scalar shape this adapter produces)."
  [doc]
  (str "# Generated by atlas.adapter.yaml-ld\n"
       (str/join "\n" (yaml-lines doc 0)) "\n"))

(defn emit!
  "Project `registry` and write `<path>.jsonld` and `<path>.yaml`.
   Returns {:nodes n :namespaces m :context-terms k}."
  [registry path opts]
  (let [doc (registry->jsonld registry opts)]
    (io/make-parents (io/file (str path ".jsonld")))
    (spit (str path ".jsonld") (->jsonld doc))
    (spit (str path ".yaml")   (->yaml doc))
    {:nodes          (count (get doc "@graph"))
     :namespaces     (count (filter (fn [[_ v]] (string? v)) (get doc "@context")))
     :context-terms  (count (get doc "@context"))}))
