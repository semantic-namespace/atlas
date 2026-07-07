(ns atlas.llm-ide.view
  "Composite registry view for LLM-driven SDD workflow.
  
   `view-summary` packages four lenses on the active registry into one payload:
   - shape (entity-count + by-type)
   - connections (type×property edge matrix derived from type-refs)
   - per-type aspect fingerprint
   - invariants status (live invariants run against the active registry)
   - anchors (high-incoming-dep entities to touch with care)

   Honours `*registry-override*`, so the same call works against a
   cloud-pulled snapshot once the override is bound."
  (:require [atlas.registry :as reg]
            [atlas.invariant :as inv]
            [clojure.spec.alpha :as s]))

;; =============================================================================
;; HELPERS
;; =============================================================================

(defn- atlas-type
  "Pick the :atlas/<x> member of a compound-id (i.e. its entity type)."
  [compound-id]
  (some #(when (= "atlas" (namespace %)) %) compound-id))

(defn- dev-id->type
  "Index dev-id → entity-type for the given registry map."
  [reg]
  (into {}
        (keep (fn [[cid props]]
                (when-let [dev-id (:atlas/dev-id props)]
                  (when-let [t (atlas-type cid)]
                    [dev-id t]))))
        reg))

(defn- entities-of-type
  [reg t]
  (filter (fn [[cid _]] (contains? cid t)) reg))

(defn- by-type
  [reg]
  (->> reg
       (keep (fn [[cid _]] (atlas-type cid)))
       frequencies))

;; =============================================================================
;; TYPE×PROPERTY CONNECTION MATRIX
;; =============================================================================

(defn- coerce-targets
  "Normalise a property value into a flat seq of dev-id keywords."
  [v]
  (cond
    (nil? v)                  []
    (qualified-keyword? v)    [v]
    (or (set? v) (sequential? v)) (filter qualified-keyword? v)
    :else                     []))

(defn- type-ref-edges
  "Walk all :atlas/type-ref declarations and count actual edge usage per
   (source-type, property). Returns rows enriched with the *distribution*
   of target entity types so the LLM sees what each property actually links to."
  [reg id->type]
  (let [type-refs (->> reg
                       (keep (fn [[cid props]]
                               (when (and (contains? cid :atlas/type-ref)
                                          (:type-ref/property props))
                                 props))))]
    (for [{:type-ref/keys [source property datalog-verb cardinality]} type-refs
          :let [source-ents (entities-of-type reg source)
                {:keys [rows-using edges all-targets]}
                (reduce (fn [acc [_ props]]
                          (let [tgts (coerce-targets (get props property))]
                            (-> acc
                                (update :rows-using + (if (seq tgts) 1 0))
                                (update :edges + (count tgts))
                                (update :all-targets into tgts))))
                        {:rows-using 0 :edges 0 :all-targets #{}}
                        source-ents)
                target-types (->> all-targets (keep id->type) frequencies)]]
      {:source        source
       :property      property
       :verb          datalog-verb
       :cardinality   cardinality
       :rows-using    rows-using
       :edges         edges
       :target-types  target-types
       ::raw-targets  all-targets})))

;; =============================================================================
;; FINGERPRINT — per-type aspect distribution
;; =============================================================================

(defn- by-type-fingerprint
  "For each entity type, return total count and top-N most common aspects.
   Filters out :atlas/* meta aspects since they're infrastructure tags."
  [reg top-n]
  (let [groups (group-by (fn [[cid _]] (atlas-type cid)) reg)]
    (into {}
          (for [[t entries] groups
                :when t
                :let [aspect-counts (->> entries
                                         (mapcat (fn [[cid _]] (disj cid t)))
                                         (remove #(= "atlas" (namespace %)))
                                         frequencies)
                      top-aspects (->> aspect-counts
                                       (sort-by (comp - val))
                                       (take top-n)
                                       (mapv (fn [[k n]] [k n])))]]
            [t {:total       (count entries)
                :top-aspects top-aspects}]))))

;; =============================================================================
;; ANCHORS — top entities by incoming-dep count
;; =============================================================================

(defn- anchors
  "Top-N entities by *incoming* dep-edge count, computed from the same
   type-ref edge data used for the connection matrix. No datalog needed —
   keeps anchors consistent with what `:overview/connections` reports."
  [edge-rows id->type top-n]
  (let [incoming (->> edge-rows
                      (mapcat ::raw-targets)
                      frequencies
                      (sort-by (comp - val))
                      (take top-n))]
    (mapv (fn [[dev-id n]]
            {:dev-id   dev-id
             :incoming n
             :type     (get id->type dev-id)})
          incoming)))

;; =============================================================================
;; INVARIANTS — live invariants against active registry
;; =============================================================================

(defn- invariants-status
  "Run all live invariants. When called under *registry-override*, the invariant
   *bodies* still come from the live registry (snapshots strip :invariant/fn),
   but their datalog evaluation runs against the override DB. Tagged so the LLM
   knows where the rules came from."
  []
  (try
    (let [report (inv/check-all)
          violations (or (:violations report) [])
          by-inv (group-by :invariant violations)
          failing (mapv (fn [[inv-id vs]]
                          {:invariant inv-id
                           :count     (count vs)
                           :sample    (vec (take 3 (map :entity vs)))})
                        by-inv)
          total (count (inv/all-invariants))]
      {:total            total
       :passing          (max 0 (- total (count failing)))
       :failing          failing
       :checked-against  :live-registry})
    (catch #?(:clj Exception :cljs :default) e
      {:error           #?(:clj (.getMessage e) :cljs (.-message e))
       :checked-against :live-registry})))

;; =============================================================================
;; PUBLIC API
;; =============================================================================

(defn view-summary
  "Composite registry view. Honours `*registry-override*` so the same call
   works against pulled snapshots.

   Options (all optional):
   - :query/top-aspects     — per-type aspect rows  (default 10)
   - :query/top-anchors     — anchor entities       (default 10)
   - :query/top-connections — connection rows       (default 30)"
  ([] (view-summary {}))
  ([{:query/keys [top-aspects top-anchors top-connections]
     :or {top-aspects 10 top-anchors 10 top-connections 30}}]
   (let [reg       (reg/current-registry)
         id->type  (dev-id->type reg)
         all-edges (type-ref-edges reg id->type)
         conns     (->> all-edges
                        (sort-by (comp - :edges))
                        (take top-connections)
                        (mapv #(dissoc % ::raw-targets)))]
     {:overview/entity-count        (count reg)
      :overview/by-type             (by-type reg)
      :overview/connections         conns
      :overview/by-type-fingerprint (by-type-fingerprint reg top-aspects)
      :overview/invariants          (invariants-status)
      :overview/anchors             (anchors all-edges id->type top-anchors)})))

;; =============================================================================
;; SELF-REGISTRATION
;; =============================================================================

(s/def :query/top-aspects nat-int?)
(s/def :query/top-anchors nat-int?)
(s/def :query/top-connections nat-int?)

(reg/register!
 :atlas.llm-ide/view-summary
 :atlas/execution-function
 #{:tier/tooling :domain/llm-ide :intent/explain :tool/view-summary}
 {:execution-function/context  [:query/top-aspects :query/top-anchors :query/top-connections]
  :execution-function/response [:overview/entity-count :overview/by-type
                                :overview/connections :overview/by-type-fingerprint
                                :overview/invariants :overview/anchors]
  :execution-function/deps     #{}
  :atlas/docs "Composite registry view for SDD: shape + type×property edge matrix + per-type aspect fingerprint + invariant pass/fail + top anchor entities. Works against cloud snapshots via cloud--version. Invariants are sourced from the LIVE registry (snapshots strip invariant fns) and tagged :checked-against :live-registry."
  :atlas/impl                  view-summary})
