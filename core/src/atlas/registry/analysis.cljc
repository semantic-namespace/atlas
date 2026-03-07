(ns atlas.registry.analysis
  "Analytical utilities over the registry: clustering, correlation,
   anomaly detection, heuristics, and visualization.
   Separated from the kernel (atlas.registry) to keep the kernel
   dependency-free of atlas.query."
  (:require [atlas.registry :as registry]
            [atlas.query :as query]
            [clojure.string :as str]
            #?(:cljs [goog.string :as gstring])))

#?(:clj (defn- format* [fmt & args]
          (apply format fmt args))
   :cljs (defn- format* [fmt & args]
           (apply gstring/format fmt args)))

;; =============================================================================
;; Clustering & Correlation
;; =============================================================================

(defn clusters
  "Group identities by primary namespace, returning a map {:ns [identities]}."
  []
  (reduce (fn [acc id-set]
            (reduce (fn [inner aspect]
                      (let [ns-part (namespace aspect)]
                        (update inner (keyword ns-part)
                                (fnil conj #{}) id-set)))
                    acc id-set))
          {}
          (query/all-identities @registry/registry)))

(defn correlation-matrix
  "Return normalized correlation matrix of aspect co-occurrence (as 0-1 floats)."
  []
  (let [aspects (keys (query/aspect-frequency @registry/registry))
        all-ids (query/all-identities @registry/registry)
        total (max 1 (count all-ids))]
    (into {}
          (for [a1 aspects]
            [a1 (into {}
                      (for [a2 aspects]
                        [a2 (/ (count (filter #(and (contains? % a1)
                                                    (contains? % a2))
                                              all-ids))
                               total)]))]))))

;; =============================================================================
;; Heuristics & Diagnostics
;; =============================================================================

(defn missing-aspects
  "Suggest aspects potentially missing from an identity based on correlation."
  [identity-set]
  (let [similar (query/semantic-similarity @registry/registry identity-set 0.0)
        all-aspects (->> similar
                         (mapcat :identity)
                         (frequencies)
                         (sort-by val >))]
    (->> all-aspects
         (remove (fn [[aspect _]] (contains? identity-set aspect)))
         (take 5)
         (mapv (fn [[aspect freq]]
                 {:aspect aspect
                  :correlation (/ freq (count similar))})))))

(defn find-anomalies
  "Return list of identities that violate expected semantic constraints."
  []
  (let [all-ids (query/all-identities @registry/registry)]
    (->> all-ids
         (filter (fn [id]
                   (or (and (contains? id :sync/operation)
                            (contains? id :async/operation))
                       (and (contains? id :api/endpoint)
                            (not (contains? id :auth/required)))))))))

;; =============================================================================
;; Visualization
;; =============================================================================

(defn to-graphviz
  "Return a Graphviz DOT string representing compound-identity relationships."
  []
  (let [edges (for [id (query/all-identities @registry/registry)
                    aspect id]
                [aspect id])]
    (str "digraph CompoundIdentity {\n"
         "  rankdir=LR;\n"
         "  node [shape=box];\n"
         (str/join "\n"
                   (for [[aspect id] edges]
                      (format* "  \"%s\" -> \"%s\";"
                               (pr-str aspect)
                               (pr-str id))))
         "\n}")))

(defn to-mermaid
  "Return a Mermaid diagram (graph TD) for namespace-based clusters."
  []
  (let [clusters (clusters)]
    (str "graph TD\n"
         (str/join "\n"
                   (for [[ns-key ids] clusters]
                     (str "  subgraph " (name ns-key) "\n"
                          (str/join "\n"
                                     (for [id ids]
                                       (format* "    %s[\"%s\"]"
                                                (hash id)
                                                (str/join ", " (map name id)))))
                          "\n  end"))))))

;; =============================================================================
;; Summary
;; =============================================================================

(defn summary
  "Return data summary of the current identity registry:
   {:total :unique-aspects :namespaces :top-aspects :largest :anomalies}"
  []
  (let [ids (query/all-identities @registry/registry)
        aspects (query/aspect-frequency @registry/registry)
        clusters (clusters)]
    {:total (count ids)
     :unique-aspects (count aspects)
     :namespaces (keys clusters)
     :top-aspects (take 5 aspects)
     :largest (take 3 (sort-by count > ids))
     :anomalies (find-anomalies)}))
