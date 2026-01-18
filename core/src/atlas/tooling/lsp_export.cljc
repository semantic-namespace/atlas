(ns atlas.tooling.lsp-export
  "Export semantic registry data for LSP consumption.

  Exports to .clj-kondo/.cache/ for integration with clj-kondo and clj-lsp."
  (:require [atlas.registry :as cid]
            [atlas.query :as q]
            [atlas.cljc.platform :as platform]
            #?(:clj [clojure.data.json :as json])
            #?(:clj [clojure.java.io :as io])))

;; =============================================================================
;; HELPERS
;; =============================================================================

(defn- extract-aspect-by-ns
  "Extract first aspect with given namespace from identity set."
  [identity-set ns-name]
  (first (filter #(= ns-name (namespace %)) identity-set)))

(defn- aspects->grouped-map
  "Convert aspects to map grouped by namespace.
  E.g., #{:tier/foundation :domain/users :operation/read :operation/write}
  becomes {\"tier\" [\"foundation\"], \"domain\" [\"users\"], \"operation\" [\"read\" \"write\"]}"
  [identity-set]
  (->> identity-set
       (group-by namespace)
       (map (fn [[ns aspects]]
              [ns (mapv name aspects)]))
       (into {})))

(defn- identity->export-map
  "Convert a single identity set to export format."
  [identity-set]
  (let [value (cid/fetch identity-set)
        dev-id (:atlas/dev-id value)]
    {:dev-id (str dev-id)
     :identity (vec (sort (map str identity-set)))
     :aspects (aspects->grouped-map identity-set)
     :docs (:docs/content value)
     :tier (when-let [t (extract-aspect-by-ns identity-set "tier")]
             (str t))
     :domain (when-let [d (extract-aspect-by-ns identity-set "domain")]
               (str d))
     ;; Include context/response for functions
     :context (when-let [ctx (:interface-endpoint/context value)]
                (mapv str ctx))
     :response (when-let [resp (:interface-endpoint/response value)]
                 (mapv str resp))
     :deps (when-let [deps (:execution-function/deps value)]
             (mapv str deps))}))

;; =============================================================================
;; EXPORT FUNCTIONS
;; =============================================================================

(defn export-registry-for-lsp
  "Export registry to .clj-kondo/.cache for LSP navigation.

  Creates two files:
  - semantic-registry.json: Full registry with aspects, docs, metadata
  - semantic-definitions.json: Minimal format for go-to-definition"
  []
  #?(:clj
     (let [all-entries (q/all-identities @cid/registry)
           export-data {:identities (map identity->export-map all-entries)
                        :aspects
                        (let [freq (q/aspect-frequency @cid/registry)]
                          (map (fn [[aspect cnt]]
                                 {:aspect (str aspect)
                                  :namespace (namespace aspect)
                                  :name (name aspect)
                                  :usage-count cnt})
                               freq))
                        :metadata
                        {:total-identities (count all-entries)
                         :total-aspects (count (q/aspect-frequency @cid/registry))
                         :exported-at (platform/now-ms)}}

           output-dir ".clj-kondo/.cache"
           registry-file (str output-dir "/semantic-registry.json")
           definitions-file (str output-dir "/semantic-definitions.json")]

       ;; Write full registry
       (io/make-parents registry-file)
       (with-open [w (io/writer registry-file)]
         (json/write export-data w))

       ;; Write definitions map (dev-id -> identity info)
       ;; Note: Source location tracking would need to be added to cid/register!
       (let [definitions (->> all-entries
                              (map (fn [identity-set]
                                     (let [value (cid/fetch identity-set)
                                           dev-id (:atlas/dev-id value)]
                                       [(str dev-id)
                                        {:identity (vec (sort (map str identity-set)))
                                         ;; Source location - populated if available
                                         :file (:source/file value)
                                         :line (:source/line value)
                                         :column (:source/column value)}])))
                              (into {}))]
         (with-open [w (io/writer definitions-file)]
           (json/write {:definitions definitions
                        :note "Source locations require :source/file :source/line :source/column in registry values"}
                       w)))

       {:exported-identities (count all-entries)
        :exported-aspects (count (q/aspect-frequency @cid/registry))
        :files [registry-file definitions-file]})
     :cljs (platform/unsupported! :lsp-export)))

;; =============================================================================
;; WATCH WITH DEBOUNCE
;; =============================================================================

(def ^:private export-scheduled (atom false))
(def ^:private debounce-ms 1000)

(defn- schedule-export!
  "Schedule an export after debounce period."
  []
  #?(:clj
     (when (compare-and-set! export-scheduled false true)
       (future
         (Thread/sleep debounce-ms)
         (try
           (export-registry-for-lsp)
           (finally
             (reset! export-scheduled false)))))
     :cljs (platform/unsupported! :lsp-export)))

(defn watch-and-export
  "Watch registry for changes and auto-export (debounced).
  Waits for registry to be stable for 1 second before exporting."
  []
  #?(:clj
     (add-watch cid/registry ::lsp-export
                (fn [_ _ old new]
                  (when (not= old new)
                    (schedule-export!))))
     :cljs (platform/unsupported! :lsp-export)))

(defn unwatch
  "Stop watching registry."
  []
  #?(:clj (remove-watch cid/registry ::lsp-export)
     :cljs (platform/unsupported! :lsp-export)))

;; =============================================================================
;; CONVENIENCE
;; =============================================================================

(defn export-now!
  "Export immediately (bypasses debounce)."
  []
  (export-registry-for-lsp))

(comment
  ;; Export registry for LSP
  (export-now!)
  ;; => {:exported-identities 42, :exported-aspects 35, :files [...]}

  ;; Watch for changes (debounced)
  (watch-and-export)

  ;; Stop watching
  (unwatch)
  )
