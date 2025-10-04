(ns atlas.atlas-ui.server
  "Atlas UI Server - Visual explorer for semantic registries

  Unified server combining:
  - Registry API from atlas.atlas-ui.api.handler
  - Static file serving for the compiled UI
  - Developer-friendly API (start!/stop!/status)
  - Browser auto-opening and registry watching"
  (:require [atlas.registry :as reg]
            [atlas.atlas-ui.server.handler :as handler]
            [atlas-ui.sample-registry :as sample]
            [ring.adapter.jetty :as jetty]
            [ring.middleware.resource :as resource]
            [ring.middleware.content-type :as content-type]
            [ring.util.response :as response]))

;; =============================================================================
;; State Management
;; =============================================================================

(defonce ^:private servers (atom {}))
(defonce ^:private registry-watchers (atom {}))

;; =============================================================================
;; HTTP Handlers
;; =============================================================================

(defn- app-handler
  "Ring handler that serves static files and the registry API"
  [registry-atom]
  (fn [request]
    (cond
      ;; API endpoint for registry - reuse existing handler
      (= (:uri request) "/api/atlas/registry")
      (handler/registry-handler registry-atom request)

      ;; CORS preflight
      (and (= (:uri request) "/api/atlas/registry")
           (= (:request-method request) :options))
      (handler/options-handler request)

      ;; Serve index.html for root
      (= (:uri request) "/")
      (response/resource-response "index.html" {:root "public"})

      ;; Serve static files from resources/public
      :else
      ((-> (constantly (response/not-found "Not found"))
           (resource/wrap-resource "public")
           (content-type/wrap-content-type))
       request))))

;; =============================================================================
;; Server Lifecycle
;; =============================================================================

(declare stop!)

(defn start!
  "Start the Atlas UI server.

  Args:
    registry-atom - (Optional) Atom containing the registry map.
                    If not provided, uses atlas.registry/registry.
                    The atom will be watched for changes.
    opts          - Optional map with:
                    :port (default 8082) - HTTP server port
                    :open-browser? (default true) - Automatically open browser
                    :load-sample? (default false) - Load sample registry

  Returns:
    Server instance (can be passed to stop!)

  Examples:
    ;; Use global registry
    (start!)

    ;; Use custom registry atom
    (start! my-registry {:port 8082})

    ;; Load sample data
    (start! {:load-sample? true})"
  ([]
   (start! reg/registry {}))
  ([opts-or-registry]
   (if (map? opts-or-registry)
     (start! reg/registry opts-or-registry)
     (start! opts-or-registry {})))
  ([registry-atom {:keys [port open-browser? load-sample?]
                   :or {port 8082
                        open-browser? true
                        load-sample? false}
                   :as opts}]
   (when (get @servers port)
     (println (str "⚠️  Atlas UI already running on port " port ". Stopping it first..."))
     (stop! port))

   ;; Load sample registry if requested
   (when load-sample?
     (println "Loading sample registry...")
     (reset! registry-atom sample/sample-registry)
     (println "✓ Loaded" (count @registry-atom) "entities"))

   (let [server (jetty/run-jetty (app-handler registry-atom)
                                  {:port port
                                   :join? false})]
     (swap! servers assoc port server)

     ;; Watch registry for changes
     (let [watcher-key (keyword (str "atlas-ui-" port))]
       (remove-watch registry-atom watcher-key)
       (add-watch registry-atom watcher-key
                  (fn [_key _ref _old new]
                    (println "🔄 Registry updated (" (count new) " entities) - refresh browser to see changes")))
       (swap! registry-watchers assoc port {:key watcher-key :atom registry-atom}))

     (let [url (str "http://localhost:" port)]
       (println "┌─────────────────────────────────────────────────────")
       (println "│ 🚀 Atlas UI Server Started")
       (println "├─────────────────────────────────────────────────────")
       (println (str "│ 🌐 URL:      " url))
       (println (str "│ 📊 Registry: " (count @registry-atom) " entities"))
       (println "├─────────────────────────────────────────────────────")
       (println "│ 💡 Tips:")
       (println "│   - Registry updates require browser refresh")
       (println (str "│   - Use (stop! " port ") to shut down"))
       (println "│   - Press ESC in UI to clear selections")
       (println "│   - Press L for lens selector")
       (println "└─────────────────────────────────────────────────────")

       ;; Open browser
       (when open-browser?
         (try
           (let [os (System/getProperty "os.name")]
             (cond
               (.contains os "Mac")
               (.exec (Runtime/getRuntime) (into-array ["open" url]))

               (.contains os "Windows")
               (.exec (Runtime/getRuntime) (into-array ["cmd" "/c" "start" url]))

               :else ; Linux/Unix
               (.exec (Runtime/getRuntime) (into-array ["xdg-open" url]))))
           (catch Exception e
             (println (str "⚠️  Could not open browser automatically: " (.getMessage e)))
             (println (str "   Please open manually: " url)))))

       server))))

(defn stop!
  "Stop the Atlas UI server running on the given port.

  Args:
    port-or-server - Either a port number (default 8082) or server instance

  Example:
    (stop! 8082)
    ;; or
    (stop! server-instance)"
  ([]
   (stop! 8082))
  ([port-or-server]
   (let [port (if (number? port-or-server)
                port-or-server
                (first (for [[p s] @servers :when (= s port-or-server)] p)))]
     (when-let [server (get @servers port)]
       (.stop server)
       (swap! servers dissoc port)

       ;; Remove watcher
       (when-let [watcher-info (get @registry-watchers port)]
         (remove-watch (:atom watcher-info) (:key watcher-info))
         (swap! registry-watchers dissoc port))

       (println (str "🛑 Atlas UI server stopped (port " port ")"))
       true))))

(defn restart!
  "Restart the Atlas UI server on the given port.

  Useful when you want to reload with a fresh registry state.

  Args:
    registry-atom - (Optional) Atom containing the registry map
    opts          - Same options as start!

  Examples:
    (restart!)
    (restart! my-registry {:port 8082})"
  ([]
   (restart! reg/registry {}))
  ([opts-or-registry]
   (if (map? opts-or-registry)
     (restart! reg/registry opts-or-registry)
     (restart! opts-or-registry {})))
  ([registry-atom opts]
   (let [port (:port opts 8082)]
     (stop! port)
     (Thread/sleep 500) ; Give port time to release
     (start! registry-atom opts))))

(defn status
  "Show status of all running Atlas UI servers"
  []
  (if (empty? @servers)
    (println "No Atlas UI servers currently running")
    (doseq [[port _server] @servers]
      (let [watcher-info (get @registry-watchers port)
            entity-count (when watcher-info (count @(:atom watcher-info)))]
        (println (str "✅ Atlas UI running on http://localhost:" port
                      (when entity-count (str " (" entity-count " entities)"))))))))

;; =============================================================================
;; Legacy Compatibility
;; =============================================================================

(defn start-server!
  "Legacy function for compatibility with atlas.atlas-ui.dev-server

  Starts server on port 3000 with sample registry loaded."
  []
  (start! reg/registry {:port 3000 :load-sample? true :open-browser? false}))

(defn stop-server!
  "Legacy function for compatibility with atlas.atlas-ui.dev-server"
  []
  (stop! 3000))

(comment
  ;; Example usage:

  ;; Start with global registry
  (start!)

  ;; Start with custom registry
  (require '[atlas.registry :as registry])
  (start! registry/registry {:port 8082})

  ;; Load sample data
  (start! {:load-sample? true})

  ;; Check status
  (status)

  ;; Stop server
  (stop! 8082)

  ;; Restart with new registry
  (restart! registry/registry {:port 8082})

  ;; Legacy API
  (start-server!) ; Port 3000, sample data
  (stop-server!)
  )
