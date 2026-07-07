(ns atlas.atlas-ui.server
  "Atlas UI Server - Visual explorer for semantic registries

  Unified server combining:
  - Registry API from atlas.atlas-ui.api.handler
  - Static file serving for the compiled UI
  - Developer-friendly API (start!/stop!/status)
  - Browser auto-opening and registry watching
  - V3: SSE-driven two-momentum layout viewer (POST /api/v3/state, GET /api/v3/events)"
  (:require [atlas.registry :as reg]
            [atlas.atlas-ui.server.handler :as handler]
            [atlas-ui.sample-registry :as sample]
            [ring.middleware.resource :as resource]
            [ring.middleware.file :as file]
            [ring.middleware.content-type :as content-type]
            [ring.util.response :as response]
            [ring.util.servlet :as servlet]
            [clojure.java.io :as io]
            [clojure.data.json :as json])
  (:import [org.eclipse.jetty.server Server ServerConnector
            HttpConfiguration HttpConnectionFactory]
           [org.eclipse.jetty.server.handler AbstractHandler]
           [javax.servlet DispatcherType]
           [javax.servlet.http HttpServletRequest HttpServletResponse]))

;; =============================================================================
;; State Management
;; =============================================================================

(defonce ^:private servers (atom {}))
(defonce ^:private registry-watchers (atom {}))

;; =============================================================================
;; V3 — SSE state (LLM-controlled two-momentum layout)
;; =============================================================================

(defonce v3-state
  (atom {:mode "home" :entity nil :aspect nil :data-key nil :narrative nil
         :scenario nil :step nil :step-total nil}))

;; id -> LinkedBlockingQueue<String>; sse-aware-handler polls per client
(defonce ^:private v3-sse-clients (atom {}))

(defn- setup-v3-sse-watcher! []
  (add-watch v3-state ::sse-broadcast
             (fn [_ _ _ new-state]
               (let [msg (json/write-str new-state)]
                 (doseq [[_id ^java.util.concurrent.LinkedBlockingQueue q] @v3-sse-clients]
                   (.offer q msg))))))

(defn- v3-state-get-handler [_req]
  {:status  200
   :headers {"Content-Type"                "application/json"
             "Access-Control-Allow-Origin" "*"}
   :body    (json/write-str @v3-state)})

(defn- v3-state-post-handler [req]
  (try
    (let [new-state (json/read-str (slurp (:body req)) :key-fn keyword)]
      (swap! v3-state merge new-state)
      {:status  200
       :headers {"Content-Type"                "application/json"
                 "Access-Control-Allow-Origin" "*"}
       :body    (json/write-str @v3-state)})
    (catch Exception e
      {:status  400
       :headers {"Content-Type" "application/json"}
       :body    (json/write-str {:error (.getMessage e)})})))

(defn- v3-narrative-post-handler [req]
  (try
    (let [{:keys [text]} (json/read-str (slurp (:body req)) :key-fn keyword)]
      (swap! v3-state assoc :narrative text)
      {:status  200
       :headers {"Content-Type"                "application/json"
                 "Access-Control-Allow-Origin" "*"}
       :body    (json/write-str {:ok true})})
    (catch Exception e
      {:status  400
       :headers {"Content-Type" "application/json"}
       :body    (json/write-str {:error (.getMessage e)})})))

;; =============================================================================
;; HTTP Handlers
;; =============================================================================

(defn- serve-index [ui-root file-root]
  (or (when file-root (response/file-response "index.html" {:root file-root}))
      (response/resource-response "index.html" {:root ui-root})))

(defn- static-middleware [ui-root file-root]
  (if file-root
    #(file/wrap-file % file-root)
    #(resource/wrap-resource % ui-root)))

(defn- app-handler
  "Ring handler for all routes except /api/v3/events (SSE handled natively).
  file-root is an absolute filesystem path used when classpath resources
  aren't available (e.g. when the JAR version predates the UI version)."
  [registry-atom ui-root file-root]
  (fn [request]
    (cond
      (and (= (:uri request) "/api/v3/state")
           (= (:request-method request) :get))
      (v3-state-get-handler request)

      (and (= (:uri request) "/api/v3/state")
           (= (:request-method request) :post))
      (v3-state-post-handler request)

      (and (= (:uri request) "/api/v3/narrative")
           (= (:request-method request) :post))
      (v3-narrative-post-handler request)

      (= (:uri request) "/api/atlas/registry")
      (handler/registry-handler @registry-atom request)

      (and (= (:uri request) "/api/atlas/registry")
           (= (:request-method request) :options))
      (handler/options-handler request)

      (= (:uri request) "/")
      (serve-index ui-root file-root)

      :else
      ((-> (constantly (response/not-found "Not found"))
           ((static-middleware ui-root file-root))
           (content-type/wrap-content-type))
       request))))

(defn- sse-aware-handler
  "Jetty AbstractHandler wrapping the Ring app-handler.
  /api/v3/events is handled natively: headers are committed via flushBuffer
  before the blocking loop, so each flush delivers data immediately.
  All other requests delegate to the Ring handler via servlet/build-request-map."
  [ring-handler]
  (proxy [AbstractHandler] []
    (handle [_target base-request
             ^HttpServletRequest request
             ^HttpServletResponse response]
      (when-not (= (.getDispatcherType request) DispatcherType/ERROR)
        (.setHandled base-request true)
        (if (= (.getRequestURI request) "/api/v3/events")
          (let [id    (str (java.util.UUID/randomUUID))
                queue (java.util.concurrent.LinkedBlockingQueue.)]
            (.offer queue (json/write-str @v3-state))
            (swap! v3-sse-clients assoc id queue)
            (doto response
              (.setStatus 200)
              (.setContentType "text/event-stream;charset=UTF-8")
              (.setHeader "Cache-Control" "no-cache")
              (.setHeader "Connection" "keep-alive")
              (.setHeader "X-Accel-Buffering" "no")
              (.setHeader "Access-Control-Allow-Origin" "*")
              .flushBuffer)
            (try
              (let [^java.io.PrintWriter writer (.getWriter response)]
                (loop []
                  (let [msg (.poll queue 25 java.util.concurrent.TimeUnit/SECONDS)]
                    (.print writer (if msg (str "data: " msg "\n\n") ": keepalive\n\n"))
                    (.flush writer)
                    (recur))))
              (catch Exception _
                (swap! v3-sse-clients dissoc id))))
          (let [resp-map (ring-handler (servlet/build-request-map request))]
            (when resp-map
              (servlet/update-servlet-response response resp-map))))))))

(defn- create-jetty-server! [ring-handler port]
  (let [server    (Server.)
        http-cfg  (doto (HttpConfiguration.) (.setSendServerVersion false))
        connector (doto (ServerConnector. server (into-array [(HttpConnectionFactory. http-cfg)]))
                    (.setPort port))]
    (.addConnector server connector)
    (.setHandler server (sse-aware-handler ring-handler))
    (.start server)
    server))

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
                    :ui-version (default :v1) - UI version (:v1 or :v2)

  Returns:
    Server instance (can be passed to stop!)

  Examples:
    ;; Use global registry
    (start!)

    ;; Use custom registry atom
    (start! my-registry {:port 8082})

    ;; Load sample data
    (start! {:load-sample? true})

    ;; Use v2 UI
    (start! {:ui-version :v2})"
  ([]
   (start! reg/registry {}))
  ([opts-or-registry]
   (if (map? opts-or-registry)
     (start! reg/registry opts-or-registry)
     (start! opts-or-registry {})))
  ([registry-atom {:keys [port open-browser? load-sample? ui-version file-root]
                   :or {port 8082
                        open-browser? true
                        load-sample? false
                        ui-version :v1}}]
   (when (get @servers port)
     (println (str "⚠️  Atlas UI already running on port " port ". Stopping it first..."))
     (stop! port))

   ;; Load sample registry if requested
   (when load-sample?
     (println "Loading sample registry...")
     (reset! registry-atom sample/sample-registry)
     (println "✓ Loaded" (count @registry-atom) "entities"))

   (reset! v3-sse-clients {})
   (setup-v3-sse-watcher!)
   (let [ui-root   (case ui-version
                     :v2 "public-v2"
                     :v3 "public-v3"
                     "public")
         ;; file-root: explicit override beats classpath (use when JAR fd is stale).
         ;; Auto-fallback to local filesystem when classpath resource is unavailable.
         file-root (or file-root
                       (when (nil? (io/resource (str ui-root "/index.html")))
                         (some (fn [base]
                                 (let [f (java.io.File. (str base "/" ui-root))]
                                   (when (.exists f) (.getAbsolutePath f))))
                               [(System/getProperty "user.dir")
                                (str (System/getProperty "user.home") "/git/semantic-namespace/atlas/ui/resources")
                                "ui/resources"
                                "resources"])))
         _         (when file-root (println (str "│   Static files from: " file-root)))
         server    (create-jetty-server! (app-handler registry-atom ui-root file-root) port)]
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
       (println (str "│ 🎨 UI:       " (name ui-version)))
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
  (stop-server!))
