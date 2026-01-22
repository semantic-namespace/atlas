(ns atlas-ui.config)

(goog-define API_BASE_URL "")

(defn api-base-url []
  (let [root (if (exists? js/globalThis) js/globalThis js/window)
        runtime (aget root "ATLAS_API_BASE_URL")
        search-params (js/URLSearchParams. (.-search js/location))
        port (.get search-params "port")]
    (cond
      ;; Query param ?port=8082 (for development) - use current hostname
      port
      (str js/location.protocol "//" js/location.hostname ":" port)

      ;; Runtime global variable
      (and (string? runtime) (seq runtime))
      runtime

      ;; Compile-time goog-define
      (seq API_BASE_URL)
      API_BASE_URL

      ;; Default: same host/port as page
      :else
      (str js/location.protocol "//" js/location.hostname ":" js/location.port))))
