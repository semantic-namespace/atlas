(ns atlas-ui.config)

(goog-define API_BASE_URL "")

(defn api-base-url []
  (let [root (if (exists? js/globalThis) js/globalThis js/window)
        runtime (aget root "ATLAS_API_BASE_URL")]
    (cond
      (and (string? runtime) (seq runtime))
      runtime

      (seq API_BASE_URL)
      API_BASE_URL

      :else
      (str js/location.protocol "//" js/location.hostname ":" js/location.port))))
