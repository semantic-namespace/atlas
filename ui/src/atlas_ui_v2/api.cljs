(ns atlas-ui-v2.api
  "API layer for fetching registry data"
  (:require [ajax.core :refer [GET]]
            [cognitect.transit :as transit]
            [goog.crypt.base64 :as base64]))

(defn get-url-params []
  (let [search-params (js/URLSearchParams. (.-search js/location))]
    {:port (.get search-params "port")
     :user (.get search-params "user")
     :password (.get search-params "password")}))

(defn api-url []
  (let [{:keys [port]} (get-url-params)]
    (if port
      ;; Development: ?port=8082 specified in URL - use current hostname
      (str (.-protocol js/location) "//" (.-hostname js/location) ":" port "/api/atlas/registry")
      ;; Production: use relative URL (same host/port as page)
      "/api/atlas/registry")))

(defn basic-auth-header [user password]
  (when (and user password)
    (let [credentials (str user ":" password)
          encoded (base64/encodeString credentials)]
      (str "Basic " encoded))))

(defn decode-transit [s]
  (let [reader (transit/reader :json)]
    (transit/read reader s)))

(defn fetch-registry!
  "Fetch registry from backend, call on-success or on-error.

   Supports basic auth via URL params:
   ?port=3002&user=atlas&password=atlasp"
  [on-success on-error]
  (let [{:keys [user password]} (get-url-params)
        auth-header (basic-auth-header user password)
        headers (cond-> {"Accept" "application/transit+json"}
                  auth-header (assoc "Authorization" auth-header))]
    (js/console.log "Fetching registry from:" (api-url))
    (when auth-header
      (js/console.log "Using basic auth for user:" user))
    (GET (api-url)
      {:headers headers
       :response-format :text
       :handler (fn [response-text]
                  (try
                    (let [data (decode-transit response-text)]
                      (js/console.log "Registry loaded successfully")
                      (on-success data))
                    (catch :default e
                      (js/console.error "Error decoding transit:" e)
                      (on-error {:type :decode-error :error e}))))
       :error-handler (fn [error]
                        (js/console.warn "API error:" error)
                        (on-error {:type :fetch-error :error error}))})))
