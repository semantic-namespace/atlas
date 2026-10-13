(ns atlas-ui-v3.api
  "Data layer: registry fetch (transit) + SSE subscription for v3 state."
  (:require [ajax.core :refer [GET]]
            [cognitect.transit :as transit]
            [goog.crypt.base64 :as base64]))

(defn- get-url-param [name]
  (let [params (js/URLSearchParams. (.-search js/location))]
    (.get params name)))

(defn- api-url []
  (let [port (get-url-param "port")]
    (if port
      (str (.-protocol js/location) "//" (.-hostname js/location) ":" port "/api/atlas/registry")
      "/api/atlas/registry")))

(defn- decode-transit [s]
  (let [reader (transit/reader :json)]
    (transit/read reader s)))

(defn- basic-auth-header []
  (let [user     (get-url-param "user")
        password (get-url-param "password")]
    (when (and user password)
      (str "Basic " (base64/encodeString (str user ":" password))))))

(defn fetch-registry!
  "Fetch full registry via transit. Calls on-success with decoded registry map."
  [on-success on-error]
  (let [auth    (basic-auth-header)
        headers (cond-> {"Accept" "application/transit+json"}
                  auth (assoc "Authorization" auth))]
    (GET (api-url)
      {:headers         headers
       :response-format :text
       :handler         (fn [text]
                          (try
                            (on-success (decode-transit text))
                            (catch :default e
                              (on-error {:type :decode-error :error e}))))
       :error-handler   (fn [err]
                          (on-error {:type :fetch-error :error err}))})))

(defn fetch-live-state!
  "GET /api/v3/state. Calls on-live with the server's view state when the
  server holds one (the atlas-ui server does), on-absent otherwise, so the
  page can navigate on its own where it is mounted without that endpoint."
  [on-live on-absent]
  (let [port (get-url-param "port")
        url  (if port
               (str (.-protocol js/location) "//" (.-hostname js/location) ":" port "/api/v3/state")
               "/api/v3/state")]
    (-> (js/fetch url)
        (.then (fn [r] (if (.-ok r) (.json r) (throw (js/Error. (str "status " (.-status r)))))))
        (.then (fn [data] (on-live (js->clj data :keywordize-keys true))))
        (.catch (fn [_] (on-absent))))))

(defn subscribe-sse!
  "Open SSE connection to /api/v3/events.
  Calls on-state with a keywordized map each time the server pushes state.
  Returns the EventSource object (call .close on it to disconnect)."
  [on-state on-error]
  (let [port (get-url-param "port")
        url  (if port
               (str (.-protocol js/location) "//" (.-hostname js/location) ":" port "/api/v3/events")
               "/api/v3/events")
        es   (js/EventSource. url)]
    (set! (.-onmessage es)
          (fn [e]
            (try
              (on-state (js->clj (js/JSON.parse (.-data e)) :keywordize-keys true))
              (catch :default err
                (js/console.error "SSE parse error:" err)))))
    (set! (.-onerror es)
          (fn [_]
            (when on-error (on-error "SSE connection error"))))
    es))
