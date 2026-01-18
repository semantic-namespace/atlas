(ns atlas-ui-v2.api
  "API layer for fetching registry data"
  (:require [ajax.core :refer [GET]]
            [cognitect.transit :as transit]))

(defn api-url []
  (let [host (.-hostname js/location)]
    (str "http://" host ":8082/api/atlas/registry")))

(defn decode-transit [s]
  (let [reader (transit/reader :json)]
    (transit/read reader s)))

(defn fetch-registry!
  "Fetch registry from backend, call on-success or on-error"
  [on-success on-error]
  (js/console.log "Fetching registry from:" (api-url))
  (GET (api-url)
    {:headers {"Accept" "application/transit+json"}
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
                      (on-error {:type :fetch-error :error error}))}))
