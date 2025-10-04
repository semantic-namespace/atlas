(ns atlas-ui.api
  (:require [ajax.core :as ajax]
            [atlas-ui.sample-registry :as sample]
            [atlas-ui.config :as config]
            [cognitect.transit :as transit]))

(def ^:private transit-reader
  (transit/reader :json))

(defn- decode-transit [s]
  (transit/read transit-reader s))

(defn fetch-registry!
  "Fetch registry from backend server (Transit+JSON)."
  [on-success on-error]
  (ajax/GET (str (config/api-base-url) "/api/atlas/registry")
    {:handler (fn [response-text]
                (try
                  (on-success (decode-transit response-text))
                  (catch js/Error e
                    (js/console.error "Error parsing registry:" e)
                    (on-error e))))
     :error-handler on-error
     :headers {"Accept" "application/transit+json"}
     :response-format :text}))

(defn fetch-registry-with-fallback!
  "Fetch registry with fallback to sample data if server unavailable"
  [on-success]
  (js/console.log "Fetching registry from backend...")
  (fetch-registry!
   (fn [data]
     (js/console.log "Registry fetch SUCCESS")
     (on-success data))
   (fn [error]
     (js/console.warn "Could not fetch from server, using sample data" error)
     ;; Provide minimal sample data for development
     (on-success {:atlas-ui.api.response/registry sample/sample-registry}))))
