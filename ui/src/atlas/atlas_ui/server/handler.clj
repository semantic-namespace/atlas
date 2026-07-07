(ns atlas.atlas-ui.server.handler
  "Ring handler to serve Atlas registry (EDN or Transit) for the UI"
  (:require [atlas.registry :as reg]
            [atlas.ide :as ide]
            [cognitect.transit :as transit])
  (:import (java.io ByteArrayInputStream ByteArrayOutputStream)))

(defn cors-headers
  "Add CORS headers for development"
  [response]
  (update response :headers merge
          {"Access-Control-Allow-Origin" "*"
           "Access-Control-Allow-Methods" "GET, POST, OPTIONS"
           "Access-Control-Allow-Headers" "Content-Type, Accept"}))

(defn- transit-safe-leaf? [v]
  (or (nil? v) (string? v) (number? v) (boolean? v)
      (keyword? v) (symbol? v) (inst? v) (uuid? v)))

(defn- clean-value
  "Recursively strip non-transit-safe leaves from any value."
  [v]
  (cond
    (transit-safe-leaf? v) v
    (map? v)        (reduce-kv (fn [m k val]
                                 (let [c (clean-value val)]
                                   (if (nil? c) m (assoc m k c))))
                               {} v)
    (set? v)        (into #{} (keep clean-value v))
    (sequential? v) (into [] (keep clean-value v))
    :else           nil))

(defn- sanitize-registry
  "Prepare registry for UI serialization: recursively strip non-transit-safe values."
  [registry]
  (reduce-kv (fn [m id entity]
               (assoc m id (clean-value entity)))
             {} registry))

(defn- registry-payload [registry]
  (let [sanitized (sanitize-registry registry)]
    {:atlas-ui.api.response/registry sanitized
     :atlas-ui.api.response/aspect-stats (ide/list-aspects registry)
     :timestamp (System/currentTimeMillis)
     :count (count registry)}))

(defn- registry-edn-handler
  "Handler that returns the current registry as EDN.

  Note: we intentionally avoid `*print-namespace-maps*` here to keep the
  output maximally compatible across EDN readers."
  [registry _request]
  (cors-headers
   {:status 200
    :headers {"Content-Type" "application/edn"}
    :body (pr-str (registry-payload registry))}))

(defn- transit-bytes [data]
  (let [out    (ByteArrayOutputStream.)
        writer (transit/writer out :json)]
    (transit/write writer data)
    (.toByteArray out)))

(defn- registry-transit-handler
  "Handler that returns the current registry as Transit+JSON."
  [registry _request]
  (cors-headers
   {:status 200
    :headers {"Content-Type" "application/transit+json"}
    :body (ByteArrayInputStream. (transit-bytes (registry-payload registry)))}))

(defn- request-wants-transit?
  [request]
  (let [accept (or (get-in request [:headers "accept"]) "")]
    (boolean (re-find #"application/transit\+json" accept))))

(defn registry-handler
  "Handler that returns the current registry.

  Takes a registry map (not an atom) and a request.

  Responds with Transit+JSON when the request `Accept` header includes
  `application/transit+json`, otherwise defaults to EDN."
  [registry request]
  (if (request-wants-transit? request)
    (registry-transit-handler registry request)
    (registry-edn-handler registry request)))

(defn options-handler
  "Handler for CORS preflight requests"
  [_request]
  (cors-headers
   {:status 200
    :headers {"Content-Type" "text/plain"}
    :body "OK"}))

(comment
  ;; Test the handler with an empty registry map
  (registry-handler {} {:uri "/api/atlas/registry"}))
