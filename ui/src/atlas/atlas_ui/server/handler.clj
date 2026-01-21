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

(defn- registry-payload [registry-atom]
  (let [registry @registry-atom]
    {:atlas-ui.api.response/registry registry
     :atlas-ui.api.response/aspect-stats (ide/list-aspects registry)
     :timestamp (System/currentTimeMillis)
     :count (count registry)}))

(defn- registry-edn-handler
  "Handler that returns the current registry as EDN.

  Note: we intentionally avoid `*print-namespace-maps*` here to keep the
  output maximally compatible across EDN readers."
  [registry-atom _request]
  (cors-headers
   {:status 200
    :headers {"Content-Type" "application/edn"}
    :body (pr-str (registry-payload registry-atom))}))

(defn- transit-bytes [data]
  (let [out (ByteArrayOutputStream.)
        writer (transit/writer out :json)]
    (transit/write writer data)
    (.toByteArray out)))

(defn- registry-transit-handler
  "Handler that returns the current registry as Transit+JSON."
  [registry-atom _request]
  (cors-headers
   {:status 200
    :headers {"Content-Type" "application/transit+json"}
    :body (ByteArrayInputStream. (transit-bytes (registry-payload registry-atom)))}))

(defn- request-wants-transit?
  [request]
  (let [accept (or (get-in request [:headers "accept"]) "")]
    (boolean (re-find #"application/transit\+json" accept))))

(defn registry-handler
  "Handler that returns the current registry.

  Responds with Transit+JSON when the request `Accept` header includes
  `application/transit+json`, otherwise defaults to EDN."
  [registry-atom request]
  (if (request-wants-transit? request)
    (registry-transit-handler registry-atom request)
    (registry-edn-handler registry-atom request)))

(defn options-handler
  "Handler for CORS preflight requests"
  [_request]
  (cors-headers
   {:status 200
    :headers {"Content-Type" "text/plain"}
    :body "OK"}))

(comment
  ;; Test the handler
  (registry-handler {}))
