(ns atlas.cljc.platform
  "Cross-platform helpers for JVM Clojure and ClojureScript.")

(def timeout-sentinel ::timeout)

(defn timeout?
  "Return true if value represents a timeout from `call-with-timeout`."
  [value]
  (= value timeout-sentinel))

(defn now-ms
  "Return the current time in milliseconds on all supported platforms."
  []
  #?(:clj  (System/currentTimeMillis)
     :cljs (.getTime (js/Date.))))

(defn call-with-timeout
  "Invoke `f` with a timeout on JVM, or directly on ClojureScript.

  On Clojure, the function runs in a future and is cancelled if it exceeds
  `timeout-ms`, returning ::timeout. On ClojureScript, futures and blocking
  timeouts are not available, so the function executes immediately."
  [timeout-ms f]
  #?(:clj  (let [fut (future (f))
                 result (deref fut timeout-ms timeout-sentinel)]
             (when (timeout? result)
               (future-cancel fut))
             result)
     :cljs (f)))

(defn unsupported!
  "Raise a consistent unsupported-platform error for JVM-specific features."
  [feature]
  (throw (ex-info (str (name feature) " is only supported on JVM Clojure")
                  {:feature feature
                   :platform :cljs})))
