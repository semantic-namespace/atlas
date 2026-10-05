(ns atlas.review.server
  (:require [atlas.review.decorate]
            [atlas.review.registry :as reg]
            [sdiff.serve :as serve]))

(defn -main [& args]
  (let [opt (fn [k d] (or (second (drop-while #(not= k %) args)) d))
        store (opt "--store" nil) prefix (opt "--prefix" nil)]
    (when-not (and store prefix)
      (println "usage: --store <registry store checkout> --prefix <path inside it> [--branch origin/main] [--port 7878] [--host 127.0.0.1]")
      (System/exit 2))
    (reg/configure! store prefix (opt "--branch" "origin/main"))
    (let [{:keys [url]} (serve/start! (parse-long (opt "--port" "7878")) (opt "--host" "127.0.0.1"))]
      (println (str "atlas review server on " url "  (registry from " store "/" (:prefix @reg/config) "; Ctrl-C to stop)"))
      @(promise))))
