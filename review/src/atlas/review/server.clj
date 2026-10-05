(ns atlas.review.server
  (:require [atlas.review.decorate :as decorate]
            [atlas.review.registry :as reg]
            [sdiff.serve :as serve]))

(defn -main [& args]
  (let [opt (fn [k d] (or (second (drop-while #(not= k %) args)) d))
        org (opt "--org" nil) project (opt "--project" nil)]
    (when-not (and org project) (println "usage: --org <org> --project <project> [--port 7878] [--host 127.0.0.1]") (System/exit 2))
    (decorate/configure! org project)
    (let [{:keys [url]} (serve/start! (parse-long (opt "--port" "7878")) (opt "--host" "127.0.0.1"))]
      (println (str "atlas review server on " url "  (" org "/" project " from " reg/*cloud-url* "; Ctrl-C to stop)"))
      @(promise))))
