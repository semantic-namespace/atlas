(ns atlas.review.server
  (:require [atlas.review.decorate]
            [atlas.review.reach-review :as reach-review]
            [atlas.review.registry :as reg]
            [sdiff.serve :as serve]))

(defn -main [& args]
  (let [opt (fn [k d] (or (second (drop-while #(not= k %) args)) d))
        store (opt "--store" nil) prefix (opt "--prefix" nil) reach (opt "--reach-config" nil)]
    (when (and store (not prefix))
      (println "usage: [--store <registry store checkout> --prefix <path inside it> [--branch origin/main]] [--reach-config <file>] [--port 7878] [--host 127.0.0.1]")
      (System/exit 2))
    (when store (reg/configure! store prefix (opt "--branch" "origin/main")))
    (when reach (reach-review/configure! reach))
    (let [{:keys [url]} (serve/start! (parse-long (opt "--port" "7878")) (opt "--host" "127.0.0.1"))]
      (println (str "atlas review server on " url
                    (if store (str "  (registry from " store "/" (:prefix @reg/config) ")") "  (no registry store)")
                    (when reach (str "  (reach config " reach ")"))))
      @(promise))))
