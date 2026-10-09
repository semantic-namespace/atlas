(ns atlas.review.server
  (:require [atlas.review.decorate]
            [atlas.review.deps-bridge :as deps-bridge]
            [atlas.review.ownership :as ownership]
            [atlas.review.registry :as reg]
            [clojure.string :as str]
            [sdiff.serve :as serve]))

(defn -main [& args]
  (let [opt (fn [k d] (or (second (drop-while #(not= k %) args)) d))
        store (opt "--store" nil) prefix (opt "--prefix" nil)
        defining (opt "--defining" "(^|/)(register!|bind|init-key)$")]
    (when (and store (not prefix))
      (println "usage: [--store <registry store checkout> --prefix <path inside it> [--branch origin/main]] [--defining <regex over the var that defines a keyword>] [--verb-namespaces <extra aspect namespaces whose names are verbs, comma separated>] [--port 7878] [--host 127.0.0.1]")
      (System/exit 2))
    (when store (reg/configure! store prefix (opt "--branch" "origin/main")))
    (deps-bridge/install! (re-pattern defining))
    (when-let [vs (opt "--verb-namespaces" nil)]
      (swap! ownership/settings update :verb-namespaces into (str/split vs #",")))
    (ownership/install!)
    (let [{:keys [url]} (serve/start! (parse-long (opt "--port" "7878")) (opt "--host" "127.0.0.1"))]
      (println (str "atlas review server on " url
                    (if store (str "  (registry from " store "/" (:prefix @reg/config) ")") "  (no registry store)")
                    (str "  (keyword bridge " defining ")")))
      @(promise))))
