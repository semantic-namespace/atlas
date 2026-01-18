(ns atlas.atlas-ui.main
  "CLI entry point for atlas-ui server.

   Run standalone:
     clj -M -m atlas.atlas-ui.main

   With options:
     clj -M -m atlas.atlas-ui.main --port 3000 --no-open

   Load a registry from a namespace:
     clj -M -m atlas.atlas-ui.main --load my.app/init-registry!"
  (:require [atlas.atlas-ui.server :as server]
            [atlas.registry :as registry])
  (:gen-class))

(defn- parse-args [args]
  (loop [args args
         opts {:port 8082
               :open-browser? true
               :load-sample? false}]
    (if (empty? args)
      opts
      (let [[arg & rest-args] args]
        (case arg
          ("--port" "-p")
          (recur (rest rest-args) (assoc opts :port (parse-long (first rest-args))))

          ("--no-open" "-n")
          (recur rest-args (assoc opts :open-browser? false))

          ("--sample" "-s")
          (recur rest-args (assoc opts :load-sample? true))

          ("--load" "-l")
          (recur (rest rest-args) (assoc opts :load-fn (first rest-args)))

          ("--help" "-h")
          (assoc opts :help? true)

          ;; Unknown arg, skip
          (recur rest-args opts))))))

(defn- print-help []
  (println "
atlas-ui - Visual explorer for Atlas registries

Usage:
  clj -M -m atlas.atlas-ui.main [options]

Options:
  -p, --port PORT     Server port (default: 8082)
  -n, --no-open       Don't open browser automatically
  -s, --sample        Load sample registry data
  -l, --load FN       Load registry by calling a function (e.g., my.app/init!)
  -h, --help          Show this help

Examples:
  # Start with empty registry
  clj -M -m atlas.atlas-ui.main

  # Start with sample data on port 3000
  clj -M -m atlas.atlas-ui.main --port 3000 --sample

  # Load your app's registry
  clj -M -m atlas.atlas-ui.main --load my.app/init-registry!

From another project (using local atlas-ui):
  clj -Sdeps '{:deps {atlas-ui {:local/root \"/path/to/atlas/ui\"}}}' \\
      -M -m atlas.atlas-ui.main --load my.app/init!
"))

(defn -main [& args]
  (let [opts (parse-args args)]
    (if (:help? opts)
      (print-help)
      (do
        ;; Load registry if --load specified
        (when-let [load-fn-str (:load-fn opts)]
          (let [[ns-str fn-str] (clojure.string/split load-fn-str #"/")
                ns-sym (symbol ns-str)
                fn-sym (symbol fn-str)]
            (println (str "Loading " ns-sym "..."))
            (require ns-sym :reload)
            (println (str "Calling " load-fn-str "..."))
            ((resolve (symbol load-fn-str)))))

        ;; Start server (blocks on this thread)
        (server/start! registry/registry (dissoc opts :load-fn :help?))

        ;; Keep alive
        (println "\nPress Ctrl+C to stop")
        @(promise)))))
