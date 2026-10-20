(ns shop.scheduler
  "The shop's own little registry: scheduled jobs, declared with defjob.")

(defmacro defjob [name opts & body]
  `(def ~name (assoc ~opts :job/run (fn [] ~@body))))
