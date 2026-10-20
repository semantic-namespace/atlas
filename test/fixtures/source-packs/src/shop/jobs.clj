(ns shop.jobs
  (:require [shop.scheduler :as scheduler]))

(scheduler/defjob nightly-digest
  {:job/reads [:user/email :order/total]
   :job/schedule "0 3 * * *"}
  (println "digest"))
