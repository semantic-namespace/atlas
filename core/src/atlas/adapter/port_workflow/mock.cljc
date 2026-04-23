(ns atlas.adapter.port-workflow.mock)

(defmulti producer
  (fn [job-type _payload deps] job-type))

(defmethod producer :default
  [_ _ _]  
 )

