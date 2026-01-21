(ns dev
  (:require [clojure.tools.namespace.repl :refer [refresh]]
            [app.calendar-availability :as calendar-app]
            [app.cart :as cart-app]
            [atlas.ontology :as ontology]
            [app.pet-shop :as pet-shop-app]
            [atlas.registry :as registry]
            [atlas.atlas-ui.server :as ui]
            ))



(comment
  ;; Refresh code
  (refresh)
  ;; reset registry
  (reset! registry/registry {})

  ;; let's add ontology
;;  (ontology/register-entity-types!)
  
  ;; Initialize example calendar-availability registry
;;  (cart-app/init-registry!)
  (calendar-app/init-registry!)

  ;; Start Atlas UI
  (ui/start! registry/registry {:port 8082 :ui-version :v2})
  (ui/stop! 8082)
  (ui/status)
  )
