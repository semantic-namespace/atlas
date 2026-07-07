(ns dev
  (:require [clojure.tools.namespace.repl :refer [refresh]]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [app.calendar-availability :as calendar-app]
            [app.cart :as cart-app]
            [atlas.ontology :as ontology]
            [app.pet-shop :as pet-shop-app]
            [atlas.registry :as registry]
            [atlas.atlas-ui.server :as ui]
            ))
(defn read-edn-resource
  [path]
  (-> path
      (io/resource)
      (slurp)
      (edn/read-string)))

(defn load-registry-snapshot!
  "Replace the live registry with a snapshot EDN file (a `{compound-id props}`
   map, e.g. one pulled from atlas-cloud). `path` is a filesystem path."
  [path]
  (reset! registry/registry (edn/read-string (slurp path))))

(comment
  ;; Refresh code
  (refresh)

  ;; Load a registry snapshot from disk (e.g. pulled via cloud-pull)
  (load-registry-snapshot! "/path/to/snapshot.edn")

  ;; let's add ontology
;;  (ontology/register-entity-types!)

  ;; Initialize example registries
;;  (cart-app/init-registry!)
;;  (pet-shop-app/init-registry!)
  (calendar-app/init-registry!)

  ;; Start Atlas UI
  (ui/start! registry/registry {:port 8082 :ui-version :v2})
  (ui/stop! 8082)
  (ui/status)
  )
