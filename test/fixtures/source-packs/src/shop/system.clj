(ns shop.system
  (:require [clojure.spec.alpha :as s]
            [integrant.core :as ig]))

(defmethod ig/init-key :shop/db [_ {:keys [url]}] {:url url})
(defmethod ig/halt-key! :shop/db [_ _] nil)

;; the composite key's parent
(defmethod ig/init-key :shop/http-server [_ opts] opts)

;; ::keywords: :shop.system/const, and a key no config has
(defmethod ig/init-key ::const [_ v] v)
(defmethod ig/init-key ::unused [_ v] v)

(derive :shop/base-path ::const)

;; a spec under a component's own key: the shape of its config
(s/def :shop/mailer map?)
