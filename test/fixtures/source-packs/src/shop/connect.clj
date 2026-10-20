(ns shop.connect
  "A project's own wrapper: defresolver's arguments, plus whatever the
   project adds around it."
  (:require [com.wsscode.pathom3.connect.operation :as pco]))

(defmacro defresolver [name & args]
  `(pco/defresolver ~name ~@args))
