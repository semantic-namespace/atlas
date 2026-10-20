(ns shop.resolvers2
  "A Pathom 2 resolver: options are always written."
  (:require [com.wsscode.pathom.connect :as pc]))

(pc/defresolver product-name [env {:product/keys [id]}]
  {::pc/input #{:product/id}
   ::pc/output [:product/name]}
  {:product/name "a product"})
