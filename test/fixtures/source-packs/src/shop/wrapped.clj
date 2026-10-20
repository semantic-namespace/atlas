(ns shop.wrapped
  (:require [shop.connect :as connect]))

(connect/defresolver product-price [{:product/keys [id]}]
  {:product/price 10})
