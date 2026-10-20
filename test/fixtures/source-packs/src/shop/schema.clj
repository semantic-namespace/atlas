(ns shop.schema)

(def registry
  "The shop's malli registry."
  {:order/id :uuid
   :order/lines [:vector [:map [:product/id :uuid] [:qty :int]]]
   ;; overrides one of malli's own schemas: not a data key
   :map (:map (m/base-schemas))})
