(ns shop.api)

(defn ^{:atlas.source/id :endpoint/order} order
  [req]
  {:status 200 :body (:order req)})
