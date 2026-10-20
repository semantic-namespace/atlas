(ns shop.handlers)

(defn ^{:atlas.source/id :fn/register-user} register-user
  [{:user/keys [email name phone]}]
  {:user/id (random-uuid) :user/email email :user/name name :user/phone phone})

(defn ^{:atlas.source/id :fn/place-order} place-order
  [{:user/keys [id] :order/keys [lines] :as ctx}]
  {:order/id (random-uuid)
   :order/total (- (reduce + (map :qty lines)) (:order/discount ctx 0))})
