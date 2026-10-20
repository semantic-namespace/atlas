(ns shop.resolvers
  "Pathom 3 resolvers and a mutation over the shop's data keys."
  ;; a :refer names a var: never a form the pack reads
  (:require [com.wsscode.pathom3.connect.operation :as pco :refer [defresolver]]))

;; input and output declared
(pco/defresolver user-by-email [{:user/keys [email]}]
  {::pco/input  [:user/email]
   ::pco/output [:user/id :user/name]}
  {:user/id (random-uuid) :user/name "someone"})

;; input inferred from the destructuring, output from the returned map
(pco/defresolver order-total [{:order/keys [lines]}]
  {:order/total (reduce + (map :qty lines))})

;; a nested output: a join from a user to its orders
(pco/defresolver user-orders [env {:user/keys [id]}]
  {::pco/output [{:user/orders [:order/id :order/total]}]}
  {:user/orders []})

;; nothing produces :user/loyalty-tier
(pco/defresolver user-discount [{:user/keys [loyalty-tier]}]
  {:order/discount (if (= loyalty-tier :gold) 10 0)})

(pco/defmutation place-order! [env {:user/keys [id] :order/keys [lines]}]
  {::pco/params [:user/id :order/lines]
   ::pco/output [:order/id]}
  {:order/id (random-uuid)})

(def resolvers [user-by-email order-total user-orders user-discount place-order!])

;; an option naming a def in the same file
(def user-properties [:user/id :user/name :user/email])

(pco/defresolver user-by-id [{:user/keys [id]}]
  {::pco/output user-properties}
  {})

;; and inside a join
(pco/defresolver order-buyer [{:order/keys [id]}]
  {::pco/output [{:order/buyer user-properties}]}
  {})

;; an option written as code: its keys are not guessed statically, and the
;; running environment knows them
(def seed-keys [:user/loyalty-tier])

(pco/defresolver seed []
  {::pco/output (vec seed-keys)}
  {:user/loyalty-tier :gold})

;; a resolver built as a value: only the running environment has it
(def list-price
  (pco/resolver 'shop.resolvers/list-price
    {::pco/input [:product/price] ::pco/output [:product/list-price]}
    (fn [_ {:product/keys [price]}] {:product/list-price (* 2 price)})))
