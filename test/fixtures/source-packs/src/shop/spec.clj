(ns shop.spec
  (:require [clojure.spec.alpha :as s]))

(s/def :user/id uuid?)
(s/def :user/email (s/and string? #(re-find #"@" %)))
(s/def :user/name string?)
(s/def :order/discount number?)

;; :user/emial is a typo: s/keys accepts a key with no spec of its own
(s/def ::user (s/keys :req [:user/id :user/email] :opt [:user/name :user/emial]))
