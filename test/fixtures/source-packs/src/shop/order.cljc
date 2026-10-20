(ns shop.order
  (:require [clojure.spec.alpha :as s]))

(s/def :order/total #?(:clj decimal? :cljs number?))
