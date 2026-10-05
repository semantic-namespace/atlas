(ns atlas.review.diff
  (:require [clojure.set :as set]))

(defn- by-id [reg] (into {} (map (fn [[cid e]] [(:atlas/dev-id e) (assoc e ::cid cid)])) reg))

(defn- tuples [id e]
  (set (for [[k v] (dissoc e ::cid :atlas/dev-id :atlas/type :atlas/docs)
             x (if (and (coll? v) (not (map? v))) v [v])]
         [id k x])))

(defn entity-delta
  "What changed for `id` between two registries: new or deleted, aspects added
  and removed, property tuples `[id key value]` added and removed."
  [base cand id]
  (let [a (get (by-id base) id) b (get (by-id cand) id)]
    {:new? (and (nil? a) (some? b))
     :deleted? (and (some? a) (nil? b))
     :aspects-added (set/difference (or (::cid b) #{}) (or (::cid a) #{}))
     :aspects-removed (set/difference (or (::cid a) #{}) (or (::cid b) #{}))
     :props-added (set/difference (if b (tuples id b) #{}) (if a (tuples id a) #{}))
     :props-removed (set/difference (if a (tuples id a) #{}) (if b (tuples id b) #{}))}))

(defn summary
  "`{:new [ids] :changed [ids] :deleted [ids]}` between two registries; changed
  means an aspect or a property differs."
  [base cand]
  (let [ba (by-id base) ca (by-id cand)
        ids (set/union (set (keys ba)) (set (keys ca)))
        d (into {} (for [id ids] [id (entity-delta base cand id)]))]
    {:new (sort (filter #(:new? (d %)) ids))
     :deleted (sort (filter #(:deleted? (d %)) ids))
     :changed (sort (filter #(let [x (d %)] (and (not (:new? x)) (not (:deleted? x))
                                                (some seq [(:aspects-added x) (:aspects-removed x) (:props-added x) (:props-removed x)])))
                            ids))}))
