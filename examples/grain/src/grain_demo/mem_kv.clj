(ns grain-demo.mem-kv
  "Atom-backed in-memory KVStore for the demo's read-model cache.

   Stands in for kv-store-lmdb, whose native library requires a newer glibc
   than some hosts have. LMDB passes keys/values as byte arrays; byte arrays
   compare by reference, so keys are wrapped in vectors for value equality."
  (:require [ai.obney.grain.kv-store.interface.protocol :as p]))

(defn- kkey [k] (if (bytes? k) (vec k) k))

(defrecord MemKV [store]
  p/KVStore
  (start [this] this)
  (stop [this] this)
  (get! [_ {:keys [k]}] (get @store (kkey k)))
  (put! [_ {:keys [k v]}] (swap! store assoc (kkey k) v) nil)
  (put-batch! [_ {:keys [entries]}]
    (swap! store
           (fn [m] (reduce (fn [m {:keys [k v]}] (assoc m (kkey k) v)) m entries)))
    nil))

(defn ->mem-kv []
  (->MemKV (atom {})))
