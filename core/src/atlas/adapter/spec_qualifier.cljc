(ns atlas.adapter.spec-qualifier
  "EXPERIMENTAL (see EXPERIMENTAL.md) — fast key qualification using pre-compiled
   transformation descriptors. Supports clojure.spec.alpha/multi-spec."
  (:require [clojure.spec.alpha :as s]
            [clojure.walk :as walk]
            [clojure.set :as set]))

;; -----------------------------------------------------------------------------
;; Spec analysis
;; -----------------------------------------------------------------------------

(defn- args->m [xs] (apply hash-map xs))

(defn- un->q-map [kws]
  (into {} (map (fn [qk] [(keyword (name qk)) qk])) kws))

(defn- spec-form [sp]
  (cond
    (keyword? sp) (some-> (s/get-spec sp) s/form)
    (s/spec? sp)  (s/form sp)
    (seq? sp)     sp
    :else         nil))

(declare analyze-spec)

(defn- analyze-keys-spec [form]
  (let [opts    (args->m (rest form))
        req-un  (:req-un opts)
        opt-un  (:opt-un opts)
        req     (:req opts)
        opt     (:opt opts)
        key-map (un->q-map (concat req-un opt-un))
        allowed (set (concat (vals key-map) req opt))
        nested  (->> allowed
                     (map (fn [qk] [qk (analyze-spec qk)]))
                     (filter (fn [[_ a]] (some? a)))
                     (into {}))]
    {:type :keys
     :key-map key-map
     :allowed allowed
     :nested nested}))

(defn- analyze-coll-spec [form]
  (let [elem (analyze-spec (second form))]
    (when elem {:type :coll :elem elem})))

(defn- analyze-tuple-spec [form]
  (let [analyses (mapv analyze-spec (rest form))]
    (when (some some? analyses)
      {:type :tuple :elems analyses})))

(defn- analyze-map-of-spec [form]
  (let [k (analyze-spec (second form))
        v (analyze-spec (nth form 2))]
    (when (or k v)
      {:type :map-of :key-spec k :val-spec v})))

(defn- analyze-and-spec [form]
  (let [specs   (->> (rest form) (map analyze-spec) (filter some?) vec)
        allowed (when (seq specs) (apply set/union (keep :allowed specs)))]
    (when (seq specs) {:type :and :specs specs :allowed allowed})))

(defn- analyze-merge-spec [form]
  (let [specs   (->> (rest form) (map analyze-spec) (filter some?) vec)
        allowed (when (seq specs) (apply set/union (keep :allowed specs)))]
    (when (seq specs) {:type :merge :specs specs :allowed allowed})))

(defn- analyze-or-spec [sp form]
  (let [r        (rest form)
        tags     (take-nth 2 r)
        subs     (take-nth 2 (rest r))
        branches (mapv (fn [t sub]
                         {:tag t
                          :spec-ref sub
                          :analysis (analyze-spec sub)})
                       tags subs)]
    (when (some #(some? (:analysis %)) branches)
      {:type :or
       :spec-kw (when (keyword? sp) sp)
       :branches branches
       :allowed (let [sets (keep #(some-> % :analysis :allowed) branches)]
                  (when (seq sets) (apply set/union sets)))})))

(defn- analyze-nilable-spec [form]
  (let [inner (analyze-spec (second form))]
    (when inner {:type :nilable :inner inner})))

#?(:clj
   (defn- analyze-multi-spec [form]
     (let [mm-sym       (second form)
           retag        (nth form 2)
           mm-var       (when (symbol? mm-sym) (resolve mm-sym))
           mm-val       (when mm-var (deref mm-var))
           method-table (when mm-val
                          (.getMethodTable ^clojure.lang.MultiFn mm-val))]
       (when (seq method-table)
         (let [tag-key  (when (keyword? retag) retag)
               branches (into []
                          (keep (fn [[dispatch-val method-fn]]
                                  (let [input (if tag-key
                                                {tag-key dispatch-val}
                                                dispatch-val)
                                        sp    (try (method-fn input)
                                                   (catch Throwable _ nil))
                                        a     (when sp (analyze-spec sp))]
                                    (when a
                                      {:tag      dispatch-val
                                       :spec-ref sp
                                       :analysis a}))))
                          method-table)]
           (when (seq branches)
             {:type     :multi
              :retag    retag
              :branches branches
              :allowed  (let [sets (keep #(some-> % :analysis :allowed) branches)]
                          (when (seq sets) (apply set/union sets)))}))))))

(defn analyze-spec [sp]
  (let [form (spec-form sp)]
    (when (seq? form)
      (case (first form)
        clojure.spec.alpha/keys     (analyze-keys-spec form)
        clojure.spec.alpha/coll-of  (analyze-coll-spec form)
        clojure.spec.alpha/every    (analyze-coll-spec form)
        clojure.spec.alpha/tuple    (analyze-tuple-spec form)
        clojure.spec.alpha/map-of   (analyze-map-of-spec form)
        clojure.spec.alpha/every-kv (analyze-map-of-spec form)
        clojure.spec.alpha/and      (analyze-and-spec form)
        clojure.spec.alpha/merge    (analyze-merge-spec form)
        clojure.spec.alpha/or       (analyze-or-spec sp form)
        clojure.spec.alpha/nilable  (analyze-nilable-spec form)
        clojure.spec.alpha/spec     (analyze-spec (second form))
        #?@(:clj [clojure.spec.alpha/multi-spec (analyze-multi-spec form)])
        nil))))

;; -----------------------------------------------------------------------------
;; Runtime transformation
;; -----------------------------------------------------------------------------

(defn- prune-keys [allowed m]
  (if (and allowed (map? m))
    (reduce-kv (fn [acc k v]
                 (if (contains? allowed k) (assoc acc k v) acc))
               (empty m)
               m)
    m))

(declare transform*)

(defn- transform-keys [{:keys [key-map nested allowed]} x prune?]
  (if-not (map? x)
    x
    (let [x1 (reduce-kv (fn [m uk qk]
                          (if (and (contains? m uk) (not (contains? m qk)))
                            (-> m (dissoc uk) (assoc qk (get m uk)))
                            m))
                        x
                        key-map)
          x2 (reduce-kv (fn [m qk a]
                          (if (contains? m qk)
                            (update m qk #(transform* a % true))
                            m))
                        x1
                        nested)]
      (if prune? (prune-keys allowed x2) x2))))

(defn- transform-coll [{:keys [elem]} x]
  (if (sequential? x)
    (mapv #(transform* elem % true) x)
    x))

(defn- transform-tuple [{:keys [elems]} x]
  (if (and (vector? x) (= (count x) (count elems)))
    (mapv (fn [a v] (if a (transform* a v true) v)) elems x)
    x))

(defn- transform-map-of [{:keys [key-spec val-spec]} x]
  (if (map? x)
    (into (empty x)
          (map (fn [[k v]]
                 [(if key-spec (transform* key-spec k true) k)
                  (if val-spec (transform* val-spec v true) v)]))
          x)
    x))

(defn- spec-form-head [spec-ref]
  (when (keyword? spec-ref)
    (let [f (some-> (s/get-spec spec-ref) s/form)]
      (when (seq? f) (first f)))))

(defn- maybe-matches-or-branch? [spec-ref x]
  (cond
    (keyword? spec-ref)
    (let [head (spec-form-head spec-ref)]
      (if (= head 'clojure.spec.alpha/multi-spec)
        ;; multi-spec s/valid? requires qualified keys; fall back to shape check
        (map? x)
        (try (s/valid? spec-ref x) (catch #?(:clj Throwable :cljs :default) _ false))))

    (seq? spec-ref)
    (let [h (first spec-ref)]
      (cond
        (= h 'clojure.spec.alpha/keys)       (map? x)
        (= h 'clojure.spec.alpha/merge)      (map? x)
        (= h 'clojure.spec.alpha/and)        true
        (= h 'clojure.spec.alpha/or)         true
        (= h 'clojure.spec.alpha/nilable)    (or (nil? x) (maybe-matches-or-branch? (second spec-ref) x))
        (= h 'clojure.spec.alpha/coll-of)    (sequential? x)
        (= h 'clojure.spec.alpha/every)      (sequential? x)
        (= h 'clojure.spec.alpha/map-of)     (map? x)
        (= h 'clojure.spec.alpha/tuple)      (vector? x)
        (= h 'clojure.spec.alpha/spec)       (maybe-matches-or-branch? (second spec-ref) x)
        (= h 'clojure.spec.alpha/multi-spec) (map? x)
        :else false))

    :else false))

(defn- transform-or [{:keys [spec-kw branches]} x prune?]
  (let [chosen
        (or
          (when spec-kw
            (let [cv (s/conform spec-kw x)]
              (when-not (= ::s/invalid cv)
                (let [tag (first cv)]
                  (some #(when (= tag (:tag %)) %) branches)))))
          (some (fn [{:keys [spec-ref] :as b}]
                  (when (maybe-matches-or-branch? spec-ref x) b))
                branches))]
    (if-let [a (:analysis chosen)]
      (transform* a x prune?)
      x)))

(defn- transform-nilable [{:keys [inner]} x]
  (if (nil? x) x (transform* inner x true)))

(defn- transform-multi [{:keys [retag branches]} x prune?]
  (if (map? x)
    (let [tag-key (if (keyword? retag) retag (keyword (name retag)))
          dv      (or (get x tag-key)
                      (when (qualified-keyword? tag-key)
                        (get x (keyword (name tag-key)))))
          chosen  (some #(when (= dv (:tag %)) %) branches)]
      (if-let [a (:analysis chosen)]
        ;; Patch key-map: add unqualified->qualified for :req keys so
        ;; unqualified input gets qualified before pruning
        (let [allowed   (:allowed a)
              extra-map (into {}
                          (keep (fn [qk]
                                  (let [uk (keyword (name qk))]
                                    (when (and (qualified-keyword? qk)
                                              (not= uk qk)
                                              (not (contains? (:key-map a) uk)))
                                      [uk qk]))))
                          allowed)
              patched   (update a :key-map merge extra-map)]
          (transform* patched x prune?))
        x))
    x))

(defn transform* [analysis x prune?]
  (when analysis
    (case (:type analysis)
      :keys    (transform-keys analysis x prune?)
      :coll    (transform-coll analysis x)
      :tuple   (transform-tuple analysis x)
      :map-of  (transform-map-of analysis x)
      :and     (let [v (reduce (fn [v a] (transform* a v false)) x (:specs analysis))]
                 (if prune? (prune-keys (:allowed analysis) v) v))
      :merge   (let [v (reduce (fn [v a] (transform* a v false)) x (:specs analysis))]
                 (if prune? (prune-keys (:allowed analysis) v) v))
      :or      (transform-or analysis x prune?)
      :nilable (transform-nilable analysis x)
      :multi   (transform-multi analysis x prune?)
      x)))

(defn transform [analysis x]
  (transform* analysis x true))

;; -----------------------------------------------------------------------------
;; Public API
;; -----------------------------------------------------------------------------

(defn compile-qualifier [spec-kw]
  (let [analysis (analyze-spec spec-kw)]
    (if analysis
      (fn [x]
        (transform analysis x))
      identity)))

(def ^:private qualifier-cache (atom {}))

(defn qualify [spec-kw data]
  (let [qualifier (or (get @qualifier-cache spec-kw)
                      (let [q (compile-qualifier spec-kw)]
                        (swap! qualifier-cache assoc spec-kw q)
                        q))]
    (qualifier data)))

(defn clear-cache! []
  (reset! qualifier-cache {}))

(defn unqualify-map-keys [x]
  (walk/postwalk
   (fn [v]
     (if (map? v)
       (reduce-kv (fn [m k val]
                    (assoc m (if (keyword? k) (keyword (name k)) k) val))
                  (empty v)
                  v)
       v))
   x))
