(ns atlas.review.decorate
  (:require [atlas.ide :as ide]
            [atlas.registry :as registry]
            [atlas.registry.lookup :as lookup]
            [atlas.review.candidate :as candidate]
            [atlas.review.diff :as diff]
            [atlas.review.registry :as reg]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [rewrite-clj.node :as n]
            [sdiff.core :as core]
            [sdiff.decorate :as d :refer [derived]]))

(defonce ^:private attempts (atom {}))

(defn- candidate-for [{:keys [num head repo]}]
  (or (@attempts [num head])
      (let [r (try (candidate/fetch repo num head) (catch Exception e {:reason (ex-message e)}))]
        (swap! attempts assoc [num head] r)
        r)))

(defn forget-attempt! [num head] (swap! attempts dissoc [num head]))

(defn context [report]
  (if-not (:store @reg/config)
    {:report report}
    (let [base-v (or (reg/version-for (:base report)) (reg/latest))
          exact? (boolean (reg/version-for (:base report)))
          {:keys [registry label reason]} (candidate-for (:pr report))]
      (cond-> {:report report
               :base-v (str (reg/label base-v) (when-not exact? ", the newest recorded; the merge base itself was not recorded"))
               :base (reg/registry-at (:commit base-v))
               :cand-v label :reason reason}
        registry (assoc :cand registry)))))

(defn- in [{:keys [cand base]} f] (reg/with-version (or cand base) (f)))

(defn- source-name [{:keys [cand-v base-v]}] (str "registry store @" (or cand-v base-v)))

(defn declared-id [form]
  (let [[head arg] (:id form)]
    (when (and arg (re-find #"register!|bind|def-.*tool|defmethod" (str head)) (str/starts-with? arg ":"))
      (try (edn/read-string arg) (catch Exception _ nil)))))

(defn- delta [{:keys [base cand]} id]
  (when cand (diff/entity-delta base cand id)))

(defn- changed? [d] (boolean (and d (some seq [(:props-added d) (:props-removed d) (:aspects-added d) (:aspects-removed d)]))))

(def contract-keys
  [:execution-function/context :execution-function/deps :execution-function/response
   :mcp-tool/req-input-args :mcp-tool/opt-input-args :mcp-tool/output-args
   :endpoint/method :endpoint/input :endpoint/output :endpoint/deps
   :structure-component/deps :workflow-producer/signals :workflow-producer/output :test-case/target])

(defn- ent [id] [:code.ent (str id)])
(defn- ids [xs] (if (seq xs) [:ul.ids (for [i xs] [:li (ent i)])] [:span.mute "nobody in the registry"]))

(defn- tests-of [id]
  (sort (for [[_ e] (registry/current-registry) :when (and (= :atlas/test-case (:atlas/type e)) (= id (:test-case/target e)))] (:atlas/dev-id e))))

(defn- state-of [{:keys [cand-v]} d]
  (cond (:new? d) "new in this PR" (:deleted? d) "deleted by this PR" (changed? d) "contract changed" cand-v "contract unchanged" :else "as on main; no candidate version staged"))

(defn declares [ctx id]
  (let [[cid props] (in ctx #(do [(lookup/identity-for id) (lookup/props-for id)]))
        d (delta ctx id)]
    (if-not cid
      (derived (source-name ctx) [:p "declares " (ent id) ", " (if (:deleted? d) "deleted by this PR" "not in the registry")])
      (derived (if (:cand-v ctx) (str "registry store " (:base-v ctx) " → CI " (:cand-v ctx)) (source-name ctx))
               [:p "declares " (ent id) " " [:span.mute (name (:atlas/type props))] " · "
                [:span.tag {:class (if (contains? #{"contract unchanged" "as on main; no candidate version staged"} (state-of ctx d)) "tag-note" "tag-ext")} (state-of ctx d)]]
               [:div.aspects (interpose " " (for [a (sort-by str cid)]
                                              [:span.aspect {:class (cond ((:aspects-added d #{}) a) "add" ((:aspects-removed d #{}) a) "del")} (str a)]))]
               (when-let [c (seq (select-keys props contract-keys))]
                 [:table.contract [:tbody (for [[k v] c] [:tr [:td (name k)] [:td (interpose " " (for [x (if (coll? v) v [v])] [:code (str x)]))]])]])
               (when (or (seq (:props-added d)) (seq (:props-removed d)))
                 [:table.contract.delta
                  [:tbody (for [[cls tuples] [["del" (sort-by str (:props-removed d))] ["add" (sort-by str (:props-added d))]] [_ attr v] tuples]
                            [:tr {:class cls} [:td (name attr)] [:td [:code (pr-str v)]]])]])))))

(defn affects [ctx id]
  (in ctx (fn []
            (when-let [props (lookup/props-for id)]
              (let [produced (concat (:execution-function/response props) (:endpoint/output props))
                    deps (ide/dependents-of id)
                    tests (tests-of id)]
                (derived (source-name ctx)
                         [:h5 "Declares it as a dependency"] (ids deps)
                         (when (seq produced)
                           (list [:h5 "Consumes what it produces"]
                                 [:ul.ids (for [k produced :let [cs (ide/consumers-of k)]]
                                            [:li [:code (str k)] " → " (if (seq cs) (interpose ", " (map ent cs)) [:span.mute "nobody"])])]))
                         [:h5 "Covered by"] (ids tests)))))))

(def ^:private kw-re #"(?<![\w:]):([\w.\-]+/[\w.\-!?*+]+)")

(defn- mentioned-keys [ctx file form]
  (let [src (some-> (get (core/index (:new file)) (:id form)) n/string)]
    (in ctx (fn []
              (let [reg (registry/current-registry)
                    known (set (mapcat (fn [[_ e]] (mapcat e [:execution-function/context :execution-function/response :endpoint/input :endpoint/output])) reg))]
                (->> (re-seq kw-re (or src ""))
                     (map (comp keyword second)) distinct
                     (filter known)
                     (map (fn [k] [k (ide/producers-of k) (ide/consumers-of k)]))
                     (remove (fn [[_ p c]] (and (empty? p) (empty? c))))))))))

(defn mentions [ctx file form]
  (when-let [ks (seq (mentioned-keys ctx file form))]
    (derived (source-name ctx)
             [:h5 "Data keys mentioned"]
             [:ul.ids (for [[k p c] ks]
                        [:li [:code (str k)] " · produced by " (if (seq p) (interpose ", " (map ent p)) [:span.mute "nobody"])
                         " · consumed by " (if (seq c) (interpose ", " (map ent c)) [:span.mute "nobody"])])])))

(defn registry-decoration [ctx file form]
  (when (:base ctx)
    (let [full-form (some #(when (= (:id form) (:id %)) %) (:forms (some (fn [f] (when (= (:path file) (:path f)) f)) (:clj (:report ctx)))))
          id (declared-id form)
          was-id (when (:was full-form) (declared-id {:id (:was full-form)}))
          file* (some #(when (= (:path file) (:path %)) %) (:clj (:report ctx)))]
      (when-let [parts (seq (remove nil? [(when id (declares ctx id))
                                          (when (and was-id (not= was-id id)) (derived (source-name ctx) [:p "formerly declared " (ent was-id)]))
                                          (when id (affects ctx id))
                                          (when file* (mentions ctx file* full-form))]))]
        (list* parts)))))

(defn- anchors [report]
  (into {} (for [f (:clj report) form (:forms f) :let [id (declared-id form)] :when id] [id (d/anchor f form)])))

(defn- entity-annotations [ctx]
  (when-let [as (seq (filter #(and (map? (:on %)) (:entity (:on %))) (:annotations ctx)))]
    [:div.ent-annotations
     (for [a as] [:div [:p.mute "on " (ent (:entity (:on a)))] (d/render-annotation a)])]))

(defn registry-header [ctx report]
  (when (:base ctx)
    (let [{:keys [base base-v cand cand-v reason]} ctx]
      (list
       (if-not cand
         (derived "registry store"
                  [:p "registry " [:code base-v] " · no registry for this PR's head, so entities are shown as on main"]
                  (when reason [:p.mute reason]))
         (let [{:keys [new changed deleted]} (diff/summary base cand)
               a (anchors report)
               link (fn [id] (if-let [h (a id)] [:a {:href (str "#" h)} (ent id)] (list (ent id) " " [:span.mute "(no form in this diff)"])))]
           (derived (str "registry store " base-v " → CI " cand-v)
                    [:p "Registry: " (count new) " new, " (count changed) " changed, " (count deleted) " deleted"
                     (when (every? empty? [new changed deleted]) " — this PR changes no contract")]
                    (when (seq new) (list [:h5 "New"] [:ul.ids (for [id new] [:li (link id)])]))
                    (when (seq changed) (list [:h5 "Changed"] [:ul.ids (for [id changed] [:li (link id)])]))
                    (when (seq deleted) (list [:h5 "Deleted"] [:ul.ids (for [id deleted] [:li (ent id)])])))))
       (entity-annotations ctx)))))

(defn validate-entity [ctx on]
  (when-let [id (:entity on)]
    (let [id (if (keyword? id) id (edn/read-string (str id)))]
      (when-not (in ctx #(lookup/identity-for id)) (str "no entity " id " in the registry")))))

(defn entity-view [ctx id]
  (when (:base ctx)
    (list (declares ctx id) (affects ctx id))))

(d/use-context! context)
(d/add-entity-renderer! ::registry entity-view)
(d/add-form-decorator! ::registry registry-decoration)
(d/add-header-decorator! ::registry registry-header)
(d/add-ref-validator! ::entity validate-entity)
