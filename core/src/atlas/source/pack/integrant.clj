(ns atlas.source.pack.integrant
  "EXPERIMENTAL (see EXPERIMENTAL.md) — the integrant pack: a system's
   components, read as data from its config files and from the code that
   implements them. Nothing is loaded or evaluated.

   `pack` is what atlas.source.index runs (see the Packs section of
   atlas.source.index), enabled by :integrant under atlas-source.edn's
   :packs:

     :integrant {:configs [\"resources/config\"]  ; config files or dirs of .edn
                 :vars [app.system/config]        ; configs written in code
                 :provided [:duct/logger]}        ; keys something else adds (a duct module)

   A component is keyed by its config key. A composite key is keyed by its
   name, the keyword no other config key uses, the others being what it
   derives from: :app/web in duct's [:duct/server :app/web], and
   :password.slack/webhook in [:password.slack/webhook :app/rotable-password]
   when every password shares :app/rotable-password. When that doesn't single
   one out, the last keyword, as duct writes them. Its entry, in the pack's
   own vocabulary:

     :integrant/refs        the keys it refers to: #ig/ref, #ig/refset, (ig/ref k)
     :integrant/derives     a composite key's other keywords
     :integrant/env         the environment variables its config reads (#env)
     :integrant/config-in   each \"file:line\" that configures it
     :integrant/varies      true when those files configure it differently
                            (a .mock.edn beside the real one)
     :integrant/profiles    the duct profiles that configure it
     :integrant/isa         what (derive k parent) calls in the code derive it
                            from, in files that use integrant
     :integrant/init-key, :integrant/halt-key!, ...
                            true when the code has that defmethod for it

   Config files are read all at once, so a key several files configure
   (variants of one system) is one entry with every file listed, not a
   conflict. Tags no reader knows (aero's #env, #or, #profile) are kept as
   data; refs inside any branch count. A duct profile key
   (:duct.profile/base {...}) holds components: each is read as one, at the
   profile's line."
  (:require [atlas.source.read :as read]
            [clojure.string :as str]))

(def kind :integrant)

(def ^:private ig-methods
  '#{init-key halt-key! resume-key suspend-key! prep-key expand-key resolve-key assert-key pre-init-spec})

;; =============================================================================
;; Reading a config
;; =============================================================================

(defn- nodes
  "Every value inside `v`, through collections and tagged literals."
  [v]
  (tree-seq #(or (coll? %) (tagged-literal? %))
            #(if (tagged-literal? %) [(:form %)] (seq %))
            v))

(defn- ref-target [x]
  (cond
    (and (tagged-literal? x) (#{'ig/ref 'ig/refset} (:tag x))) (:form x)
    ;; a config written in code: (ig/ref :k), (integrant.core/refset :k)
    (and (seq? x) (symbol? (first x)) (namespace (first x)) (#{"ref" "refset"} (name (first x))))
    (second x)))

(defn- refs [v]
  (vec (sort (distinct (for [x (nodes v)
                             :let [t (ref-target x)]
                             k (if (vector? t) [(last t)] [t])
                             :when (qualified-keyword? k)]
                         k)))))

(defn- env-vars [v]
  (vec (sort (distinct (for [x (nodes v)
                             :when (and (tagged-literal? x) (#{"env" "duct/env"} (str (:tag x))))
                             :let [f (:form x)]
                             n (if (sequential? f) [(first f)] [f])
                             :when (or (symbol? n) (string? n))]
                         (str n))))))

(defn- composite? [k] (and (vector? k) (seq k) (every? qualified-keyword? k)))

(defn- component-id
  "A config key's id. `shared` is how many composite keys each keyword is in."
  ([k] (component-id k {}))
  ([k shared]
   (cond
     (qualified-keyword? k) k
     (composite? k) (let [own (filter #(= 1 (get shared % 1)) k)]
                      (if (= 1 (count own)) (first own) (last k))))))

(defn- combine
  "One result per component from `defs`, [{:key :value :file :path :line}]
   for every file or var that configures it."
  [defs]
  (let [shared (frequencies (apply concat (distinct (filter composite? (map :key defs)))))]
    (vec (for [[id ds] (sort-by key (group-by #(component-id (:key %) shared) defs))
             :when id
             :let [ds (sort-by (juxt :path :line) ds)
                   k (:key (first ds))
                   values (set (map :value ds))]]
         {:file (:file (first ds)) :row (:line (first ds)) :kind kind :dev-id id
          :entry (cond-> {:integrant/config-in (vec (for [d ds] (str (:path d) ":" (:line d))))}
                   (seq (refs (map :value ds))) (assoc :integrant/refs (refs (map :value ds)))
                   (vector? k) (assoc :integrant/derives (vec (remove #{id} k)))
                   (seq (env-vars (map :value ds))) (assoc :integrant/env (env-vars (map :value ds)))
                   (> (count values) 1) (assoc :integrant/varies true)
                   (seq (keep :profile ds)) (assoc :integrant/profiles (vec (sort (distinct (keep :profile ds))))))}))))

(defn- profile? [k v]
  (and (qualified-keyword? k) (= "duct.profile" (namespace k)) (map? v)))

(defn- config-defs
  "Each component a config's top-level [k v line]s configure, duct
   profiles opened."
  [kvs file path]
  (for [[k v line] kvs
        [k v profile] (if (profile? k v) (for [[ik iv] v] [ik iv k]) [[k v nil]])
        :when (component-id k)]
    (cond-> {:key k :value v :file file :path path :line line}
      profile (assoc :profile profile))))

(defn- read-configs
  "Every config file at once: [{:file :path :text}]."
  [files _opts]
  (let [read (for [{:keys [file path text]} files]
               (let [kvs (try (read/keyed-lines text {:tags :keep}) (catch Exception ex ex))]
                 (cond
                   (instance? Exception kvs) {:skipped {:file file :row 1 :kind kind :skipped (str "not readable: " (ex-message kvs))}}
                   (nil? kvs) {:skipped {:file file :row 1 :kind kind :skipped "not a map of config keys"}}
                   :else {:defs (config-defs kvs file path)})))]
    (into (combine (mapcat :defs read)) (keep :skipped read))))

;; =============================================================================
;; Reading the code
;; =============================================================================

(defn- find-forms
  "Each defmethod form of an integrant multimethod, and each config var the
   options name. The site is the defmethod form itself: clj-kondo's
   :dispatch-val-str gives ::keywords the wrong namespace, so the form is
   read with the file's."
  [analysis opts]
  (let [vars (set (get-in opts [:integrant :vars]))
        ;; derive also builds hierarchies of exceptions and the like: only
        ;; files that use integrant say something about components
        ig-files (set (for [u (:var-usages analysis) :when (= 'integrant.core (:to u))] (:filename u)))
        ig-rows (set (for [u (:var-usages analysis)
                           :when (and (:defmethod u) (= 'integrant.core (:to u)) (ig-methods (:name u)))]
                       [(:filename u) (:row u)]))]
    (concat
     (for [u (:var-usages analysis)
           :when (and (= 'clojure.core (:to u)) (= 'defmethod (:name u)) (ig-rows [(:filename u) (:row u)]))]
       (assoc (select-keys u [:filename :row :name-row :name-col]) :method true))
     (for [u (:var-usages analysis)
           :when (and (= 'clojure.core (:to u)) (= 'derive (:name u)) (= 2 (:arity u)) (ig-files (:filename u)))]
       (assoc (select-keys u [:filename :row :name-row :name-col]) :derive true))
     (for [d (:var-definitions analysis)
           :when (contains? vars (symbol (str (:ns d)) (str (:name d))))]
       {:filename (:filename d) :row (:row d) :name-row (:row d) :name-col (inc (:col d)) :config-var (:name d)}))))

(defn- read-forms [sites {:keys [form-at own-ns]} _opts]
  (let [{methods-sites :method derive-sites :derive var-sites :var}
        (group-by #(cond (:method %) :method (:derive %) :derive :else :var) sites)]
    (vec (concat
          ;; one result per derived key in this file, its parents together
          (for [[child ds] (group-by :child
                                     (for [{:keys [row name-row name-col]} derive-sites
                                           :let [[_ child parent] (form-at name-row name-col)]
                                           :when (and (qualified-keyword? child) (qualified-keyword? parent))]
                                       {:row row :child child :parent parent}))]
            {:row (:row (first ds)) :kind kind :dev-id child
             :entry {:integrant/isa (vec (sort (distinct (map :parent ds))))}})
          (for [{:keys [row name-row name-col]} methods-sites
                :let [form (form-at name-row name-col)
                      [_ mm dispatch] (when (seq? form) form)
                      id (component-id dispatch)]]
            (if (and id (symbol? mm) (ig-methods (symbol (name mm))))
              {:row row :kind kind :dev-id id :entry {(keyword "integrant" (name mm)) true}}
              {:row row :kind kind :skipped (str "dispatch value " (pr-str dispatch) " is not a config key")}))
          (for [{:keys [row name-row name-col config-var]} var-sites
                :let [form (form-at name-row name-col)
                      m (when (and (seq? form) (>= (count form) 3)) (last form))]
                r (if (map? m)
                    (combine (for [[k v] m :when (component-id k)] {:key k :value v :path (str own-ns "/" config-var) :line row}))
                    [{:kind kind :skipped "the config var's value is not a map"}])]
            (assoc r :row row))))))

;; =============================================================================
;; Checks and the hover
;; =============================================================================

(defn- checks
  "- :ref-to-undefined (warning): a component refers to a key no config
     read defines, directly or as a composite key's parent, and that
     :provided doesn't list. Integrant can't init the system unless
     something adds it (a duct module does, at prep time);
   - :init-key-without-config (info): the code has an init-key method for
     a key no config defines, and no composite key derives from: unused, or
     configured somewhere not read. A key some configured component
     derives from, by a composite key or a derive call, counts as configured."
  [{:keys [of-kinds loc opts]}]
  (let [components (sort-by key (of-kinds #{kind}))
        configured (filter #(get-in (val %) [:entry :integrant/config-in]) components)
        parents #(concat (get-in (val %) [:entry :integrant/derives]) (get-in (val %) [:entry :integrant/isa]))
        defined (-> (set (map key configured))
                    (into (mapcat parents) configured)
                    (into (get-in opts [:integrant :provided])))]
    (concat
     (for [[id e] configured
           :let [missing (vec (remove defined (get-in e [:entry :integrant/refs])))]
           :when (seq missing)]
       {:severity :warning :problem :ref-to-undefined :dev-id id :keys missing :definition (loc e)})
     (for [[id e] components
           :when (and (get-in e [:entry :integrant/init-key]) (not (contains? defined id)))]
       {:severity :info :problem :init-key-without-config :dev-id id :definition (loc e)}))))

(defn- describe [entry]
  (when (some #(= "integrant" (namespace %)) (keys entry))
    (let [ms (sort (for [[k v] entry :when (and (true? v) (ig-methods (symbol (name k))))] (name k)))
          n (count (:integrant/config-in entry))]
      (str/join "  ·  " (remove nil? [(str "component  refs " (count (:integrant/refs entry)))
                                       (when (seq ms) (str/join " " ms))
                                       (when (pos? n) (str "in " n " config" (when (> n 1) "s")
                                                           (when (:integrant/varies entry) ", varies")))
                                       (when (seq (:integrant/env entry)) (str "env " (str/join " " (:integrant/env entry))))])))))

(def pack
  {:pack/id :integrant
   :pack/config-keys #{:integrant}
   :pack/kinds #{kind}
   :pack/vocabulary #{"integrant"}
   :pack/data-keys? false
   :pack/find find-forms
   :pack/read read-forms
   :pack/data-files (fn [opts] (get-in opts [:integrant :configs]))
   :pack/read-data read-configs
   :pack/checks checks
   :pack/describe describe})
