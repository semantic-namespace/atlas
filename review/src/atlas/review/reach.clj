(ns atlas.review.reach
  "Which I/O each registered entity can reach, read from the source alone, and
  how that compares with the aspects the entity declares.

  The call graph is clj-kondo's, over top-level forms and each protocol method
  a record, type or reify implements, with three bridges
  a plain call graph lacks: a form that names a keyword reaches the form that
  registers it (an entity or an integrant component), and a call to a protocol
  method reaches its implementations. A protocol with one implementation is an
  exact edge; one with several is followed only for 'may reach', since the
  call could go to any of them. Functions passed as values are not seen."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [rewrite-clj.node :as n]
            [rewrite-clj.parser :as p]))

(defn- runner []
  (if (System/getProperty "babashka.version")
    (do ((requiring-resolve 'babashka.pods/load-pod) 'clj-kondo/clj-kondo "2026.01.19")
        (requiring-resolve 'pod.borkdude.clj-kondo/run!))
    (requiring-resolve 'clj-kondo.core/run!)))

(defn analyse
  "clj-kondo's analysis, with the unresolved-symbol findings added as
  `:unresolved`, since calls to functions defined at load time show up there."
  [root dirs]
  (let [{:keys [analysis findings]} ((runner) {:lint (mapv #(str (io/file root %)) dirs)
                                                :config {:output {:canonical-paths true}
                                                         :linters {:unresolved-symbol {:level :warning}}
                                                         :analysis {:keywords true :protocol-impls true :java-class-usages true}}})]
    (assoc analysis :unresolved (filter #(= :unresolved-symbol (:type %)) findings))))

(defn- sources [root dirs]
  (for [d dirs f (file-seq (io/file root d))
        :when (and (.isFile f) (re-find #"\.clj[cs]?$" (.getName f)))]
    (.getCanonicalPath f)))

(defn- code? [x] (not (#{:whitespace :newline :comma :comment :uneval} (n/tag x))))

(defn- top-forms
  "`[{:file :row :end-row :head :children}]`, one per top-level form, with the
  positions of its first children so a kondo keyword can be matched to them."
  [file]
  (for [node (n/children (p/parse-string-all (slurp file)))
        :when (and (code? node) (= :list (n/tag node)))
        :let [{:keys [row end-row]} (meta node) kids (filter code? (n/children node))]]
    {:file file :row row :end-row end-row
     :head (some-> (first kids) n/string)
     :args (vec (rest kids))}))

(defn- kw-at
  "The keyword kondo resolved at `node`'s position, so `::alias/k` comes back qualified."
  [kws-by-pos file node]
  (let [{:keys [row col]} (meta node)] (get kws-by-pos [file row col])))

(defn- deps-keywords
  "Keywords under any `*/deps` key of the first map literal among `args`: the
  components an entity says it needs."
  [kws-by-pos file args]
  (when-let [m (first (filter #(= :map (n/tag %)) args))]
    (let [kids (filter code? (n/children m))]
      (set (for [[k v] (partition 2 kids)
                 :when (some-> (kw-at kws-by-pos file k) name (= "deps"))
                 :when (#{:set :vector} (n/tag v))
                 x (filter code? (n/children v))
                 :let [kw (kw-at kws-by-pos file x)] :when kw]
             kw)))))

(defn- set-keywords [kws-by-pos file node]
  (when (= :set (n/tag node))
    (set (keep #(kw-at kws-by-pos file %) (filter code? (n/children node))))))

(defn graph
  "Forms as nodes `[file row]`, call and keyword edges between them, the
  registrations each form makes, and the I/O sites each form contains.

  `config`: `:registration-heads` and `:component-heads` regexes matched against
  a form's head symbol, `:sinks` `{kind regex}` over called namespaces,
  `:services` `{service regex}` naming an HTTP call by the namespace making it,
  `:java-sinks` `{kind regex}` over Java classes used, `:generators`
  `{kind regex}` over qualified vars that define functions at load time (a
  call to a var such a namespace does not visibly define is that kind of I/O),
  and `:opaque` a regex of namespaces whose forms are not walked through."
  [root dirs {:keys [registration-heads component-heads sinks services java-sinks generators opaque]}]
  (let [a (analyse root dirs)
        kws-by-pos (into {} (for [{:keys [filename row col ns name]} (:keywords a) :when ns]
                              [[filename row col] (keyword (str ns) name)]))
        forms (vec (mapcat top-forms (sources root dirs)))
        methods (for [{:keys [filename row end-row]} (:protocol-impls a) :when (and row end-row)]
                  {:file filename :row row :end-row end-row})
        by-file (update-vals (group-by :file (concat forms methods)) #(sort-by (fn [f] (- (:end-row f) (:row f))) %))
        form-at (fn [file row] (when row (some #(when (<= (:row %) row (:end-row %)) [file (:row %)]) (by-file file))))
        ns-of (into {} (for [{:keys [filename ns]} (:var-definitions a)] [filename ns]))
        registers (into {} (for [{:keys [file row head args]} forms
                                 :when (and head (re-find registration-heads head))
                                 :let [id (some->> (first args) (kw-at kws-by-pos file))
                                       type (when (re-find #"register!$" head)
                                              (some->> (second args) (kw-at kws-by-pos file)))]
                                 :when id]
                             [id {:node [file row] :type type :aspects (some #(set-keywords kws-by-pos file %) args)
                                  :deps (deps-keywords kws-by-pos file args)}]))
        components (into {} (for [{:keys [file row head args]} forms
                                  :when (and head (= "defmethod" head) (re-find component-heads (n/string (first args))))
                                  :let [k (some->> (second args) (kw-at kws-by-pos file))] :when k]
                              [k [file row]]))
        defines (into {} (concat (for [[k {:keys [node]}] registers] [k node]) components))
        var-def (into {} (for [{:keys [filename row ns name]} (:var-definitions a) :let [f (form-at filename row)] :when f]
                           [[ns name] f]))
        impls (reduce (fn [m {:keys [protocol-ns method-name filename row]}]
                        (if-let [f (form-at filename row)] (update m [protocol-ns method-name] (fnil conj #{}) f) m))
                      {} (:protocol-impls a))
        opaque? (fn [[file]] (boolean (some->> (ns-of file) str (re-find opaque))))
        add (fn [m [from to]] (if (and to (not= from to) (not (opaque? to))) (update m from (fnil conj #{}) to) m))
        direct (concat
                (for [{:keys [filename row to name]} (:var-usages a) :let [from (form-at filename row)] :when from]
                  [from (var-def [to name])])
                (for [{:keys [filename row to name]} (:var-usages a)
                      :let [from (form-at filename row) only (impls [to name])]
                      :when (and from (= 1 (count only)))]
                  [from (first only)])
                (for [{:keys [filename row ns name]} (:keywords a) :when ns
                      :let [from (form-at filename row)] :when from]
                  [from (defines (keyword (str ns) name))]))
        via-protocols (for [{:keys [filename row to name]} (:var-usages a) :let [from (form-at filename row)] :when from
                            t (impls [to name])]
                        [from t])
        direct-edges (reduce add {} direct)
        edges (reduce add direct-edges via-protocols)
        sink-kinds (fn [file to]
                     (let [to (str to)]
                       (for [[kind re] sinks :when (re-find re to)]
                         (if (= :http kind)
                           (or (some (fn [[svc re]] (when (re-find re (str (ns-of file))) svc)) services) :http)
                           kind))))
        io (reduce (fn [m {:keys [filename row to]}]
                     (let [from (form-at filename row)]
                       (reduce #(update %1 from (fnil conj #{}) %2) m (when (and from to) (sink-kinds filename to)))))
                   {} (:var-usages a))
        defined (set (map (juxt :ns :name) (:var-definitions a)))
        generated (reduce (fn [m {:keys [from to name]}]
                            (reduce (fn [m [kind re]] (if (re-find re (str to "/" name)) (assoc m from kind) m)) m generators))
                          {} (:var-usages a))
        io (reduce (fn [m {:keys [filename row to name]}]
                     (let [kind (generated to) from (form-at filename row)]
                       (if (and kind from (not (defined [to name]))) (update m from (fnil conj #{}) kind) m)))
                   io (:var-usages a))
        io (reduce (fn [m {:keys [filename row]}]
                     (let [kind (generated (ns-of filename)) from (form-at filename row)]
                       (if (and kind from) (update m from (fnil conj #{}) kind) m)))
                   io (:unresolved a))
        io (reduce (fn [m {:keys [filename row class]}]
                     (let [from (form-at filename row)]
                       (reduce #(update %1 from (fnil conj #{}) %2) m
                               (when from (for [[kind re] java-sinks :when (re-find re (str class))] kind)))))
                   io (:java-class-usages a))]
    {:edges edges :direct-edges direct-edges :io io :registers registers :components components :ns-of ns-of}))

(defn reach
  "Every I/O kind reachable from `node`, with one path to each, shortest first.
  Nodes in `stop` are reached but not walked through."
  ([g node] (reach g node #{}))
  ([{:keys [edges io]} node stop]
  (loop [frontier [[node [node]]] seen #{node} found {}]
    (if-let [[[n path] & more] (seq frontier)]
      (let [found (reduce (fn [f k] (if (f k) f (assoc f k path))) found (io n))
            next (when (or (= n node) (not (stop n))) (for [m (edges n) :when (not (seen m))] [m (conj path m)]))]
        (recur (into (vec more) next) (into seen (map first next)) found))
      found))))

(defn compare-aspects
  "Per entity of a type `executable?` accepts: the I/O it does itself through
  direct calls and keyword bridges, stopping at any other registered entity
  (`:reached`), the I/O it may do once protocol calls fan out to every
  implementation (`:may`), the I/O it reaches through other entities, the aspects it declares, and the
  comparable kinds where its own I/O and its declarations disagree. A kind
  counts as declared by an aspect in `aspects-of-kind`, or by a component in
  the entity's `*/deps` matching `deps-of-kind`, since a database or a cache is
  usually declared once, on the component the entity depends on. `aspects-of-kind` maps each kind to the aspects
  that would declare it; kinds absent from it are reported but not compared."
  ([g aspects-of-kind executable?] (compare-aspects g aspects-of-kind executable? {}))
  ([g aspects-of-kind executable? deps-of-kind]
  (for [entity-nodes [(set (map :node (vals (:registers g))))]
        [id {:keys [node aspects type deps]}] (sort-by key (:registers g))
        :when (executable? type)
        :let [reached (reach (assoc g :edges (:direct-edges g)) node entity-nodes)
              may (reach g node entity-nodes)
              transitive (reach g node)
              by-aspect (set (for [[kind as] aspects-of-kind :when (some (or aspects #{}) as)] kind))
              by-dep (set (for [[kind re] deps-of-kind d deps :when (re-find re (str d))] kind))
              declared (into by-aspect by-dep)
              comparable (set (keys aspects-of-kind))
              derived (set (filter comparable (keys reached)))]]
    {:id id :type type :reached reached :may (set (keys may)) :transitive (set (keys transitive))
     :declared declared :by-dep by-dep :deps deps :aspects aspects
     :generic (boolean (some #{:integration/external} aspects))
     :undeclared (sort (remove declared derived))
     :unreached (sort (remove derived declared))})))

(defn- io-nodes
  "Nodes from which some I/O site can be reached."
  [{:keys [edges io]}]
  (let [rev (reduce (fn [m [from tos]] (reduce #(update %1 %2 (fnil conj #{}) from) m tos)) {} edges)]
    (loop [frontier (vec (keys io)) seen (set (keys io))]
      (if-let [n (peek frontier)]
        (let [nx (remove seen (rev n))] (recur (into (pop frontier) nx) (into seen nx)))
        seen))))

(defn- visited [{:keys [edges]} node stop]
  (loop [frontier [node] seen #{node}]
    (if-let [n (peek frontier)]
      (let [nx (when (or (= n node) (not (stop n))) (remove seen (edges n)))]
        (recur (into (pop frontier) nx) (into seen nx)))
      seen)))

(defn snapshot
  "What a review needs from one commit, keyed by source path relative to
  `root`: per executable entity its file, own I/O with one path per kind, the
  I/O it may reach, its declarations and where they disagree; per form that
  leads to I/O, the kinds it reaches; and per entity, which of those forms its
  own I/O passes through."
  [g root aspects-of-kind executable? deps-of-kind]
  (let [root (str root "/")
        rel (fn [[f r]] [(if (str/starts-with? f root) (subs f (count root)) f) r])
        stop (set (map :node (vals (:registers g))))
        direct (assoc g :edges (:direct-edges g))
        ions (io-nodes direct)
        rows (compare-aspects g aspects-of-kind executable? deps-of-kind)]
    {:entities (into {} (for [r rows :let [node (get-in g [:registers (:id r) :node])]]
                          [(:id r) (-> r
                                       (select-keys [:type :may :declared :undeclared :unreached :by-dep])
                                       (assoc :node (rel node)
                                              :own (update-vals (:reached r) #(mapv rel %))))]))
     :nodes (into {} (for [n ions :when (not (stop n))]
                       [(rel n) (set (keys (reach direct n stop)))]))
     :passes (into {} (for [r rows :let [node (get-in g [:registers (:id r) :node])]]
                        [(:id r) (set (map rel (filter ions (visited direct node stop))))]))}))
