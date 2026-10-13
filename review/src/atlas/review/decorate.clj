(ns atlas.review.decorate
  (:require [atlas.ide :as ide]
            [clojure.set]
            [atlas.registry :as registry]
            [atlas.registry.lookup :as lookup]
            [atlas.review.candidate :as candidate]
            [atlas.review.diff :as diff]
            [atlas.review.registry :as reg]
            [atlas.review.risk :as risk]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [rewrite-clj.node :as n]
            [sdiff.core :as core]
            [sdiff.decorate :as d :refer [derived]]
            [sdiff.group :as group]))

(defonce ^:private attempts (atom {}))

(defn- candidate-for [{:keys [num head repo]}]
  (or (@attempts [num head])
      (let [r (try (candidate/fetch repo num head) (catch Exception e {:reason (ex-message e)}))]
        (swap! attempts assoc [num head] r)
        r)))

(defn forget-attempt! [num head] (swap! attempts dissoc [num head]))

(defn- stored [v] (str (subs (:commit v) 0 7) (when-let [s (or (:source-sha v) (:source v))] (str " (built from " (subs s 0 (min 12 (count s))) ")"))))

(defn- short-sha [s] (when s (subs s 0 (min 7 (count s)))))

(defn- a-or-an [word] (str (if (re-find #"(?i)^[aeiou]" (str word)) "an " "a ") word))

(defn- choose-base
  "The version on `line` the candidate is compared with: the newest recorded
  at or before the commit CI built it on, named by the artifact's sidecar;
  without one, the newest recorded by the time the run started; without a
  candidate, the newest at or before the PR's merge base."
  [line report {:keys [registry built-on run-at]}]
  (let [ancestry (candidate/ancestry (get-in report [:pr :repo]))]
    (cond
      (:base-sha built-on) (assoc (reg/base-for line (:base-sha built-on) ancestry) :sha (:base-sha built-on) :of :ci)
      (and registry run-at) (assoc (reg/base-recorded-by line run-at) :run-at run-at)
      :else (assoc (reg/base-for line (:base report) ancestry) :sha (:base report) :of :merge-base))))

(defn- base-note
  "How the base version was chosen, after its label."
  [{:keys [version how sha of run-at]}]
  (let [whose (if (= :ci of) "the PR's base" "this PR's merge base")
        built (short-sha (or (:source-sha version) (:source version) (:commit version)))
        inferred (when run-at (str " (inferred from the CI run's start " run-at ")"))]
    (case how
      :exact (str " · " whose)
      (:ancestor :committed) (str " · registry unchanged from " built " to " whose " " (short-sha sha) (when (= :committed how) ", by commit time"))
      :run-time (str " · registry unchanged from " built " to the PR's base" inferred)
      :oldest (list " " [:span.tag.tag-ext "the store has no version as old as this PR's base; comparing with the oldest one"] inferred)
      :latest (str " · " whose " could not be placed in the store; comparing with the newest"))))

(defn context
  "The registries a report is read against. The base is the newest version
  recorded on the configured branch at or before the commit the candidate was
  built on (see `choose-base`), with `:stale` the versions recorded after it.
  With an `against` request parameter naming another store branch, the base is
  the newest version on that branch, and that version is kept as `:merge` so
  the header can tell what was merged since from what the PR changes. Under
  `:against`, `:step` is how the PR's base stands to what the environment runs
  (see `reg/relation`): only when it descends from it is the first step
  something merged since the deploy."
  [report]
  (if-not (:store @reg/config)
    {:report report}
    (let [line (:branch @reg/config)
          asked (not-empty (get d/*params* "against"))
          ref (when asked (reg/resolve-against asked))
          env-v (when (and ref (not= ref line)) (reg/latest ref))
          {:keys [registry label reason] :as cand} (candidate-for (:pr report))
          {base-v :version :as choice} (choose-base line report cand)
          base-label (reg/label base-v)
          note (base-note choice)
          problem (cond (and asked (not ref)) (str "against=" asked ": the registry store has no such branch; compared against " (reg/shown line) " instead")
                        (and ref (not= ref line) (not env-v)) (str "against=" asked ": " (reg/shown ref) " holds no recorded registry; compared against " (reg/shown line) " instead"))]
      (cond-> {:report report :line line
               :base-v base-label :base-note note
               :base (reg/registry-at (:commit base-v))
               :cand-v label :reason reason}
        registry (assoc :cand registry
                        :stale (reg/since line base-v)
                        :built-on (or (some-> (:base-sha (:built-on cand)) short-sha) (when-let [t (:run-at cand)] (str (reg/shown line) " as of " t))))
        problem (assoc :against-problem problem)
        env-v (assoc :against (assoc env-v :ref ref :name (or (:env env-v) asked)
                                     :step (reg/relation line env-v base-v (:ancestor? (candidate/ancestry (get-in report [:pr :repo])))))
                     :merge-v base-label :merge (reg/registry-at (:commit base-v))
                     :base-v (stored env-v) :base (reg/registry-at (:commit env-v)))))))

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

(defn- merged-step? [against] (= :descends (:step against)))

(defn- differs [env] (str "differs between " env " and this PR's base"))

(defn- state-of
  "`[text quiet?]`, the state of `id` against the base. Against an environment
  each difference is put on its step: merged since the deploy, or this PR.
  When the PR's base does not descend from what the environment runs, the
  first step is only a difference between the two, not something merged."
  [{:keys [cand cand-v merge base against line]} id d]
  (let [env (:name against)
        since (str "since " env "'s deploy (merged, not there yet)")
        step (if (merged-step? against)
               {:new (str "new " since) :deleted (str "deleted " since)
                :changed (str "contract changed " since) :both (str "contract changed by this PR and " since)}
               {:new (str "not in what " env " runs; " (differs env) ", not this PR")
                :deleted (str "in what " env " runs, not in this PR's base; not this PR")
                :changed (str "contract " (differs env) ", not this PR")
                :both (str "contract changed by this PR and " (differs env))})
        own (when (and against cand) (diff/entity-delta merge cand id))
        drift (when against (diff/entity-delta base merge id))]
    (cond
      (:new? d) [(if (or (not against) (:new? own)) "new in this PR" (:new step))]
      (:deleted? d) [(if (or (not against) (:deleted? own)) "deleted by this PR" (:deleted step))]
      (changed? d) [(cond (not against) "contract changed"
                          (and (changed? own) (changed? drift)) (:both step)
                          (changed? own) "contract changed by this PR"
                          :else (:changed step))]
      cand-v ["contract unchanged" true]
      against [(str "as " env " runs it; no candidate version staged") true]
      :else [(str "as on " (reg/shown line) "; no candidate version staged") true])))

(defn- test-case? [props] (= :atlas/test-case (:atlas/type props)))

(defn- cap [xs n] (if (> (count xs) n) (concat (take n xs) [(str "+" (- (count xs) n) " more")]) xs))

(defn declares [ctx id]
  (let [[cid props] (in ctx #(do [(lookup/identity-for id) (lookup/props-for id)]))
        d (delta ctx id)
        [state quiet?] (state-of ctx id d)
        state-tag [:span.tag {:class (if quiet? "tag-note" "tag-ext")} state]]
    (cond
      (not cid)
      (derived "contract" [:p "declares " (ent id) ", " (if (:deleted? d) state "not in the registry")])

      (test-case? props)
      (derived "contract" [:p "test case on " (ent (:test-case/target props)) " · " state-tag])

      :else
      (let [added (sort-by str (:aspects-added d #{})) removed (sort-by str (:aspects-removed d #{}))
            rows (fn [m] [:table.contract [:tbody (for [[k v] m] [:tr [:td (name k)] [:td (interpose " " (for [x (if (coll? v) v [v])] [:code (str x)]))]])]])]
        (derived "contract"
                 [:p "declares " (ent id) " " [:span.mute (name (:atlas/type props))] " · " state-tag " · "
                  [:span.mute {:title (str/join " " (map str (sort-by str cid)))} (str (count cid) " aspects")]]
                 (when (or (seq added) (seq removed))
                   [:div.aspects (interpose " " (concat (for [a added] [:span.aspect.add (str "+" a)])
                                                        (for [a removed] [:span.aspect.del (str "-" a)])))])
                 (cond
                   (:new? d) (when-let [c (seq (select-keys props contract-keys))] (rows c))
                   (or (seq (:props-added d)) (seq (:props-removed d)))
                   [:table.contract.delta
                    [:tbody (for [[cls tuples] [["del" (sort-by str (:props-removed d))] ["add" (sort-by str (:props-added d))]] [_ attr v] tuples]
                              [:tr {:class cls} [:td (name attr)] [:td [:code (pr-str v)]]])]]))))))

(defn affects [ctx id]
  (in ctx (fn []
            (when-let [props (lookup/props-for id)]
              (when-not (test-case? props)
                (let [produced (concat (:execution-function/response props) (:endpoint/output props))
                      deps (ide/dependents-of id)
                      consumed (for [k produced :let [cs (ide/consumers-of k)] :when (seq cs)] [k cs])
                      tests (tests-of id)]
                  (when (or (seq deps) (seq consumed) (seq tests))
                    (derived "graph"
                             (when (seq deps) [:p "used by " (interpose ", " (map #(if (string? %) [:span.mute %] (ent %)) (cap deps 6)))])
                             (for [[k cs] consumed] [:p [:code (str k)] " → " (interpose ", " (map #(if (string? %) [:span.mute %] (ent %)) (cap cs 4)))])
                             (when (seq tests) [:p.mute {:title (str/join " " (map str tests))} (str (count tests) " test case" (when (> (count tests) 1) "s") " cover it")])))))))))

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

(defn- risk-of [{:keys [base cand] :as ctx} id]
  (let [d (delta ctx id)
        change (cond (:deleted? d) :deleted (or (:new? d) (changed? d)) :contract :else :code)]
    (reg/with-version (if (= :deleted change) base (or cand base))
      (when (lookup/identity-for id) (risk/assess id change)))))

(defn- level-tag [level]
  [:span.tag {:class (case level :high "tag-del" :medium "tag-ext" "tag-note")} (str (name level) " risk")])

(defn risk-line [ctx id]
  (when-let [{:keys [level reasons]} (risk-of ctx id)]
    (derived "graph"
             [:p (level-tag level) " " (interpose " · " reasons)])))

(defn registry-decoration [ctx file form]
  (when (:base ctx)
    (let [full-form (some #(when (= (:id form) (:id %)) %) (:forms (some (fn [f] (when (= (:path file) (:path f)) f)) (:clj (:report ctx)))))
          id (declared-id form)
          was-id (when (:was full-form) (declared-id {:id (:was full-form)}))
          file* (some #(when (= (:path file) (:path %)) %) (:clj (:report ctx)))]
      (when-let [parts (seq (remove nil? [(when id (risk-line ctx id))
                                          (when id (declares ctx id))
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

(defn risk-summary [{:keys [base cand merge against] :as ctx} report]
  (when base
    (let [a (anchors report)
          {:keys [new changed deleted]} (if cand (diff/summary base cand) {})
          own (when (and against cand) (set (apply concat (vals (diff/summary merge cand)))))
          merged? (merged-step? against)
          other? (fn [id] (boolean (and own (not (own id)))))
          step (fn [id] (when (other? id) [:span.mute (str " · merged since " (:name against) "'s deploy, not this PR")]))
          ids (distinct (concat (keys a) new changed deleted))
          test-case? (fn [id] (reg/with-version (or cand base) (= :atlas/test-case (:atlas/type (lookup/props-for id)))))
          aside? (fn [id] (and (not merged?) (other? id)))
          rows (->> ids (remove test-case?) (keep #(risk-of ctx %))
                    (sort-by (juxt (comp aside? :id) (comp - :score) (comp str :id))))
          link (fn [id] (if-let [h (a id)] [:a {:href (str "#" h)} (ent id)] (ent id)))
          row (fn [{:keys [id level reasons]}]
                (if (aside? id)
                  [:li [:span.tag.tag-note "context"] " " (link id) " — " [:span.mute (str (differs (:name against)) ", not this PR")]]
                  [:li (level-tag level) " " (link id) " — " (interpose " · " reasons) (step id)]))
          [top more] (split-at 5 rows)]
      (when (seq rows)
        (derived "registry graph"
                 [:h5 "Risk, from the system graph"]
                 [:ul.ids.risk (map row top)]
                 (when (seq more)
                   [:details [:summary (str (count more) " more, lower in the ranking")] [:ul.ids.risk (map row more)]]))))))

(defn- counts [{:keys [new changed deleted]}]
  (str (count new) " new, " (count changed) " changed, " (count deleted) " deleted"))

(defn- entity-lists
  "The new, changed and deleted entities of `summary`, each linked to the form
  that declares it; test cases are read from registry `to`."
  [report to {:keys [new changed deleted]}]
  (let [a (anchors report)
        link (fn [id] (if-let [h (a id)] [:a {:href (str "#" h)} (ent id)] (list (ent id) " " [:span.mute "(no form in this diff)"])))
        target-of (fn [id] (reg/with-version to (let [p (lookup/props-for id)] (when (test-case? p) (:test-case/target p)))))
        grouped (fn [xs] (let [{tests true others false} (group-by #(boolean (target-of %)) xs)]
                           (concat (for [id others] [:li (link id)])
                                   (for [[t ts] (group-by target-of tests)]
                                     [:li {:title (str/join " " (map str ts))} (str (count ts) " test case" (when (> (count ts) 1) "s") " on ") (ent t)]))))]
    (list (when (seq new) (list [:h5 "New"] [:ul.ids (grouped new)]))
          (when (seq changed) (list [:h5 "Changed"] [:ul.ids (grouped changed)]))
          (when (seq deleted) (list [:h5 "Deleted"] [:ul.ids (for [id deleted] [:li (ent id)])])))))

(defn- stale-note [{:keys [stale built-on line]}]
  (when (seq stale)
    (let [n (count stale)]
      [:p [:span.tag.tag-ext "stale candidate"]
       " Built against " built-on "; " n " registry change" (when (> n 1) "s") " merged since"
       [:span.mute {:title (str/join " " (map :subject stale))}
        " (" (str/join ", " (cap (map #(short-sha (or (:source-sha %) (:source %) (:commit %))) stale) 5)) ")"]
       ". Re-run CI to see this PR against current " [:code (reg/shown line)] "."])))

(defn- base-line
  "Which registry the page is compared against, and how it was chosen."
  [{:keys [line base-v base-note against merge-v cand-v] :as ctx}]
  (if-not against
    (list [:p "base " [:code (reg/shown line)] " @ " base-v base-note]
          (stale-note ctx))
    (let [{:keys [ref name from date]} against
          off-line? (and from (not= from line) (not= from (reg/shown line)))]
      (list [:p "base " [:code (reg/shown ref)] " @ " base-v " · what " name " runs"
             (when (and from (not off-line?)) (list ", deployed from " [:code from])) (when date (str ", recorded " date))
             (when off-line? (list " " [:span.tag.tag-ext (str "deployed from " from ", not " (reg/shown line))]))]
            [:p "→ " [:code (reg/shown line)] " @ " merge-v base-note ": "
             (case (:step against)
               :descends (str "merged, not yet on " name)
               :behind (str "older than what " name " runs")
               :diverged (str "diverged from what " name " runs")
               (str "not known to be older or newer than what " name " runs"))]
            [:p "→ this PR · " (if cand-v (str "CI " cand-v) "no registry for its head")]
            (stale-note ctx)))))

(defn- step-title
  "The first step of the header against an environment, named by how the
  PR's base stands to what the environment runs."
  [{:keys [name step from source-sha source]} line]
  (let [runs (str name " runs " (or from (short-sha (or source-sha source))))
        line (reg/shown line)]
    (case step
      :descends (str "Merged since " name "'s deploy, not there yet")
      :behind (str "Differs between " name " and this PR's base: " runs "; this PR is built on an older " line)
      :diverged (str "Differs between " name " and this PR's base: " runs "; this PR is built on " (a-or-an line) " that diverged from it")
      (str "Differs between " name " and this PR's base"))))

(defn registry-header [ctx report]
  (when (:base ctx)
    (let [{:keys [base base-v cand cand-v reason against merge merge-v against-problem]} ctx]
      (list
       (when against-problem (d/problem against-problem))
       (cond
         against
         (let [drift (diff/summary base merge)
               own (when cand (diff/summary merge cand))
               env (:name against)]
           (derived (str "registry store " (reg/shown (:ref against)) " " base-v " → " (reg/shown (:line ctx)) " " merge-v (when cand-v (str " → CI " cand-v)))
                    (base-line ctx)
                    (when-not cand [:p "no registry for this PR's head, so entities are shown as " env " runs them"])
                    (when (and (not cand) reason) [:p.mute reason])
                    [:p (step-title against (:line ctx)) ": " (counts drift)]
                    (entity-lists report merge drift)
                    (when own
                      (list [:p "This PR: " (counts own) (when (every? empty? (vals own)) " — this PR changes no contract")]
                            (entity-lists report cand own)))))

         (not cand)
         (derived "registry store"
                  (base-line ctx)
                  [:p "registry " [:code base-v] " · no registry for this PR's head, so entities are shown as on " (reg/shown (:line ctx))]
                  (when reason [:p.mute reason]))

         :else
         (let [s (diff/summary base cand)]
           (derived (str "registry store " base-v " → CI " cand-v)
                    (base-line ctx)
                    [:p "Registry: " (counts s)
                     (when (every? empty? (vals s)) " — this PR changes no contract")]
                    (entity-lists report cand s))))
       (risk-summary ctx report)
       (entity-annotations ctx)))))

(defn validate-entity [ctx on]
  (when-let [id (:entity on)]
    (let [id (if (keyword? id) id (edn/read-string (str id)))]
      (when-not (in ctx #(lookup/identity-for id)) (str "no entity " id " in the registry")))))

(defn entity-view [ctx id]
  (when (:base ctx)
    (list (risk-line ctx id) (declares ctx id) (affects ctx id))))

(defn- declared-by [report]
  (into {} (for [f (:clj report) :when (= :semantic (:verdict f)) form (:forms f)
                 :let [id (declared-id form)] :when id]
             [[(:path f) (d/form-id form)] id])))

(defn- related [ctx id]
  (in ctx #(when-let [p (lookup/props-for id)]
             (cond-> (set (remove string? (ide/dependents-of id)))
               (:test-case/target p) (conj (:test-case/target p))))))

(defn- domains [ctx ids]
  (in ctx #(sort (distinct (for [id ids a (lookup/identity-for id) :when (= "domain" (namespace a))] (name a))))))

(defn- code-edges [report declared]
  (let [owner (into {} (map (fn [[k v]] [v k]) declared))]
    (into (group/call-edges report)
          (for [[n kws] (group/keyword-mentions report) kw kws
                :let [m (owner kw)] :when (and m (not= m n))]
            #{n m}))))

(defn- seed-groups
  "Forms that declare entities, merged when the registry relates their entities."
  [ctx declared]
  (let [owner (into {} (map (fn [[k v]] [v k]) declared))
        edges (for [[n id] declared r (related ctx id) :let [m (owner r)] :when (and m (not= m n))] #{n m})]
    (group/components (keys declared) edges)))

(defn- attach
  "`{node #{group-index}}`: nodes reached from `owner`'s groups in `active`
  through `edges`, each with every group at the least distance. A node two
  groups reach equally, or one of the `hubs`, is not walked through."
  [owner edges hubs active]
  (let [adj (reduce (fn [m e] (let [[a b] (seq e)] (-> m (update a (fnil conj #{}) b) (update b (fnil conj #{}) a)))) {} edges)]
    (loop [frontier (keys owner) owner owner]
      (let [step (reduce (fn [m n]
                           (let [gs (owner n)]
                             (if (and (= 1 (count gs)) (active (first gs)) (not (hubs n)))
                               (reduce (fn [m x] (if (owner x) m (update m x (fnil into #{}) gs))) m (adj n))
                               m)))
                         {} frontier)]
        (if (empty? step) owner (recur (keys step) (merge owner step)))))))

(defn- base-name [path] (-> path (str/replace #".*/" "") (str/replace #"\.clj[cs]?$" "")))

(defn- keep-test-files-together
  "Each test file's forms go to one group: the group of the source file it is
  named after, when the PR changes one, else the group most of its forms
  reached."
  [owner nodes declared hubs]
  (let [single (fn [n] (let [gs (owner n)] (when (= 1 (count gs)) (first gs))))
        majority (fn [ns] (some->> (seq (keep single ns)) frequencies (sort-by (fn [[g c]] [(- c) g])) ffirst))
        by-file (group-by first nodes)
        by-name (into {} (for [[p ns] by-file :when (not (group/test-path? p))] [(base-name p) ns]))]
    (reduce (fn [owner [p ns]]
              (let [paired (by-name (str/replace (base-name p) #"_test$" ""))
                    target (or (when paired (majority paired)) (majority ns))]
                (if-not target
                  owner
                  (reduce (fn [o n] (if (or (declared n) (hubs n)) o (assoc o n #{target}))) owner ns))))
            owner
            (filter (comp group/test-path? key) by-file))))

(defn by-meaning
  "One group per changed part of the system: the forms that declare entities
  the registry relates, with the code that reaches them through calls or
  keyword mentions, nearest group first. Code two groups reach equally is
  shared. Code no group reaches joins the group holding most of its file, or
  is grouped by its own calls. Groups come in order of their highest risk."
  [ctx report]
  (if-not (:base ctx)
    (group/by-calls ctx report)
    (let [declared (declared-by report)
          nodes (group/nodes report)
          new-modules (for [f (:clj report)
                            :when (and (= "A" (:status f)) (= :semantic (:verdict f)) (not (group/test-path? (:path f)))
                                       (not-any? #(declared [(:path f) (d/form-id %)]) (:forms f)))]
                        (mapv #(vector (:path f) (d/form-id %)) (:forms f)))
          entity-seeds (seed-groups ctx declared)
          seeds (vec (concat entity-seeds new-modules))
          edges (code-edges report declared)
          hubs (apply disj (group/hubs edges 3) (keys declared))
          start (into {} (for [[i g] (map-indexed vector seeds) n g] [n #{i}]))
          first-pass (attach start edges hubs (set (range (count entity-seeds))))
          owner (keep-test-files-together (attach first-pass edges hubs (set (range (count seeds)))) nodes declared hubs)
          by-group (group-by (fn [n] (let [gs (owner n)] (cond (hubs n) :shared (nil? gs) :none (= 1 (count gs)) (first gs) :else :shared))) nodes)
          file-home (into {} (for [[path ns] (group-by first (mapcat (fn [i] (map #(vector (first %) i) (by-group i))) (range (count seeds))))]
                               [path (key (apply max-key val (frequencies (map second ns))))]))
          {homed true orphans false} (group-by #(boolean (file-home (first %))) (:none by-group))
          members (reduce (fn [m n] (update m (file-home (first n)) (fnil conj []) n)) (into {} (for [i (range (count seeds))] [i (vec (by-group i))])) homed)
          members (update-vals members (fn [ns] (filterv (set ns) nodes)))
          risk (memoize #(risk-of ctx %))
          test? (fn [id] (in ctx #(test-case? (lookup/props-for id))))
          lead (fn [ids] (first (sort-by (fn [id] [(if (test? id) 1 0) (- (or (:score (risk id)) -1)) (str id)]) ids)))
          describe (fn [ns]
                     (let [ids (keep declared ns)
                           main (remove test? ids)
                           top (lead ids)
                           ds (when (seq main) (sort (reduce clojure.set/intersection (map #(set (domains ctx [%])) main))))]
                       (str (count ns) " form" (when (> (count ns) 1) "s")
                            (if (seq main) (str ", " (count main) " entit" (if (= 1 (count main)) "y" "ies")) ", a new namespace outside the registry")
                            (when (> (count ids) (count main)) (str " and " (- (count ids) (count main)) " test case" (when (> (- (count ids) (count main)) 1) "s")))
                            (when (seq ds) (str " · " (str/join ", " (take 3 ds))))
                            (when-let [l (some-> top risk :level name)] (str " · " l " risk")))))
          weight (fn [ns] (reduce max -1 (keep #(some-> (declared %) risk :score) ns)))]
      (concat
       (for [[_ ns] (sort-by (fn [[_ ns]] [(- (weight ns)) (- (count ns))]) members) :when (seq ns)]
         (group/section (str "Around " (or (lead (keep declared ns)) (some #(when (str/starts-with? (second %) "ns ") (subs (second %) 3)) ns) (second (first ns))))
                        (describe ns) ns))
       (when-let [sh (seq (:shared by-group))]
         [(group/section "Shared by several groups" "Changed forms used from three or more source files, or reached equally by two groups." sh)])
       (when (seq orphans)
         (group/by-calls ctx (assoc report :clj (for [f (:clj report)]
                                                  (update f :forms (fn [fs] (filterv #((set orphans) [(:path f) (d/form-id %)]) fs)))))))))))

(d/add-param! "against")
(d/use-context! context)
(d/add-grouper! :meaning "meaning" "registry store + clj-kondo" by-meaning)
(d/add-entity-renderer! ::registry entity-view)
(d/add-form-decorator! ::registry registry-decoration)
(d/add-header-decorator! ::registry registry-header)
(d/add-ref-validator! ::entity validate-entity)
