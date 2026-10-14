(ns atlas.review.env
  "The environment page: what each `deployed/<env>` branch of the registry
  store runs, against the newest version of the line."
  (:require [atlas.registry.lookup :as lookup]
            [atlas.review.candidate :as candidate]
            [atlas.review.diff :as diff]
            [atlas.review.registry :as reg]
            [atlas.review.risk :as risk]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [sdiff.decorate :as d])
  (:import [java.time Duration OffsetDateTime ZonedDateTime]
           [java.time.format DateTimeFormatter]
           [java.time.temporal ChronoUnit]
           [java.util Locale]))

(defonce settings (atom {:envs []}))

(def ^:dynamic *now*
  "The clock times are told against, a function returning a ZonedDateTime
  whose zone the page's times are shown in."
  #(ZonedDateTime/now))

(def ^:private deployed "deployed/")

(defn- env-of [branch]
  (let [s (reg/shown branch)]
    (when (str/starts-with? s deployed) (not-empty (subs s (count deployed))))))

(defn- env-branches []
  (into (sorted-map) (for [e (distinct (keep env-of (reg/branches)))] [e (reg/named (str deployed e))])))

(defn recorded
  "`{env store-branch}` for every `deployed/<env>` branch of the store that
  holds a deploy record, the configured branch's remote first when both it
  and a local branch exist."
  []
  (into (sorted-map) (filter (comp reg/deployed val)) (env-branches)))

(defn seeded
  "The environments whose `deployed/<env>` branch holds no deploy record yet,
  only the line commits it was seeded with."
  []
  (sort (keep (fn [[e ref]] (when-not (reg/deployed ref) e)) (env-branches))))

(defn- short-sha [s] (when s (subs s 0 (min 7 (count s)))))

(defn- built-from [v] (or (:source-sha v) (:source v)))

(defn- ancestry [] (candidate/ancestry (:repo @reg/config)))

(defn- fmt [t pattern] (.format t (DateTimeFormatter/ofPattern pattern Locale/ENGLISH)))

(defn ago
  "ISO-8601 time `iso` as a reader tells it from `*now*`: minutes or hours
  ago within a day, yesterday or the weekday with the time within a week, the
  date beyond."
  [iso]
  (let [now (*now*)
        t (.atZoneSameInstant (OffsetDateTime/parse iso) (.getZone now))
        secs (.getSeconds (Duration/between t now))
        days (.between ChronoUnit/DAYS (.toLocalDate t) (.toLocalDate now))]
    (cond
      (< secs 60) "just now"
      (< secs 3600) (str (quot secs 60) " min ago")
      (< secs 86400) (str (quot secs 3600) "h ago")
      (= 1 days) (str "yesterday " (fmt t "HH:mm"))
      (< days 7) (fmt t "EEE HH:mm")
      :else (fmt t "yyyy-MM-dd"))))

(defn- when-el [iso] (when iso [:span {:title iso} (ago iso)]))

(defn- parted
  "The line version both sides hold: what the environment runs when the line
  descends from it, the line's newest when the environment is ahead of it,
  the newest line version the environment's commit descends from when they
  diverged; nil when their order is unknown."
  [line env-v line-v relation]
  (case relation
    :descends env-v
    :behind line-v
    :diverged (reg/base-for line (built-from env-v) (ancestry))
    nil))

(defn- only [ids summary] (update-vals summary #(filterv ids %)))

(defn- touched
  "`{id change}` for every entity that differs from registry `a` to `b`."
  [a b]
  (into {} (for [[change ids] (diff/summary a b) id ids] [id change])))

(defn- merges
  "The line's versions recorded after `from`, newest first, each with what it
  changed against the version before it."
  [line from]
  (when from
    (let [vs (vec (reg/since line from))]
      (mapv (fn [v before] {:version v :touched (touched (reg/registry-at (:commit before)) (reg/registry-at (:commit v)))})
            vs (concat (rest vs) [from])))))

(defn- line-start
  "The line version the environment's list of what it lacks is counted from."
  [line env-v relation part-v]
  (case relation
    :descends (or (reg/version-for line (built-from env-v)) (:version (reg/base-for line (built-from env-v) (ancestry))))
    :diverged part-v
    nil))

(defn- pull-request
  "The pull request holding the commit an environment runs when that commit is
  not on the line: a merged one first, else an open one; nil when GitHub
  cannot tell or there is no `--repo`."
  [line env-v relation]
  (let [sha (built-from env-v) repo (:repo @reg/config)]
    (when (and repo sha (not= :descends relation) (not (reg/version-for line sha)))
      (let [ps (candidate/pulls-with repo sha)]
        (or (some #(when (:merged? %) %) ps) (some #(when (= "open" (:state %)) %) ps))))))

(defn- pr-number [v] (some->> (:title v) (re-find #"\(#(\d+)\)\s*$") second parse-long))

(defn- merge-of? [pr v]
  (boolean (and pr (or (and (:merge-sha pr) (= (:merge-sha pr) (built-from v)))
                       (and (:merge-sha pr) (built-from v) (str/starts-with? (:merge-sha pr) (built-from v)))
                       (= (:number pr) (pr-number v))))))

(defn- attribute
  "The merges, each with the entities of `ids` it was the last to change and,
  for each, the older merges that changed it too; `:unplaced` the ids no
  merge changed."
  [ms ids]
  (let [by (fn [id] (filter #(contains? (:touched %) id) ms))
        owner (into {} (for [id ids :let [[m & more] (by id)] :when m] [id [m more]]))]
    {:merges (mapv (fn [m] (assoc m :ids (filterv #(= m (first (owner %))) ids)
                                  :also (into {} (for [id ids :let [[o more] (owner id)] :when (and (= m o) (seq more))] [id (mapv :version more)]))))
                   ms)
     :unplaced (filterv (complement owner) ids)}))

(defn compare-env
  "The environment's newest version on store branch `ref` against the line's
  newest. `:on-line` is what the line has and the environment does not run,
  as the line changed it since `:parted`, and `:merges` the line's versions
  since then, each with the entities of `:on-line` it last changed; `:only-env`
  what the environment's branch has and the line does not, as that branch
  changed it. Without an order between the two, `:differs` holds every
  difference, from the environment to the line. `:pr` is the pull request
  holding the commit the environment runs, when that commit is not on the
  line and GitHub knows it; `:merge?` when that commit is the pull request's
  merge itself; `:line-ahead?` when the environment's commit descends from the
  line's newest version and its branch adds nothing to it, the line past its
  newest recorded registry; `:pr-head?` when it is that pull request's head
  before it was merged and the environment's branch adds nothing to it."
  [line ref]
  (let [env-v (reg/deployed ref)
        line-v (reg/latest line)
        relation (reg/relation line env-v line-v (:ancestor? (ancestry)))
        env-r (reg/registry-at (:commit env-v))
        line-r (reg/registry-at (:commit line-v))
        differs (diff/summary env-r line-r)
        ids (set (apply concat (vals differs)))
        part (parted line env-v line-v relation)
        part-v (if (map? (:version part)) (:version part) part)
        part-r (when part-v (reg/registry-at (:commit part-v)))
        pr (pull-request line env-v relation)
        on-line (when part-v (only ids (diff/summary part-r line-r)))
        only-env (when part-v (only ids (diff/summary part-r env-r)))
        same? (boolean (and only-env (every? empty? (vals only-env))))
        merge? (boolean (when-let [m (:merge-sha pr)] (let [sha (built-from env-v)] (or (str/starts-with? m sha) (str/starts-with? sha m)))))
        line-ahead? (and same? (= :behind relation))
        pr-head? (and same? (:merged? pr) (not merge?) (not line-ahead?))
        ms (remove #(and pr-head? (merge-of? pr (:version %))) (merges line (line-start line env-v relation part-v)))
        placed (when on-line (attribute ms (vec (apply concat (vals on-line)))))]
    (cond-> {:ref ref :env-v env-v :line-v line-v :relation relation :pr pr :pr-head? pr-head? :merge? merge? :line-ahead? line-ahead?
             :registries {:env env-r :line line-r :parted part-r}}
      part-v (assoc :parted part-v :parted-how (:how part) :on-line on-line :only-env only-env
                    :merges (:merges placed) :unplaced (:unplaced placed))
      (not part-v) (assoc :differs differs))))

(defn- total [summary] (reduce + (map count (vals summary))))

(defn- counts [{:keys [new changed deleted]}]
  (str (count new) " new, " (count changed) " changed, " (count deleted) " deleted"))

(defn- plural [n word] (str n " " word (when (not= 1 n) "s")))

(defn- entities [n] (if (= 1 n) "1 entity" (str n " entities")))

(defn- gh-url [& parts] (when-let [repo (:repo @reg/config)] (apply str "https://github.com/" repo parts)))

(defn- commit-link [sha]
  (when sha
    (if-let [u (gh-url "/commit/" sha)] [:a {:href u} [:code (short-sha sha)]] [:code (short-sha sha)])))

(defn- pr-link [n] (if-let [u (gh-url "/pull/" n)] [:a {:href u} (str "#" n)] (str "#" n)))

(defn- env-href [env] (str "/env?against=" (java.net.URLEncoder/encode (str deployed env) "UTF-8")))

(def ^:private style
  [:style
   ".env-side{margin:4px 0}
    .env-store{font-size:12px}
    .env-list{margin-top:18px}
    .env-list li,.env-overview li,.env-merge,.env-group{overflow-wrap:anywhere}
    .env-merge{margin:14px 0 4px}
    .env-type{margin:6px 0 2px;font-size:13px}
    .env-overview{padding-left:18px}
    .env-overview li{margin:6px 0}"])

(defn- store-ref [v] [:small.mute.env-store "store " [:code (short-sha (:commit v))]])

(defn- merge-name
  "A line version as the merge it records: its subject with the pull request
  number linked, and the project commit it was built from."
  [v]
  (let [title (:title v)
        [_ before n] (when title (re-matches #"(.*?)\s*\(#(\d+)\)\s*" title))]
    (list (cond n (list before " (" (pr-link n) ")")
                title title
                :else "")
          (when title " · ") (commit-link (built-from v)))))

(defn- merge-label [v] (if-let [n (pr-number v)] (pr-link n) (commit-link (built-from v))))

(def ^:private type-names {"execution-function" "exec-fn" "interface-endpoint" "endpoint" "endpoint" "endpoint"
                           "data-key" "data key" "test-case" "test case" "invariant" "invariant"})

(def ^:private known-types (conj (set (vals type-names)) "MCP tool"))

(defn type-label
  "A short name for entity type `t`: exec-fn, endpoint, MCP tool, data key,
  test case, invariant; any other type by its name without the namespace."
  [t]
  (let [n (if t (name t) "entity")]
    (or (type-names n)
        (when (str/ends-with? n "mcp-tool") "MCP tool")
        (when (str/ends-with? n "-endpoint") "endpoint")
        n)))

(defn type-count
  "`n` entities of type label `t`: a known type in the plural, any other as
  its name and the count, since its plural cannot be told."
  [n t]
  (if (known-types t) (plural n t) (str t " (" n ")")))

(defn- assessed [from to {:keys [new changed deleted]}]
  (for [[change ids] [[:new new] [:changed changed] [:deleted deleted]] id ids]
    (let [in (if (= :deleted change) from to)]
      (reg/with-version in
        (let [t (:atlas/type (lookup/props-for id))
              test? (= :atlas/test-case t)
              r (when (and (not test?) (lookup/identity-for id))
                  (risk/assess id (case change :deleted :deleted :new :new :contract)))]
          {:id id :change change :test? test? :type (type-label t) :risk r
           :reasons (vec (remove #{(name change)} (:reasons r)))})))))

(def ^:private shared-wording
  {"no test case covers it or what depends on it" "none of these has a test case covering it or what depends on it"
   "it writes or notifies" "all of these write or notify"})

(defn- shared-reasons
  "The reasons every assessed row gives, when there are at least two."
  [rows]
  (let [rs (map (comp set :reasons) (remove :test? rows))]
    (if (< (count rs) 2) #{} (reduce set/intersection rs))))

(defn- shared-note [shared]
  (when (seq shared)
    (let [said (map #(get shared-wording % (str "all of these: " %)) (sort shared))]
      [:p.mute (str (str/capitalize (first said)) (apply str (map #(str "; " %) (rest said))) ".")])))

(defn- change-tag [change]
  [:span.tag {:class (case change :new "tag-add" :deleted "tag-del" "tag-ext")} (name change)])

(defn- relative-names
  "The namespace `ids` share, or the dotted prefix their namespaces share, and
  each id's name after it."
  [ids]
  (let [nss (map namespace ids)]
    (if (and (first nss) (apply = nss))
      [(str ":" (first nss) "/") (map name ids)]
      (let [segs (map #(str/split (or % "") #"\.") nss)
            common (reduce (fn [a b] (vec (take-while some? (map #(when (= %1 %2) %1) a b)))) segs)
            common (take (dec (apply min (map count segs))) common)
            prefix (when (seq common) (str ":" (str/join "." common) "."))]
        (if prefix
          [prefix (map #(subs (str %) (count prefix)) ids)]
          [nil (map str ids)])))))

(def ^:private collapse-at 4)
(def ^:private names-shown 6)

(defn- high? [row] (= :high (get-in row [:risk :level])))

(defn- row-reasons [shared row] (remove shared (:reasons row)))

(defn- also-note [vs] (when (seq vs) [:span.mute " · also changed by " (interpose ", " (map merge-label vs))]))

(defn- entity-row [shared also {:keys [id change] :as row}]
  (let [rs (row-reasons shared row)]
    [:li (change-tag change) " " [:code.ent (str id)]
     (when (high? row) (list " " [:span.tag.tag-del "high risk"]))
     (when (seq rs) (list " — " (interpose " · " rs)))
     (also-note (also id))]))

(defn- type-groups
  "Rows grouped by entity type; a long group whose rows differ in nothing but
  their names is one line."
  [rows shared also]
  (for [[t rs] (sort-by (fn [[t rs]] [(boolean (:test? (first rs))) t]) (group-by :type rows))
        :let [rs (sort-by (juxt (comp not high?) (comp - #(get-in % [:risk :score] 0)) (comp str :id)) rs)
              n (count rs)
              same? (and (>= n collapse-at) (not-any? high? rs) (not-any? (comp seq also :id) rs)
                         (apply = (map (juxt :change #(set (row-reasons shared %))) rs)))]]
    (if same?
      (let [[prefix names] (relative-names (map :id rs))
            [shown more] (split-at names-shown names)
            rs1 (row-reasons shared (first rs))]
        [:p.env-group (change-tag (:change (first rs))) " " (type-count n t) (when prefix (list " in " [:code prefix])) ": "
         (str/join ", " shown)
         (when (seq more) [:span.mute {:title (str/join ", " more)} (str ", … " (count more) " more")])
         (when (seq rs1) (list " — " (interpose " · " rs1)))])
      (list [:p.mute.env-type (type-count n t)]
            [:ul.ids.risk (for [r rs] (entity-row shared also r))]))))

(defn- one-line [title] [:div.env-list [:p [:b title] ": nothing."]])

(defn- merge-block [env shared rows-by-id {:keys [version ids touched also]}]
  (let [rows (keep rows-by-id ids)]
    (if (seq rows)
      [:div.env-merge
       [:p [:b (merge-name version)] [:span.mute " · " (entities (count rows)) " · " (when-el (:date version))]]
       (type-groups rows shared also)]
      [:p.env-merge (merge-name version) " "
       [:span.mute (if (seq touched)
                     (str "· its " (plural (count touched) "registry change") " already " (if (= 1 (count touched)) "runs" "run") " on " env)
                     "· no registry changes")]])))

(defn- on-line-list
  "What the line has and the environment lacks, under the merge that last
  changed each entity, newest first."
  [env shared rows {:keys [merges unplaced]}]
  (let [rows-by-id (into {} (map (juxt :id identity)) rows)]
    (list
     (for [m merges] (merge-block env shared rows-by-id m))
     (when-let [rs (seq (keep rows-by-id unplaced))]
       [:div.env-merge [:p [:b "Not placed on a recorded merge"]] (type-groups rs shared {})]))))

(defn- entity-section [title summary rows body]
  (if (and (zero? (total summary)) (not (seq body)))
    (one-line title)
    [:div.env-list
     [:h4 title " · " (counts summary)]
     (if (seq rows) body [:p.mute "no entity differs"])]))

(defn- side [label v]
  [:p.env-side [:b label] " · " (commit-link (built-from v))
   (when (:from v) (list " from " [:code (:from v)]))
   " · " (when-el (:date v)) " " (store-ref v)])

(defn- meaning
  "What the comparison of `env` with `line` means, in a sentence."
  [env line {:keys [relation env-v parted parted-how on-line pr pr-head? merge? line-ahead?]}]
  (let [from (or (:from env-v) "its branch")]
    (cond
      line-ahead?
      (str env " runs " (or (:from env-v) line) " at " (short-sha (built-from env-v)) (when merge? (str " (the merge of #" (:number pr) ")"))
           ", past " line "'s newest recorded registry with the same registry; nothing on " line " waits to be deployed.")

      pr-head?
      (str env " runs " from " — #" (:number pr) " before it was merged (as " (short-sha (:merge-sha pr)) "), registry-wise identical to #"
           (:number pr) " as merged" (if (zero? (total on-line)) (str "; nothing else on " line " waits to be deployed.")
                                         (str ": what follows is what " line " merged besides and " env " does not run yet.")))

      :else
      (str (when (and pr (not (:merged? pr))) (str env " runs " from " — #" (:number pr) ", not merged yet. "))
           (when merge? (str env " runs " from " at " (short-sha (built-from env-v)) ", the merge of #" (:number pr) ". "))
           (when (and (:merged? pr) (not merge?)) (str env " runs " from " — #" (:number pr) " before it was merged (as " (short-sha (:merge-sha pr)) "), with more on its branch since. "))
           (case relation
             :descends (if (zero? (total on-line))
                         (str env " runs " line "'s newest registry; nothing on " line " waits to be deployed.")
                         (str env " runs an older " line ": what follows is on " line " and not deployed to " env " yet."))
             :behind (str env " was deployed from " from ", which already holds " line "'s newest commit: what follows is only on that branch, and " line " has nothing " env " lacks.")
             :diverged (str env " was deployed from " from ", which diverged from " line
                            (if (#{:exact :ancestor :committed} parted-how)
                              (str " after " (short-sha (built-from parted)))
                              (str "; the store holds no " line " version both share, so " (short-sha (built-from parted)) " stands in for where they parted"))
                            ": the first list is what " line " changed since and " env " does not run, the second what " env "'s branch changed and " line " does not have.")
             (str "Whether " env " runs a commit on " line " cannot be told"
                  (when-not (:repo @reg/config) " (no --repo to ask GitHub)")
                  ", so every difference is listed together, as " line " has it against what " env " runs."))))))

(defn- waiting [ms] (filterv (comp seq :ids) ms))

(defn- runs
  "What an environment runs, as the overview tells it."
  [{:keys [env-v relation pr pr-head? merge? line-ahead?]} line]
  (list (cond (:from env-v) [:code (:from env-v)] line-ahead? [:code (reg/shown line)] :else "an unnamed branch") " at " (commit-link (built-from env-v))
        (cond
          merge? (list " (merge of " (pr-link (:number pr)) ")")
          line-ahead? nil
          pr-head? (list " — " (pr-link (:number pr)) " before it was merged")
          (:merged? pr) (list " — " (pr-link (:number pr)) " before it was merged, with more since")
          pr (list " — " (pr-link (:number pr)) ", not merged yet")
          (= :diverged relation) (str ", diverged from " line)
          (= :behind relation) (str ", ahead of " line)
          (nil? relation) (str ", order against " line " unknown"))))

(defn- overview-row [line env ref same seeded?]
  (if-not ref
    [:li [:b env] [:span.mute " not recorded yet" (when seeded? (str " (branch seeded from " (reg/shown line) ")"))]]
    (let [{:keys [env-v relation only-env differs merges] :as c} (compare-env line ref)
          ws (waiting merges)]
      [:li [:a {:href (env-href env)} [:b env]] " runs " (runs c line)
       (when same [:span.mute (str " (same commit as " same ")")])
       (for [part [(cond
                     differs (str (entities (total differs)) " differ")
                     (seq ws) (list (plural (count ws) "merge") " with registry changes waiting: " (interpose ", " (map (comp merge-label :version) ws)))
                     (or (#{:descends :diverged} relation) (:line-ahead? c)) "nothing waiting")
                   (when (and only-env (pos? (total only-env))) (str (entities (total only-env)) " only on its branch"))
                   (list "deployed " (when-el (:date env-v)))]
             :when part]
         (list " · " part))])))

(defn- overview [line problem]
  (let [found (recorded)
        seeds (set (seeded))
        envs (distinct (concat (:envs @settings) (sort (concat (keys found) seeds))))
        line-v (reg/latest line)
        sha-of (into {} (for [[e ref] found] [e (built-from (reg/deployed ref))]))]
    (list
     style
     (when problem (d/problem problem))
     [:p "What each environment runs against " [:code (reg/shown line)] "'s newest, "
      (commit-link (built-from line-v)) " · " (when-el (:date line-v)) " " (store-ref line-v) ". Pick an environment for the entities."]
     (if (empty? envs)
       [:p.mute "The store has no " [:code "deployed/<env>"] " branch, and no environment is expected (" [:code "--envs"] ")."]
       [:ul.env-overview
        (for [[i e] (map-indexed vector envs)
              :let [sha (sha-of e)
                    same (when sha (some #(when (= sha (sha-of %)) %) (take i envs)))]]
          (overview-row line e (found e) same (seeds e)))]))))

(defn- detail [line asked]
  (let [ref (reg/resolve-against asked)
        env (when ref (or (env-of ref) (reg/shown ref)))]
    (cond
      (not ref) (overview line (str "against=" asked ": the registry store has no such branch"))
      (= ref line) (overview line (str "against=" asked ": that is " (reg/shown line) " itself; pick a " deployed "<env> branch"))
      (not (reg/deployed ref)) (overview line (str "against=" asked ": " (reg/shown ref) " holds no deploy record yet"
                                                   (when (reg/latest ref) (str ", only the " (reg/shown line) " commits it was seeded with"))))
      :else
      (let [{:keys [env-v line-v relation on-line only-env differs registries] :as c} (compare-env line ref)
            {env-r :env line-r :line part-r :parted} registries
            shown (reg/shown line)
            a (built-from env-v) b (built-from line-v)
            on-rows (when (#{:descends :diverged} relation) (assessed (if (= :diverged relation) part-r env-r) line-r on-line))
            env-rows (when (#{:behind :diverged} relation) (assessed part-r env-r only-env))
            diff-rows (when differs (assessed env-r line-r differs))
            shared (shared-reasons (concat on-rows env-rows diff-rows))
            ms (:merges c)]
        (list
         style
         [:p [:a {:href "/env"} "All environments"]]
         [:h4 env " against " shown]
         (side env env-v)
         (side (str shown " newest") line-v)
         (when-let [u (and a b (gh-url "/compare/" a "..." b))] [:p [:a {:href u} "Compare the commits on GitHub"]])
         [:p (meaning env shown c)]
         (shared-note shared)
         (when (#{:descends :diverged} relation)
           (let [title (str "On " shown ", not deployed")]
             (if (and (zero? (total on-line)) (empty? ms))
               (one-line title)
               [:div.env-list
                [:h4 title " · " (counts on-line)
                 (when (seq ms) [:span.mute " · " (plural (count ms) "merge") " since " env "'s commit"])]
                (on-line-list env shared on-rows c)])))
         (when (#{:behind :diverged} relation)
           (entity-section (str "Only on " env "'s branch") only-env env-rows (type-groups env-rows shared {})))
         (when differs
           (entity-section (str "Differs between " env " and " shown) differs diff-rows (type-groups diff-rows shared {}))))))))

(defn page
  "GET /env: every environment, or with `against` one of them, against the
  line. The page is realised here, so a failing store read is reported on it."
  [req]
  (let [line (:branch @reg/config)]
    (if-not (:store @reg/config)
      (d/problem "no registry store; start the server with --store and --prefix")
      (try
        (let [asked (get-in req [:params "against"])]
          (when-not asked (reg/refresh! ::overview))
          (walk/postwalk identity
                         (cond
                           (not (reg/latest line)) (d/problem (str (reg/shown line) " holds no recorded registry"))
                           asked (detail line asked)
                           :else (overview line nil))))
        (catch Exception e (d/problem (str "registry store: " (ex-message e))))))))
