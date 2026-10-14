(ns atlas.review.env
  "The environment page: what each `deployed/<env>` branch of the registry
  store runs, against the newest version of the line."
  (:require [atlas.registry.lookup :as lookup]
            [atlas.review.candidate :as candidate]
            [atlas.review.diff :as diff]
            [atlas.review.registry :as reg]
            [atlas.review.risk :as risk]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [sdiff.decorate :as d]))

(defonce settings (atom {:envs []}))

(def ^:private deployed "deployed/")

(defn- env-of [branch]
  (let [s (reg/shown branch)]
    (when (str/starts-with? s deployed) (not-empty (subs s (count deployed))))))

(defn recorded
  "`{env store-branch}` for every `deployed/<env>` branch of the store, the
  configured branch's remote first when both it and a local branch exist."
  []
  (into (sorted-map) (for [e (distinct (keep env-of (reg/branches)))] [e (reg/named (str deployed e))])))

(defn- short-sha [s] (when s (subs s 0 (min 7 (count s)))))

(defn- built-from [v] (or (:source-sha v) (:source v)))

(defn- ancestry [] (candidate/ancestry (:repo @reg/config)))

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

(defn compare-env
  "The environment's newest version on store branch `ref` against the line's
  newest. `:on-line` is what the line has and the environment does not run,
  as the line changed it since `:parted`; `:only-env` what the environment's
  branch has and the line does not, as that branch changed it. Without an
  order between the two, `:differs` holds every difference, from the
  environment to the line."
  [line ref]
  (let [env-v (reg/latest ref)
        line-v (reg/latest line)
        relation (reg/relation line env-v line-v (:ancestor? (ancestry)))
        env-r (reg/registry-at (:commit env-v))
        line-r (reg/registry-at (:commit line-v))
        differs (diff/summary env-r line-r)
        ids (set (apply concat (vals differs)))
        part (parted line env-v line-v relation)
        part-v (if (map? (:version part)) (:version part) part)
        part-r (when part-v (reg/registry-at (:commit part-v)))]
    (cond-> {:ref ref :env-v env-v :line-v line-v :relation relation
             :registries {:env env-r :line line-r :parted part-r}}
      part-v (assoc :parted part-v :parted-how (:how part)
                    :on-line (only ids (diff/summary part-r line-r))
                    :only-env (only ids (diff/summary part-r env-r)))
      (not part-v) (assoc :differs differs))))

(defn- total [summary] (reduce + (map count (vals summary))))

(defn- counts [{:keys [new changed deleted]}]
  (str (count new) " new, " (count changed) " changed, " (count deleted) " deleted"))

(defn- relation-text [relation]
  (case relation
    :descends "behind the line"
    :behind "ahead of the line"
    :diverged "diverged"
    "order unknown"))

(defn- gh-url [& parts] (when-let [repo (:repo @reg/config)] (apply str "https://github.com/" repo parts)))

(defn- commit-link [sha]
  (when sha
    (if-let [u (gh-url "/commit/" sha)] [:a {:href u} [:code (short-sha sha)]] [:code (short-sha sha)])))

(defn- env-href [env] (str "/env?against=" (java.net.URLEncoder/encode (str deployed env) "UTF-8")))

(def ^:private style
  [:style
   ".env-table{border-collapse:collapse;width:100%;font-size:14px}
    .env-table th,.env-table td{text-align:left;vertical-align:top;padding:6px 8px;border-bottom:1px solid #8884}
    .env-scroll{overflow-x:auto;-webkit-overflow-scrolling:touch}
    .env-side{margin:4px 0}
    .env-list{margin-top:18px}
    .env-list li{overflow-wrap:anywhere}"])

(defn- overview-row [line env ref]
  (if-not ref
    [:tr [:td env] [:td.mute {:colspan 6} "not recorded yet"]]
    (let [{:keys [env-v relation on-line only-env differs]} (compare-env line ref)]
      [:tr
       [:td [:a {:href (env-href env)} env]]
       [:td [:code (short-sha (:commit env-v))]]
       [:td (commit-link (built-from env-v))]
       [:td (or (:from env-v) [:span.mute "unknown"])]
       [:td (:date env-v)]
       [:td (relation-text relation)]
       [:td (if differs
              (str (total differs) " differ")
              (str (total on-line) " on " (reg/shown line) ", not deployed · " (total only-env) " only on " env "'s branch"))]])))

(defn- overview [line problem]
  (let [found (recorded)
        envs (distinct (concat (:envs @settings) (keys found)))
        line-v (reg/latest line)]
    (list
     style
     (when problem (d/problem problem))
     [:p "Each environment's newest recorded registry against " [:code (reg/shown line)] "'s newest, "
      [:code (short-sha (:commit line-v))] " built from " (commit-link (built-from line-v)) ". Pick an environment for the entities."]
     (if (empty? envs)
       [:p.mute "The store has no " [:code "deployed/<env>"] " branch, and no environment is expected (" [:code "--envs"] ")."]
       [:div.env-scroll
        [:table.env-table
         [:thead [:tr (for [h ["env" "store version" "built from" "deployed from" "recorded" (str "vs " (reg/shown line)) "entities"]] [:th h])]]
         [:tbody (for [e envs] (overview-row line e (found e)))]]]))))

(defn- assessed [from to {:keys [new changed deleted]}]
  (for [[change ids] [[:new new] [:changed changed] [:deleted deleted]] id ids]
    (let [in (if (= :deleted change) from to)]
      (reg/with-version in
        {:id id :change change
         :test? (= :atlas/test-case (:atlas/type (lookup/props-for id)))
         :risk (when (lookup/identity-for id) (risk/assess id (if (= :deleted change) :deleted :contract)))}))))

(defn- change-tag [change]
  [:span.tag {:class (case change :new "tag-add" :deleted "tag-del" "tag-ext")} (name change)])

(defn- entity-list
  "The entities of `summary`, from registry `from` to `to`, by risk level."
  [title from to summary]
  (let [rows (assessed from to summary)
        {tests true others false} (group-by (comp boolean :test?) rows)
        by-level (group-by #(get-in % [:risk :level] :low) others)]
    [:div.env-list
     [:h4 title " · " (counts summary)]
     (if (empty? rows)
       [:p.mute "nothing"]
       (list
        (for [level [:high :medium :low] :let [rs (sort-by (juxt (comp - #(get-in % [:risk :score] 0)) (comp str :id)) (by-level level))] :when (seq rs)]
          (list [:h5 (str (str/capitalize (name level)) " risk")]
                [:ul.ids.risk
                 (for [{:keys [id change risk]} rs]
                   [:li (change-tag change) " " [:code.ent (str id)] (when (seq (:reasons risk)) (list " — " (interpose " · " (:reasons risk))))])]))
        (when (seq tests)
          [:p.mute {:title (str/join " " (map (comp str :id) tests))}
           (str "and " (count tests) " test case" (when (> (count tests) 1) "s"))])))]))

(defn- side [label v]
  [:p.env-side [:b label] " · store " [:code (short-sha (:commit v))] " · built from " (commit-link (built-from v))
   (when (:from v) (list " · deployed from " [:code (:from v)]))
   " · recorded " (:date v)])

(defn- meaning
  "What the comparison of `env` with `line` means, in a sentence."
  [env line {:keys [relation env-v parted parted-how on-line]}]
  (let [from (or (:from env-v) "its branch")]
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
           ", so every difference is listed together, as " line " has it against what " env " runs."))))

(defn- detail [line asked]
  (let [ref (reg/resolve-against asked)
        env (when ref (or (env-of ref) (reg/shown ref)))]
    (cond
      (not ref) (overview line (str "against=" asked ": the registry store has no such branch"))
      (= ref line) (overview line (str "against=" asked ": that is " (reg/shown line) " itself; pick a " deployed "<env> branch"))
      (not (reg/latest ref)) (overview line (str "against=" asked ": " (reg/shown ref) " holds no recorded registry"))
      :else
      (let [{:keys [env-v line-v relation on-line only-env differs registries] :as c} (compare-env line ref)
            {env-r :env line-r :line part-r :parted} registries
            shown (reg/shown line)
            a (built-from env-v) b (built-from line-v)]
        (list
         style
         [:p [:a {:href "/env"} "All environments"]]
         [:h4 env " against " shown]
         (side env env-v)
         (side (str shown " newest") line-v)
         (when-let [u (and a b (gh-url "/compare/" a "..." b))] [:p [:a {:href u} "Compare the commits on GitHub"]])
         [:p (meaning env shown c)]
         (when (#{:descends :diverged} relation)
           (entity-list (str "On " shown ", not deployed") (if (= :diverged relation) part-r env-r) line-r on-line))
         (when (#{:behind :diverged} relation)
           (entity-list (str "Only on " env "'s branch") part-r env-r only-env))
         (when differs
           (entity-list (str "Differs between " env " and " shown) env-r line-r differs)))))))

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
