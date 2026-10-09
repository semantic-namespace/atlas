(ns atlas.review.ownership
  "What a changed form means in an atlas codebase, read from the code alone.
  Walking callers back from the form finds the registrations whose
  implementation reaches it; their dev-id, type and aspects are literals in
  the source, so the change inherits them. Walking on from those owners finds
  the entry points (endpoints, MCP tools, workflows) that expose it, and their
  I/O reach at base and head tells when an owner starts touching a new kind of
  I/O. Needs no registry store: the registration forms are the record."
  (:require [clojure.string :as str]
            [rewrite-clj.node :as n]
            [rewrite-clj.parser :as p]
            [sdiff.core :as core]
            [sdiff.decorate :as d :refer [derived]]
            [sdiff.deps :as deps]
            [sdiff.github :as github]))

(defonce settings
  (atom {:registers #"(^|/)register!$"
         :surface #"endpoint|mcp-tool|workflow|llm-prompt"
         :reads #"^(query|read|resolve|search|status|get|list|lookup|fetch|predict|score|aggregation|aggregate|validate|explain|measure|poll|preflight)$"
         :verb-namespaces #{"action" "effect" "operation"}
         :shown-namespaces #{"domain" "action" "operation" "effect" "async" "integration"}
         :max-walk 4000}))

(defn- registration-id
  "The keyword a registration form defines, from its name in the graph."
  [snap node]
  (let [[head arg] (str/split (str (get-in snap [:forms node :name])) #"\s+" 3)]
    (when (and arg (re-find (:registers @settings) head) (str/starts-with? arg ":")) arg)))

(defn- code? [x] (not (#{:whitespace :newline :comma :comment :uneval} (n/tag x))))

(def ^:private parsed
  (memoize (fn [src]
             (into {} (for [node (n/children (p/parse-string-all src)) :when (code? node)]
                        [(:row (meta node)) node])))))

(defn- literal [node]
  (try (n/sexpr node) (catch Exception _ (n/string node))))

(defn- entity
  "`{:id :type :aspects}` of the registration at `node`, read from its source."
  [repo sha [path row]]
  (when-let [src (some-> @github/source-reader (apply [repo sha path]))]
    (when-let [form (get (parsed src) row)]
      (let [[_ id type & more] (filter code? (n/children form))
            aspects (some #(when (= :set (n/tag %)) (set (map literal (filter code? (n/children %))))) more)]
        {:id (n/string id) :type (some-> type n/string) :aspects (or aspects #{})}))))

(defn- walk-back
  "Registrations reached walking callers back from `node`: the first ones met
  on each path when `stop?`, every one within the walk limit otherwise."
  [snap node stop?]
  (loop [frontier [node] seen #{node} found []]
    (if-let [x (peek frontier)]
      (let [reg? (and (not= x node) (registration-id snap x))
            nx (when-not (and reg? stop?) (remove seen (get-in snap [:callers x])))
            nx (remove #(get-in snap [:forms % :test]) nx)]
        (if (> (count seen) (:max-walk @settings))
          (cond-> found reg? (conj x))
          (recur (into (pop frontier) nx) (into seen nx) (cond-> found reg? (conj x)))))
      found)))

(defn- reach [snap node]
  (loop [frontier [node] seen #{node} found #{}]
    (if-let [x (peek frontier)]
      (let [nx (remove seen (get-in snap [:calls x]))]
        (recur (into (pop frontier) nx) (into seen nx) (into found (get-in snap [:io x]))))
      found)))

(defn- node-of [snap path src id]
  (when (seq src)
    (when-let [row (:row (meta (get (core/index src) id)))]
      (let [k [path row]] (when (get-in snap [:forms k]) k)))))

(defn- writes?
  "An aspect set with a verb that is not a known read: a write is assumed until
  the verb says otherwise."
  [aspects]
  (let [{:keys [reads verb-namespaces]} @settings]
    (boolean (some #(and (keyword? %) (verb-namespaces (namespace %)) (not (re-find reads (name %)))) aspects))))

(defn- surface? [e] (boolean (some->> (:type e) (re-find (:surface @settings)))))

(defonce ^:private meanings (atom {}))

(declare meaning*)

(defn meaning
  "For one changed form: the entities owning it at head, the entry points that
  expose them, and the I/O each owner gains or loses against base."
  [report file form]
  (let [k [(get-in report [:pr :repo]) (:base report) (:head report) (:path file) (d/form-id form)]]
    (if (contains? @meanings k)
      (@meanings k)
      (let [m (meaning* report file form)] (swap! meanings assoc k m) m))))

(defn- meaning* [report file form]
  (when-let [{:keys [base head] :as dt} (deps/data report)]
    (when-not (:error dt)
      (let [repo (get-in report [:pr :repo])
            h (node-of head (:path file) (:new file) (:id form))
            b (node-of base (:path file) (:old file) (or (:was form) (:id form)))
            [snap sha node] (if h [head (:head report) h] [base (:base report) b])]
        (when (and node (not (get-in snap [:forms node :test])))
          (let [ent (memoize #(entity repo sha %))
                self? (registration-id snap node)
                owners (if self? [node] (walk-back snap node true))
                exposers (distinct (concat (filter (comp surface? ent) owners)
                                           (filter (comp surface? ent) (mapcat #(walk-back snap % false) owners))))
                base-node (fn [o] (some (fn [[k v]] (when (= (:name v) (get-in head [:forms o :name])) k)) (:forms base)))]
            (when (seq owners)
              {:self? (boolean self?)
               :owners (vec (for [o owners :let [e (ent o)] :when e]
                              (let [r-h (when h (reach head o))
                                    bo (when h (base-node o))
                                    r-b (when bo (reach base bo))]
                                (assoc e :node o
                                       :reaches (sort r-h)
                                       :reaches-added (when bo (sort (remove (set r-b) r-h)))))))
               :exposed (vec (keep ent exposers))})))))))

(defn weight
  "Why a change needs attention by what it is part of: it sits inside a write,
  or an owner now reaches new I/O. Exposure alone does not count: prompts and
  endpoints sit above nearly everything. Nil when none applies."
  [{:keys [owners]}]
  (seq (remove nil? [(when (some (comp writes? :aspects) owners) :writes)
                     (when (some (comp seq :reaches-added) owners) :new-io)])))

(defn- short-type [t] (some-> t (str/replace #"^:atlas/" "")))

(defn- aspect-tags [aspects]
  (let [shown (sort-by str (filter #(and (keyword? %) ((into (:shown-namespaces @settings) (:verb-namespaces @settings)) (namespace %))) aspects))]
    (interpose " " (for [a shown] [:span.aspect {:class (when (writes? #{a}) "del")} (str a)]))))

(defn- cap [xs n] (if (> (count xs) n) [(take n xs) (- (count xs) n)] [xs 0]))

(defn decoration [ctx file form]
  (when-let [{:keys [self? owners exposed] :as m} (meaning (:report ctx) file form)]
    (let [[shown more] (cap owners 4)
          by-type (frequencies (map (comp short-type :type) exposed))]
      (derived "atlas · registrations read from the code"
               [:div.atlas-own
                [:p (if self? "registers " "inside ")
                 (interpose ", " (for [{:keys [id type]} shown] (list [:code.ent id] " " [:span.mute (short-type type)])))
                 (when (pos? more) [:span.mute (str " +" more " more")])]
                (for [{:keys [id aspects reaches-added reaches]} shown :when (or (seq aspects) (seq reaches))]
                  [:p.mute [:code id] " " (aspect-tags aspects)
                   (when (seq reaches) (list " · reaches " (str/join ", " (map name reaches))))
                   (when (seq reaches-added) (list " · " [:span.tag.tag-ext (str "now reaches " (str/join ", " (map name reaches-added)))]))])
                (when (seq exposed)
                  [:p "exposed via "
                   (interpose ", " (for [[t c] (sort-by (comp - val) by-type)] (str c " " t)))
                   [:span.mute {:title (str/join "\n" (map :id exposed))} (str " · " (str/join ", " (map :id (take 3 exposed))) (when (> (count exposed) 3) " …"))]])
                (when-let [w (weight m)]
                  [:p (for [k w] [:span.tag.tag-del ({:writes "inside a write" :new-io "owner gains I/O"} k)])])]))))

(defn- form-at [report path id]
  (let [f (some #(when (= path (:path %)) %) (:clj report))]
    [f (some #(when (= id (d/form-id %)) %) (:forms f))]))

(defn by-risk
  "sdiff's risk bands, with changes inside writes, exposed entry points or
  owners gaining I/O lifted into the first band."
  [ctx report]
  (let [bands (deps/by-risk ctx report)
        lifted? (fn [[_ p id]] (let [[f form] (form-at report p id)] (and form (weight (meaning report f form)))))
        {lift true stay false} (group-by (comp boolean lifted?) (:items (some #(when (= "Other changes" (:title %)) %) bands)))]
    (if (empty? lift)
      bands
      (let [attention {:title "Needs attention" :items (into (vec (:items (some #(when (= "Needs attention" (:title %)) %) bands))) lift)}]
        (into [attention]
              (keep (fn [b] (case (:title b)
                              "Needs attention" nil
                              "Other changes" (when (seq stay) (assoc b :items (vec stay)))
                              b))
                    bands))))))

(defn install! []
  (d/add-form-decorator! ::ownership decoration)
  (d/add-grouper! :risk "risk" "deps · clj-kondo, with atlas registrations read from the code" by-risk))
