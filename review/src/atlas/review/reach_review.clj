(ns atlas.review.reach-review
  "The I/O reach of a pull request's entities, base against head, on the review
  page: a header section with what changed, and under each changed form the
  I/O it does or leads to. Source for both commits comes from GitHub through
  `gh`; per-repository settings come from a config file, so nothing about a
  project lives in this module."
  (:require [atlas.review.decorate :as decorate]
            [atlas.review.reach :as reach]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]]
            [clojure.string :as str]
            [sdiff.core :as core]
            [sdiff.decorate :as d :refer [derived]]))

(defonce config (atom {}))

(defn configure! [file]
  (reset! config (edn/read-string (slurp file))))

(defn- regexes [m] (into (if (map? m) {} []) (for [[k v] m] [k (re-pattern v)])))

(defn- settings [repo]
  (when-let [c (get @config repo)]
    {:dirs (:dirs c ["src"])
     :graph {:registration-heads (re-pattern (:registration-heads c)) :component-heads (re-pattern (:component-heads c))
             :sinks (regexes (:sinks c)) :services (regexes (:services c)) :java-sinks (regexes (:java-sinks c))
             :generators (regexes (:generators c)) :opaque (re-pattern (:opaque c))}
     :aspects-of-kind (:aspects-of-kind c)
     :deps-of-kind (regexes (:deps-of-kind c))
     :executable? (let [re (re-pattern (:executable c))] (fn [t] (boolean (and t (re-find re (name t))))))
     :compared (set (keys (:aspects-of-kind c)))
     :hash (hash c)}))

(def ^:private cache-root (io/file (System/getProperty "user.home") ".cache" "atlas-review"))

(defn- source-dir [repo sha dirs]
  (let [dir (io/file cache-root "src" (str/replace repo "/" "--") sha)]
    (when-not (.exists (io/file dir ".complete"))
      (.mkdirs dir)
      (let [tgz (io/file dir "src.tgz")
            {:keys [exit err]} (sh "bash" "-c" (str "gh api repos/" repo "/tarball/" sha " > " tgz
                                                   " && tar -xzf " tgz " -C " dir " --strip-components=1 --wildcards "
                                                   (str/join " " (map #(str "'*/" % "/*'") dirs))
                                                   " && rm -f " tgz))]
        (when-not (zero? exit) (throw (ex-info (str "could not fetch " repo "@" sha ": " err) {})))
        (spit (io/file dir ".complete") "")))
    (str (.getCanonicalPath dir))))

(defonce ^:private snapshots (atom {}))

(defn snapshot [repo sha]
  (when-let [{:keys [dirs graph aspects-of-kind executable? deps-of-kind hash]} (settings repo)]
    (let [k [repo sha hash] f (io/file cache-root "reach" (str (str/replace repo "/" "--") "-" sha "-" hash ".edn"))]
      (or (@snapshots k)
          (let [s (if (.exists f)
                    (edn/read-string (slurp f))
                    (let [root (source-dir repo sha dirs)
                          s (reach/snapshot (reach/graph root dirs graph) root aspects-of-kind executable? deps-of-kind)]
                      (io/make-parents f) (spit f (pr-str s)) s))]
            (swap! snapshots assoc k s)
            s)))))

(defn- kinds-of [e] (set (keys (:own e))))

(defn- changed-files [report] (set (map :path (:clj report))))

(defn- touched [report base head]
  (let [files (changed-files report)
        on-path (fn [snap id] (or (files (first (get-in snap [:entities id :node])))
                                  (some (comp files first) (get-in snap [:passes id]))))]
    (sort-by str (filter #(or (on-path base %) (on-path head %)) (set (concat (keys (:entities base)) (keys (:entities head))))))))

(defn diff [report base head]
  (vec (for [id (touched report base head)
             :let [b (get-in base [:entities id]) a (get-in head [:entities id])]]
         {:id id :status (cond (nil? b) :new (nil? a) :removed :else :changed)
          :gained (sort (remove (kinds-of b) (kinds-of a))) :lost (sort (remove (kinds-of a) (kinds-of b)))
          :undeclared (vec (:undeclared a)) :introduced (sort (remove (set (:undeclared b)) (:undeclared a)))})))

(defn- chip [cls k & [t]] [:span.tag {:class cls :title t} (name k)])

(defn- io-chips [compared e]
  (let [und (set (:undeclared e)) dep (set (:by-dep e))]
    (interpose " " (for [k (sort (keys (:own e)))]
      (cond (und k) (chip "tag-del" k "done, not declared")
            (not (compared k)) (chip "tag-note" k "not compared")
            (dep k) (chip "tag-add" k "declared through a component in its deps")
            :else (chip "tag-add" k "declared by an aspect"))))))

(defn- context [report]
  (let [ctx (decorate/context report)
        repo (get-in report [:pr :repo])]
    (if-not (and repo (settings repo))
      ctx
      (try (assoc ctx :reach {:base (snapshot repo (:base report)) :head (snapshot repo (:head report))
                              :compared (:compared (settings repo))})
           (catch Exception e (assoc ctx :reach-error (ex-message e)))))))

(defn header [{:keys [reach reach-error]} report]
  (cond
    reach-error (derived "reach" [:p "I/O reach could not be computed: " reach-error])
    reach
    (let [{:keys [base head]} reach
          ds (diff report base head)
          moved (filter #(or (seq (:gained %)) (seq (:lost %)) (not= :changed (:status %))) ds)
          introduced (filter (comp seq :introduced) ds)]
      (derived "reach · clj-kondo over base and head"
               [:h5 "I/O"]
               [:p (count ds) " registered entities touched · "
                (count (filter #(= :new (:status %)) ds)) " new · "
                (count (filter #(= :removed (:status %)) ds)) " removed · "
                (count (filter #(and (= :changed (:status %)) (or (seq (:gained %)) (seq (:lost %)))) ds)) " whose own I/O changed · "
                (count introduced) " with undeclared I/O introduced"]
               (if (empty? moved)
                 [:p.mute "No entity's own I/O changed."]
                 [:ul.ids (for [{:keys [id status gained lost]} moved]
                            [:li [:code.ent (str id)] " " (name status)
                             (when (seq gained) (list " · gains " (interpose " " (for [k gained] (chip "tag-add" k)))))
                             (when (seq lost) (list " · loses " (interpose " " (for [k lost] (chip "tag-del" k)))))])])
               (when (seq introduced)
                 [:p "Undeclared I/O introduced: "
                  (interpose ", " (for [{:keys [id introduced]} introduced] (list [:code.ent (str id)] " " (str/join ", " (map name introduced)))))])))))

(defn- row-of [src id] (when (seq src) (:row (meta (get (core/index src) id)))))

(defn form-line [{:keys [reach report]} file form]
  (when reach
    (let [{:keys [base head compared]} reach
          file (or (some #(when (= (:path file) (:path %)) %) (:clj report)) file)
          full (some #(when (= (:id form) (:id %)) %) (:forms file))
          id (decorate/declared-id form)
          path (:path file)]
      (if (and id (or (get-in head [:entities id]) (get-in base [:entities id])))
        (let [a (get-in head [:entities id]) b (get-in base [:entities id])
              gained (sort (remove (kinds-of b) (kinds-of a))) lost (sort (remove (kinds-of a) (kinds-of b)))]
          (derived "reach"
                   [:p "own I/O " (if (seq (:own a)) (io-chips compared a) [:span.mute "none"])
                    (cond (nil? b) [:span.mute " · new"]
                          (or (seq gained) (seq lost)) (list " · was " (if (seq (:own b)) (str/join ", " (map name (sort (kinds-of b)))) "none"))
                          :else [:span.mute " · unchanged"])]
                   (when (seq (:own a))
                     [:details [:summary.mute "paths"]
                      (for [[k p] (sort-by key (:own a))] [:p.mute [:b (name k)] " " (str/join " → " (map (fn [[f r]] (str f ":" r)) (rest p)))])])))
        (let [new-row (row-of (:new file) (:id form))
              old-row (row-of (:old file) (or (:was full) (:id form)))
              now (when new-row (get-in head [:nodes [path new-row]]))
              before (when old-row (get-in base [:nodes [path old-row]]))
              users (when new-row (sort-by str (for [[eid ns] (:passes head) :when (ns [path new-row])] eid)))]
          (when (or (seq now) (seq before))
            (derived "reach"
                     (if-not new-row
                       [:p "removed · led to " (str/join ", " (map name (sort before)))]
                       [:p "leads to " (if (seq now) (interpose " " (for [k (sort now)] (chip "tag-note" k))) [:span.mute "no I/O"])
                      (when (not= (set now) (set before))
                        (list " · was " (if (seq before) (str/join ", " (map name (sort before))) "no I/O")))
                      (when (seq users) (list " · on the I/O path of " (count users) " entit" (if (= 1 (count users)) "y" "ies")))])
                     (when (seq users)
                       [:details [:summary.mute "which"] [:p (interpose " " (for [u users] [:code.ent (str u)]))]]))))))))

(d/use-context! context)
(d/add-header-decorator! ::reach header)
(d/add-form-decorator! ::reach form-line)
