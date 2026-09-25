(ns atlas-review.impact
  "Atlas impact of a branch, per changed file: which dev-ids the file registers
  before (BASE) and after (the worktree), each with its entity type and how many
  entities depend on it in the loaded registry.

  Load in the worktree's REPL (the registry must be the branch's), then:
    (atlas-review.impact/report \"/path/to/worktree\" \"<base-rev>\")

  Registrations are found textually (the registry keeps no source location):
  the dev-id alone on the line after a `(register!` / `(bind` / `(def` opener, a call whose first argument is
  the dev-id (`(register! :x`, `(bind :x`, `(def :x`), or an `:atlas/dev-id :x`
  entry. Treat the result as a lead to verify in the diff, not as proof."
  (:require [clojure.java.shell :as sh]
            [clojure.string :as str]
            [atlas.registry.lookup :as lookup]
            [atlas.ide :as ide]))

(def ^:private kw-re #":[\w.\-]+/[\w.\-!?*+]+")

(defn- registered
  "Dev-ids that look registered in TEXT."
  [text]
  (let [lines (vec (str/split-lines (or text "")))]
    (->> (range (count lines))
         (keep (fn [i]
                 (let [l (str/trim (lines i))
                       prev (when (pos? i) (str/trim (lines (dec i))))
                       bare (re-matches (re-pattern (str "(" kw-re ")")) l)
                       call (re-find (re-pattern (str "^\\((?:[\\w.\\-]+/)?(?:register!|bind|def)\\s+(" kw-re ")")) l)
                       dev-id (re-find (re-pattern (str ":atlas/dev-id\\s+(" kw-re ")")) l)
                       ;; a bare dev-id counts when the previous line opens the registering call
                       opener (and prev (re-find #"\((?:[\w.\-]+/)?(?:register!|bind|def)\s*$" prev))
                       m (or (when (and bare opener) bare) call dev-id)]
                   (when m (keyword (subs (second m) 1))))))
         set)))

(defn- info [kw]
  (let [t (some-> (lookup/props-for kw) :atlas/type name)
        n (try (count (ide/recursive-dependents-of kw)) (catch Throwable _ nil))]
    (str kw " [" (or t "not in registry") (when n (str ", " n " dependents")) "]")))

(defn file-impact
  "{:added :removed :kept} dev-ids registered by FILE, BASE vs worktree WT."
  [wt base file]
  (let [head (let [f (java.io.File. wt file)] (when (.exists f) (slurp f)))
        before (let [r (sh/sh "git" "show" (str base ":" file) :dir wt)] (when (zero? (:exit r)) (:out r)))
        h (registered head) b (registered before)]
    {:added (sort (remove b h)) :removed (sort (remove h b)) :kept (sort (filter b h))}))

(defn report
  "Print the impact of every changed Clojure file in WT against BASE."
  [wt base]
  (let [files (->> (:out (sh/sh "git" "diff" "--name-only" base :dir wt))
                   str/split-lines
                   (filter #(re-find #"\.clj[cs]?$" %)))]
    (doseq [f files]
      (let [{:keys [added removed kept]} (file-impact wt base f)]
        (println "##" f)
        (when (seq added) (println "  + registers:" (str/join "; " (map info added))))
        (when (seq removed) (println "  - no longer registers:" (str/join "; " (map str removed))))
        (when (seq kept) (println "  = still registers:" (str/join "; " (map info kept))))))))
