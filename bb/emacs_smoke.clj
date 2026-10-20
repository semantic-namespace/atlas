(ns emacs-smoke
  "Runs atlas-source.el's batch tests against a real `bb source serve':
   emacs/test/atlas-source-flymake-test.el (Flymake, on the shop fixture),
   and emacs/test/atlas-source-test.el (eldoc, xref, describe, on the cart
   example) when clojure-mode can be found: $CLOJURE_MODE_DIR, or where
   Debian's elpa-clojure-mode, straight or package.el put it. Needs Emacs 27
   or later: $EMACS, or the first of emacs, /snap/bin/emacs that is."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.string :as str]))

(defn- version [emacs]
  (try (some->> (:out (p/shell {:out :string :err :string :continue true} emacs "--version"))
                (re-find #"GNU Emacs (\d+)") second parse-long)
       (catch Exception _ nil)))

(defn- clojure-mode-dir []
  (let [home (System/getProperty "user.home")]
    (->> (concat [(System/getenv "CLOJURE_MODE_DIR")]
                 (map str (fs/glob "/usr/share/emacs/site-lisp" "elpa*/clojure-mode*"))
                 [(str home "/.emacs.d/straight/build/clojure-mode")]
                 (map str (fs/glob (str home "/.emacs.d/elpa") "clojure-mode-*")))
         (remove nil?)
         (filter #(fs/exists? (fs/path % "clojure-mode.el")))
         first)))

(defn- run [emacs & args]
  (:exit (apply p/shell {:continue true} emacs "--batch" "-Q" args)))

(defn -main [& _]
  (let [emacs (first (filter #(some-> (version %) (>= 27))
                             (remove nil? [(System/getenv "EMACS") "emacs" "/snap/bin/emacs"])))]
    (when-not emacs
      (println "emacs-smoke: needs Emacs 27 or later (set EMACS)")
      (System/exit 1))
    (println "emacs-smoke:" (str/trim (first (str/split-lines (:out (p/shell {:out :string} emacs "--version"))))))
    (let [flymake (run emacs "-l" "emacs/test/atlas-source-flymake-test.el" "-f" "ert-run-tests-batch-and-exit")
          dir (clojure-mode-dir)
          hover (if dir
                  (run emacs "-L" dir "-l" "emacs/test/atlas-source-test.el")
                  (do (println "emacs-smoke: atlas-source-test.el skipped: no clojure-mode (set CLOJURE_MODE_DIR)") 0))]
      (System/exit (if (and (zero? flymake) (zero? hover)) 0 1)))))
