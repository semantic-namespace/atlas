(ns atlas.source.read
  "EXPERIMENTAL (see EXPERIMENTAL.md) — reading EDN as data with the line of
   each top-level key, for atlas.source.index and the packs (bb and JVM)."
  (:import (java.io StringReader)
           (clojure.lang LineNumberingPushbackReader)))

(defn keyed-lines
  "[[k v line] ...] for the top-level map in `text`, `line` being where k is
   written, or nil when the text doesn't start with a map. The reader keeps
   no lines on maps, so the map is read one key and value at a time, noting
   the line before each key. With `:tags :keep`, a tag no reader knows
   (#ig/ref, #env) is kept as a tagged literal instead of failing the read."
  ([text] (keyed-lines text {}))
  ([text {:keys [tags]}]
   (let [r (LineNumberingPushbackReader. (StringReader. text))
         skip (fn [] (loop []
                       (let [c (.read r)]
                         (cond
                           (= c -1) c
                           (or (Character/isWhitespace c) (= c (int \,))) (recur)
                           (= c (int \;)) (do (while (not (#{-1 (int \newline)} (.read r)))) (recur))
                           :else c))))]
     (binding [*read-eval* false
               *default-data-reader-fn* (if (= tags :keep) tagged-literal *default-data-reader-fn*)]
       (when (= (int \{) (skip))
         (loop [acc []]
           (let [c (skip)]
             (if (or (= c -1) (= c (int \})))
               acc
               (do (.unread r c)
                   (let [line (.getLineNumber r)
                         k (read r)
                         v (read r)]
                     (recur (conj acc [k v line]))))))))))))
