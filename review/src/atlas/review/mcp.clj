(ns atlas.review.mcp
  "The registry questions of `bb registry` as an MCP server over stdio,
   for a git store checkout, with no JVM: an LLM asks what has an aspect,
   who produces or consumes a data key, what a change reaches, what an
   entity is. Each tool is a `bb registry` command, answered by
   atlas.review.cli/run at the store ref the call names (HEAD by default).

     bb registry-mcp <store-dir> [--prefix p]

   JSON-RPC 2.0, one message per line: initialize, tools/list, tools/call."
  (:require [atlas.review.cli :as cli]
            [atlas.review.registry :as store]
            [cheshire.core :as json]
            [clojure.string :as str]))

(def ^:private ref-prop {:type "string" :description "store ref (branch, tag, commit); HEAD by default"})

(defn- id-prop [what] {:type "string" :description (str what ", a qualified keyword: :ns/name")})

(def tools
  "Tool name → its `bb registry` command, description, and argument names
   in the command's order."
  {"by_aspect" {:cmd "by-aspect" :args ["aspect"] :doc "The entities carrying an aspect"}
   "entity_detail" {:cmd "entity-detail" :args ["dev_id"] :doc "An entity: its type, aspects, props, and what it depends on and is depended on by"}
   "producers_of" {:cmd "producers-of" :args ["data_key"] :doc "The entities producing a data key"}
   "consumers_of" {:cmd "consumers-of" :args ["data_key"] :doc "The entities consuming a data key"}
   "dependencies" {:cmd "dependencies" :args ["dev_id"] :doc "What an entity depends on, effectively: what it declares, and the producers of the data it consumes"}
   "dependents" {:cmd "dependents" :args ["dev_id"] :doc "What depends on an entity, effectively: what declares a dependency on it, and the consumers of the data it produces"}
   "blast_radius" {:cmd "blast-radius" :args ["dev_id"] :doc "Everything that depends on an entity, transitively, by hops, through declared dependencies and data (what consumes what it produces): what a change to it reaches"}
   "trace_causes" {:cmd "trace-causes" :args ["dev_id"] :doc "Everything an entity depends on, transitively, by hops, through declared dependencies and data (what produces what it consumes): where a failure in it may come from"}
   "overview" {:cmd "overview" :args [] :doc "The registry's entity types, aspects and counts"}})

(defn- schema [{:keys [args]}]
  {:type "object"
   :properties (into {"ref" ref-prop}
                     (for [a args] [a (case a
                                        "aspect" (id-prop "the aspect")
                                        "data_key" (id-prop "the data key")
                                        (id-prop "the entity's dev-id"))]))
   :required args})

(defn- call
  "The tool's answer as JSON text, or an error result."
  [dir name arguments]
  (if-let [{:keys [cmd args]} (get tools name)]
    (try
      (let [ref (or (get arguments "ref") "HEAD")
            vals (for [a args] (get arguments a))]
        (when-let [missing (seq (filter #(str/blank? (get arguments %)) args))]
          (throw (ex-info (str "missing " (str/join ", " missing)) {})))
        {:content [{:type "text" :text (json/generate-string (cli/jsonable (cli/run cmd dir (cons ref vals) {})))}]})
      (catch Exception e
        {:content [{:type "text" :text (or (ex-message e) (str e))}] :isError true}))
    {:content [{:type "text" :text (str "unknown tool " name)}] :isError true}))

(defn- answer [dir {:strs [method params]}]
  (case method
    "initialize" {:protocolVersion (get params "protocolVersion" "2024-11-05")
                  :capabilities {:tools {}}
                  :serverInfo {:name "atlas-registry" :version "0.1.0"}}
    "tools/list" {:tools (vec (for [[n t] (sort-by key tools)]
                                {:name n :description (:doc t) :inputSchema (schema t)}))}
    "tools/call" (call dir (get params "name") (get params "arguments" {}))
    "ping" {}
    ::unknown))

(defn serve
  "Answers JSON-RPC requests from `in` on `out`, one per line, about the
   store at `dir`, until `in` ends. Notifications get no answer."
  [dir in out]
  (doseq [line (line-seq in) :when (not (str/blank? line))]
    (let [msg (try (json/parse-string line) (catch Exception _ ::bad))
          reply (fn [m] (.write out (str (json/generate-string (assoc m :jsonrpc "2.0")) "\n")) (.flush out))]
      (cond
        (= ::bad msg) (reply {:id nil :error {:code -32700 :message "parse error"}})
        (not (contains? msg "id")) nil
        :else (let [r (answer dir msg)]
                (if (= ::unknown r)
                  (reply {:id (get msg "id") :error {:code -32601 :message (str "unknown method " (get msg "method"))}})
                  (reply {:id (get msg "id") :result r})))))))

(defn -main [& [dir & more]]
  (when-not (and dir (.isDirectory (java.io.File. ^String dir)))
    (binding [*out* *err*] (println "usage: bb registry-mcp <store-dir> [--prefix p]"))
    (System/exit 1))
  (store/configure! dir (or (second (drop-while #(not= "--prefix" %) more)) "") nil)
  (serve dir (java.io.BufferedReader. *in*) *out*))
