;; Phase-1 capture for the grain→atlas import (see docs/adapter-grain-import.md).
;;
;; Runs inside the GRAIN APP's own JVM — it needs the app's classpath, not
;; atlas. From the app checkout (example: grain-todo-list):
;;
;;   clojure -M:dev -e "(load-file \"/path/to/atlas/examples/grain/scripts/capture.clj\")"
;;
;; Edit the `config` map below for a different app: namespaces to require
;; (whose def* macros populate grain's global registries at load time),
;; the schemas namespaces (their defschemas var names classify event vs
;; command vs query schemas), source root, and output path.
;;
;; Output: plain EDN consumed by atlas.adapter.grain/import-catalog!.

(def config
  {:require-nss '[cjbarre.grain-todo-list.service.todo-list-service.commands
                  cjbarre.grain-todo-list.service.todo-list-service.queries
                  cjbarre.grain-todo-list.service.todo-list-service.read-models
                  cjbarre.grain-todo-list.service.todo-list-service.todo-processors
                  cjbarre.grain-todo-list.service.todo-list-service.schemas
                  cjbarre.grain-todo-list.service.user-service.commands
                  cjbarre.grain-todo-list.service.user-service.queries
                  cjbarre.grain-todo-list.service.user-service.read-models
                  cjbarre.grain-todo-list.service.user-service.todo-processors
                  cjbarre.grain-todo-list.service.user-service.schemas]
   :schema-nss '[cjbarre.grain-todo-list.service.todo-list-service.schemas
                 cjbarre.grain-todo-list.service.user-service.schemas]
   ;; defschemas var names whose keys classify the schema registry
   :schema-class-vars '[event-schemas command-schemas query-schemas
                        read-model-schemas common-schemas]
   :source-root "/home/tangrammer/git/grain-todo-list/src"
   :out "/tmp/todo-list-capture.edn"})

(apply require (:require-nss config))
(require '[ai.obney.grain.command-processor-v2.interface :as cp]
         '[ai.obney.grain.query-processor.interface :as qp]
         '[ai.obney.grain.read-model-processor-v2.interface :as rmp]
         '[ai.obney.grain.todo-processor-v2.interface :as tp]
         '[ai.obney.grain.periodic-task.interface :as pt]
         '[ai.obney.grain.schema-util.interface :as su])

(letfn [(readable? [x] (try (let [_ (read-string (pr-str x))] true)
                            (catch Exception _ false)))
        (src [v] (let [m (meta v)] {:file (:file m) :line (:line m)}))
        (entry [[k {:keys [handler-fn reducer-fn] :as opts}]]
          [k (cond-> (into {} (filter (fn [[_ v]] (readable? v))
                                      (dissoc opts :handler-fn :reducer-fn :authorized?)))
               (contains? opts :authorized?) (assoc :authorized?/present? true)
               (var? (or handler-fn reducer-fn)) (assoc :source (src (or handler-fn reducer-fn))))])
        (reg->map [r] (into {} (map entry) r))
        (schema-class [ns-sym var-sym]
          (when-let [v (ns-resolve ns-sym var-sym)] (set (keys @v))))]
  (let [classes (into {} (for [c (:schema-class-vars config)]
                           [(keyword c)
                            (reduce (fn [acc n] (into acc (or (schema-class n c) #{})))
                                    #{} (:schema-nss config))]))
        schemas (into {} (comp (filter (fn [[k _]] (some #(contains? % k) (vals classes))))
                               (filter (fn [[_ v]] (readable? v))))
                      @su/registry*)
        capture {:commands (reg->map (cp/global-command-registry))
                 :queries (reg->map (qp/global-query-registry))
                 :read-models (reg->map (rmp/global-read-model-registry))
                 :processors (reg->map @tp/processor-registry*)
                 :periodic (reg->map @pt/periodic-trigger-registry*)
                 :schema-classes classes
                 :schemas schemas
                 :source-root (:source-root config)}]
    (spit (:out config) (pr-str capture))
    (println "captured to" (:out config) "—"
             (into {} (map (fn [[k v]] [k (count v)]))
                   (select-keys capture [:commands :queries :read-models
                                         :processors :periodic :schemas])))))
