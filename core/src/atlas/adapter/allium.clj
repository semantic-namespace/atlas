(ns atlas.adapter.allium
  "EXPERIMENTAL — emit .allium v3 text (JUXT Allium, https://github.com/juxt/allium)
   from an Atlas registry slice. Sibling of atlas.adapter.yaml-ld / .overarch:
   pure data + clojure.string, same `{compound-id-set props-map}` input, same
   `aspect-selector` machinery (reused from yaml-ld), same `registry->…` +
   `emit!` shape.

   Atlas owns STRUCTURAL truth (identity, kinds, data-flow, the joins allium
   lacks); Allium owns BEHAVIORAL truth (predicate bodies, actor meaning). This
   adapter projects the structural layer and — where the registry declares no
   rule entities — SCAFFOLDS the behavioral layer from data-flow, marking every
   precondition it cannot know as a `-- TODO(/elicit)` hole rather than guessing.
   It never parses, evaluates, or validates an allium predicate; `allium check`
   stays the only engine for that.

   Two modes (see docs/adapter-allium-emit.md):
     :scaffold  — structure + data-flow-inferred rule stubs + holes. One-time
                  bootstrap; a `-- @atlas scaffold` header warns against
                  regenerating over a file that /tend has since owned.
     :project   — (future) full regeneration from :atlas/allium-rule entities
                  carrying opaque allium-syntax strings. Not needed until such
                  entities exist; the banking registry has none, so this file
                  implements :scaffold.

   Mapping (scaffold):
     :atlas/data-schema        -> entity <Name> { field: Unknown … }
     :atlas/structure-component-> external entity <Name> {}   (placeholder)
     :atlas/execution-function -> rule <Name> { when:/requires:(hole)/ensures:/traces: }
     :atlas/interface-endpoint -> surface <Name> { facing/provides:/traces: }
     data-flow  context -> when(params) ; response -> ensures(produced)
     endpoint deps EF   -> surface provides: matches that rule's when: (the join)

   Usage:
     (require '[atlas.adapter.allium :as al]
              '[atlas.adapter.yaml-ld :as yld])
     (al/emit! reg \"/tmp/accounts\" {:select (yld/aspect-selector :domain/auth)
                                       :module-name \"auth\"})
     ;; => writes /tmp/accounts.allium"
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

;; ---------------------------------------------------------------------------
;; Accessors (same convention as yaml-ld / overarch)
;; ---------------------------------------------------------------------------

(defn- dev-id [props] (or (:atlas/dev-id props) (:entity/dev-id props)))
(defn- etype  [props] (:atlas/type props))
(defn- did-str
  "Readable dev-id for a trace comment: :fn/authenticate -> \"fn/authenticate\"."
  [props] (subs (str (dev-id props)) 1))

;; ---------------------------------------------------------------------------
;; Name rendering — atlas qualified keywords -> allium identifiers
;; ---------------------------------------------------------------------------

(defn- pascal
  "Type/rule/surface name: :endpoint/reset-password -> \"ResetPassword\"."
  [kw]
  (->> (str/split (name kw) #"[-_]")
       (map str/capitalize)
       (str/join)))

(defn- ident
  "Field/param name: :operation/success? -> \"success\" (strip qualifier + non-word)."
  [kw]
  (-> (name kw)
      (str/replace #"-" "_")
      (str/replace #"[^A-Za-z0-9_]" "")))

(defn- param-list [ks] (str/join ", " (map ident ks)))

(defn- by-dev-id
  "Index {dev-id props} so endpoint deps can be resolved to their entity type."
  [registry]
  (into {} (for [[_ p] registry] [(dev-id p) p])))

;; ---------------------------------------------------------------------------
;; Blocks — one .allium construct per atlas entity
;; ---------------------------------------------------------------------------

(defn- entity-block
  [props]
  (let [fields (:data-schema/fields props)]
    (str "entity " (pascal (dev-id props)) " {\n"
         ;; String is the reference's own placeholder for an un-refined field
         ;; type; the registry carries names, not allium primitives
         (when (seq fields)
           (str (str/join "\n" (map #(str "  " (ident %) ": String") fields)) "\n"))
         "  -- @atlas types are placeholders (String); /elicit to refine\n"
         "  -- atlas: " (did-str props) "\n"
         "}")))

(defn- external-block
  [props]
  ;; zero fields = allium's documented dependency-inversion placeholder
  (str "external entity " (pascal (dev-id props)) " {}"
       "  -- atlas: " (did-str props)))

(defn- rule-block
  [props]
  (let [nm   (pascal (dev-id props))
        ctx  (:execution-function/context props)
        resp (:execution-function/response props)
        deps (:execution-function/deps props)]
    (str "rule " nm " {\n"
         ;; when: is the ONE clause structure gives us — the trigger interface
         ;; (params from the fn's declared context). requires:/ensures: are
         ;; behavioral truth the registry does NOT hold, so they are holes, not
         ;; guesses. The produced data-keys ride along as an ensures hint so
         ;; /elicit knows the intended outcome.
         "  when: " nm "(" (param-list ctx) ")\n"
         "  -- TODO(/elicit): requires <precondition>\n"
         "  -- TODO(/elicit): ensures <outcome>"
         (when (seq resp) (str " (produces: " (str/join ", " (map ident resp)) ")")) "\n"
         (when (seq deps)
           (str "  -- atlas deps: " (str/join ", " (map #(subs (str %) 1) (sort deps))) "\n"))
         "  -- atlas: " (did-str props) "\n"
         "}")))

(defn- surface-block
  [props idx]
  (let [ctx    (:interface-endpoint/context props)
        deps   (:interface-endpoint/deps props)
        fn-dep (first (filter #(= :atlas/execution-function (etype (idx %))) (sort deps)))
        action (if fn-dep (pascal fn-dep) (pascal (dev-id props)))]
    (str "surface " (pascal (dev-id props)) " {\n"
         "  facing _: Client        -- TODO(/elicit): actor\n"
         ;; provides: matches the dep rule's when: — the surface→rule join
         ;; allium needs (else the trigger is unreachable) and atlas has as an edge
         "  provides: " action "(" (param-list ctx) ")\n"
         "  -- atlas: " (did-str props) "\n"
         "}")))

;; ---------------------------------------------------------------------------
;; :project blocks — full regeneration from entities that carry their allium
;; bodies as OPAQUE strings. Atlas owns identity (dev-id <-> Name), the kind
;; (:atlas/type -> construct), the joins (:rule/implemented-by) and the version
;; history of these strings; it never parses or evaluates them. This is the
;; registry-master "inversion": the .allium file is a projection of atlas.
;; ---------------------------------------------------------------------------

(defn- project-entity-block
  [props]
  (str "entity " (:allium/name props) " {\n"
       (:allium/entity-body props) "\n"
       "  -- atlas: " (did-str props) "\n"
       "}"))

(defn- project-rule-block
  [props]
  (str "rule " (:rule/name props) " {\n"
       "  when: " (:rule/when props) "\n"
       (apply str (map #(str "  requires: " % "\n") (:rule/requires props)))
       (apply str (map #(str "  ensures: " % "\n") (:rule/ensures props)))
       (when-let [impl (:rule/implemented-by props)]
         (str "  -- atlas implemented-by: " (subs (str impl) 1) "\n"))
       "  -- atlas: " (did-str props) "\n"
       "}"))

(defn- project-surface-block
  [props]
  (str "surface " (:allium/name props) " {\n"
       (:allium/surface-body props) "\n"
       "  -- atlas: " (did-str props) "\n"
       "}"))

;; ---------------------------------------------------------------------------
;; Projection
;; ---------------------------------------------------------------------------

(defn registry->allium
  "Project an Atlas `registry` slice into an .allium v3 module (a string).

   opts:
     :select       (fn [compound-id props] -> bool)   required — the module slice
                   (use yld/aspect-selector / aspects-selector)
     :module-name  string, for the header (default \"module\")
     :mode         :scaffold | :project  (default: auto — :project iff the slice
                   contains any :atlas/allium-rule entity)

   :scaffold — structure + data-flow-inferred rule stubs + /elicit holes.
   :project  — verbatim projection of rule/entity/surface bodies carried as
               opaque allium strings (the inversion). No holes.

   Returns {:allium <string> :stats {…} :mode … :holes [ … ]}."
  [registry {:keys [select module-name mode] :or {module-name "module"}}]
  (let [sel     (for [[cid props] registry :when (select cid props)] props)
        by-type (group-by etype sel)
        mode    (or mode (if (seq (:atlas/allium-rule by-type)) :project :scaffold))]
    (if (= mode :project)
      ;; -------- :project — verbatim projection from opaque strings ----------
      (let [entities (sort-by dev-id (:atlas/data-schema by-type))
            rules    (sort-by dev-id (:atlas/allium-rule by-type))
            surfaces (sort-by dev-id (:atlas/interface-endpoint by-type))
            blocks   (concat (map project-entity-block entities)
                             (map project-rule-block rules)
                             (map project-surface-block surfaces))
            header   (str "-- allium: 3\n"
                          "-- @atlas projection (module " module-name ") — generated;\n"
                          "-- source of truth is the atlas registry. Do not hand-edit.\n")]
        {:allium (str header "\n" (str/join "\n\n" blocks) "\n")
         :mode   :project
         :stats  {:entities (count entities) :rules (count rules) :surfaces (count surfaces)}
         :holes  []})
      ;; -------- :scaffold — structural + data-flow stubs + holes ------------
      (let [idx       (by-dev-id registry)
            entities  (sort-by dev-id (:atlas/data-schema by-type))
            externals (sort-by dev-id (:atlas/structure-component by-type))
            rules     (sort-by dev-id (:atlas/execution-function by-type))
            surfaces  (sort-by dev-id (:atlas/interface-endpoint by-type))
            ;; placeholder actor so surface `facing _: Client` resolves
            actor-decl (when (seq surfaces) ["actor Client {}  -- TODO(/elicit): real actor(s)"])
            blocks (concat (map entity-block entities)
                           (map external-block externals)
                           actor-decl
                           (map rule-block rules)
                           (map #(surface-block % idx) surfaces))
            holes  (concat (map #(array-map :rule (dev-id %) :field :requires) rules)
                           (map #(array-map :surface (dev-id %) :field :actor) surfaces))
            header (str "-- allium: 3\n"
                        "-- @atlas scaffold (module " module-name ") — do not regenerate;\n"
                        "-- /tend or /elicit transfers ownership of this file to allium.\n")]
        {:allium (str header "\n" (str/join "\n\n" blocks) "\n")
         :mode   :scaffold
         :stats  {:entities (count entities) :externals (count externals)
                  :rules (count rules) :surfaces (count surfaces)}
         :holes  (vec holes)}))))

(defn emit!
  "Project `registry` and write `<path>.allium`. Returns {:stats … :holes …}."
  [registry path opts]
  (let [{:keys [allium] :as res} (registry->allium registry opts)]
    (io/make-parents (io/file (str path ".allium")))
    (spit (str path ".allium") allium)
    (assoc (dissoc res :allium) :file (str path ".allium"))))
