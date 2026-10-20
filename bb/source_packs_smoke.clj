(ns source-packs-smoke
  "Runs atlas.source.index with the schema and Pathom packs over the shop
   fixture (test/fixtures/source-packs): clojure.spec s/def calls and a
   malli registry, Pathom 3 and 2 resolvers, plain annotated defns, and EDN
   files that give the functions' contracts and a data key's privacy class."
  (:require [atlas.source.hover :as hover]
            [atlas.source.index :as index]
            [babashka.fs :as fs]
            [clojure.edn :as edn]
            [clojure.string :as str]))

(def failures (atom 0))

(defn- check [what ok?]
  (println (if ok? "ok  " "FAIL") what)
  (when-not ok? (swap! failures inc)))

(def root "test/fixtures/source-packs")

(def job-pack
  "A project's own pack: the shop's defjob registry. Enabled by its
   qualified symbol under :packs; a job reads the data keys :job/reads lists."
  {:pack/id :jobs
   :pack/kinds #{:job}
   :pack/vocabulary #{"job"}
   :pack/dataflow {:consumes [:job/reads]}
   :pack/find (fn [analysis _opts]
                (for [u (:var-usages analysis)
                      :when (and (= 'shop.scheduler (:to u)) (= 'defjob (:name u)))]
                  (select-keys u [:filename :row :name-row :name-col])))
   :pack/read (fn [sites {:keys [form-at own-ns]} _opts]
                (vec (for [{:keys [row name-row name-col]} sites
                           :let [[_ nm opts] (form-at name-row name-col)]]
                       {:row row :kind :job :dev-id (keyword own-ns (name nm)) :entry opts})))
   :pack/describe (fn [entry] (when-let [s (:job/schedule entry)] (str "job  " s)))})

(def pathom-again
  "A pack claiming a kind the Pathom pack gives."
  {:pack/id :pathom-again :pack/kinds #{:pathom} :pack/find (fn [_ _] []) :pack/read (fn [_ _ _] [])})

(defn- pathom-part
  "An entry's :pathom/* keys: :shop.resolvers/seed is also, deliberately,
   an integrant key."
  [entry]
  (into {} (filter #(= "pathom" (namespace (key %)))) entry))

(def broken-pack
  "A project's pack that fails on every file it finds."
  {:pack/id :broken :pack/kinds #{:broken}
   :pack/find (fn [analysis _] (for [u (:var-usages analysis) :when (= 'defjob (:name u))]
                                 (select-keys u [:filename :row :name-row :name-col])))
   :pack/read (fn [_ _ _] (throw (ex-info "boom" {})))})

(defn- problems [idx problem]
  (filter #(= problem (:problem %)) (index/check idx)))

(defn -main [& _]
  (let [with-runtime (assoc (edn/read-string (slurp (str root "/atlas-source.edn"))) :root root)
        ;; static reading alone: the checks below until "runtime snapshot"
        cfg (update-in with-runtime [:packs :pathom] dissoc :snapshot)
        idx (index/build cfg)
        email (index/data-key idx :user/email)]
    (check "every s/def and registry entry is read"
           (= {:spec {:read 7 :not-read 0} :malli {:read 2 :not-read 0}}
              (select-keys (:packs (index/summary idx)) [:spec :malli])))
    (check "every entity a pack gives needs no :atlas/type"
           (empty? (problems idx :entity-without-type)))
    (check "no errors: a data key with only a privacy entry needs no :atlas/type"
           (not-any? #(= :error (:severity %)) (index/check idx)))

    (check "a spec merges with what EDN says about the same key"
           (= {:spec/form "(s/and string? #(re-find #\"@\" %1))" :data/privacy :pii}
              (:entry (index/where idx :user/email))))
    (check "its primary definition is the s/def"
           (= {:file "src/shop/spec.clj" :line 5 :pack :spec} (:definition (index/where idx :user/email))))
    (check "data: shape, other props, the schemas it is a key of, producers"
           (and (= [:spec] (map :pack (:shape email)))
                (= {:data/privacy :pii} (:props email))
                (= [:shop.spec/user] (map :dev-id (:member-of email)))
                (= [:fn/register-user :shop.resolvers/order-buyer :shop.resolvers/user-by-id] (map :dev-id (:producers email)))))
    (check "a .cljc s/def is read with its reader conditional as :clj"
           (= "decimal?" (:form (first (:shape (index/data-key idx :order/total))))))
    (check "a registry's unqualified key (:map, overriding malli's own) is not a data key"
           (nil? (index/where idx :map)))
    (check "a malli registry entry, with its :map keys"
           (= {:malli/form "[:vector [:map [:product/id :uuid] [:qty :int]]]"
               :malli/keys [:product/id] :malli/unqualified-keys [:qty]}
              (:entry (index/where idx :order/lines))))

    (check "every defresolver and defmutation is read, Pathom 3 and 2"
           (= {:read 10 :not-read 1} (:pathom (:packs (index/summary idx)))))
    (check "the one not read is the pco/defresolver inside the wrapper macro, whose name is ~name"
           (= [[:pathom "src/shop/connect.clj" 7]] (map (juxt :pack :file :line) (problems idx :pack-form-not-read))))
    (check "an option naming a def in the same file is read through it"
           (= [:user/id :user/name :user/email] (get-in (index/where idx :shop.resolvers/user-by-id) [:entry :pathom/output])))
    (check "and inside a join"
           (= [:order/buyer :user/id :user/name :user/email]
              (get-in (index/where idx :shop.resolvers/order-buyer) [:entry :pathom/output])))
    (check "a project's own wrapper macro, named in :resolver-macros"
           (= #:pathom{:op :resolver :input [:product/id] :output [:product/price] :inferred #{:input :output}}
              (:entry (index/where idx :shop.wrapped/product-price))))
    (check "an option written as code is not guessed, and check says so"
           (and (= #:pathom{:op :resolver :not-read #{:output}} (pathom-part (:entry (index/where idx :shop.resolvers/seed))))
                (= [[:shop.resolvers/seed [:output]]] (map (juxt :dev-id :options) (problems idx :operation-option-not-read)))))
    (check "an unproduced input names the outputs that couldn't be read, which may produce it"
           (every? #(= [:shop.resolvers/seed] (:unread-outputs %)) (problems idx :input-without-producer)))
    (check "declared input and output"
           (= #:pathom{:op :resolver :input [:user/email] :output [:user/id :user/name]}
              (:entry (index/where idx :shop.resolvers/user-by-email))))
    (check "inferred input from the destructuring, output from the returned map"
           (= #:pathom{:op :resolver :input [:order/lines] :output [:order/total] :inferred #{:input :output}}
              (:entry (index/where idx :shop.resolvers/order-total))))
    (check "a join's key and the keys inside it are outputs; env is not the input"
           (= #:pathom{:op :resolver :input [:user/id] :output [:user/orders :order/id :order/total] :inferred #{:input}}
              (:entry (index/where idx :shop.resolvers/user-orders))))
    (check "a mutation's params are what it consumes"
           (= #:pathom{:op :mutation :params [:user/id :order/lines] :output [:order/id]}
              (:entry (index/where idx :shop.resolvers/place-order!))))
    (check "Pathom 2: a set input"
           (= [:product/id] (get-in (index/where idx :shop.resolvers2/product-name) [:entry :pathom/input])))
    (check "one data key across three registries: an EDN contract and a resolver produce it, a resolver consumes it, a spec shapes it"
           (and (= [:fn/register-user :shop.resolvers/order-buyer :shop.resolvers/user-by-id] (map :dev-id (:producers email)))
                (= [:fn/register-user :shop.resolvers/user-by-email] (map :dev-id (:consumers email)))))
    (check "resolver outputs count as producers, next to function contracts"
           (= [:fn/place-order :shop.resolvers/order-total :shop.resolvers/user-orders]
              (map :dev-id (:producers (index/data-key idx :order/total)))))
    (check "a resolver input nothing produces"
           (= [[:shop.resolvers/order-total [:order/lines]]
               [:shop.resolvers/user-discount [:user/loyalty-tier]]
               [:shop.resolvers2/product-name [:product/id]]
               [:shop.wrapped/product-price [:product/id]]]
              (map (juxt :dev-id :keys) (problems idx :input-without-producer))))
    (check "hover: a resolver's inputs and outputs"
           (str/includes? (:eldoc (hover/lookup idx :shop.resolvers/order-total))
                          "resolver  [:order/lines] → [:order/total]  (inferred: input, output)"))
    (check ":dataflow in atlas-source.edn makes a project's own keys consume data"
           (let [own (index/build (-> cfg (update :registry conj "registry-jobs") (assoc :dataflow {:consumes [:job/reads]})))]
             (and (= [:fn/register-user :job/nightly-digest :shop.resolvers/user-by-email]
                     (map :dev-id (:consumers (index/data-key own :user/email))))
                  (empty? (:consumers (index/data-key idx :order/total))))))

    (let [own (index/build (assoc-in cfg [:packs 'source-packs-smoke/job-pack] {}))]
      (check "a project's own pack, named by its symbol, reads its own macro"
             (= {:job/reads [:user/email :order/total] :job/schedule "0 3 * * *"}
                (:entry (index/where own :shop.jobs/nightly-digest))))
      (check "its :pack/dataflow makes the job a consumer next to functions and resolvers"
             (= [:fn/register-user :shop.jobs/nightly-digest :shop.resolvers/user-by-email]
                (map :dev-id (:consumers (index/data-key own :user/email)))))
      (check "its entities need no :atlas/type, and it counts its reads"
             (and (empty? (problems own :entity-without-type))
                  (= {:read 1 :not-read 0} (:job (:packs (index/summary own))))))
      (check "the hover uses its :pack/describe"
             (str/includes? (:eldoc (hover/lookup own :shop.jobs/nightly-digest)) "job  0 3 * * *")))
    (check "two packs giving the same kind are refused"
           (= {:kinds [:pathom]}
              (try (index/build (assoc-in cfg [:packs 'source-packs-smoke/pathom-again] {})) nil
                   (catch clojure.lang.ExceptionInfo e (ex-data e)))))

    ;; ---- integrant: resources/*.edn and shop.system
    (let [ig #(:entry (index/where idx %))
          ig-part #(into {} (filter (fn [[k _]] (= "integrant" (namespace k)))) (ig %))]
      (check "every config component, defmethod and derive is read"
             (= {:read 13 :not-read 0} (:integrant (:packs (index/summary idx)))))
      (check "a component's refs, from #ig/ref, nested in its config"
             (= [:shop/db :shop/mailer :shop/metrics] (:integrant/refs (ig :shop/handler))))
      (check "variant files configuring one key: one entity, every file listed, :varies"
             (= #:integrant{:config-in ["resources/system.edn:2" "resources/system.test.edn:2"]
                            :env ["DATABASE_URL"] :varies true :init-key true :halt-key! true}
                (ig-part :shop/db)))
      (check "a composite key, named by the keyword no other key uses, deriving from the rest"
             (= [:shop/http-server] (:integrant/derives (ig :shop.http/main))))
      (check "a duct profile's components"
             (= [:duct.profile/base] (:integrant/profiles (ig :shop/cache))))
      (check "a ::keyword defmethod, with the file's namespace"
             (true? (:integrant/init-key (ig :shop.system/const))))
      (check "a derive call in a file that uses integrant"
             (= [:shop.system/const] (:integrant/isa (ig :shop/base-path))))
      (check "a ref to a key no config defines"
             (= [[:shop/handler [:shop/metrics]]] (map (juxt :dev-id :keys) (problems idx :ref-to-undefined))))
      (check ":provided lists keys something else adds"
             (empty? (problems (index/build (assoc-in cfg [:packs :integrant :provided] [:shop/metrics])) :ref-to-undefined)))
      (check "an init-key for a key no config has; a composite's parent and a derived-from key count as configured"
             (= [:shop.system/unused] (map :dev-id (problems idx :init-key-without-config))))
      (check "a component an EDN contract depends on"
             (= [:shop/mailer] (map :dev-id (:depends-on (index/dependencies idx :fn/register-user)))))
      (check "a spec under a component's key is its shape, not a second thing: no collision, still a dependency"
             (and (= "map?" (:spec/form (ig :shop/mailer)))
                  (not-any? #(= :shop/mailer (:dev-id %)) (problems idx :id-in-several-packs))))
      (check "one keyword two packs declare things under: a collision"
             (= [[:shop.resolvers/seed [:integrant :pathom]]] (map (juxt :dev-id :packs) (problems idx :id-in-several-packs))))
      (check "who refers to a component"
             (= [:shop.http/main] (map :dev-id (:referenced-by (index/uses idx :shop/handler)))))
      (check "hover: a component's methods, configs and env"
             (str/includes? (:eldoc (hover/lookup idx :shop/db)) "component  refs 0  ·  halt-key! init-key  ·  in 2 configs, varies  ·  env DATABASE_URL")))
    (check "a :refer of a pack's macro is not read as a form"
           (not-any? #(= "src/shop/resolvers.clj" (:file %)) (problems idx :pack-form-not-read)))
    (check "a pack that throws costs its files, not the index"
           (let [b (index/build (assoc-in cfg [:packs 'source-packs-smoke/broken-pack] {}))]
             (some #(and (= :broken (:pack %)) (str/includes? (:reason %) "boom")) (problems b :pack-form-not-read))))

    ;; ---- diagnostics: what an editor underlines, per file
    (let [diags #(set (map (juxt :line :col :end-col :severity :problem) (hover/diagnostics idx %)))]
      (check "diagnostics: a key used outside the contract, at the key in the code"
             (= #{[10 49 64 "note" "keys-outside-contract"]} (diags "src/shop/handlers.clj")))
      (check "diagnostics: an absolute path, as editors send"
             (= (diags "src/shop/handlers.clj")
                (diags (.getCanonicalPath (java.io.File. (str root "/src/shop/handlers.clj"))))))
      (check "diagnostics: a key only destructured, at its name, within its own form"
             (contains? (diags "src/shop/resolvers.clj") [13 45 50 "note" "input-without-producer"]))
      (check "diagnostics: a dev-id written as the defining form's symbol"
             (contains? (diags "src/shop/resolvers.clj") [48 18 22 "warning" "id-in-several-packs"]))
      (check "diagnostics: severities follow check's"
             (contains? (diags "resources/system.edn") [10 84 97 "warning" "ref-to-undefined"]))
      (check "diagnostics: a file with nothing to report"
             (empty? (diags "src/shop/order.cljc"))))

    (check "data keys are not dependencies"
           (empty? (:depends-on (index/dependencies idx :fn/place-order))))
    (check "a typo in s/keys: a key with no spec of its own"
           (= [{:dev-id :shop.spec/user :keys [:user/emial]}]
              (map #(select-keys % [:dev-id :keys]) (problems idx :schema-key-without-shape))))
    (check "unqualified malli keys, which dataflow can't follow"
           (= [[:order/lines [:qty]]] (map (juxt :dev-id :keys) (problems idx :unqualified-schema-keys))))
    (check "a contract key no schema shapes"
           (= [[:fn/register-user [:user/phone]]
               [:shop.resolvers/order-buyer [:order/buyer]]
               [:shop.resolvers/user-discount [:user/loyalty-tier]]
               [:shop.resolvers/user-orders [:user/orders]]
               [:shop.resolvers2/product-name [:product/name]]
               [:shop.wrapped/product-price [:product/price]]]
              (map (juxt :dev-id :keys) (problems idx :data-key-without-schema))))
    (check "code reading a spec'd key that no contract names"
           (= [[:fn/place-order [:order/discount]]] (map (juxt :dev-id :keys) (problems idx :keys-outside-contract))))

    (let [h (hover/lookup idx :user/email)]
      (check "hover: a shaped key is shown as a data key, spec and privacy on the eldoc line"
             (and (= "data-key" (:kind h))
                  (str/includes? (:eldoc h) "spec (s/and string?")
                  (str/includes? (:eldoc h) ":data/privacy :pii")))
      (check "hover: M-. goes to the s/def first"
             (str/ends-with? (:file (first (:definitions h))) "src/shop/spec.clj")))

    (let [plain (index/build (dissoc cfg :packs))]
      (check "the pack is opt-in: without :packs no s/def becomes an entity"
             (and (nil? (index/where plain :shop.spec/user))
                  (empty? (problems plain :data-key-without-schema)))))

    ;; ---- runtime snapshot: snapshots/pathom.edn, written from real Pathom 3
    ;; and 2 environments by make_pathom_snapshot.clj
    (let [rt (index/build with-runtime)]
      (check "the snapshot is read: 10 operations the running environments registered"
             (= 10 (get-in (index/summary rt) [:pack-snapshots :pathom :entities])))
      (check "code and the running environments agree on every operation both have"
             (empty? (problems rt :runtime-disagrees)))
      (check "an output written as code is filled from the running environment"
             (= #:pathom{:op :resolver :output [:user/loyalty-tier] :from-runtime #{:output}}
                (pathom-part (:entry (index/where rt :shop.resolvers/seed)))))
      (check "so the input it produces is no longer unproduced, and nothing is left unread"
             (and (not-any? #(= :shop.resolvers/user-discount (:dev-id %)) (problems rt :input-without-producer))
                  (empty? (problems rt :operation-option-not-read))
                  (not-any? :unread-outputs (problems rt :input-without-producer))))
      (check "a resolver built as a value: only the running environment has it"
             (and (= [:shop.resolvers/list-price] (map :dev-id (problems rt :runtime-only)))
                  (= [:shop.resolvers/list-price] (map :dev-id (:producers (index/data-key rt :product/list-price))))))
      (check "a resolver defined in the code and never registered"
             (= [:shop.resolvers/order-buyer] (map :dev-id (problems rt :not-in-runtime))))
      (check "where lists the running environment as a source"
             (some :runtime-as-of (:sources (index/where rt :shop.resolvers/user-by-email))))
      (check "the hover says where an entity only the running environment has comes from"
             (str/includes? (:describe (hover/lookup rt :shop.resolvers/list-price)) "running process: snapshot")))

    ;; ---- code edited after the snapshot, and a snapshot that disagrees with
    ;; unchanged code: on a copy, with mtimes set
    (let [dir (fs/create-temp-dir {:prefix "atlas-source-packs"})
          _ (fs/copy-tree root dir)
          snap (str (fs/path dir "snapshots" "pathom.edn"))
          past (- (System/currentTimeMillis) 60000)
          data (edn/read-string (slurp snap))]
      (try
        (doseq [f (fs/glob dir "**")] (.setLastModified (fs/file f) past))
        ;; the snapshot was taken now, after every file; one entity disagrees
        (spit snap (pr-str (-> data
                               (assoc :written (+ past 30000))
                               (update-in [:entities :shop.wrapped/product-price :pathom/output] conj :product/currency))))
        ;; then the code changes: user-by-email now also outputs :user/phone
        (let [f (str (fs/path dir "src" "shop" "resolvers.clj"))]
          (spit f (str/replace-first (slurp f) "::pco/output [:user/id :user/name]}" "::pco/output [:user/id :user/name :user/phone]}"))
          (.setLastModified (fs/file f) (System/currentTimeMillis)))
        (let [idx (index/build (assoc with-runtime :root (str dir)))
              ds (into {} (map (juxt :dev-id identity)) (problems idx :runtime-disagrees))]
          (check "a disagreement names the slot and which side has each key"
                 (and (= {:pathom/output {:only-in-code [:user/phone]}} (get-in ds [:shop.resolvers/user-by-email :diff]))
                      (= {:pathom/output {:only-at-runtime [:product/currency]}} (get-in ds [:shop.wrapped/product-price :diff]))))
          (check "and says when the file changed after the snapshot, so the snapshot may be stale"
                 (and (true? (get-in ds [:shop.resolvers/user-by-email :changed-since-snapshot]))
                      (false? (get-in ds [:shop.wrapped/product-price :changed-since-snapshot]))))
          (check "a disagreement is a warning"
                 (every? #(= :warning (:severity %)) (vals ds))))
        (finally (fs/delete-tree dir))))

    (if (zero? @failures)
      (println "source-packs-smoke: all checks passed")
      (do (println "source-packs-smoke:" @failures "failed") (System/exit 1)))))
