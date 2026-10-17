(ns invariants-smoke
  "Runs atlas's invariants in babashka, with no Datascript: the function
   invariants (atlas.invariant/check-all) and the DSL invariants through
   atlas.invariant.dsl.operators. The DSL definitions of the cart and pet-shop
   examples are read from their files as data (those namespaces require
   atlas.invariant.unified, which needs Datascript). The expected results are
   what the Datalog backend gives on the JVM, checked there by
   atlas.invariant.dsl.operators-test."
  (:require [atlas.invariant :as inv]
            [atlas.invariant.dsl.operators :as ops]
            [atlas.registry :as registry]
            [babashka.classpath :as cp]
            [clojure.java.io :as io]))

;; the example apps live under test/, which bb.edn's :paths leaves out
(cp/add-classpath "test")

(def failures (atom 0))

(defn- check [what ok?]
  (println (if ok? "ok  " "FAIL") what)
  (when-not ok? (swap! failures inc)))

(defn- dsl-defs
  "The (dsl-invariant id level doc when assert) definitions in `file`, as data."
  [file]
  (with-open [r (java.io.PushbackReader. (io/reader file))]
    (vec (for [form (take-while #(not= ::eof %) (repeatedly #(read {:eof ::eof} r)))
               :when (and (seq? form) (= 'def (first form)))
               :let [call (last form)]
               :when (and (seq? call) (= "dsl-invariant" (name (first call))))
               :let [[_ id level doc when assert] call]]
           {:invariant/id id :invariant/level level :invariant/doc doc
            :invariant/when when :invariant/assert assert}))))

(defn- fresh! [app init]
  (registry/reset-all!)
  (doseq [n '[atlas.ontology atlas.ontology.execution-function atlas.ontology.structure-component
              atlas.ontology.interface-endpoint atlas.ontology.data-schema atlas.ontology.interface-protocol]]
    (require n :reload))
  (require app)
  ((resolve init)))

(defn -main [& _]
  (fresh! 'app.cart 'app.cart/init-registry!)
  (let [r (inv/check-all)]
    (check "function invariants run: the cart has no errors and one warning"
           (and (:valid? r) (= [:internal-fn-outputs-consumed] (map :invariant (:warnings r))))))

  (doseq [[app init file] [['app.cart 'app.cart/init-registry! "test/app/cart_invariants.clj"]
                           ['app.pet-shop 'app.pet-shop/init-registry! "test/app/pet_shop_invariants.clj"]]]
    (fresh! app init)
    (let [defs (dsl-defs file)
          r (try (ops/check-invariants defs) (catch Exception e {:thrown (ex-message e)}))]
      (check (str app ": " (count defs) " DSL invariants evaluate without Datascript") (and (< 10 (count defs)) (not (:thrown r))))
      (check (str app ": no violations, as the Datalog backend finds") (empty? (:violations r)))))

  (if (pos? @failures)
    (do (println @failures "failed") (System/exit 1))
    (println "OK")))
