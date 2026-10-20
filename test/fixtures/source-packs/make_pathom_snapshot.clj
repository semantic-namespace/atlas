;; Writes snapshots/pathom.edn from real Pathom 3 and Pathom 2 environments
;; built from the fixture's resolvers. Run from the atlas checkout:
;;
;;   clojure -Sdeps '{:paths ["core/src" "test/fixtures/source-packs/src"]
;;                    :deps {com.wsscode/pathom3 {:mvn/version "2023.08.22-alpha"}
;;                           com.wsscode/pathom {:mvn/version "2.4.0"}}}' \
;;           -M test/fixtures/source-packs/make_pathom_snapshot.clj
;;
;; order-buyer is left out on purpose: defined in the code, never registered.
;; list-price is built with pco/resolver: registered, but in no defresolver.
(require '[atlas.source.pack.pathom :as pathom]
         '[com.wsscode.pathom3.connect.indexes :as pci]
         '[com.wsscode.pathom.connect :as pc]
         'shop.resolvers 'shop.resolvers2 'shop.wrapped)

(def env
  (pci/register [shop.resolvers/user-by-email shop.resolvers/order-total shop.resolvers/user-orders
                 shop.resolvers/user-discount shop.resolvers/place-order! shop.resolvers/user-by-id
                 shop.resolvers/seed shop.resolvers/list-price shop.wrapped/product-price]))

(def indexes-p2 (pc/register {} [shop.resolvers2/product-name]))

(println (pathom/snapshot! [env indexes-p2] {:file "test/fixtures/source-packs/snapshots/pathom.edn"}))
