(ns app.cart
  (:require
   [atlas.registry :as registry]
   [atlas.invariant :as invariant]))

(defn init-registry!
  "Shopping cart semantic registry."
  []

  ;; ===========================================================================
  ;; DATA SCHEMAS
  ;; ===========================================================================

  (registry/register!
   :schema/cart
   :atlas/data-schema
   #{:domain/cart :entity/aggregate}
   {:data-schema/fields [:cart/id :cart/items :cart/user-id :cart/shipping-address :cart/guest?]})

  (registry/register!
   :schema/cart-item
   :atlas/data-schema
   #{:domain/cart :entity/nested}
   {:data-schema/fields [:item/product-id :item/quantity :item/price :item/size :item/color]})

  (registry/register!
   :schema/product
   :atlas/data-schema
   #{:domain/catalog}
   {:data-schema/fields [:product/id :product/name :product/price :product/sizes :product/colors]})

  (registry/register!
   :schema/shipping-address
   :atlas/data-schema
   #{:domain/cart :entity/nested :compliance/pii}
   {:data-schema/fields [:address/line1 :address/line2 :address/city :address/postal-code :address/country]})

  ;; ===========================================================================
  ;; DATA KEYS
  ;; ===========================================================================

  ;; NOTE: Data keys are NOT registered individually. They are inferred from:
  ;; - Function context declarations (what functions consume)
  ;; - Function response declarations (what functions produce)
  ;; - Endpoint context (external inputs from API)
  ;;
  ;; This avoids polluting the registry with dev-ids in aspect sets.

  ;; ===========================================================================
  ;; COMPONENTS (foundation tier)
  ;; ===========================================================================

  (registry/register!
   :component/db
   :atlas/structure-component
   #{:tier/foundation :integration/internal}
   {:structure-component/deps #{}})

  (registry/register!
   :component/session-store
   :atlas/structure-component
   #{:tier/foundation :integration/internal :domain/session}
   {:structure-component/deps #{:component/db}})

  ;; ===========================================================================
  ;; FUNCTIONS (service tier)
  ;; ===========================================================================

  ;; Session management
  (registry/register!
   :fn/get-session
   :atlas/execution-function
   #{:tier/service :domain/session :operation/read}
   {:execution-function/context [:session/id]
    :execution-function/response [:session/data]
    :execution-function/deps #{:component/session-store}})

  (registry/register!
   :fn/save-session
   :atlas/execution-function
   #{:tier/service :domain/session :operation/update}
   {:execution-function/context [:session/id :session/data]
    :execution-function/response [:session/id]
    :execution-function/deps #{:component/session-store}})

  ;; Product lookup
  (registry/register!
   :fn/get-product
   :atlas/execution-function
   #{:tier/service :domain/product :operation/read}
   {:execution-function/context [:product/id]
    :execution-function/response [:product/id :product/name :product/price :product/sizes :product/colors]
    :execution-function/deps #{:component/db}})

  ;; Cart operations
  (registry/register!
   :fn/get-cart
   :atlas/execution-function
   #{:tier/service :domain/cart :operation/read}
   {:execution-function/context [:session/data]
    :execution-function/response [:cart/id :cart/items :cart/user-id :cart/shipping-address :cart/guest?]
    :execution-function/deps #{}})

  (registry/register!
   :fn/add-to-cart
   :atlas/execution-function
   #{:tier/service :domain/cart :operation/create}
   {:execution-function/context [:session/data :item/product-id :item/quantity :item/size :item/color]
    :execution-function/response [:cart/items :item/price]
    :execution-function/deps #{:fn/get-product}
    :atlas/business-pattern :price-lookup})

  (registry/register!
   :fn/remove-from-cart
   :atlas/execution-function
   #{:tier/service :domain/cart :operation/delete :operation/remove-item}
   {:execution-function/context [:session/data :item/product-id :item/size :item/color]
    :execution-function/response [:cart/items]
    :execution-function/deps #{}})

  (registry/register!
   :fn/update-quantity
   :atlas/execution-function
   #{:tier/service :domain/cart :operation/update :operation/modify-item}
   {:execution-function/context [:session/data :item/product-id :item/size :item/color :item/quantity]
    :execution-function/response [:cart/items]
    :execution-function/deps #{}})

  (registry/register!
   :fn/clear-cart
   :atlas/execution-function
   #{:tier/service :domain/cart :operation/delete :operation/clear-all}
   {:execution-function/context [:session/data]
    :execution-function/response [:cart/items]
    :execution-function/deps #{}})

  (registry/register!
   :fn/merge-carts
   :atlas/execution-function
   #{:tier/service :domain/cart :operation/update :operation/merge :compliance/pii :compliance/audited}
   {:execution-function/context [:merge/source-session-id :merge/target-user-id]
    :execution-function/response [:cart/items :cart/user-id]
    :execution-function/deps #{:component/session-store}
    :semantic-namespace/merge-strategy :sum-quantities
    :semantic-namespace/description "Merge source cart into target user's cart. Duplicate items (same product/size/color) have quantities summed."})

  (registry/register!
   :fn/update-shipping-address
   :atlas/execution-function
   #{:tier/service :domain/cart :operation/update :operation/set-address :compliance/pii :compliance/audited}
   {:execution-function/context [:session/data :address/line1 :address/line2 :address/city :address/postal-code :address/country]
    :execution-function/response [:cart/shipping-address]
    :execution-function/deps #{}})

  ;; Cart total calculation (server-side)
  (registry/register!
   :fn/calculate-cart-subtotal
   :atlas/execution-function
   #{:tier/service :domain/cart :operation/calculate :operation/calculate-subtotal :effect/pure}
   {:execution-function/context [:cart/items]
    :execution-function/response [:cart/subtotal]
    :execution-function/deps #{}})

  (registry/register!
   :fn/calculate-cart-tax
   :atlas/execution-function
   #{:tier/service :domain/cart :operation/calculate :operation/calculate-tax :effect/pure}
   {:execution-function/context [:cart/subtotal :address/country]
    :execution-function/response [:cart/tax]
    :execution-function/deps #{}})

  (registry/register!
   :fn/calculate-cart-total
   :atlas/execution-function
   #{:tier/service :domain/cart :operation/calculate :operation/calculate-total :effect/pure}
   {:execution-function/context [:cart/subtotal :cart/tax]
    :execution-function/response [:cart/total]
    :execution-function/deps #{}})

  ;; NOTE: Computed fields (:cart/subtotal, :cart/tax, :cart/total) are NOT
  ;; registered separately. They are declared in function responses and can be
  ;; queried via :operation/calculate aspect on the functions that produce them.

  ;; ===========================================================================
  ;; ENDPOINT (api tier)
  ;; ===========================================================================

  (registry/register!
   :endpoint/cart-graph-api
   :atlas/interface-endpoint
   #{:tier/api :domain/cart
     :protocol/graphql :authorization/required :compliance/pii :compliance/audited}
   {:interface-endpoint/context [:session/id :user/id]
    :interface-endpoint/response [:cart/id :cart/items :cart/subtotal :cart/tax :cart/total
                                  :cart/shipping-address :cart/user-id :cart/guest?
                                  :product/name :product/price :product/sizes :product/colors]
    :interface-endpoint/deps #{:fn/get-session
                               :fn/save-session
                               :fn/get-product
                               :fn/get-cart
                               :fn/add-to-cart
                               :fn/remove-from-cart
                               :fn/update-quantity
                               :fn/clear-cart
                               :fn/merge-carts
                               :fn/update-shipping-address
                               :fn/calculate-cart-subtotal
                               :fn/calculate-cart-tax
                               :fn/calculate-cart-total}}))

(defn check-invariants
  "Validate shopping cart registry."
  []
  (invariant/check-all))

(comment
  (init-registry!)
  (check-invariants)
  (registry/summary))
