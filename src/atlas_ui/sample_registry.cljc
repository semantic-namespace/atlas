(ns atlas-ui.sample-registry
  "Shared sample registry used by both the UI fallback and the backend dev server.

  Data only: safe to load from both Clojure (JVM) and ClojureScript (JS)."
  (:require [atlas-ui.registry :as ui-reg]
            [atlas.registry :as cid]))

(def shopping-cart-entities
  [{::dev-id :schema/cart
    ::identity #{:atlas/data-schema :domain/cart}
    ::props {:data-schema/fields [:cart/id
                                  :cart/items
                                  :cart/total
                                  :cart/updated-at]}}

   {::dev-id :component/cart-db
    ::identity #{:atlas/structure-component :tier/foundation :domain/cart :storage/db}
    ::props {:structure-component/deps #{}
             :structure-component/provides [:cart/id :cart/items :cart/total :cart/updated-at]}}

   {::dev-id :component/pricing-engine
    ::identity #{:atlas/structure-component :tier/foundation :domain/pricing}
    ::props {:structure-component/deps #{}
             :structure-component/provides [:cart/total]}}

   {::dev-id :fn/get-cart
    ::identity #{:atlas/execution-function :tier/service :domain/cart :operation/read}
    ::props {:execution-function/context [:cart/id]
             :execution-function/response [:cart/items :cart/total]
             :execution-function/deps #{:component/cart-db}}}

   {::dev-id :fn/add-item
    ::identity #{:atlas/execution-function :tier/service :domain/cart :operation/update :effect/write}
    ::props {:execution-function/context [:cart/id :product/id :quantity]
             :execution-function/response [:cart/items :cart/total]
             :execution-function/deps #{:component/cart-db :component/pricing-engine}}}

   {::dev-id :endpoint/get-cart
    ::identity #{:atlas/interface-endpoint :tier/api :domain/cart :http-method/get}
    ::props {:interface-endpoint/context [:cart/id]
             :interface-endpoint/response [:cart/items :cart/total]
             :interface-endpoint/deps #{:fn/get-cart}}}

   {::dev-id :endpoint/add-item
    ::identity #{:atlas/interface-endpoint :tier/api :domain/cart :http-method/post}
    ::props {:interface-endpoint/context [:cart/id :product/id :quantity]
             :interface-endpoint/response [:cart/items :cart/total]
             :interface-endpoint/deps #{:fn/add-item}}}])

(def shopping-cart-registry
  "Minimal shopping cart registry for UI demos."
  (into {}
        (map (fn [{:keys [:atlas-ui.sample-registry/dev-id :atlas-ui.sample-registry/identity :atlas-ui.sample-registry/props]}]
               [identity (assoc props :atlas/dev-id dev-id)])
             shopping-cart-entities)))

(def sample-registry
  "Default sample registry used by the UI fallback and dev server."
  ui-reg/ui-registry)

(def sample-registry-with-ui
  "Shopping cart registry merged with atlas-ui self-description."
  (merge shopping-cart-registry ui-reg/ui-registry))

;; ============================================================================
;; INTERFACE PROTOCOLS
;; ============================================================================

(cid/register!
 :protocol/email-provider
 :atlas/interface-protocol
 #{ :domain/inbox}
  {:atlas/dev-id :protocol/email-provider
   :interface-protocol/functions
   [:fn/read-inbox]})

(cid/register!
 :protocol/oauth2
 :atlas/interface-protocol
 #{ :domain/security}
  {:atlas/dev-id :protocol/oauth2
   :interface-protocol/functions
   [:fn/grant-consent :fn/revoke-consent]})

(cid/register!
 :protocol/wikidata
 :atlas/interface-protocol
 #{ :domain/enrichment}
  {:atlas/dev-id :protocol/wikidata
   :interface-protocol/functions
   [:fn/enrich-service]})

;; ============================================================================
;; IDENTITY ROLES
;; ============================================================================

(cid/register!
 :role/user
 :atlas/identity-role
 #{ :domain/security}
  {:atlas/dev-id :role/user
   :identity-role/description "Inbox owner granting explicit consent"
   :identity-role/data-access [:email/metadata :email/content]
   :identity-role/privacy-constraint :consent-required
   :identity-role/audit-logged true})

;; ============================================================================
;; DATA SCHEMAS
;; ============================================================================

(cid/register!
 :schema/service-identity
 :atlas/data-schema
 #{ :domain/service}
  {:atlas/dev-id :schema/service-identity
   :data-schema/fields
   [:service/id :service/type :service/first-seen :service/last-seen]})

(cid/register!
 :schema/pending-result
 :atlas/data-schema
 #{ :domain/policy}
  {:atlas/dev-id :schema/pending-result
   :data-schema/fields
   [:entity/id :ttl/expires-at :policy/source]})

;; ============================================================================
;; STRUCTURE COMPONENTS
;; ============================================================================

(cid/register!
 :component/inbox-connector
 :atlas/structure-component
 #{ :domain/inbox}
  {:atlas/dev-id :component/inbox-connector
   :structure-component/provides [:capability/read-inbox]
   :structure-component/deps #{:protocol/email-provider :protocol/oauth2}})

(cid/register!
 :component/pending-policy-cache
 :atlas/structure-component
 #{ :domain/policy}
  {:atlas/dev-id :component/pending-policy-cache
   :structure-component/provides [:capability/pending-results]
   :structure-component/consumes [:schema/service-identity]
   :structure-component/emits [:schema/pending-result]})

(cid/register!
 :component/wikidata-enricher
 :atlas/structure-component
 #{ :domain/enrichment}
  {:atlas/dev-id :component/wikidata-enricher
   :structure-component/consumes [:schema/service-identity]
   :structure-component/deps #{:protocol/wikidata}})

;; ============================================================================
;; EXECUTION FUNCTIONS (ASYNC, DISCOVERY-ONLY)
;; ============================================================================

(cid/register!
 :fn/scan-initial
 :atlas/execution-function
 #{ :domain/inbox :intent/user-initial :temporal/async}
  {:atlas/dev-id :fn/scan-initial
   :execution-function/context [:user/id :provider/id]
   :execution-function/response [:scan/completed-at]
   :execution-function/deps #{:component/inbox-connector
                              :component/pending-policy-cache}})

(cid/register!
 :fn/scan-historical
 :atlas/execution-function
 #{ :domain/inbox :intent/user-historical :temporal/async}
  {:atlas/dev-id :fn/scan-historical
   :execution-function/context [:user/id :time/range]
   :execution-function/response [:scan/completed-at]
   :execution-function/deps #{:component/inbox-connector
                              :component/pending-policy-cache}})

(cid/register!
 :fn/scan-partial
 :atlas/execution-function
 #{ :domain/inbox :execution-rule/continuation :temporal/async}
  {:atlas/dev-id :fn/scan-partial
   :execution-function/context [:user/id :scan/last-finished-at]
   :execution-function/response [:scan/completed-at]
   :execution-function/deps #{:component/inbox-connector
                              :component/pending-policy-cache}})

;; ============================================================================
;; GOVERNANCE CONSTRAINTS
;; ============================================================================

(cid/register!
 :constraint/privacy-retention
 :atlas/governance-constraint
 #{ :domain/policy}
  {:atlas/dev-id :constraint/privacy-retention
   :governance-constraint/rationale
   "Data is transient by default and expires unless explicitly promoted"
   :governance-constraint/user-benefit
   "No indefinite retention without consent"
   :governance-constraint/revocable true
   :governance-constraint/recovery-path
   :user/promote-to-dashboard})

(cid/register!
 :constraint/oauth-consent
 :atlas/governance-constraint
 #{ :domain/security}
  {:atlas/dev-id :constraint/oauth-consent
   :governance-constraint/rationale
   "Inbox access requires explicit provider-granted consent"
   :governance-constraint/compliance-requirement :oauth2
   :governance-constraint/revocable true})

;; ============================================================================
;; RISK FAILURE MODES
;; ============================================================================

(cid/register!
 :risk/provider-throttling
 :atlas/risk-failure-mode
 #{ :domain/inbox}
  {:atlas/dev-id :risk/provider-throttling
   :risk-failure-mode/triggered-by :scan/high-volume
   :risk-failure-mode/detection :http-429
   :risk-failure-mode/recovery-path :retry-with-backoff
   :risk-failure-mode/preventable false})

;; ============================================================================
;; SUMMARY
;; ============================================================================

;; - Scans are async, discovery-only, and idempotent
;; - Identity is inbox-derived and policy-governed
;; - Pending results are transient, privacy-enforcing views
;; - Wikidata enriches but never defines entities
;; - User intent and execution rules are strictly separated
