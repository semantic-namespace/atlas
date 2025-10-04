(ns app.calendar-availability
  (:require
   [atlas.registry :as registry]
   [atlas.ontology :as ontology]
   [atlas.invariant :as invariant]))

(defn init-registry!
  "Complete semantic registry for calendar availability system with OAuth, admin, and calendar features."
  []

  ;; ==========================================================================
  ;; DATA SCHEMAS
  ;; ==========================================================================

  (registry/register!
   :schema/users
   :atlas/data-schema
   #{:domain/users}
   {:data-schema/fields [:user/id :user/email :user/name :user/phone
                         :user/language :user/languages :user/picture
                         :user/is-admin? :user/gcal-refresh-token
                         :user/has-calendar-access? :user/has-refresh-token?]})

  (registry/register!
   :schema/oauth-credentials
   :atlas/data-schema
   #{:domain/auth :protocol/oauth}
   {:data-schema/fields [:oauth/access-token :oauth/refresh-token
                         :oauth/authorization-url :oauth/state
                         :oauth/code :oauth/redirect-uri :auth/id-token
                         :auth/client-id]})

  (registry/register!
   :schema/calendar-events
   :atlas/data-schema
   #{:domain/scheduling}
   {:data-schema/fields [:calendar/events :calendar/event-id
                         :calendar/start-time :calendar/end-time
                         :calendar/summary]})

  (registry/register!
   :schema/scheduling-query
   :atlas/data-schema
   #{:domain/scheduling :dataflow/input}
   {:data-schema/fields [:query/date :query/language :query/start-date
                         :query/end-date :query/start-time :query/end-time]})

  (registry/register!
   :schema/availability
   :atlas/data-schema
   #{:domain/scheduling :dataflow/output}
   {:data-schema/fields [:availability/users]})

  (registry/register!
   :schema/admin-operations
   :atlas/data-schema
   #{:domain/admin}
   {:data-schema/fields [:admin/requester-email :operation/success?]})

  (registry/register!
   :schema/http-responses
   :atlas/data-schema
   #{:domain/http}
   {:data-schema/fields [:http/html :http/redirect-url :http/status
                         :health/status :user/session-token]})

  ;; NOTE: Data keys are NOT registered individually. They are inferred from:
  ;; - Function context declarations (what functions consume)
  ;; - Function response declarations (what functions produce)
  ;; - Endpoint context (external inputs from API)
  ;;
  ;; This avoids polluting the registry with dev-ids in aspect sets.

  ;; ==========================================================================
  ;; PROTOCOLS
  ;; ==========================================================================

  (registry/register!
   :protocol/user-repository
   :atlas/interface-protocol
   #{:protocol/user-repository}
   {:atlas/dev-id :protocol/user-repository
    :interface-protocol/functions [:protocol.user-repository/find-users-by-language
                                   :protocol.user-repository/get-user
                                   :protocol.user-repository/create-user
                                   :protocol.user-repository/update-user
                                   :protocol.user-repository/delete-user
                                   :protocol.user-repository/list-users
                                   :protocol.user-repository/check-admin]})

  (registry/register!
   :protocol/oauth
   :atlas/interface-protocol
   #{:protocol/oauth}
   {:atlas/dev-id :protocol/oauth
    :interface-protocol/functions [:protocol.oauth/refresh-token
                                   :protocol.oauth/generate-auth-url
                                   :protocol.oauth/exchange-code
                                   :protocol.oauth/verify-signin-token]})

  (registry/register!
   :protocol/calendar-availability
   :atlas/interface-protocol
   #{:protocol/calendar-availability}
   {:atlas/dev-id :protocol/calendar-availability
    :interface-protocol/functions [:protocol.calendar-availability/check-availability
                                   :protocol.calendar-availability/list-events
                                   :protocol.calendar-availability/get-events-in-range]})

  (registry/register!
   :protocol/credential-storage
   :atlas/interface-protocol
   #{:protocol/credential-storage}
   {:atlas/dev-id :protocol/credential-storage
    :interface-protocol/functions [:protocol.credential/save
                                   :protocol.credential/retrieve
                                   :protocol.credential/has-refresh-token]})

  ;; ==========================================================================
  ;; COMPONENTS (Foundation Tier)
  ;; ==========================================================================

  (registry/register!
   :component/db
   :atlas/structure-component
   #{:tier/foundation :domain/users :protocol/user-repository :protocol/credential-storage}
   {:atlas/dev-id :component/db
    :structure-component/deps #{}})

  (registry/register!
   :component/google-oauth
   :atlas/structure-component
   #{:tier/foundation :domain/google :integration/external :service/authentication :protocol/oauth}
   {:atlas/dev-id :component/google-oauth
    :structure-component/deps #{}})

  (registry/register!
   :component/gcal-client
   :atlas/structure-component
   #{:tier/foundation :domain/google :integration/external :service/calendar-api :protocol/calendar-availability}
   {:atlas/dev-id :component/gcal-client
    :structure-component/deps #{:component/google-oauth}})

  ;; ==========================================================================
  ;; FUNCTIONS - AUTHENTICATION & AUTHORIZATION (Service Tier)
  ;; ==========================================================================

  (registry/register!
   :fn/verify-google-signin-token
   :atlas/execution-function
   #{:tier/service :domain/auth :integration/external}
   {:atlas/dev-id :fn/verify-google-signin-token
    :execution-function/context [:auth/id-token :auth/client-id]
    :execution-function/response [:user/email :user/name :user/picture]
    :execution-function/deps #{:component/google-oauth}})

  (registry/register!
   :fn/check-calendar-access-granted
   :atlas/execution-function
   #{:tier/service :domain/auth :domain/google}
   {:atlas/dev-id :fn/check-calendar-access-granted
    :execution-function/context [:user/email]
    :execution-function/response [:user/has-calendar-access?]
    :execution-function/deps #{:component/db}})

  (registry/register!
   :fn/get-or-create-user
   :atlas/execution-function
   #{:tier/service :domain/users :effect/write}
   {:atlas/dev-id :fn/get-or-create-user
    :execution-function/context [:user/email :user/name :user/picture]
    :execution-function/response [:user/id :user/email :user/name]
    :execution-function/deps #{:component/db}})

  (registry/register!
   :fn/check-user-is-admin
   :atlas/execution-function
   #{:tier/service :domain/auth}
   {:atlas/dev-id :fn/check-user-is-admin
    :execution-function/context [:user/email]
    :execution-function/response [:user/is-admin?]
    :execution-function/deps #{:component/db}})

  ;; ==========================================================================
  ;; FUNCTIONS - OAUTH FLOW (Service Tier)
  ;; ==========================================================================

  (registry/register!
   :fn/generate-oauth-authorization-url
   :atlas/execution-function
   #{:tier/service :domain/google :protocol/oauth :operation/generate-url}
   {:atlas/dev-id :fn/generate-oauth-authorization-url
    :execution-function/context [:oauth/redirect-uri :user/email]
    :execution-function/response [:oauth/authorization-url :oauth/state]
    :execution-function/deps #{:component/google-oauth}})

  (registry/register!
   :fn/exchange-oauth-code-for-tokens
   :atlas/execution-function
   #{:tier/service :domain/google :integration/external :protocol/oauth :effect/write}
   {:atlas/dev-id :fn/exchange-oauth-code-for-tokens
    :execution-function/context [:oauth/code :user/email]
    :execution-function/response [:oauth/access-token :oauth/refresh-token]
    :execution-function/deps #{:component/google-oauth :component/db}})

  (registry/register!
   :fn/get-user-credential
   :atlas/execution-function
   #{:tier/service :domain/users :domain/auth :operation/read :pattern/oauth-token-refresh :failure-mode/expired-refresh-token}
   {:atlas/dev-id :fn/get-user-credential
    :execution-function/context [:user/email]
    :execution-function/response [:oauth/access-token :oauth/refresh-token]
    :execution-function/deps #{:component/db}})

  (registry/register!
   :fn/check-user-has-refresh-token
   :atlas/execution-function
   #{:tier/service :domain/users :domain/auth :operation/check}
   {:atlas/dev-id :fn/check-user-has-refresh-token
    :execution-function/context [:user/email]
    :execution-function/response [:user/has-refresh-token?]
    :execution-function/deps #{:component/db}})

  (registry/register!
   :fn/refresh-oauth-token
   :atlas/execution-function
   #{:tier/service :domain/google :integration/external :protocol/oauth :pattern/oauth-token-refresh :failure-mode/expired-refresh-token}
   {:atlas/dev-id :fn/refresh-oauth-token
    :execution-function/context [:user/gcal-refresh-token]
    :execution-function/response [:oauth/access-token]
    :execution-function/deps #{:component/google-oauth}})

  ;; ==========================================================================
  ;; FUNCTIONS - USER CRUD (Service Tier)
  ;; ==========================================================================

  (registry/register!
   :fn/find-users-by-language
   :atlas/execution-function
   #{:tier/service :domain/users :pattern/language-based-routing :value/language-filtered-matching}
   {:atlas/dev-id :fn/find-users-by-language
    :execution-function/context [:query/language]
    :execution-function/response [:user/email :user/gcal-refresh-token]
    :execution-function/deps #{:component/db}})

  (registry/register!
   :fn/create-authorized-user
   :atlas/execution-function
   #{:tier/service :domain/users :domain/admin :effect/write :operation/create :constraint/admin-only-user-management}
   {:atlas/dev-id :fn/create-authorized-user
    :execution-function/context [:user/email :user/name :user/phone :user/languages]
    :execution-function/response [:user/id]
    :execution-function/deps #{:component/db}})

  (registry/register!
   :fn/get-authorized-user
   :atlas/execution-function
   #{:tier/service :domain/users :domain/admin :operation/read :constraint/admin-only-user-management}
   {:atlas/dev-id :fn/get-authorized-user
    :execution-function/context [:user/email]
    :execution-function/response [:user/id :user/email :user/name :user/phone :user/languages]
    :execution-function/deps #{:component/db}})

  (registry/register!
   :fn/update-authorized-user
   :atlas/execution-function
   #{:tier/service :domain/users :domain/admin :effect/write :operation/update :constraint/admin-only-user-management}
   {:atlas/dev-id :fn/update-authorized-user
    :execution-function/context [:user/email :user/name :user/phone :user/languages]
    :execution-function/response [:user/id]
    :execution-function/deps #{:component/db}})

  (registry/register!
   :fn/delete-authorized-user
   :atlas/execution-function
   #{:tier/service :domain/users :domain/admin :effect/write :operation/delete :constraint/admin-only-user-management}
   {:atlas/dev-id :fn/delete-authorized-user
    :execution-function/context [:user/email]
    :execution-function/response [:operation/success?]
    :execution-function/deps #{:component/db}})

  (registry/register!
   :fn/list-all-authorized-users
   :atlas/execution-function
   #{:tier/service :domain/users :domain/admin :operation/list :constraint/admin-only-user-management}
   {:atlas/dev-id :fn/list-all-authorized-users
    :execution-function/context []
    :execution-function/response [:users/list]
    :execution-function/deps #{:component/db}})

  ;; ==========================================================================
  ;; FUNCTIONS - CALENDAR OPERATIONS (Service Tier)
  ;; ==========================================================================

  (registry/register!
   :fn/list-user-calendar-events
   :atlas/execution-function
   #{:tier/service :domain/scheduling :integration/external :operation/list :constraint/read-only-calendar-access :failure-mode/network-timeout-google-api}
   {:atlas/dev-id :fn/list-user-calendar-events
    :execution-function/context [:user/email :query/start-date :query/start-time :query/end-date :query/end-time]
    :execution-function/response [:calendar/events]
    :execution-function/deps #{:component/gcal-client}})

  (registry/register!
   :fn/get-calendar-events-in-range
   :atlas/execution-function
   #{:tier/service :domain/scheduling :integration/external :operation/query :constraint/read-only-calendar-access}
   {:atlas/dev-id :fn/get-calendar-events-in-range
    :execution-function/context [:user/email :query/date :query/start-time :query/end-time]
    :execution-function/response [:calendar/events]
    :execution-function/deps #{:component/gcal-client}})

  (registry/register!
   :fn/check-user-is-free
   :atlas/execution-function
   #{:tier/service :domain/scheduling :effect/pure :operation/check}
   {:atlas/dev-id :fn/check-user-is-free
    :execution-function/context [:calendar/events :query/start-time :query/end-time]
    :execution-function/response [:scheduling/is-free?]
    :execution-function/deps #{}})

  (registry/register!
   :fn/check-user-availability
   :atlas/execution-function
   #{:tier/service :domain/scheduling :integration/external :constraint/read-only-calendar-access :failure-mode/network-timeout-google-api}
   {:atlas/dev-id :fn/check-user-availability
    :execution-function/context [:query/date :oauth/access-token]
    :execution-function/response [:scheduling/available?]
    :execution-function/deps #{:component/gcal-client}})

  (registry/register!
   :fn/collect-available-users
   :atlas/execution-function
   #{:tier/service :domain/scheduling :effect/pure :operation/collect}
   {:atlas/dev-id :fn/collect-available-users
    :execution-function/context [:query/date :query/language :user/email]
    :execution-function/response [:availability/users]
    :execution-function/deps #{}})

  ;; ==========================================================================
  ;; ENDPOINTS - SYSTEM & UI (API Tier)
  ;; ==========================================================================

  (registry/register!
   :endpoint/home
   :atlas/interface-endpoint
   #{:tier/api :domain/ui :experience/first-time-user-onboarding :role/team-member :role/administrator}
   {:atlas/dev-id :endpoint/home
    :interface-endpoint/context []
    :interface-endpoint/response [:http/html]
    :interface-endpoint/deps #{}})

  (registry/register!
   :endpoint/health
   :atlas/interface-endpoint
   #{:tier/api :domain/system :role/team-member :role/administrator}
   {:atlas/dev-id :endpoint/health
    :interface-endpoint/context []
    :interface-endpoint/response [:health/status]
    :interface-endpoint/deps #{}})

  ;; ==========================================================================
  ;; ENDPOINTS - AUTHENTICATION (API Tier)
  ;; ==========================================================================

  (registry/register!
   :endpoint/google-signin-verify
   :atlas/interface-endpoint
   #{:tier/api :domain/auth :integration/external :pattern/explicit-consent :experience/first-time-user-onboarding}
   {:atlas/dev-id :endpoint/google-signin-verify
    :interface-endpoint/context [:auth/id-token]
    :interface-endpoint/response [:user/email :user/name :user/session-token]
    :interface-endpoint/deps #{:fn/verify-google-signin-token :fn/get-or-create-user}})

  (registry/register!
   :endpoint/calendar-access-status
   :atlas/interface-endpoint
   #{:tier/api :domain/auth :experience/calendar-access-expired :role/team-member :role/administrator}
   {:atlas/dev-id :endpoint/calendar-access-status
    :interface-endpoint/context [:user/email]
    :interface-endpoint/response [:user/has-calendar-access?]
    :interface-endpoint/deps #{:fn/check-calendar-access-granted}})

  ;; ==========================================================================
  ;; ENDPOINTS - OAUTH FLOW (API Tier)
  ;; ==========================================================================

  (registry/register!
   :endpoint/oauth-initiate
   :atlas/interface-endpoint
   #{:tier/api :domain/auth :protocol/oauth :pattern/explicit-consent :experience/first-time-user-onboarding :experience/calendar-access-expired :value/secure-calendar-integration :role/team-member :role/administrator}
   {:atlas/dev-id :endpoint/oauth-initiate
    :interface-endpoint/context [:user/email]
    :interface-endpoint/response [:oauth/authorization-url]
    :interface-endpoint/deps #{:fn/generate-oauth-authorization-url}})

  (registry/register!
   :endpoint/oauth-callback
   :atlas/interface-endpoint
   #{:tier/api :domain/auth :protocol/oauth :effect/write :pattern/explicit-consent :experience/first-time-user-onboarding :value/secure-calendar-integration :role/team-member :role/administrator}
   {:atlas/dev-id :endpoint/oauth-callback
    :interface-endpoint/context [:oauth/code :oauth/state]
    :interface-endpoint/response [:http/redirect-url]
    :interface-endpoint/deps #{:fn/exchange-oauth-code-for-tokens}})

  ;; ==========================================================================
  ;; ENDPOINTS - CALENDAR & SCHEDULING (API Tier)
  ;; ==========================================================================

  (registry/register!
   :endpoint/query-availability
   :atlas/interface-endpoint
   #{:tier/api :domain/scheduling :operation/query-availability :pattern/language-based-routing :constraint/calendar-permission-required :value/instant-availability-lookup :value/language-filtered-matching :experience/scheduling-workflow :role/scheduler :role/administrator}
   {:atlas/dev-id :endpoint/query-availability
    :interface-endpoint/context [:query/date :query/language]
    :interface-endpoint/response [:availability/users]
    :interface-endpoint/deps #{:fn/find-users-by-language
                               :fn/refresh-oauth-token
                               :fn/check-user-availability
                               :fn/collect-available-users}})

  (registry/register!
   :endpoint/calendar-events-list
   :atlas/interface-endpoint
   #{:tier/api :domain/scheduling :operation/list-events :constraint/calendar-permission-required :role/scheduler :role/administrator}
   {:atlas/dev-id :endpoint/calendar-events-list
    :interface-endpoint/context [:user/email :query/start-date :query/end-date]
    :interface-endpoint/response [:calendar/events]
    :interface-endpoint/deps #{:fn/list-user-calendar-events}})

  ;; ==========================================================================
  ;; ENDPOINTS - ADMIN USER CRUD (API Tier)
  ;; ==========================================================================

  (registry/register!
   :endpoint/admin-list-users
   :atlas/interface-endpoint
   #{:tier/api :domain/admin :domain/users :operation/list :constraint/admin-only-user-management :value/centralized-user-management :role/administrator}
   {:atlas/dev-id :endpoint/admin-list-users
    :interface-endpoint/context [:admin/requester-email]
    :interface-endpoint/response [:users/list]
    :interface-endpoint/deps #{:fn/check-user-is-admin :fn/list-all-authorized-users}})

  (registry/register!
   :endpoint/admin-create-user
   :atlas/interface-endpoint
   #{:tier/api :domain/admin :domain/users :effect/write :operation/create :constraint/admin-only-user-management :value/centralized-user-management :failure-mode/unauthorized-admin-attempt :role/administrator}
   {:atlas/dev-id :endpoint/admin-create-user
    :interface-endpoint/context [:admin/requester-email :user/email :user/name :user/phone :user/languages]
    :interface-endpoint/response [:user/id]
    :interface-endpoint/deps #{:fn/check-user-is-admin :fn/create-authorized-user}})

  (registry/register!
   :endpoint/admin-update-user
   :atlas/interface-endpoint
   #{:tier/api :domain/admin :domain/users :effect/write :operation/update :constraint/admin-only-user-management :value/centralized-user-management :failure-mode/unauthorized-admin-attempt :role/administrator}
   {:atlas/dev-id :endpoint/admin-update-user
    :interface-endpoint/context [:admin/requester-email :user/email :user/name :user/phone :user/languages]
    :interface-endpoint/response [:user/id]
    :interface-endpoint/deps #{:fn/check-user-is-admin :fn/update-authorized-user}})

  (registry/register!
   :endpoint/admin-delete-user
   :atlas/interface-endpoint
   #{:tier/api :domain/admin :domain/users :effect/write :operation/delete :constraint/admin-only-user-management :value/centralized-user-management :failure-mode/unauthorized-admin-attempt :role/administrator}
   {:atlas/dev-id :endpoint/admin-delete-user
    :interface-endpoint/context [:admin/requester-email :user/email]
    :interface-endpoint/response [:operation/success?]
    :interface-endpoint/deps #{:fn/check-user-is-admin :fn/delete-authorized-user}})

  ;; ==========================================================================
  ;; BUSINESS SEMANTICS - Patterns, Constraints, Values, and Intent
  ;; ==========================================================================
  ;;
  ;; This layer captures the "why" behind the architecture:
  ;; - WHY features exist (value propositions)
  ;; - WHY constraints are enforced (security, compliance)
  ;; - WHAT happens when things fail (failure modes)
  ;; - WHO can do what and WHY (roles and permissions)
  ;; - WHAT design decisions were made (patterns and principles)

  ;; ==========================================================================
  ;; BUSINESS PATTERNS - Design Decisions and Principles
  ;; ==========================================================================

  (registry/register!
   :pattern/explicit-consent
   :atlas/business-pattern
   #{:domain/auth :domain/google}
   {:atlas/dev-id :pattern/explicit-consent
    :business-pattern/principle "Users must explicitly grant permissions for external data access"
    :business-pattern/justification "Google OAuth security model requires separation of authentication from authorization"
    :business-pattern/experience-journey "Two separate permission prompts: 1) Sign in with Google, 2) Grant calendar access"
    :business-pattern/alternative-rejected "Silent calendar access on signin"
    :business-pattern/why-rejected "Violates principle of least privilege and Google's security policies"
    :business-pattern/business-value "User trust through transparent permission model"})

  (registry/register!
   :pattern/language-based-routing
   :atlas/business-pattern
   #{:domain/scheduling}
   {:atlas/dev-id :pattern/language-based-routing
    :business-pattern/principle "Match resources to requirements by language capability"
    :business-pattern/justification "Client meetings require team members who speak the client's language"
    :business-pattern/experience-journey "Filter available users by language preference"
    :business-pattern/business-value "Faster resource allocation, better client experience"
    :business-pattern/metrics-improved ["time-to-schedule" "client-satisfaction"]})

  (registry/register!
   :pattern/oauth-token-refresh
   :atlas/business-pattern
   #{:domain/google :protocol/oauth}
   {:atlas/dev-id :pattern/oauth-token-refresh
    :business-pattern/principle "Maintain continuous calendar access without re-prompting users"
    :business-pattern/justification "Access tokens expire hourly, re-authentication would disrupt workflow"
    :business-pattern/experience-journey "Seamless calendar access - users don't notice token refreshes"
    :business-pattern/failure-recovery :failure-mode/expired-refresh-token
    :business-pattern/business-value "Reduced friction, continuous service availability"})

  ;; ==========================================================================
  ;; CONSTRAINTS - Security Boundaries and Business Rules
  ;; ==========================================================================

  (registry/register!
   :constraint/admin-only-user-management
   :atlas/governance-constraint
   #{:domain/admin :domain/users}
   {:atlas/dev-id :constraint/admin-only-user-management
    :governance-constraint/enforced-by :fn/check-user-is-admin
    :governance-constraint/rationale "Prevent unauthorized access to sensitive employee data and system configuration"
    :governance-constraint/compliance-requirement "Data protection - only authorized personnel manage user PII"
    :governance-constraint/violation-response {:http-status 403
                                               :message "Administrator privileges required"
                                               :log-level :security-warning}
    :governance-constraint/user-sees "Feature hidden in UI or 'Access Denied' message"
    :governance-constraint/business-impact "Protects against unauthorized user creation/deletion"})

  (registry/register!
   :constraint/read-only-calendar-access
   :atlas/governance-constraint
   #{:domain/google :integration/external}
   {:atlas/dev-id :constraint/read-only-calendar-access
    :governance-constraint/enforced-by :component/gcal-client
    :governance-constraint/rationale "System only needs to check availability, not modify calendars"
    :governance-constraint/google-oauth-scope "https://www.googleapis.com/auth/calendar.readonly"
    :governance-constraint/user-benefit "Reduced security risk - system cannot accidentally modify events"
    :governance-constraint/alternative-rejected "Read-write calendar access"
    :governance-constraint/why-rejected "Violates least privilege, increases security surface area"})

  (registry/register!
   :constraint/calendar-permission-required
   :atlas/governance-constraint
   #{:domain/scheduling}
   {:atlas/dev-id :constraint/calendar-permission-required
    :governance-constraint/enforced-by :fn/check-calendar-access-granted
    :governance-constraint/rationale "Cannot check availability without calendar access"
    :governance-constraint/user-experiences "Appears as 'not available' if calendar not connected"
    :governance-constraint/recovery-path :endpoint/oauth-initiate
    :governance-constraint/business-impact "Users without calendar access won't appear in queries"
    :governance-constraint/revocable true
    :governance-constraint/revocation-path "Google account settings - Third-party app access"})

  ;; ==========================================================================
  ;; FAILURE MODES - What Can Go Wrong and How to Recover
  ;; ==========================================================================

  (registry/register!
   :failure-mode/expired-refresh-token
   :atlas/risk-failure-mode
   #{:protocol/oauth :domain/google}
   {:atlas/dev-id :failure-mode/expired-refresh-token
    :risk-failure-mode/triggered-by "Google invalidates refresh token after 6 months of inactivity"
    :risk-failure-mode/detection "OAuth token refresh returns 401 Unauthorized"
    :risk-failure-mode/user-experiences "Calendar access status shows 'Permission expired - reconnect required'"
    :risk-failure-mode/recovery-path :endpoint/oauth-initiate
    :risk-failure-mode/recovery-steps ["User clicks 'Reconnect Calendar'"
                                       "OAuth flow initiated"
                                       "User approves access again"
                                       "New tokens stored"]
    :risk-failure-mode/data-loss false
    :risk-failure-mode/business-impact "User temporarily missing from availability queries until reconnection"
    :risk-failure-mode/frequency "Low - only for inactive users"
    :risk-failure-mode/preventable false
    :risk-failure-mode/why-not-preventable "Google security policy - tokens must expire"})

  (registry/register!
   :failure-mode/network-timeout-google-api
   :atlas/risk-failure-mode
   #{:integration/external :domain/google}
   {:atlas/dev-id :failure-mode/network-timeout-google-api
    :risk-failure-mode/triggered-by "Google Calendar API unavailable or slow response"
    :risk-failure-mode/detection "HTTP request timeout after 30 seconds"
    :risk-failure-mode/user-experiences "Loading spinner, then 'Unable to check availability - please try again'"
    :risk-failure-mode/recovery-path "Automatic retry with exponential backoff"
    :risk-failure-mode/data-loss false
    :risk-failure-mode/business-impact "Delayed query results, temporary unavailability data"
    :risk-failure-mode/frequency "Rare - Google SLA is 99.9%"
    :risk-failure-mode/preventable true
    :risk-failure-mode/prevention-strategy "Circuit breaker pattern, cached availability data"})

  (registry/register!
   :failure-mode/unauthorized-admin-attempt
   :atlas/risk-failure-mode
   #{:domain/admin :domain/users}
   {:atlas/dev-id :failure-mode/unauthorized-admin-attempt
    :risk-failure-mode/triggered-by "Non-admin user attempts admin operation"
    :risk-failure-mode/detection :fn/check-user-is-admin
    :risk-failure-mode/user-experiences "HTTP 403 Forbidden or UI feature disabled/hidden"
    :risk-failure-mode/recovery-path "Contact administrator to request admin privileges"
    :risk-failure-mode/security-event true
    :risk-failure-mode/logged true
    :risk-failure-mode/log-details {:level :warning
                                    :message "Unauthorized admin access attempt"
                                    :includes [:user-email :attempted-operation :timestamp]}
    :risk-failure-mode/business-impact "Prevents unauthorized user data modification"})

  ;; ==========================================================================
  ;; VALUE PROPOSITIONS - Why Features Exist
  ;; ==========================================================================

  (registry/register!
   :value/instant-availability-lookup
   :atlas/value-proposition
   #{:domain/scheduling :value-type/time-savings}
   {:atlas/dev-id :value/instant-availability-lookup
    :value-proposition/business-problem "Manually checking multiple calendars is time-consuming and error-prone"
    :value-proposition/before-state "Project managers open 5-10 Google Calendars individually to find available resources"
    :value-proposition/after-state "Single query returns all available team members in seconds"
    :value-proposition/time-saved "15-30 minutes per resource search"
    :value-proposition/solution "Automated parallel calendar checking with language filtering"
    :value-proposition/metrics-improved ["time-to-schedule" "scheduling-accuracy" "resource-utilization"]
    :value-proposition/user-segment "Project managers, team leads, schedulers"
    :value-proposition/business-value-quantified {:time-savings "90% reduction in scheduling time"
                                                  :error-reduction "95% fewer double-bookings"
                                                  :scale "Handles 100+ user calendars simultaneously"}})

  (registry/register!
   :value/language-filtered-matching
   :atlas/value-proposition
   #{:domain/scheduling :value-type/capability-matching}
   {:atlas/dev-id :value/language-filtered-matching
    :value-proposition/business-problem "Finding bilingual resources for client meetings requires manual directory searches"
    :value-proposition/solution "Pre-filter available users by language capability before checking calendars"
    :value-proposition/user-segment "International teams, client-facing roles"
    :value-proposition/business-value "Better client experience through language-appropriate staffing"
    :value-proposition/competitive-advantage "Faster response to client meeting requests"})

  (registry/register!
   :value/secure-calendar-integration
   :atlas/value-proposition
   #{:domain/google :protocol/oauth}
   {:atlas/dev-id :value/secure-calendar-integration
    :value-proposition/business-problem "Users concerned about privacy when sharing calendar data"
    :value-proposition/solution "Explicit OAuth consent flow with revocable permissions"
    :value-proposition/implements-pattern :pattern/explicit-consent
    :value-proposition/trust-factors ["User controls what's shared"
                                      "Read-only access"
                                      "Revocable at any time"
                                      "Standard Google security"]
    :value-proposition/business-value "User trust enables adoption without security concerns"
    :value-proposition/risk-mitigation "No calendar modification capability reduces liability"})

  (registry/register!
   :value/centralized-user-management
   :atlas/value-proposition
   #{:domain/admin}
   {:atlas/dev-id :value/centralized-user-management
    :value-proposition/business-problem "User onboarding/offboarding requires coordinating multiple systems"
    :value-proposition/solution "Single admin interface for all user lifecycle operations"
    :value-proposition/user-segment "HR, IT administrators, team managers"
    :value-proposition/business-value "Reduced administrative overhead, faster onboarding"
    :value-proposition/compliance-benefit "Audit trail of user management operations"})

  ;; ==========================================================================
  ;; USER ROLES - Who Can Do What and Why
  ;; ==========================================================================

  (registry/register!
   :role/team-member
   :atlas/identity-role
   #{:domain/users}
   {:atlas/dev-id :role/team-member
    :identity-role/description "Regular user who can be queried for availability"
    :identity-role/cannot-access #{:endpoint/admin-list-users
                                   :endpoint/admin-create-user
                                   :endpoint/admin-update-user
                                   :endpoint/admin-delete-user}
    :identity-role/responsibilities ["Grant calendar access"
                                     "Keep calendar up-to-date"
                                     "Maintain language preferences"]
    :identity-role/expectations "Appears in availability queries when calendar is connected"
    :identity-role/data-access "Can only view own calendar access status"})

  (registry/register!
   :role/administrator
   :atlas/identity-role
   #{:domain/admin}
   {:atlas/dev-id :role/administrator
    :identity-role/description "Privileged user who can manage other users"
    :identity-role/cannot-access #{}
    :identity-role/responsibilities ["User lifecycle management"
                                     "Maintain user directory accuracy"
                                     "Handle access requests"
                                     "Monitor system health"]
    :identity-role/granted-by "Database configuration or system setup"
    :identity-role/data-access "Full access to user directory and operations"
    :identity-role/security-requirement "Admin privileges verified on every operation"
    :identity-role/audit-logged true})

  (registry/register!
   :role/scheduler
   :atlas/identity-role
   #{:domain/scheduling}
   {:atlas/dev-id :role/scheduler
    :identity-role/description "User who queries team availability for scheduling"
    :identity-role/typical-users ["Project managers"
                                  "Team leads"
                                  "Executive assistants"
                                  "Resource coordinators"]
    :identity-role/responsibilities ["Query availability responsibly"
                                     "Respect privacy boundaries"
                                     "Use data only for scheduling"]
    :identity-role/data-access "Can see who is available, but not event details"
    :identity-role/privacy-constraint "No access to calendar event content, only availability status"})

  ;; ==========================================================================
  ;; USER EXPERIENCES - What Users See and Feel
  ;; ==========================================================================

  (registry/register!
   :experience/first-time-user-onboarding
   :atlas/experience-journey
   #{:domain/auth}
   {:atlas/dev-id :experience/first-time-user-onboarding
    :experience-journey/user-journey ["Visit homepage"
                                      "See 'Sign in with Google' button"
                                      "Click and authenticate"
                                      "Prompted for calendar permission"
                                      "Grant permission"
                                      "Redirected to system"
                                      "Ready to use"]
    :experience-journey/time-to-complete "2-3 minutes"
    :experience-journey/friction-points ["Two separate permission prompts"
                                         "Redirection to Google"]
    :experience-journey/why-designed-this-way :pattern/explicit-consent
    :experience-journey/user-sentiment "Initially confused by two-step process, appreciates security once explained"})

  (registry/register!
   :experience/scheduling-workflow
   :atlas/experience-journey
   #{:domain/scheduling}
   {:atlas/dev-id :experience/scheduling-workflow
    :experience-journey/user-journey ["Need to schedule meeting with French speaker on Jan 15"
                                      "Open availability query"
                                      "Select language: French"
                                      "Select date: Jan 15"
                                      "Submit query"
                                      "Receive list of 3 available users"
                                      "Contact one to schedule"]
    :experience-journey/time-to-complete "30 seconds"
    :experience-journey/replaces "15-30 minutes of manual calendar checking"
    :experience-journey/user-sentiment "Significant time savings, high satisfaction"
    :experience-journey/delivers-value :value/instant-availability-lookup})

  (registry/register!
   :experience/calendar-access-expired
   :atlas/experience-journey
   #{:domain/google :protocol/oauth}
   {:atlas/dev-id :experience/calendar-access-expired
    :experience-journey/user-journey ["User hasn't used system in 6+ months"
                                      "Token expired"
                                      "User tries to check availability"
                                      "Sees 'Calendar access expired'"
                                      "Clicks 'Reconnect'"
                                      "Re-grants permission"
                                      "Access restored"]
    :experience-journey/risk-failure-mode :failure-mode/expired-refresh-token
    :experience-journey/user-sentiment "Minor inconvenience, understands security necessity"
    :experience-journey/recovery-time "1 minute"}))
#_(init-registry!)
#_(reset! registry/registry {})
;;@registry/registry
;;(ontology/register-entity-types!)
(defn check-invariants
  "Run the runtime invariant suite against the registered calendar availability entities.
  Uses the unified runtime/invariants which handle:
  - Data flow validation (context satisfiability, orphan responses)
  - Dependency validation (existence, no cycles)
  - Tier validation (components=foundation, endpoints=api)
  - Reachability validation (all functions reachable from endpoints)
  - Semantic consistency (external=async, pure=no-deps)"
  []
  (invariant/check-all))
;;(check-invariants)
