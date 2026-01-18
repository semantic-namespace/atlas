(ns atlas-ui.registry
  "Semantic registry for the Atlas UI itself - making the UI self-describing")

;; ============================================================================
;; UI Components
;; ============================================================================

(def ui-registry
  {#{:atlas/interface-protocol :protocol/user-repository}
 {:atlas/dev-id :protocol/user-repository,
  :interface-protocol/functions
  [:protocol.user-repository/find-users-by-language
   :protocol.user-repository/get-user
   :protocol.user-repository/create-user
   :protocol.user-repository/update-user
   :protocol.user-repository/delete-user
   :protocol.user-repository/list-users
   :protocol.user-repository/check-admin],
  :atlas/type :atlas/interface-protocol},
 #{:constraint/read-only-calendar-access
   :tier/service
   :integration/external
   :domain/scheduling
   :atlas/execution-function
   :operation/query}
 {:atlas/dev-id :fn/get-calendar-events-in-range,
  :execution-function/context
  [:user/email :query/date :query/start-time :query/end-time],
  :execution-function/response [:calendar/events],
  :execution-function/deps #{:component/gcal-client},
  :atlas/type :atlas/execution-function},
 #{:dataflow/output :domain/scheduling :atlas/data-schema}
 {:data-schema/fields [:availability/users],
  :atlas/dev-id :schema/availability,
  :atlas/type :atlas/data-schema},
 #{:domain/admin
   :tier/service
   :constraint/admin-only-user-management
   :atlas/execution-function
   :operation/list
   :domain/users}
 {:atlas/dev-id :fn/list-all-authorized-users,
  :execution-function/context [],
  :execution-function/response [:users/list],
  :execution-function/deps #{:component/db},
  :atlas/type :atlas/execution-function},
 #{:tier/service
   :domain/scheduling
   :effect/pure
   :atlas/execution-function
   :operation/collect}
 {:atlas/dev-id :fn/collect-available-users,
  :execution-function/context
  [:query/date :query/language :user/email],
  :execution-function/response [:availability/users],
  :execution-function/deps #{},
  :atlas/type :atlas/execution-function},
 #{:domain/admin
   :tier/service
   :constraint/admin-only-user-management
   :atlas/execution-function
   :operation/read
   :domain/users}
 {:atlas/dev-id :fn/get-authorized-user,
  :execution-function/context [:user/email],
  :execution-function/response
  [:user/id :user/email :user/name :user/phone :user/languages],
  :execution-function/deps #{:component/db},
  :atlas/type :atlas/execution-function},
 #{:domain/auth :protocol/oauth :atlas/data-schema}
 {:data-schema/fields
  [:oauth/access-token
   :oauth/refresh-token
   :oauth/authorization-url
   :oauth/state
   :oauth/code
   :oauth/redirect-uri
   :auth/id-token
   :auth/client-id],
  :atlas/dev-id :schema/oauth-credentials,
  :atlas/type :atlas/data-schema},
 #{:experience/first-time-user-onboarding
   :tier/api
   :role/team-member
   :atlas/interface-endpoint
   :domain/ui
   :role/administrator}
 {:atlas/dev-id :endpoint/home,
  :interface-endpoint/context [],
  :interface-endpoint/response [:http/html],
  :interface-endpoint/deps #{},
  :atlas/type :atlas/interface-endpoint},
 #{:domain/admin
   :tier/service
   :constraint/admin-only-user-management
   :atlas/execution-function
   :operation/delete
   :domain/users
   :effect/write}
 {:atlas/dev-id :fn/delete-authorized-user,
  :execution-function/context [:user/email],
  :execution-function/response [:operation/success?],
  :execution-function/deps #{:component/db},
  :atlas/type :atlas/execution-function},
 #{:integration/external :atlas/risk-failure-mode :domain/google}
 {:risk-failure-mode/data-loss false,
  :risk-failure-mode/user-experiences
  "Loading spinner, then 'Unable to check availability - please try again'",
  :atlas/type :atlas/risk-failure-mode,
  :risk-failure-mode/triggered-by
  "Google Calendar API unavailable or slow response",
  :risk-failure-mode/business-impact
  "Delayed query results, temporary unavailability data",
  :risk-failure-mode/frequency "Rare - Google SLA is 99.9%",
  :risk-failure-mode/detection
  "HTTP request timeout after 30 seconds",
  :atlas/dev-id :failure-mode/network-timeout-google-api,
  :risk-failure-mode/prevention-strategy
  "Circuit breaker pattern, cached availability data",
  :risk-failure-mode/recovery-path
  "Automatic retry with exponential backoff",
  :risk-failure-mode/preventable true},
 #{:domain/auth :tier/service :atlas/execution-function}
 {:atlas/dev-id :fn/check-user-is-admin,
  :execution-function/context [:user/email],
  :execution-function/response [:user/is-admin?],
  :execution-function/deps #{:component/db},
  :atlas/type :atlas/execution-function},
 #{:experience/first-time-user-onboarding
   :domain/auth
   :tier/api
   :role/team-member
   :atlas/interface-endpoint
   :pattern/explicit-consent
   :role/administrator
   :protocol/oauth
   :value/secure-calendar-integration
   :effect/write}
 {:atlas/dev-id :endpoint/oauth-callback,
  :interface-endpoint/context [:oauth/code :oauth/state],
  :interface-endpoint/response [:http/redirect-url],
  :interface-endpoint/deps #{:fn/exchange-oauth-code-for-tokens},
  :atlas/type :atlas/interface-endpoint},
 #{:constraint/read-only-calendar-access
   :tier/service
   :integration/external
   :failure-mode/network-timeout-google-api
   :domain/scheduling
   :atlas/execution-function
   :operation/list}
 {:atlas/dev-id :fn/list-user-calendar-events,
  :execution-function/context
  [:user/email
   :query/start-date
   :query/start-time
   :query/end-date
   :query/end-time],
  :execution-function/response [:calendar/events],
  :execution-function/deps #{:component/gcal-client},
  :atlas/type :atlas/execution-function},
 #{:domain/scheduling :dataflow/input :atlas/data-schema}
 {:data-schema/fields
  [:query/date
   :query/language
   :query/start-date
   :query/end-date
   :query/start-time
   :query/end-time],
  :atlas/dev-id :schema/scheduling-query,
  :atlas/type :atlas/data-schema},
 #{:domain/auth
   :tier/service
   :atlas/execution-function
   :operation/read
   :pattern/oauth-token-refresh
   :failure-mode/expired-refresh-token
   :domain/users}
 {:atlas/dev-id :fn/get-user-credential,
  :execution-function/context [:user/email],
  :execution-function/response
  [:oauth/access-token :oauth/refresh-token],
  :execution-function/deps #{:component/db},
  :atlas/type :atlas/execution-function},
 #{:domain/admin :atlas/value-proposition}
 {:atlas/dev-id :value/centralized-user-management,
  :value-proposition/business-problem
  "User onboarding/offboarding requires coordinating multiple systems",
  :value-proposition/solution
  "Single admin interface for all user lifecycle operations",
  :value-proposition/user-segment
  "HR, IT administrators, team managers",
  :value-proposition/business-value
  "Reduced administrative overhead, faster onboarding",
  :value-proposition/compliance-benefit
  "Audit trail of user management operations",
  :atlas/type :atlas/value-proposition},
 #{:domain/admin :atlas/governance-constraint :domain/users}
 {:atlas/dev-id :constraint/admin-only-user-management,
  :governance-constraint/enforced-by :fn/check-user-is-admin,
  :governance-constraint/rationale
  "Prevent unauthorized access to sensitive employee data and system configuration",
  :governance-constraint/compliance-requirement
  "Data protection - only authorized personnel manage user PII",
  :governance-constraint/violation-response
  {:http-status 403,
   :message "Administrator privileges required",
   :log-level :security-warning},
  :governance-constraint/user-sees
  "Feature hidden in UI or 'Access Denied' message",
  :governance-constraint/business-impact
  "Protects against unauthorized user creation/deletion",
  :atlas/type :atlas/governance-constraint},
 #{:protocol/oauth :domain/google :atlas/value-proposition}
 {:atlas/dev-id :value/secure-calendar-integration,
  :value-proposition/business-problem
  "Users concerned about privacy when sharing calendar data",
  :value-proposition/solution
  "Explicit OAuth consent flow with revocable permissions",
  :value-proposition/implements-pattern :pattern/explicit-consent,
  :value-proposition/trust-factors
  ["User controls what's shared"
   "Read-only access"
   "Revocable at any time"
   "Standard Google security"],
  :value-proposition/business-value
  "User trust enables adoption without security concerns",
  :value-proposition/risk-mitigation
  "No calendar modification capability reduces liability",
  :atlas/type :atlas/value-proposition},
 #{:domain/admin
   :operation/create
   :tier/api
   :constraint/admin-only-user-management
   :value/centralized-user-management
   :atlas/interface-endpoint
   :failure-mode/unauthorized-admin-attempt
   :role/administrator
   :domain/users
   :effect/write}
 {:atlas/dev-id :endpoint/admin-create-user,
  :interface-endpoint/context
  [:admin/requester-email
   :user/email
   :user/name
   :user/phone
   :user/languages],
  :interface-endpoint/response [:user/id],
  :interface-endpoint/deps
  #{:fn/create-authorized-user :fn/check-user-is-admin},
  :atlas/type :atlas/interface-endpoint},
 #{:experience/first-time-user-onboarding
   :domain/auth
   :tier/api
   :integration/external
   :atlas/interface-endpoint
   :pattern/explicit-consent}
 {:atlas/dev-id :endpoint/google-signin-verify,
  :interface-endpoint/context [:auth/id-token],
  :interface-endpoint/response
  [:user/email :user/name :user/session-token],
  :interface-endpoint/deps
  #{:fn/verify-google-signin-token :fn/get-or-create-user},
  :atlas/type :atlas/interface-endpoint},
 #{:domain/auth :atlas/experience-journey}
 {:atlas/dev-id :experience/first-time-user-onboarding,
  :experience-journey/user-journey
  ["Visit homepage"
   "See 'Sign in with Google' button"
   "Click and authenticate"
   "Prompted for calendar permission"
   "Grant permission"
   "Redirected to system"
   "Ready to use"],
  :experience-journey/time-to-complete "2-3 minutes",
  :experience-journey/friction-points
  ["Two separate permission prompts" "Redirection to Google"],
  :experience-journey/why-designed-this-way
  :pattern/explicit-consent,
  :experience-journey/user-sentiment
  "Initially confused by two-step process, appreciates security once explained",
  :atlas/type :atlas/experience-journey},
 #{:atlas/identity-role :domain/users}
 {:atlas/dev-id :role/team-member,
  :identity-role/description
  "Regular user who can be queried for availability",
  :identity-role/cannot-access
  #{:endpoint/admin-create-user
    :endpoint/admin-update-user
    :endpoint/admin-delete-user
    :endpoint/admin-list-users},
  :identity-role/responsibilities
  ["Grant calendar access"
   "Keep calendar up-to-date"
   "Maintain language preferences"],
  :identity-role/expectations
  "Appears in availability queries when calendar is connected",
  :identity-role/data-access
  "Can only view own calendar access status",
  :atlas/type :atlas/identity-role},
 #{:domain/admin
   :tier/api
   :constraint/admin-only-user-management
   :value/centralized-user-management
   :atlas/interface-endpoint
   :failure-mode/unauthorized-admin-attempt
   :role/administrator
   :operation/update
   :domain/users
   :effect/write}
 {:atlas/dev-id :endpoint/admin-update-user,
  :interface-endpoint/context
  [:admin/requester-email
   :user/email
   :user/name
   :user/phone
   :user/languages],
  :interface-endpoint/response [:user/id],
  :interface-endpoint/deps
  #{:fn/update-authorized-user :fn/check-user-is-admin},
  :atlas/type :atlas/interface-endpoint},
 #{:atlas/interface-protocol :protocol/oauth}
 {:atlas/dev-id :protocol/oauth,
  :interface-protocol/functions
  [:protocol.oauth/refresh-token
   :protocol.oauth/generate-auth-url
   :protocol.oauth/exchange-code
   :protocol.oauth/verify-signin-token],
  :atlas/type :atlas/interface-protocol},
 #{:atlas/interface-protocol :protocol/credential-storage}
 {:atlas/dev-id :protocol/credential-storage,
  :interface-protocol/functions
  [:protocol.credential/save
   :protocol.credential/retrieve
   :protocol.credential/has-refresh-token],
  :atlas/type :atlas/interface-protocol},
 #{:domain/admin
   :operation/create
   :tier/service
   :constraint/admin-only-user-management
   :atlas/execution-function
   :domain/users
   :effect/write}
 {:atlas/dev-id :fn/create-authorized-user,
  :execution-function/context
  [:user/email :user/name :user/phone :user/languages],
  :execution-function/response [:user/id],
  :execution-function/deps #{:component/db},
  :atlas/type :atlas/execution-function},
 #{:tier/service
   :atlas/execution-function
   :operation/generate-url
   :protocol/oauth
   :domain/google}
 {:atlas/dev-id :fn/generate-oauth-authorization-url,
  :execution-function/context [:oauth/redirect-uri :user/email],
  :execution-function/response
  [:oauth/authorization-url :oauth/state],
  :execution-function/deps #{:component/google-oauth},
  :atlas/type :atlas/execution-function},
 #{:operation/query-availability
   :tier/api
   :role/scheduler
   :atlas/interface-endpoint
   :domain/scheduling
   :constraint/calendar-permission-required
   :role/administrator
   :pattern/language-based-routing
   :experience/scheduling-workflow
   :value/instant-availability-lookup
   :value/language-filtered-matching}
 {:atlas/dev-id :endpoint/query-availability,
  :interface-endpoint/context [:query/date :query/language],
  :interface-endpoint/response [:availability/users],
  :interface-endpoint/deps
  #{:fn/check-user-availability
    :fn/refresh-oauth-token
    :fn/collect-available-users
    :fn/find-users-by-language},
  :atlas/type :atlas/interface-endpoint},
 #{:domain/scheduling
   :value-type/capability-matching
   :atlas/value-proposition}
 {:atlas/dev-id :value/language-filtered-matching,
  :value-proposition/business-problem
  "Finding bilingual resources for client meetings requires manual directory searches",
  :value-proposition/solution
  "Pre-filter available users by language capability before checking calendars",
  :value-proposition/user-segment
  "International teams, client-facing roles",
  :value-proposition/business-value
  "Better client experience through language-appropriate staffing",
  :value-proposition/competitive-advantage
  "Faster response to client meeting requests",
  :atlas/type :atlas/value-proposition},
 #{:domain/admin :atlas/risk-failure-mode :domain/users}
 {:risk-failure-mode/user-experiences
  "HTTP 403 Forbidden or UI feature disabled/hidden",
  :atlas/type :atlas/risk-failure-mode,
  :risk-failure-mode/triggered-by
  "Non-admin user attempts admin operation",
  :risk-failure-mode/business-impact
  "Prevents unauthorized user data modification",
  :risk-failure-mode/detection :fn/check-user-is-admin,
  :atlas/dev-id :failure-mode/unauthorized-admin-attempt,
  :risk-failure-mode/security-event true,
  :risk-failure-mode/logged true,
  :risk-failure-mode/recovery-path
  "Contact administrator to request admin privileges",
  :risk-failure-mode/log-details
  {:level :warning,
   :message "Unauthorized admin access attempt",
   :includes [:user-email :attempted-operation :timestamp]}},
 #{:domain/system
   :tier/api
   :role/team-member
   :atlas/interface-endpoint
   :role/administrator}
 {:atlas/dev-id :endpoint/health,
  :interface-endpoint/context [],
  :interface-endpoint/response [:health/status],
  :interface-endpoint/deps #{},
  :atlas/type :atlas/interface-endpoint},
 #{:domain/scheduling :atlas/identity-role}
 {:atlas/dev-id :role/scheduler,
  :identity-role/description
  "User who queries team availability for scheduling",
  :identity-role/typical-users
  ["Project managers"
   "Team leads"
   "Executive assistants"
   "Resource coordinators"],
  :identity-role/responsibilities
  ["Query availability responsibly"
   "Respect privacy boundaries"
   "Use data only for scheduling"],
  :identity-role/data-access
  "Can see who is available, but not event details",
  :identity-role/privacy-constraint
  "No access to calendar event content, only availability status",
  :atlas/type :atlas/identity-role},
 #{:tier/service
   :atlas/execution-function
   :pattern/language-based-routing
   :domain/users
   :value/language-filtered-matching}
 {:atlas/dev-id :fn/find-users-by-language,
  :execution-function/context [:query/language],
  :execution-function/response
  [:user/email :user/gcal-refresh-token],
  :execution-function/deps #{:component/db},
  :atlas/type :atlas/execution-function},
 #{:domain/admin
   :tier/api
   :constraint/admin-only-user-management
   :value/centralized-user-management
   :atlas/interface-endpoint
   :failure-mode/unauthorized-admin-attempt
   :role/administrator
   :operation/delete
   :domain/users
   :effect/write}
 {:atlas/dev-id :endpoint/admin-delete-user,
  :interface-endpoint/context [:admin/requester-email :user/email],
  :interface-endpoint/response [:operation/success?],
  :interface-endpoint/deps
  #{:fn/delete-authorized-user :fn/check-user-is-admin},
  :atlas/type :atlas/interface-endpoint},
 #{:value-type/time-savings
   :domain/scheduling
   :atlas/value-proposition}
 {:value-proposition/metrics-improved
  ["time-to-schedule" "scheduling-accuracy" "resource-utilization"],
  :atlas/type :atlas/value-proposition,
  :value-proposition/solution
  "Automated parallel calendar checking with language filtering",
  :value-proposition/after-state
  "Single query returns all available team members in seconds",
  :value-proposition/time-saved "15-30 minutes per resource search",
  :atlas/dev-id :value/instant-availability-lookup,
  :value-proposition/user-segment
  "Project managers, team leads, schedulers",
  :value-proposition/before-state
  "Project managers open 5-10 Google Calendars individually to find available resources",
  :value-proposition/business-value-quantified
  {:time-savings "90% reduction in scheduling time",
   :error-reduction "95% fewer double-bookings",
   :scale "Handles 100+ user calendars simultaneously"},
  :value-proposition/business-problem
  "Manually checking multiple calendars is time-consuming and error-prone"},
 #{:tier/service
   :atlas/execution-function
   :domain/users
   :effect/write}
 {:atlas/dev-id :fn/get-or-create-user,
  :execution-function/context [:user/email :user/name :user/picture],
  :execution-function/response [:user/id :user/email :user/name],
  :execution-function/deps #{:component/db},
  :atlas/type :atlas/execution-function},
 #{:domain/admin :atlas/identity-role}
 {:identity-role/security-requirement
  "Admin privileges verified on every operation",
  :atlas/type :atlas/identity-role,
  :identity-role/responsibilities
  ["User lifecycle management"
   "Maintain user directory accuracy"
   "Handle access requests"
   "Monitor system health"],
  :atlas/dev-id :role/administrator,
  :identity-role/audit-logged true,
  :identity-role/description
  "Privileged user who can manage other users",
  :identity-role/granted-by "Database configuration or system setup",
  :identity-role/cannot-access #{},
  :identity-role/data-access
  "Full access to user directory and operations"},
 #{:domain/scheduling :atlas/governance-constraint}
 {:governance-constraint/revocation-path
  "Google account settings - Third-party app access",
  :governance-constraint/user-experiences
  "Appears as 'not available' if calendar not connected",
  :governance-constraint/revocable true,
  :atlas/type :atlas/governance-constraint,
  :atlas/dev-id :constraint/calendar-permission-required,
  :governance-constraint/business-impact
  "Users without calendar access won't appear in queries",
  :governance-constraint/enforced-by
  :fn/check-calendar-access-granted,
  :governance-constraint/recovery-path :endpoint/oauth-initiate,
  :governance-constraint/rationale
  "Cannot check availability without calendar access"},
 #{:domain/auth
   :tier/service
   :atlas/execution-function
   :operation/check
   :domain/users}
 {:atlas/dev-id :fn/check-user-has-refresh-token,
  :execution-function/context [:user/email],
  :execution-function/response [:user/has-refresh-token?],
  :execution-function/deps #{:component/db},
  :atlas/type :atlas/execution-function},
 #{:domain/auth :atlas/business-pattern :domain/google}
 {:atlas/dev-id :pattern/explicit-consent,
  :business-pattern/principle
  "Users must explicitly grant permissions for external data access",
  :business-pattern/justification
  "Google OAuth security model requires separation of authentication from authorization",
  :business-pattern/experience-journey
  "Two separate permission prompts: 1) Sign in with Google, 2) Grant calendar access",
  :business-pattern/alternative-rejected
  "Silent calendar access on signin",
  :business-pattern/why-rejected
  "Violates principle of least privilege and Google's security policies",
  :business-pattern/business-value
  "User trust through transparent permission model",
  :atlas/type :atlas/business-pattern},
 #{:domain/scheduling :atlas/experience-journey}
 {:atlas/dev-id :experience/scheduling-workflow,
  :experience-journey/user-journey
  ["Need to schedule meeting with French speaker on Jan 15"
   "Open availability query"
   "Select language: French"
   "Select date: Jan 15"
   "Submit query"
   "Receive list of 3 available users"
   "Contact one to schedule"],
  :experience-journey/time-to-complete "30 seconds",
  :experience-journey/replaces
  "15-30 minutes of manual calendar checking",
  :experience-journey/user-sentiment
  "Significant time savings, high satisfaction",
  :experience-journey/delivers-value
  :value/instant-availability-lookup,
  :atlas/type :atlas/experience-journey},
 #{:tier/service
   :integration/external
   :atlas/execution-function
   :protocol/oauth
   :domain/google
   :effect/write}
 {:atlas/dev-id :fn/exchange-oauth-code-for-tokens,
  :execution-function/context [:oauth/code :user/email],
  :execution-function/response
  [:oauth/access-token :oauth/refresh-token],
  :execution-function/deps #{:component/db :component/google-oauth},
  :atlas/type :atlas/execution-function},
 #{:tier/service
   :integration/external
   :atlas/execution-function
   :pattern/oauth-token-refresh
   :protocol/oauth
   :failure-mode/expired-refresh-token
   :domain/google}
 {:atlas/dev-id :fn/refresh-oauth-token,
  :execution-function/context [:user/gcal-refresh-token],
  :execution-function/response [:oauth/access-token],
  :execution-function/deps #{:component/google-oauth},
  :atlas/type :atlas/execution-function},
 #{:domain/scheduling :atlas/data-schema}
 {:data-schema/fields
  [:calendar/events
   :calendar/event-id
   :calendar/start-time
   :calendar/end-time
   :calendar/summary],
  :atlas/dev-id :schema/calendar-events,
  :atlas/type :atlas/data-schema},
 #{:domain/admin
   :tier/service
   :constraint/admin-only-user-management
   :atlas/execution-function
   :operation/update
   :domain/users
   :effect/write}
 {:atlas/dev-id :fn/update-authorized-user,
  :execution-function/context
  [:user/email :user/name :user/phone :user/languages],
  :execution-function/response [:user/id],
  :execution-function/deps #{:component/db},
  :atlas/type :atlas/execution-function},
 #{:integration/external
   :service/authentication
   :atlas/structure-component
   :tier/foundation
   :protocol/oauth
   :domain/google}
 {:atlas/dev-id :component/google-oauth,
  :structure-component/deps #{},
  :atlas/type :atlas/structure-component},
 #{:atlas/risk-failure-mode :protocol/oauth :domain/google}
 {:risk-failure-mode/data-loss false,
  :risk-failure-mode/why-not-preventable
  "Google security policy - tokens must expire",
  :risk-failure-mode/user-experiences
  "Calendar access status shows 'Permission expired - reconnect required'",
  :atlas/type :atlas/risk-failure-mode,
  :risk-failure-mode/triggered-by
  "Google invalidates refresh token after 6 months of inactivity",
  :risk-failure-mode/recovery-steps
  ["User clicks 'Reconnect Calendar'"
   "OAuth flow initiated"
   "User approves access again"
   "New tokens stored"],
  :risk-failure-mode/business-impact
  "User temporarily missing from availability queries until reconnection",
  :risk-failure-mode/frequency "Low - only for inactive users",
  :risk-failure-mode/detection
  "OAuth token refresh returns 401 Unauthorized",
  :atlas/dev-id :failure-mode/expired-refresh-token,
  :risk-failure-mode/recovery-path :endpoint/oauth-initiate,
  :risk-failure-mode/preventable false},
 #{:protocol/credential-storage
   :atlas/structure-component
   :protocol/user-repository
   :tier/foundation
   :domain/users}
 {:atlas/dev-id :component/db,
  :structure-component/deps #{},
  :atlas/type :atlas/structure-component},
 #{:domain/auth
   :tier/service
   :integration/external
   :atlas/execution-function}
 {:atlas/dev-id :fn/verify-google-signin-token,
  :execution-function/context [:auth/id-token :auth/client-id],
  :execution-function/response
  [:user/email :user/name :user/picture],
  :execution-function/deps #{:component/google-oauth},
  :atlas/type :atlas/execution-function},
 #{:integration/external :atlas/governance-constraint :domain/google}
 {:atlas/dev-id :constraint/read-only-calendar-access,
  :governance-constraint/enforced-by :component/gcal-client,
  :governance-constraint/rationale
  "System only needs to check availability, not modify calendars",
  :governance-constraint/google-oauth-scope
  "https://www.googleapis.com/auth/calendar.readonly",
  :governance-constraint/user-benefit
  "Reduced security risk - system cannot accidentally modify events",
  :governance-constraint/alternative-rejected
  "Read-write calendar access",
  :governance-constraint/why-rejected
  "Violates least privilege, increases security surface area",
  :atlas/type :atlas/governance-constraint},
 #{:protocol/calendar-availability :atlas/interface-protocol}
 {:atlas/dev-id :protocol/calendar-availability,
  :interface-protocol/functions
  [:protocol.calendar-availability/check-availability
   :protocol.calendar-availability/list-events
   :protocol.calendar-availability/get-events-in-range],
  :atlas/type :atlas/interface-protocol},
 #{:protocol/calendar-availability
   :service/calendar-api
   :integration/external
   :atlas/structure-component
   :tier/foundation
   :domain/google}
 {:atlas/dev-id :component/gcal-client,
  :structure-component/deps #{:component/google-oauth},
  :atlas/type :atlas/structure-component},
 #{:protocol/oauth :atlas/business-pattern :domain/google}
 {:atlas/dev-id :pattern/oauth-token-refresh,
  :business-pattern/principle
  "Maintain continuous calendar access without re-prompting users",
  :business-pattern/justification
  "Access tokens expire hourly, re-authentication would disrupt workflow",
  :business-pattern/experience-journey
  "Seamless calendar access - users don't notice token refreshes",
  :business-pattern/failure-recovery
  :failure-mode/expired-refresh-token,
  :business-pattern/business-value
  "Reduced friction, continuous service availability",
  :atlas/type :atlas/business-pattern},
 #{:domain/admin
   :tier/api
   :constraint/admin-only-user-management
   :value/centralized-user-management
   :atlas/interface-endpoint
   :role/administrator
   :operation/list
   :domain/users}
 {:atlas/dev-id :endpoint/admin-list-users,
  :interface-endpoint/context [:admin/requester-email],
  :interface-endpoint/response [:users/list],
  :interface-endpoint/deps
  #{:fn/list-all-authorized-users :fn/check-user-is-admin},
  :atlas/type :atlas/interface-endpoint},
 #{:protocol/oauth :atlas/experience-journey :domain/google}
 {:atlas/dev-id :experience/calendar-access-expired,
  :experience-journey/user-journey
  ["User hasn't used system in 6+ months"
   "Token expired"
   "User tries to check availability"
   "Sees 'Calendar access expired'"
   "Clicks 'Reconnect'"
   "Re-grants permission"
   "Access restored"],
  :experience-journey/risk-failure-mode
  :failure-mode/expired-refresh-token,
  :experience-journey/user-sentiment
  "Minor inconvenience, understands security necessity",
  :experience-journey/recovery-time "1 minute",
  :atlas/type :atlas/experience-journey},
 #{:tier/service
   :domain/scheduling
   :effect/pure
   :atlas/execution-function
   :operation/check}
 {:atlas/dev-id :fn/check-user-is-free,
  :execution-function/context
  [:calendar/events :query/start-time :query/end-time],
  :execution-function/response [:scheduling/is-free?],
  :execution-function/deps #{},
  :atlas/type :atlas/execution-function},
 #{:experience/calendar-access-expired
   :domain/auth
   :tier/api
   :role/team-member
   :atlas/interface-endpoint
   :role/administrator}
 {:atlas/dev-id :endpoint/calendar-access-status,
  :interface-endpoint/context [:user/email],
  :interface-endpoint/response [:user/has-calendar-access?],
  :interface-endpoint/deps #{:fn/check-calendar-access-granted},
  :atlas/type :atlas/interface-endpoint},
 #{:operation/list-events
   :tier/api
   :role/scheduler
   :atlas/interface-endpoint
   :domain/scheduling
   :constraint/calendar-permission-required
   :role/administrator}
 {:atlas/dev-id :endpoint/calendar-events-list,
  :interface-endpoint/context
  [:user/email :query/start-date :query/end-date],
  :interface-endpoint/response [:calendar/events],
  :interface-endpoint/deps #{:fn/list-user-calendar-events},
  :atlas/type :atlas/interface-endpoint},
 #{:constraint/read-only-calendar-access
   :tier/service
   :integration/external
   :failure-mode/network-timeout-google-api
   :domain/scheduling
   :atlas/execution-function}
 {:atlas/dev-id :fn/check-user-availability,
  :execution-function/context [:query/date :oauth/access-token],
  :execution-function/response [:scheduling/available?],
  :execution-function/deps #{:component/gcal-client},
  :atlas/type :atlas/execution-function},
 #{:domain/http :atlas/data-schema}
 {:data-schema/fields
  [:http/html
   :http/redirect-url
   :http/status
   :health/status
   :user/session-token],
  :atlas/dev-id :schema/http-responses,
  :atlas/type :atlas/data-schema},
 #{:atlas/data-schema :domain/users}
 {:data-schema/fields
  [:user/id
   :user/email
   :user/name
   :user/phone
   :user/language
   :user/languages
   :user/picture
   :user/is-admin?
   :user/gcal-refresh-token
   :user/has-calendar-access?
   :user/has-refresh-token?],
  :atlas/dev-id :schema/users,
  :atlas/type :atlas/data-schema},
 #{:experience/first-time-user-onboarding
   :experience/calendar-access-expired
   :domain/auth
   :tier/api
   :role/team-member
   :atlas/interface-endpoint
   :pattern/explicit-consent
   :role/administrator
   :protocol/oauth
   :value/secure-calendar-integration}
 {:atlas/dev-id :endpoint/oauth-initiate,
  :interface-endpoint/context [:user/email],
  :interface-endpoint/response [:oauth/authorization-url],
  :interface-endpoint/deps #{:fn/generate-oauth-authorization-url},
  :atlas/type :atlas/interface-endpoint},
 #{:domain/scheduling :atlas/business-pattern}
 {:atlas/dev-id :pattern/language-based-routing,
  :business-pattern/principle
  "Match resources to requirements by language capability",
  :business-pattern/justification
  "Client meetings require team members who speak the client's language",
  :business-pattern/experience-journey
  "Filter available users by language preference",
  :business-pattern/business-value
  "Faster resource allocation, better client experience",
  :business-pattern/metrics-improved
  ["time-to-schedule" "client-satisfaction"],
  :atlas/type :atlas/business-pattern},
 #{:domain/admin :atlas/data-schema}
 {:data-schema/fields [:admin/requester-email :operation/success?],
  :atlas/dev-id :schema/admin-operations,
  :atlas/type :atlas/data-schema},
 #{:domain/auth
   :tier/service
   :atlas/execution-function
   :domain/google}
 {:atlas/dev-id :fn/check-calendar-access-granted,
  :execution-function/context [:user/email],
  :execution-function/response [:user/has-calendar-access?],
  :execution-function/deps #{:component/db},
  :atlas/type :atlas/execution-function}})


(def types
  [   {:registry-definition/for :semantic-namespace/flow-data-flow
    :registry-definition/keys [:flow-data-flow/source
                               :flow-data-flow/destination
                               :flow-data-flow/side-effect
                               :flow-data-flow/versioning]}
      {:registry-definition/for :semantic-namespace/visual-feature
    :registry-definition/keys [:visual-feature/encoding-rules
                               :visual-feature/purpose
                               :visual-feature/shape
                               :visual-feature/shown-aspects
                               :visual-feature/max-badges
                               :visual-feature/format
                               :visual-feature/label-rules
                               :visual-feature/styling
                               :visual-feature/layout-modes]}

   {:registry-definition/for :semantic-namespace/state-derived
    :registry-definition/keys [:state-derived/derives-from
                               :state-derived/output
                               :state-derived/consumed-by
                               :state-derived/transformation
                               :state-derived/business-pattern]}
   {:registry-definition/for :semantic-namespace/state-atom
    :registry-definition/keys [:state-atom/fields]}
   {:registry-definition/for :semantic-namespace/interaction-intent
    :registry-definition/keys [:interaction-intent/triggers
                               :interaction-intent/state-mutation
                               :interaction-intent/side-effect
                               :interaction-intent/visual-feedback]}
   {:registry-definition/for :semantic-namespace/interaction-event
    :registry-definition/keys [:interaction-event/description]}

   {:registry-definition/for :semantic-namespace/presentation-view
    :registry-definition/keys [:presentation-view/composes
                               :presentation-view/visual-purpose]}])
