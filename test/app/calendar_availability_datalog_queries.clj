(ns app.calendar-availability-datalog-queries
  "Demonstration of powerful datalog queries for the calendar availability system.

  This showcases protocol-oriented semantic queries that reveal architectural insights."
  (:require
   [atlas.datalog :as dlog]
   [app.calendar-availability :as app]
   [atlas.registry :as cid]
   [clojure.pprint :refer [pprint]]))

(defn reset-and-init! []
  (reset! cid/registry {})
  (app/init-registry!))

(comment
  ;; Initialize the registry
  (reset-and-init!)

  ;; Create the datalog database
  (def db (dlog/create-db))

  ;; =============================================================================
  ;; QUERY 1: Protocol Implementers by Domain
  ;; Shows which domains use which protocols
  ;; =============================================================================

  (dlog/query-protocol-implementers-by-domain db)
  ;; => {:domain/google
  ;;     {:components [:component/google-oauth :component/gcal-client]
  ;;      :protocols [:protocol/oauth :protocol/calendar-availability]}
  ;;     :domain/users
  ;;     {:components [:component/db]
  ;;      :protocols [:protocol/user-repository]}}

  ;; Insight: We can see the Google domain uses 2 protocols, Users domain uses 1


  ;; =============================================================================
  ;; QUERY 2: Functions Using OAuth
  ;; Find all functions that depend on OAuth (transitively)
  ;; =============================================================================

  (dlog/query-functions-using-protocol db :protocol/oauth)
  ;; => [:fn/refresh-oauth-token]

  ;; Insight: Only one function directly uses OAuth. This is good - centralized OAuth handling.


  ;; =============================================================================
  ;; QUERY 3: Protocol Usage Matrix
  ;; Shows which functions use which protocols through their component dependencies
  ;; =============================================================================

  (dlog/query-protocol-usage-matrix db)
  ;; => [{:function :fn/find-users-by-language, :protocols [:protocol/user-repository]}
  ;;     {:function :fn/refresh-oauth-token, :protocols [:protocol/oauth]}
  ;;     {:function :fn/check-user-availability, :protocols [:protocol/calendar-availability]}]

  ;; Insight: Clean separation - each function uses exactly one protocol
  ;; This shows good single-responsibility design


  ;; =============================================================================
  ;; QUERY 4: External Integration Points
  ;; Find all components that integrate with external services
  ;; =============================================================================

  (dlog/query-integration-points db)
  ;; => ([[:component/google-oauth :domain/google :protocol/oauth]
  ;;      [:component/gcal-client :domain/google :protocol/calendar-availability]])

  ;; Insight: Both external integrations are in the Google domain
  ;; This shows domain cohesion


  ;; =============================================================================
  ;; QUERY 5: Pure vs Impure Functions
  ;; Categorize functions by whether they have component dependencies
  ;; =============================================================================

  (dlog/query-pure-vs-impure-functions db)
  ;; => {:pure [:fn/collect-available-users]
  ;;     :impure [:fn/find-users-by-language
  ;;              :fn/refresh-oauth-token
  ;;              :fn/check-user-availability]}

  ;; Insight: We have 1 pure orchestration function and 3 impure functions
  ;; The pure function coordinates the impure ones - good architectural layering


  ;; =============================================================================
  ;; QUERY 6: Protocol Dependency Graph
  ;; Shows which protocols depend on which other protocols
  ;; =============================================================================

  (dlog/query-protocol-dependency-graph db)
  ;; => [{:protocol :protocol/calendar-availability
  ;;      :depends-on [:protocol/oauth]}]

  ;; Insight: Calendar availability protocol depends on OAuth protocol
  ;; This makes sense - you need OAuth to access calendar APIs
  ;; No circular dependencies (good!)


  ;; =============================================================================
  ;; QUERY 7: Aspect Co-occurrence
  ;; Find architectural patterns by seeing which aspects appear together
  ;; =============================================================================

  (take 10 (dlog/query-aspect-co-occurrence db))
  ;; => [{:aspect-a :domain/google, :aspect-b :integration/external, :count 2}
  ;;     {:aspect-a :integration/external, :aspect-b :tier/foundation, :count 2}
  ;;     {:aspect-a :domain/google, :aspect-b :tier/foundation, :count 2}
  ;;     ...]

  ;; Insight: Common patterns emerge:
  ;; - :domain/google + :integration/external appear together often
  ;; - External integrations are always foundation tier
  ;; This reveals our architectural conventions


  ;; =============================================================================
  ;; QUERY 8: Endpoint Protocol Coverage
  ;; For each endpoint, show all protocols used in its dependency tree
  ;; =============================================================================

  (dlog/query-endpoint-protocol-coverage db)
  ;; => [{:endpoint :endpoint/query-availability
  ;;      :protocols [:protocol/oauth
  ;;                  :protocol/calendar-availability
  ;;                  :protocol/user-repository]}]

  ;; Insight: The endpoint uses ALL THREE protocols through its dependency tree!
  ;; This shows the complete integration surface for this API endpoint
  ;; Useful for:
  ;; - Security review (what external services are touched?)
  ;; - Performance analysis (how many external calls?)
  ;; - Testing strategy (which protocols need mocking?)


  ;; =============================================================================
  ;; ARCHITECTURAL INSIGHTS FROM QUERIES
  ;; =============================================================================

  ;; 1. CLEAN PROTOCOL BOUNDARIES
  ;;    Each function uses exactly one protocol - good separation of concerns

  ;; 2. DOMAIN COHESION
  ;;    External integrations are grouped in the Google domain

  ;; 3. LAYERED ARCHITECTURE
  ;;    Pure function orchestrates impure functions - clear separation

  ;; 4. PROTOCOL DEPENDENCY CHAIN
  ;;    Calendar → OAuth (sensible hierarchy, no cycles)

  ;; 5. INTEGRATION SURFACE VISIBILITY
  ;;    Endpoint uses 3 protocols - we know exactly what external deps it has

  ;; 6. TESTABILITY
  ;;    Protocol usage matrix shows clear mock boundaries for testing


  ;; =============================================================================
  ;; ADVANCED QUERIES - Composition
  ;; Combine queries to answer complex questions
  ;; =============================================================================

  ;; Q: Which protocols are used by endpoints but not directly by any function?
  (let [endpoint-protocols (set (mapcat :protocols (dlog/query-endpoint-protocol-coverage db)))
        function-protocols (set (mapcat :protocols (dlog/query-protocol-usage-matrix db)))]
    (clojure.set/difference endpoint-protocols function-protocols))
  ;; => #{}  (empty - all protocols are used somewhere)

  ;; Q: What's the "integration density" per domain?
  ;;    (How many protocols per domain?)
  (let [by-domain (dlog/query-protocol-implementers-by-domain db)]
    (map (fn [[domain data]]
           {:domain domain
            :protocol-count (count (:protocols data))
            :component-count (count (:components data))
            :protocols-per-component (float (/ (count (:protocols data))
                                               (count (:components data))))})
         by-domain))
  ;; => ({:domain :domain/google, :protocol-count 2, :component-count 2, :protocols-per-component 1.0}
  ;;     {:domain :domain/users, :protocol-count 1, :component-count 1, :protocols-per-component 1.0})

  ;; Insight: Perfect 1:1 ratio - each component implements exactly one protocol
  ;; This is excellent for modularity and testing


  ;; Q: What would break if we removed OAuth?
  (let [oauth-users (dlog/query-functions-using-protocol db :protocol/oauth)
        ;; Find what depends on those functions
        find-dependents (fn [fn-id]
                         (d/q '[:find [?dep ...]
                                :in $ ?target
                                :where
                                [?e :entity/depends ?target]
                                [?e :atlas/dev-id ?dep]]
                              db fn-id))]
    {:direct oauth-users
     :transitive (mapcat find-dependents oauth-users)})
  ;; => {:direct [:fn/refresh-oauth-token], :transitive []}

  ;; Insight: Removing OAuth only breaks the refresh-token function directly
  ;; The endpoint depends on it transitively, so it would break the entire endpoint

  )
