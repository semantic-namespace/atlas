(ns app.pet-shop
  "v5.2: Refactored to use raw cid/register! calls for explicitness."
  (:require
   [atlas.registry :as registry]
   [atlas.query :as query]
   [atlas.invariant :as invariant]))

;; =============================================================================
;; SCHEMAS
;; =============================================================================

(defn- register-schemas! []
  (registry/register!

   :schema/pet

   :atlas/data-schema

   #{ :domain/inventory}
   {:atlas/dev-id :schema/pet
    :data-schema/fields [:pet/id :pet/name :pet/species :pet/breed :pet/age :pet/price :pet/status]})

  (registry/register!
   :schema/customer
   :atlas/data-schema
   #{ :domain/customers}
   {:atlas/dev-id :schema/customer
    :data-schema/fields [:customer/id :customer/email :customer/name :customer/phone :customer/loyalty-points]})

  (registry/register!
   :schema/order
   :atlas/data-schema
   #{ :domain/orders}
   {:atlas/dev-id :schema/order
    :data-schema/fields [:order/id :order/customer-id :order/items :order/total :order/status :order/created-at]})

  (registry/register!
   :schema/appointment
   :atlas/data-schema
   #{ :domain/grooming}
   {:atlas/dev-id :schema/appointment
    :data-schema/fields [:appointment/id :appointment/pet-id :appointment/datetime :appointment/service :appointment/status]}))

;; =============================================================================
;; COMPONENTS
;; =============================================================================

(defn- register-components! []
  (registry/register!

   :component/db

   :atlas/structure-component

   #{ :tier/foundation :domain/persistence :integration/internal :component.type/database}
   {:atlas/dev-id :component/db
    :structure-component/deps #{}})

  (registry/register!
   :component/cache
   :atlas/structure-component
   #{ :tier/foundation :domain/persistence :integration/internal :component.type/cache}
   {:atlas/dev-id :component/cache
    :structure-component/deps #{}})

  (registry/register!
   :component/stripe
   :atlas/structure-component
   #{ :tier/foundation :domain/payments :integration/external :temporal/async :component.type/payment-gateway :protocol/http}
   {:atlas/dev-id :component/stripe
    :structure-component/deps #{}})

  (registry/register!
   :component/twilio
   :atlas/structure-component
   #{ :tier/foundation :domain/notifications :integration/external :temporal/async :component.type/sms-provider :protocol/http}
   {:atlas/dev-id :component/twilio
    :structure-component/deps #{}})

  (registry/register!
   :component/email-service
   :atlas/structure-component
   #{ :tier/foundation :domain/notifications :integration/external :temporal/async :component.type/email-provider :protocol/smtp}
   {:atlas/dev-id :component/email-service
    :structure-component/deps #{}}))

;; =============================================================================
;; SERVICE FUNCTIONS
;; =============================================================================

(defn- register-services! []
  ;; Inventory
  (registry/register!

   :fn/list-available-pets

   :atlas/execution-function

   #{ :tier/service :domain/inventory :effect/read :operation/list}
   {:atlas/dev-id :fn/list-available-pets
    :execution-function/context [:pet/species :pet/breed]
    :execution-function/response [:inventory/available-pets]
    :execution-function/deps #{:component/db :component/cache}})

  (registry/register!
   :fn/get-pet-details
   :atlas/execution-function
   #{ :tier/service :domain/inventory :effect/read :operation/get}
   {:atlas/dev-id :fn/get-pet-details
    :execution-function/context [:pet/id]
    :execution-function/response [:pet/name :pet/species :pet/breed :pet/age :pet/price :pet/status]
    :execution-function/deps #{:component/db}})

  (registry/register!
   :fn/reserve-pet
   :atlas/execution-function
   #{ :tier/service :domain/inventory :effect/write :operation/update :operation/reserve}
   {:atlas/dev-id :fn/reserve-pet
    :execution-function/context [:pet/id :customer/id]
    :execution-function/response [:pet/status]
    :execution-function/deps #{:component/db}})

  (registry/register!
   :fn/release-reservation
   :atlas/execution-function
   #{ :tier/service :domain/inventory :effect/write :operation/update :operation/release}
   {:atlas/dev-id :fn/release-reservation
    :execution-function/context [:pet/id]
    :execution-function/response [:pet/status]
    :execution-function/deps #{:component/db}})

  ;; Customers
  (registry/register!

   :fn/get-customer

   :atlas/execution-function

   #{ :tier/service :domain/customers :effect/read :operation/get}
   {:atlas/dev-id :fn/get-customer
    :execution-function/context [:customer/id]
    :execution-function/response [:customer/email :customer/name :customer/phone :customer/loyalty-points]
    :execution-function/deps #{:component/db}})

  (registry/register!
   :fn/create-customer
   :atlas/execution-function
   #{ :tier/service :domain/customers :effect/write :operation/create}
   {:atlas/dev-id :fn/create-customer
    :execution-function/context [:customer/email :customer/name :customer/phone]
    :execution-function/response [:customer/id]
    :execution-function/deps #{:component/db}})

  (registry/register!
   :fn/update-loyalty-points
   :atlas/execution-function
   #{ :tier/service :domain/customers :effect/write :operation/update :operation/loyalty}
   {:atlas/dev-id :fn/update-loyalty-points
    :execution-function/context [:customer/id :order/total]
    :execution-function/response [:customer/loyalty-points]
    :execution-function/deps #{:component/db}})

  ;; Orders
  (registry/register!

   :fn/create-order

   :atlas/execution-function

   #{ :tier/service :domain/orders :effect/write :operation/create}
   {:atlas/dev-id :fn/create-order
    :execution-function/context [:customer/id :order/items]
    :execution-function/response [:order/id :order/total :order/status]
    :execution-function/deps #{:component/db :fn/reserve-pet}})

  (registry/register!
   :fn/process-payment
   :atlas/execution-function
   #{ :tier/service :domain/payments :effect/write :operation/process :integration/external :temporal/async}
   {:atlas/dev-id :fn/process-payment
    :execution-function/context [:order/id :order/total :payment/token]
    :execution-function/response [:payment/result :order/status]
    :execution-function/deps #{:component/stripe}})

  (registry/register!
   :fn/complete-order
   :atlas/execution-function
   #{ :tier/service :domain/orders :effect/write :operation/update :operation/complete}
   {:atlas/dev-id :fn/complete-order
    :execution-function/context [:order/id :payment/result]
    :execution-function/response [:order/status]
    :execution-function/deps #{:component/db :fn/update-loyalty-points}})

  (registry/register!
   :fn/cancel-order
   :atlas/execution-function
   #{ :tier/service :domain/orders :effect/write :operation/delete :operation/cancel}
   {:atlas/dev-id :fn/cancel-order
    :execution-function/context [:order/id]
    :execution-function/response [:order/status]
    :execution-function/deps #{:component/db :fn/release-reservation}})

  (registry/register!
   :fn/get-order-history
   :atlas/execution-function
   #{ :tier/service :domain/orders :effect/read :operation/list}
   {:atlas/dev-id :fn/get-order-history
    :execution-function/context [:customer/id]
    :execution-function/response [:order/id :order/items :order/total :order/status :order/created-at]
    :execution-function/deps #{:component/db}})

  ;; Grooming
  (registry/register!

   :fn/get-grooming-slots

   :atlas/execution-function

   #{ :tier/service :domain/grooming :effect/read :operation/list}
   {:atlas/dev-id :fn/get-grooming-slots
    :execution-function/context [:appointment/datetime :appointment/service]
    :execution-function/response [:grooming/slots]
    :execution-function/deps #{:component/db}})

  (registry/register!
   :fn/book-appointment
   :atlas/execution-function
   #{ :tier/service :domain/grooming :effect/write :operation/create}
   {:atlas/dev-id :fn/book-appointment
    :execution-function/context [:customer/id :appointment/pet-id :appointment/datetime :appointment/service]
    :execution-function/response [:appointment/id :appointment/status]
    :execution-function/deps #{:component/db}})

  (registry/register!
   :fn/cancel-appointment
   :atlas/execution-function
   #{ :tier/service :domain/grooming :effect/write :operation/delete}
   {:atlas/dev-id :fn/cancel-appointment
    :execution-function/context [:appointment/id]
    :execution-function/response [:appointment/status]
    :execution-function/deps #{:component/db}})

  ;; Notifications — DISTINCT via channel
  (registry/register!

   :fn/send-order-confirmation

   :atlas/execution-function

   #{ :tier/service :domain/notifications :effect/write :operation/send :integration/external :temporal/async :notification.channel/email}
   {:atlas/dev-id :fn/send-order-confirmation
    :execution-function/context [:customer/email :order/id :order/items :order/total]
    :execution-function/response [:notification/sent?]
    :execution-function/deps #{:component/email-service}})

  (registry/register!
   :fn/send-appointment-reminder
   :atlas/execution-function
   #{ :tier/service :domain/notifications :effect/write :operation/send :integration/external :temporal/async :notification.channel/sms}
   {:atlas/dev-id :fn/send-appointment-reminder
    :execution-function/context [:customer/phone :appointment/datetime :appointment/service]
    :execution-function/response [:notification/sent?]
    :execution-function/deps #{:component/twilio}})

  ;; Pure functions
  (registry/register!

   :fn/calculate-order-total

   :atlas/execution-function

   #{ :tier/service :domain/orders :effect/pure :operation/calculate}
   {:atlas/dev-id :fn/calculate-order-total
    :execution-function/context [:order/items :customer/loyalty-points]
    :execution-function/response [:order/total]
    :execution-function/deps #{}})

  (registry/register!
   :fn/validate-pet-availability
   :atlas/execution-function
   #{ :tier/service :domain/inventory :effect/pure :operation/validate}
   {:atlas/dev-id :fn/validate-pet-availability
    :execution-function/context [:pet/status]
    :execution-function/response [:pet/status]
    :execution-function/deps #{}}))

;; =============================================================================
;; INVARIANTS
;; =============================================================================

(defn- register-invariants! []
  (registry/register!

   :invariant/pet-lifecycle

   :semantic-namespace/invariant

   #{ :domain/inventory}
   {:atlas/dev-id :invariant/pet-lifecycle
    :description "Pet status transitions"
    :states [:available :reserved :sold]
    :transitions {:available #{:reserved}
                  :reserved #{:sold :available}
                  :sold #{}}
    :enforced-by #{:fn/reserve-pet :fn/release-reservation}
    :ux/errors
    [{:when {:pet/status :reserved}
      :code :pet-unavailable
      :title "Sorry!"
      :message "{{pet/name}} is no longer available."
      :cta {:label "Browse Other Pets" :target :endpoint/list-pets}}
     {:when {:pet/status :sold}
      :code :pet-sold
      :title "Already Adopted"
      :message "{{pet/name}} has found a home."
      :cta {:label "Browse Other Pets" :target :endpoint/list-pets}}]})

  (registry/register!
   :invariant/order-lifecycle
   :semantic-namespace/invariant
   #{ :domain/orders}
   {:atlas/dev-id :invariant/order-lifecycle
    :description "Order status transitions"
    :states [:pending :paid :fulfilled :cancelled]
    :transitions {:pending #{:paid :cancelled}
                  :paid #{:fulfilled :cancelled}
                  :fulfilled #{}
                  :cancelled #{}}
    :enforced-by #{:fn/create-order :fn/complete-order :fn/cancel-order}})

  (registry/register!
   :invariant/payment-failure
   :semantic-namespace/invariant
   #{ :domain/payments}
   {:atlas/dev-id :invariant/payment-failure
    :description "Payment failure handling"
    :enforced-by #{:fn/process-payment}
    :ux/errors
    [{:when {:payment/result :declined}
      :code :payment-declined
      :title "Payment Failed"
      :message "Your card was declined."
      :cta [{:label "Try Different Card" :action :retry}
            {:label "Cancel" :target :endpoint/list-pets}]}
     {:when {:payment/result :timeout}
      :code :payment-timeout
      :title "Processing..."
      :message "This is taking longer than expected."
      :cta {:label "Check Status Later" :target :endpoint/order-history}}]})

  (registry/register!
   :invariant/reservation-exclusivity
   :semantic-namespace/invariant
   #{ :domain/inventory :domain/orders}
   {:atlas/dev-id :invariant/reservation-exclusivity
    :description "A pet can only be reserved by one active order"
    :enforced-by #{:fn/reserve-pet :fn/release-reservation}})

  (registry/register!
   :invariant/loyalty-points
   :semantic-namespace/invariant
   #{ :domain/customers :domain/orders}
   {:atlas/dev-id :invariant/loyalty-points
    :description "1 point per $10 spent, rounded down"
    :enforced-by #{:fn/update-loyalty-points :fn/calculate-order-total}}))

;; =============================================================================
;; USER FLOWS
;; =============================================================================

(defn- register-flows! []
  (registry/register!

   :flow/purchase-pet

   :semantic-namespace/flow

   #{ :flow.type/purchase}
   {:atlas/dev-id :flow/purchase-pet
    :ux.flow/name "Pet Purchase"
    :ux.flow/entry :endpoint/get-pet
    :ux.flow/steps
    [{:id :confirm
      :title "Confirm Selection"
      :display [:pet/name :pet/species :pet/price :customer/loyalty-points :order/total]
      :cta "Continue to Payment"}
     {:id :payment
      :title "Payment"
      :input [:payment/token]
      :display [:order/total]
      :async? true
      :cta "Pay Now"}
     {:id :confirmation
      :title "Order Confirmed"
      :display [:order/id :order/status :pet/name]
      :terminal? true
      :cta {:label "View Order History" :target :endpoint/order-history}}]})

  (registry/register!
   :flow/book-grooming
   :semantic-namespace/flow
   #{ :flow.type/booking}
   {:atlas/dev-id :flow/book-grooming
    :ux.flow/name "Book Grooming"
    :ux.flow/entry :endpoint/grooming-availability
    :ux.flow/steps
    [{:id :select-slot
      :title "Choose Time"
      :input [:appointment/service :appointment/datetime]
      :display [:grooming/slots]}
     {:id :confirm
      :title "Confirm Details"
      :input [:appointment/pet-id]
      :display [:appointment/datetime :appointment/service]
      :cta "Confirm Booking"}
     {:id :booked
      :title "Appointment Booked"
      :display [:appointment/id :appointment/datetime :appointment/service]
      :terminal? true}]})

  (registry/register!
   :flow/signup
   :semantic-namespace/flow
   #{ :flow.type/onboarding}
   {:atlas/dev-id :flow/signup
    :ux.flow/name "Customer Signup"
    :ux.flow/entry :endpoint/signup
    :ux.flow/steps
    [{:id :details
      :title "Create Account"
      :input [:customer/email :customer/name :customer/phone]
      :cta "Sign Up"}
     {:id :complete
      :title "Welcome!"
      :display [:customer/id]
      :terminal? true}]}))

;; =============================================================================
;; NOTIFICATION TEMPLATES
;; =============================================================================

(defn- register-notifications! []
  (registry/register!

   :notification/order-confirmation

   :semantic-namespace/notification

   #{ :domain/orders :notification.channel/email :notification.type/confirmation}
   {:atlas/dev-id :notification/order-confirmation
    :trigger :fn/send-order-confirmation
    :subject "Your Pet Shop Order #{{order/id}}"
    :body "Hi {{customer/name}},

Thank you for your purchase!

Order: #{{order/id}}
Items: {{order/items}}
Total: {{order/total}}

Your new friend is waiting for you!

- Pet Shop Team"})

  (registry/register!
   :notification/appointment-reminder
   :semantic-namespace/notification
   #{ :domain/grooming :notification.channel/sms :notification.type/reminder}
   {:atlas/dev-id :notification/appointment-reminder
    :trigger :fn/send-appointment-reminder
    :body "Pet Shop: {{appointment/service}} on {{appointment/datetime}}. Reply CANCEL to cancel."})

  (registry/register!
   :notification/order-cancelled
   :semantic-namespace/notification
   #{ :domain/orders :notification.channel/email :notification.type/cancellation}
   {:atlas/dev-id :notification/order-cancelled
    :trigger :fn/cancel-order
    :subject "Order #{{order/id}} Cancelled"
    :body "Hi {{customer/name}},

Your order #{{order/id}} has been cancelled.

If you have questions, please contact us.

- Pet Shop Team"}))

;; =============================================================================
;; ENDPOINTS (complete coverage)
;; =============================================================================

(defn- register-endpoints! []
  ;; Public endpoints
  (registry/register!

   :endpoint/list-pets

   :atlas/interface-endpoint

   #{ :atlas/execution-function :tier/api :protocol/http :domain/inventory :operation/list :ux.visibility/public}
   {:atlas/dev-id :endpoint/list-pets
    :interface-endpoint/context [:pet/species :pet/breed]
    :interface-endpoint/response [:inventory/available-pets]
    :interface-endpoint/deps #{:fn/list-available-pets}
    :ux/copy {:title "Available Pets"
              :empty "No pets match your filters"
              :cta "View Details"}
    :ux/platform {:mobile/pattern :card-grid
                  :mobile/pull-to-refresh true
                  :mobile/infinite-scroll true}})

  (registry/register!
   :endpoint/get-pet
   :atlas/interface-endpoint
   #{ :atlas/execution-function :tier/api :protocol/http :domain/inventory :operation/get :ux.visibility/public}
   {:atlas/dev-id :endpoint/get-pet
    :interface-endpoint/context [:pet/id]
    :interface-endpoint/response [:pet/name :pet/species :pet/breed :pet/age :pet/price :pet/status]
    :interface-endpoint/deps #{:fn/get-pet-details}
    :ux/copy {:title "{{pet/name}}"
              :cta-auth "Login to Purchase"
              :cta "Purchase"}
    :ux/errors [:invariant/pet-lifecycle]
    :ux/platform {:mobile/pattern :detail-view
                  :mobile/sticky-cta true
                  :mobile/image-hero true}})

  (registry/register!
   :endpoint/grooming-availability
   :atlas/interface-endpoint
   #{ :atlas/execution-function :tier/api :protocol/http :domain/grooming :operation/list :ux.visibility/public}
   {:atlas/dev-id :endpoint/grooming-availability
    :interface-endpoint/context [:appointment/datetime :appointment/service]
    :interface-endpoint/response [:grooming/slots]
    :interface-endpoint/deps #{:fn/get-grooming-slots}
    :ux/copy {:title "Grooming Services"
              :cta-auth "Login to Book"}
    :ux/platform {:mobile/pattern :calendar-picker}})

  (registry/register!
   :endpoint/signup
   :atlas/interface-endpoint
   #{ :atlas/execution-function :tier/api :protocol/http :domain/customers :operation/create :ux.visibility/public}
   {:atlas/dev-id :endpoint/signup
    :interface-endpoint/context [:customer/email :customer/name :customer/phone]
    :interface-endpoint/response [:customer/id]
    :interface-endpoint/deps #{:fn/create-customer}
    :ux/copy {:title "Create Account"
              :success "Welcome to Pet Shop!"
              :cta "Sign Up"}
    :ux/flow :flow/signup})

  ;; Authenticated endpoints
  (registry/register!

   :endpoint/purchase-pet

   :atlas/interface-endpoint

   #{ :atlas/execution-function :tier/api :protocol/http :domain/orders :operation/create :authorization/required}
   {:atlas/dev-id :endpoint/purchase-pet
    :interface-endpoint/context [:auth/user-id :pet/id :payment/token]
    :interface-endpoint/response [:order/id :order/status]
    :interface-endpoint/deps #{:fn/get-customer
                               :fn/get-pet-details
                               :fn/validate-pet-availability
                               :fn/calculate-order-total
                               :fn/create-order
                               :fn/process-payment
                               :fn/complete-order
                               :fn/send-order-confirmation}
    :ux/copy {:title "Purchase: {{pet/name}}"
              :success "{{pet/name}} is yours! 🎉"
              :success-detail "Confirmation sent to {{customer/email}}"
              :cta "Continue to Payment"
              :cta-processing "Processing..."
              :cta-success "View Order History"}
    :ux/flow :flow/purchase-pet
    :ux/errors [:invariant/pet-lifecycle :invariant/payment-failure]
    :ux/platform {:mobile/pattern :checkout-flow
                  :mobile/native-pay [:apple-pay :google-pay]}})

  (registry/register!
   :endpoint/order-history
   :atlas/interface-endpoint
   #{ :atlas/execution-function :tier/api :protocol/http :domain/orders :operation/list :authorization/required}
   {:atlas/dev-id :endpoint/order-history
    :interface-endpoint/context [:auth/user-id]
    :interface-endpoint/response [:order/id :order/items :order/total :order/status :order/created-at]
    :interface-endpoint/deps #{:fn/get-order-history}
    :ux/copy {:title "Your Orders"
              :empty "No orders yet"}})

  (registry/register!
   :endpoint/cancel-order
   :atlas/interface-endpoint
   #{ :atlas/execution-function :tier/api :protocol/http :domain/orders :operation/delete :authorization/required}
   {:atlas/dev-id :endpoint/cancel-order
    :interface-endpoint/context [:auth/user-id :order/id]
    :interface-endpoint/response [:order/status]
    :interface-endpoint/deps #{:fn/cancel-order}
    :ux/copy {:title "Cancel Order"
              :confirm "Are you sure you want to cancel this order?"
              :success "Order cancelled"
              :cta-confirm "Yes, Cancel Order"
              :cta-cancel "Keep Order"}
    :ux/errors [:invariant/order-lifecycle]})

  (registry/register!
   :endpoint/book-grooming
   :atlas/interface-endpoint
   #{ :atlas/execution-function :tier/api :protocol/http :domain/grooming :operation/create :authorization/required}
   {:atlas/dev-id :endpoint/book-grooming
    :interface-endpoint/context [:auth/user-id :appointment/pet-id :appointment/datetime :appointment/service]
    :interface-endpoint/response [:appointment/id :appointment/status]
    :interface-endpoint/deps #{:fn/book-appointment :fn/send-appointment-reminder}
    :ux/copy {:title "Book Grooming"
              :success "Appointment Booked!"
              :success-detail "Reminder will be sent to {{customer/phone}}"
              :cta "Confirm Booking"}
    :ux/flow :flow/book-grooming})

  (registry/register!
   :endpoint/cancel-grooming
   :atlas/interface-endpoint
   #{ :atlas/execution-function :tier/api :protocol/http :domain/grooming :operation/delete :authorization/required}
   {:atlas/dev-id :endpoint/cancel-grooming
    :interface-endpoint/context [:auth/user-id :appointment/id]
    :interface-endpoint/response [:appointment/status]
    :interface-endpoint/deps #{:fn/cancel-appointment}
    :ux/copy {:title "Cancel Appointment?"
              :confirm "Are you sure you want to cancel?"
              :cta-confirm "Yes, Cancel"
              :cta-cancel "Keep It"}}))

;; =============================================================================
;; PUBLIC API
;; =============================================================================

(defn init-registry! []
  (register-schemas!)
  (register-components!)
  (register-services!)
  (register-invariants!)
  (register-flows!)
  (register-notifications!)
  (register-endpoints!))

(defn check-invariants []
  (invariant/check-all))

(defn summary []
  (let [qfn (fn [& aspects] (count (query/query-superset @registry/registry (vec aspects))))]
    {:schemas       (qfn :atlas/data-schema)
     :data-keys     (qfn :semantic-namespace/data-key)
     :components    (qfn :atlas/structure-component)
     :functions     (qfn :atlas/execution-function)
     :endpoints     (qfn :atlas/interface-endpoint)
     :invariants    (qfn :semantic-namespace/invariant)
     :flows         (qfn :semantic-namespace/flow)
     :notifications (qfn :semantic-namespace/notification)
     :external      (qfn :integration/external)
     :authed        (qfn :authorization/required)}))

;; =============================================================================
;; QUERIES
;; =============================================================================

(comment
  (init-registry!)
  (check-invariants)
  (summary)

  ;; Notification channels now distinct
  (query/query-superset @registry/registry [:notification.channel/email])  ; → send-order-confirmation only
  (query/query-superset @registry/registry [:notification.channel/sms])    ; → send-appointment-reminder only

  ;; All endpoints
  (query/query-superset @registry/registry [:atlas/interface-endpoint])
  ;; Should now include: signup, cancel-order

  ;; Verify all functions reachable
  (query/query-superset @registry/registry [:atlas/execution-function])
  ;; calculate-order-total, cancel-order, create-customer, release-reservation
  ;; all now reachable via endpoints
  )
