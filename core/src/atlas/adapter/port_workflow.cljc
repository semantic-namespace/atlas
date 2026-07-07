(ns atlas.adapter.port-workflow
  "EXPERIMENTAL (see EXPERIMENTAL.md) — execution bridge for
   port.workflow/producer multimethods.

   port.workflow/producer dispatch convention:
     (port.workflow/producer dispatch-key opts deps)
       -> [signal seconds-to-delay result]

   This differs from the ig/init-key pattern (see atlas.adapter.tilakone/exec)
   where init-key returns a producer-fn that is called separately with context.

   Usage:
     :atlas/impl (atlas.adapter.port-workflow/exec ::spec.workflow.lbi/search-instant)")

(defn exec
  "Build an :atlas/impl function that delegates to a port.workflow/producer
   multimethod.

   dispatch-key: the defmethod dispatch value
                 (e.g. :spec.workflow.lbi/search-instant)

   Returns a (fn [deps context]) that:
     1. Resolves port.workflow/producer lazily (no compile-time dep)
     2. Calls (port.workflow/producer dispatch-key context deps)
     3. Zipmaps result with [:workflow/signal :async/seconds-to-delay dispatch-key]

   Result map shape: {:workflow/signal    :spec.workflow/success
                      :async/seconds-to-delay 0
                      <dispatch-key>      <result-value>}"
  [dispatch-key]
  (let [response-keys [:workflow/signal :async/seconds-to-delay dispatch-key]]
    (fn [args]
      (let [producer (requiring-resolve 'accounts-by-email.boundaries.port.workflow/producer)
            result   (producer dispatch-key args args)]
        (zipmap response-keys result)))))


