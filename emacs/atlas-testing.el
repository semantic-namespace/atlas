;;; atlas-testing.el --- Test template generation for Atlas entities -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; Author: @tangrammer + LLMs

;;; Commentary:
;;
;; Insert test-case templates for Atlas entities via dropdown selection.
;; Templates generate `registry/register!` calls for `:atlas/test-case`
;; entities that can be run via the runner and Kaocha.
;;
;; Requires a running CIDER connection with atlas.ide loaded.
;;
;; Commands:
;;   atlas-insert-exec-fn-test   — select an execution-function, insert test-case
;;   atlas-insert-endpoint-test  — select an endpoint, insert test-case

;;; Code:

(require 'atlas-core)
(require 'atlas-completion)

;; ============================================================================
;; Shared helpers
;; ============================================================================

(defun atlas-testing--candidates-for-type (entity-type)
  "Get all dev-ids of ENTITY-TYPE for completion."
  (let* ((result (atlas--eval (format "(list-entities-of-type %s)" entity-type)))
         (entries (when result (atlas--to-list result))))
    (mapcar (lambda (e)
              (let ((s (atlas--to-string e)))
                (if (string-prefix-p ":" s)
                    (substring s 1)
                  s)))
            entries)))

(defun atlas-testing--completing-read (prompt entity-type)
  "Read a dev-id of ENTITY-TYPE with completion using PROMPT."
  (let* ((candidates (atlas-testing--candidates-for-type entity-type))
         (choice (completing-read prompt candidates nil t)))
    (if (string-prefix-p ":" choice)
        choice
      (concat ":" choice))))

(defun atlas-testing--insert-template (dev-id form-fn label)
  "Insert template for DEV-ID using FORM-FN, labelled LABEL."
  (let* ((form (funcall form-fn dev-id))
         (template (atlas--eval form)))
    (if template
        (let ((text (atlas--unescape-template template)))
          (insert text)
          (message "%s test-case inserted for %s" label dev-id))
      (message "Failed to generate template for %s" dev-id))))

;; ============================================================================
;; Execution-function test
;; ============================================================================

;;;###autoload
(defun atlas-insert-exec-fn-test ()
  "Select an execution-function and insert a test-case registration at point.

The template registers an :atlas/test-case with:
- Target exec-fn
- Fixture with sample context values
- Result and trace expectations
- Runner invocation"
  (interactive)
  (let ((dev-id (atlas-testing--completing-read
                 "Exec-fn to test: " ":atlas/execution-function")))
    (atlas-testing--insert-template
     dev-id
     (lambda (id) (format "(exec-fn-test-template %s)" id))
     "Exec-fn")))

;; ============================================================================
;; Endpoint test
;; ============================================================================

;;;###autoload
(defun atlas-insert-endpoint-test ()
  "Select an endpoint and insert a test-case registration at point.

The template registers an :atlas/test-case with:
- Target endpoint
- Fixture with sample context values
- Mocks for structure-component deps
- Result and trace expectations
- Runner invocation with integrant config"
  (interactive)
  (let ((dev-id (atlas-testing--completing-read
                 "Endpoint to test: " ":atlas/interface-endpoint")))
    (atlas-testing--insert-template
     dev-id
     (lambda (id)
       (format "(do (require '[atlas.ontology.interface-endpoint.llm-ide :as ep-ide] :reload) (:endpoint/template ((:atlas/impl (atlas.registry.lookup/props-for :atlas.llm-ide/endpoint-test-template)) {:entity/dev-id %s})))" id))
     "Endpoint")))

(provide 'atlas-testing)
;;; atlas-testing.el ends here
