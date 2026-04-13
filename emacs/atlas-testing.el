;;; atlas-testing.el --- Test template generation for Atlas entities -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; Author: @tangrammer + LLMs

;;; Commentary:
;;
;; Insert test templates for Atlas entities via dropdown selection.
;; Requires a running CIDER connection with atlas.ide loaded.
;;
;; Commands:
;;   atlas-insert-exec-fn-test  — select an execution-function, insert REPL template

;;; Code:

(require 'atlas-core)
(require 'atlas-completion)

(defun atlas-testing--exec-fn-candidates ()
  "Get all execution-function dev-ids for completion."
  (let* ((result (atlas--eval "(list-entities-of-type :atlas/execution-function)"))
         (entries (when result (atlas--to-list result))))
    (mapcar (lambda (e)
              (let ((s (atlas--to-string e)))
                (if (string-prefix-p ":" s)
                    (substring s 1)
                  s)))
            entries)))

(defun atlas-testing--completing-read-exec-fn (prompt)
  "Read an execution-function dev-id with completion using PROMPT."
  (let* ((candidates (atlas-testing--exec-fn-candidates))
         (choice (completing-read prompt candidates nil t)))
    (if (string-prefix-p ":" choice)
        choice
      (concat ":" choice))))

;;;###autoload
(defun atlas-insert-exec-fn-test ()
  "Select an execution-function by dev-id and insert a REPL test template at point.

The template includes:
- Components from integrant dev/*system* (editable)
- Dep impls from registry :atlas/impl (editable)
- Sample context values
- The executor/execute call"
  (interactive)
  (let* ((dev-id (atlas-testing--completing-read-exec-fn "Exec-fn to test: "))
         (form (format "(exec-fn-test-template %s)" dev-id))
         (template (atlas--eval form)))
    (if template
        (let ((text (if (stringp template) template (atlas--to-string template))))
          ;; Strip surrounding quotes if EDN string
          (when (and (> (length text) 1)
                     (string-prefix-p "\"" text)
                     (string-suffix-p "\"" text))
            (setq text (substring text 1 -1))
            ;; Unescape newlines and quotes
            (setq text (replace-regexp-in-string "\\\\n" "\n" text))
            (setq text (replace-regexp-in-string "\\\\\"" "\"" text)))
          (insert text)
          (message "Test template inserted for %s" dev-id))
      (message "Failed to generate template for %s" dev-id))))

(provide 'atlas-testing)
;;; atlas-testing.el ends here
