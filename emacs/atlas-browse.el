;;; atlas-browse.el --- Browse commands for Atlas IDE support -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; Author: @tangrammer + LLMs

;;; Commentary:
;;
;; Basic browse commands for Atlas Emacs integration:
;; - List entities and aspects
;; - Entity info and data flow
;; - Dependencies and dependents
;; - Producers and consumers
;; - Invariant checking

;;; Code:

(require 'atlas-core)
(require 'atlas-display)
(require 'atlas-completion)

;;;###autoload
(defun atlas-browse-list-entities ()
  "List all registered semantic entities."
  (interactive)
  (atlas--invalidate-cache)
  (let* ((entities (atlas--get-entities-with-metadata))
         (buf (atlas--buffer "entities")))
    (when atlas-debug
      (message "[atlas DEBUG] Received %d entities" (length entities))
      (when (> (length entities) 0)
        (message "[atlas DEBUG] First entity: %S" (elt entities 0))
        (message "[atlas DEBUG] First entity type: %s"
                 (type-of (elt entities 0)))))
    (with-current-buffer buf
      (setq atlas--last-command #'atlas-browse-list-entities)
      (atlas--insert-header "Registered Entities")
      (if (or (not entities) (= 0 (length entities)))
          (insert (propertize "  No entities found.\n  Make sure you've initialized the registry (e.g., (app/init-registry!))\n"
                              'face 'font-lock-comment-face))
        (let* ((entities-list (atlas--to-list entities))
               (by-type (seq-group-by
                         (lambda (e)
                           (let ((entity-type (or (atlas--get e 'entity/type)
                                                  (atlas--get e 'type))))
                             (when atlas-debug
                               (message "[atlas DEBUG] Entity type result: %S for entity: %S"
                                        entity-type e))
                             (or entity-type 'unknown)))
                         entities-list)))
          (when atlas-debug
            (message "[atlas DEBUG] Grouped by type: %S" (mapcar #'car by-type)))
          (dolist (type '(endpoint function component protocol
                          business-pattern constraint failure-mode
                          value-proposition user-role user-experience
                          other unknown))
            (let ((items (or (cdr (assq type by-type))
                            (cdr (assq (intern (concat ":" (symbol-name type))) by-type)))))
              (when items
                (let ((display-name (pcase type
                                      ('business-pattern "Business Patterns")
                                      ('constraint "Constraints")
                                      ('failure-mode "Failure Modes")
                                      ('value-proposition "Value Propositions")
                                      ('user-role "User Roles")
                                      ('user-experience "User Experiences")
                                      (_ (format "%ss" (capitalize (symbol-name type)))))))
                  (atlas--insert-subheader
                   (format "%s (%d)" display-name (length items)))
                  (dolist (item items)
                    (insert "  ")
                    (let ((dev-id (atlas--get item 'entity/dev-id)))
                      (when atlas-debug
                        (message "[atlas DEBUG] Extracted dev-id: %S from item: %S" dev-id item))
                      (atlas--insert-entity dev-id))
                    (insert "\n"))
                  (insert "\n")))))))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun atlas-browse-list-aspects ()
  "List all semantic aspects with usage counts."
  (interactive)
  (atlas--invalidate-cache)
  (let* ((aspects (atlas--get-aspects-with-counts))
         (buf (atlas--buffer "aspects")))
    (if (not aspects)
        (message "No aspects found or error retrieving aspects")
      (with-current-buffer buf
        (setq atlas--last-command #'atlas-browse-list-aspects)
        (atlas--insert-header "Semantic Aspects")
        (let ((aspects-list (if (vectorp aspects) (append aspects nil) aspects)))
          (when atlas-debug
            (message "[atlas DEBUG] aspects-list length: %d" (length aspects-list))
            (when (> (length aspects-list) 0)
              (message "[atlas DEBUG] First aspect raw: %S" (car aspects-list))
              (message "[atlas DEBUG] First aspect type: %s" (type-of (car aspects-list)))))
          (dolist (a aspects-list)
            (let ((aspect-val (or (atlas--get a 'aspect/aspect)
                                  (atlas--get a 'aspect)
                                  a)))
              (when atlas-debug
                (message "[atlas DEBUG] aspect item: %S" a)
                (message "[atlas DEBUG] extracted aspect: %S" aspect-val))
              (insert "  ")
              (atlas--insert-aspect aspect-val)
              (insert (propertize (format " (%d)\n" (or (atlas--get a 'aspect/count)
                                                         (atlas--get a 'count) 0))
                                  'face 'atlas-annotation-face)))))
        (goto-char (point-min))
        (read-only-mode 1))
      (pop-to-buffer buf))))

;;;###autoload
(defun atlas-browse-find-by-aspect (aspect)
  "Find all entities with ASPECT."
  (interactive
   (list (atlas--completing-read-aspect "Aspect: ")))
  (let* ((aspect-kw (atlas--to-keyword aspect))
         (entities (atlas--eval-safe
                    (format "(entities-with-aspect %s)" aspect-kw) []))
         (buf (atlas--buffer (format "aspect:%s" aspect))))
    (with-current-buffer buf
      (setq atlas--last-command (lambda () (atlas-browse-find-by-aspect aspect)))
      (atlas--insert-header (format "Entities with :%s" aspect))
      (let ((entities-list (if (vectorp entities) (append entities nil) entities)))
        (if (and entities-list (> (length entities-list) 0))
            (dolist (entity entities-list)
              (insert "  ")
              (atlas--insert-entity entity)
              (insert "\n"))
          (insert "  (none found)\n")))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun atlas-browse-entity-info (entity)
  "Show detailed info for ENTITY."
  (interactive
   (list (atlas--completing-read-entity "Entity: ")))
  (let* ((entity-kw (atlas--to-keyword entity))
         (info (atlas--eval-safe (format "(entity-info %s)" entity-kw)))
         (buf (atlas--buffer (format "entity:%s" entity))))
    (with-current-buffer buf
      (setq atlas--last-command (lambda () (atlas-browse-entity-info entity)))
      (atlas--insert-header (format "Entity: %s" entity))
      (if (not info)
          (insert "  (no info available or error)\n")
        ;; Aspects
        (when-let ((aspects (or (atlas--get info 'entity/aspects)
                                (atlas--get info 'aspects))))
          (atlas--insert-subheader "Aspects")
          (let ((aspects-list (if (vectorp aspects) (append aspects nil) aspects)))
            (dolist (aspect aspects-list)
              (insert "  ")
              (atlas--insert-aspect aspect)
              (insert "\n")))
          (insert "\n"))

        ;; Properties
        (when-let ((definition-values (or (atlas--get info 'entity/definition-values)
                                          (atlas--get info 'definition-values)
                                          (atlas--get info 'entity/props)
                                          (atlas--get info 'props))))
          (let* ((entries (atlas--map-entries definition-values))
                 (entries (seq-filter (lambda (pair) (not (null (cdr pair)))) entries)))
            (when entries
              (dolist (pair entries)
                (atlas--insert-subheader (atlas--to-string (car pair)))
                (atlas--insert-property-value (cdr pair))
                (insert "\n"))))))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun atlas-browse-data-flow (entity)
  "Show data flow for ENTITY."
  (interactive
   (list (atlas--completing-read-entity "Function: ")))
  (let* ((entity-kw (atlas--to-keyword entity))
         (flow (atlas--eval-safe (format "(data-flow %s)" entity-kw)))
         (buf (atlas--buffer (format "flow:%s" entity))))
    (with-current-buffer buf
      (setq atlas--last-command (lambda () (atlas-browse-data-flow entity)))
      (atlas--insert-header (format "Data Flow: %s" entity))
      (let ((flow-list (atlas--to-list flow)))
        (if (and flow-list (> (length flow-list) 0))
            (dolist (item flow-list)
              (let ((needs (or (atlas--get item 'dataflow/needs)
                               (atlas--get item 'needs)))
                    (produced-by (or (atlas--get item 'dataflow/produced-by)
                                     (atlas--get item 'produced-by)))
                    (satisfied (or (atlas--get item 'dataflow/satisfied?)
                                   (atlas--get item 'satisfied?))))
                (insert "  ")
                (atlas--insert-data-key needs)
                (insert " <- ")
                (let ((produced-list (atlas--to-list produced-by)))
                  (if (and produced-list (> (length produced-list) 0))
                      (progn
                        (atlas--insert-entity (car produced-list))
                        (insert (if satisfied
                                    (propertize " [ok]" 'face 'atlas-success-face)
                                  (propertize " [?]" 'face 'atlas-warning-face))))
                    (insert (propertize "(endpoint input)" 'face 'font-lock-comment-face))))
                (insert "\n")))
          (insert "  (no data flow info)\n")))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun atlas-browse-check-invariants ()
  "Run invariant validation and show results."
  (interactive)
  (let* ((result (atlas--eval-safe "(check-invariants)"))
         (buf (atlas--buffer "invariants")))
    (with-current-buffer buf
      (setq atlas--last-command #'atlas-browse-check-invariants)
      (atlas--insert-header "Axiom Validation")
      (if (not result)
          (insert (propertize "[ERROR] Could not check invariants\n"
                              'face 'atlas-error-face))
        (if (atlas--get result 'valid?)
            (insert (propertize "[PASS] All invariants pass\n\n"
                                'face 'atlas-success-face))
          (insert (propertize "[FAIL] Validation failed\n\n"
                              'face 'atlas-error-face)))

        (when-let ((errors (atlas--get result 'errors)))
          (let ((errors-list (atlas--to-list errors)))
            (when (> (length errors-list) 0)
              (atlas--insert-subheader "Errors")
              (dolist (e errors-list)
                (insert (propertize "  [X] " 'face 'atlas-error-face))
                (insert (format "%s\n" (atlas--get e 'invariant)))
                (insert (format "      %s\n" (atlas--get e 'message))))
              (insert "\n"))))

        (when-let ((warnings (atlas--get result 'warnings)))
          (let ((warnings-list (atlas--to-list warnings)))
            (when (> (length warnings-list) 0)
              (atlas--insert-subheader "Warnings")
              (dolist (w warnings-list)
                (insert (propertize "  [!] " 'face 'atlas-warning-face))
                (insert (format "%s\n" (atlas--get w 'invariant)))
                (insert (format "      %s\n" (atlas--get w 'message))))
              (insert "\n")))))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun atlas-browse-dependents (entity)
  "Find what depends on ENTITY."
  (interactive
   (list (atlas--completing-read-entity "Entity: ")))
  (let* ((entity-kw (atlas--to-keyword entity))
         (deps (atlas--eval-safe (format "(dependents-of %s)" entity-kw) []))
         (buf (atlas--buffer (format "dependents:%s" entity))))
    (with-current-buffer buf
      (setq atlas--last-command (lambda () (atlas-browse-dependents entity)))
      (atlas--insert-header (format "Dependents of %s" entity))
      (insert (propertize "What depends on this entity:\n\n"
                          'face 'font-lock-comment-face))
      (let ((deps-list (atlas--to-list deps)))
        (if (and deps-list (> (length deps-list) 0))
            (dolist (d deps-list)
              (insert "  ")
              (atlas--insert-entity d)
              (insert "\n"))
          (insert "  (nothing depends on this)\n")))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun atlas-browse-dependencies (entity)
  "Find ENTITY's dependencies."
  (interactive
   (list (atlas--completing-read-entity "Entity: ")))
  (let* ((entity-kw (atlas--to-keyword entity))
         (deps (atlas--eval-safe (format "(dependencies-of %s)" entity-kw) []))
         (buf (atlas--buffer (format "deps:%s" entity))))
    (with-current-buffer buf
      (setq atlas--last-command (lambda () (atlas-browse-dependencies entity)))
      (atlas--insert-header (format "Dependencies of %s" entity))
      (insert (propertize "What this entity depends on:\n\n"
                          'face 'font-lock-comment-face))
      (let ((deps-list (atlas--to-list deps)))
        (if (and deps-list (> (length deps-list) 0))
            (dolist (d deps-list)
              (insert "  ")
              (atlas--insert-entity d)
              (insert "\n"))
          (insert "  (no dependencies)\n")))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun atlas-browse-producers (data-key)
  "Find functions that produce DATA-KEY."
  (interactive
   (list (atlas--completing-read-data-key "Data key: ")))
  (let* ((data-kw (atlas--to-keyword data-key))
         (producers (atlas--eval-safe (format "(producers-of %s)" data-kw) []))
         (buf (atlas--buffer (format "producers:%s" data-key))))
    (with-current-buffer buf
      (setq atlas--last-command (lambda () (atlas-browse-producers data-key)))
      (atlas--insert-header (format "Producers of %s" data-key))
      (let ((producers-list (atlas--to-list producers)))
        (if (and producers-list (> (length producers-list) 0))
            (dolist (p producers-list)
              (insert "  ")
              (atlas--insert-entity p)
              (insert "\n"))
          (insert "  (no producers - endpoint input?)\n")))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun atlas-browse-consumers (data-key)
  "Find functions that consume DATA-KEY."
  (interactive
   (list (atlas--completing-read-data-key "Data key: ")))
  (let* ((data-kw (atlas--to-keyword data-key))
         (consumers (atlas--eval-safe (format "(consumers-of %s)" data-kw) []))
         (buf (atlas--buffer (format "consumers:%s" data-key))))
    (with-current-buffer buf
      (setq atlas--last-command (lambda () (atlas-browse-consumers data-key)))
      (atlas--insert-header (format "Consumers of %s" data-key))
      (let ((consumers-list (atlas--to-list consumers)))
        (if (and consumers-list (> (length consumers-list) 0))
            (dolist (c consumers-list)
              (insert "  ")
              (atlas--insert-entity c)
              (insert "\n"))
          (insert "  (no consumers)\n")))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun atlas-browse-execution-order ()
  "Show topologically sorted execution order."
  (interactive)
  (let* ((order (atlas--eval-safe "(execution-order)" []))
         (buf (atlas--buffer "execution-order")))
    (with-current-buffer buf
      (setq atlas--last-command #'atlas-browse-execution-order)
      (atlas--insert-header "Execution Order (by data flow)")
      (let ((order-list (atlas--to-list order)))
        (if (and order-list (> (length order-list) 0))
            (let ((n 1))
              (dolist (entity order-list)
                (insert (format "  %d. " n))
                (atlas--insert-entity entity)
                (insert "\n")
                (setq n (1+ n))))
          (insert "  (no execution order available)\n")))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun atlas-browse-system-summary ()
  "Show system overview."
  (interactive)
  (let* ((summary (atlas--eval-safe "(system-summary)"))
         (buf (atlas--buffer "summary")))
    (with-current-buffer buf
      (setq atlas--last-command #'atlas-browse-system-summary)
      (atlas--insert-header "System Summary")
      (if (not summary)
          (insert "  (no summary available)\n")
        (insert (format "%s\n\n" (atlas--get summary 'summary)))
        (when-let ((domains (atlas--get summary 'domains)))
          (atlas--insert-subheader "Domains")
          (let ((domains-list (atlas--to-list domains)))
            (dolist (d domains-list)
              (insert "  ")
              (atlas--insert-aspect d)
              (insert "\n")))))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun atlas-browse-generate-docs ()
  "Generate and display markdown documentation."
  (interactive)
  (let* ((md (atlas--eval-safe "(generate-markdown)"))
         (buf (atlas--buffer "docs.md")))
    (with-current-buffer buf
      (if (not md)
          (insert "Error generating documentation\n")
        (insert md))
      (goto-char (point-min))
      (when (fboundp 'markdown-mode)
        (markdown-mode))
      (read-only-mode 1))
    (pop-to-buffer buf)))

(provide 'atlas-browse)
;;; atlas-browse.el ends here
