;;; atlas-interactive.el --- Interactive entity authoring for Atlas -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; Author: @tangrammer + LLMs

;;; Commentary:
;;
;; Interactive, guided entity authoring with:
;; - Step-by-step field completion
;; - Smart suggestions based on context
;; - Real-time similarity feedback
;; - Aspect builder with namespace grouping

;;; Code:

(require 'atlas-core)
(require 'atlas-completion)
(require 'atlas-authoring)

;;; Interactive Entity Builder State

(defvar-local atlas-interactive--state nil
  "State for interactive entity builder.
Structure: {:type :atlas/execution-function
            :dev-id :fn/my-function
            :aspects #{:domain/auth :tier/service}
            :properties {...}
            :point-start <marker>}")

(defvar-local atlas-interactive--field-overlays nil
  "List of overlays for editable fields.")

;;; Entity Type Selection

(defun atlas-interactive--get-entity-types ()
  "Get all registered entity types from the registry."
  (let* ((form "(list-entity-types)")
         (result (atlas--eval form)))
    (when result
      (mapcar #'atlas--to-string (atlas--to-list result)))))

(defun atlas-interactive--select-entity-type ()
  "Select entity type with completion."
  (let* ((types-from-registry (atlas-interactive--get-entity-types))
         ;; Fallback to common types if registry query fails
         (types (or types-from-registry
                    '(":atlas/execution-function"
                      ":atlas/interface-endpoint"
                      ":atlas/structure-component"
                      ":atlas/data-schema"
                      ":atlas/interface-protocol")))
         (type-str (completing-read "Entity type: " types nil t)))
    (if (string-prefix-p ":" type-str)
        (intern type-str)
      (intern (concat ":" type-str)))))

;;; Dev-ID Field

(defun atlas-interactive--choose-dev-id (entity-type)
  "Choose or create dev-id for ENTITY-TYPE."
  (let ((choice (completing-read
                 "Dev-id: "
                 '("Create new..." "Choose existing...")
                 nil t)))
    (cond
     ((string= choice "Create new...")
      (atlas-interactive--create-new-dev-id entity-type))
     ((string= choice "Choose existing...")
      (let ((existing (atlas--completing-read-entity "Select dev-id: ")))
        (if (string-prefix-p ":" existing)
            (intern existing)
          (intern (concat ":" existing)))))
     (t
      (intern (concat ":" choice))))))

(defun atlas-interactive--create-new-dev-id (entity-type)
  "Create new dev-id based on ENTITY-TYPE."
  (let* ((default-ns (pcase entity-type
                       (:atlas/execution-function "fn")
                       (:atlas/interface-endpoint "endpoint")
                       (:atlas/structure-component "component")
                       (:atlas/data-schema "schema")
                       (:atlas/interface-protocol "protocol")
                       (_ "entity")))
         (ns (read-string (format "Namespace [%s]: " default-ns) nil nil default-ns))
         (name (read-string (format "Name in %s/: " ns))))
    (intern (format ":%s/%s" ns name))))

;;; Aspect Builder with Similarity Feedback

(defun atlas-interactive--build-aspects-interactive ()
  "Build aspect set interactively with similarity feedback."
  (let ((aspects '())
        (done nil))
    (while (not done)
      (let* ((current-identity (cons :atlas/type aspects))
             (similarity-info (when aspects
                                (atlas-interactive--get-similarity current-identity)))
             (prompt (if similarity-info
                         (format "Add aspect (%d similar entities found, closest: %.2f): "
                                 (length similarity-info)
                                 (or (car (atlas--get (car similarity-info) 'similarity)) 0.0))
                       "Add aspect (start typing or RET to finish): "))
             (aspect-choice (completing-read prompt
                                            '("DONE" "Browse by namespace..." "View similar entities...")
                                            nil nil)))
        (cond
         ;; Finished
         ((or (string= aspect-choice "DONE") (string-empty-p aspect-choice))
          (setq done t))

         ;; Browse by namespace
         ((string= aspect-choice "Browse by namespace...")
          (let ((aspect (atlas-interactive--choose-aspect-by-namespace)))
            (when aspect
              (push aspect aspects)
              (message "Added %s (total: %d aspects)" aspect (length aspects)))))

         ;; View similar entities
         ((string= aspect-choice "View similar entities...")
          (atlas-interactive--show-similarity-popup current-identity))

         ;; Direct aspect input
         (t
          (let ((aspect (if (string-prefix-p ":" aspect-choice)
                            (intern aspect-choice)
                          (intern (concat ":" aspect-choice)))))
            (push aspect aspects)
            (message "Added %s (total: %d aspects)" aspect (length aspects)))))))
    (reverse aspects)))

(defun atlas-interactive--choose-aspect-by-namespace ()
  "Choose aspect with namespace-first selection."
  (let* ((namespaces (atlas-authoring--get-aspect-namespaces))
         (ns (completing-read "Aspect namespace: " namespaces nil t)))
    (when ns
      (let* ((names (atlas-authoring--get-aspect-names-in-namespace ns))
             (name (completing-read (format "Name in %s/: " ns) names nil t)))
        (when name
          (intern (format ":%s/%s" ns name)))))))

(defun atlas-interactive--get-similarity (compound-identity)
  "Get similar entities for COMPOUND-IDENTITY."
  (let* ((aspects-str (mapconcat (lambda (a) (format "%s" a))
                                 (remove :atlas/type compound-identity)
                                 " "))
         (form (format "(similar-with-diff #{%s})" aspects-str))
         (result (atlas--eval form)))
    (when result
      (atlas--to-list result))))

(defun atlas-interactive--show-similarity-popup (compound-identity)
  "Show similarity information in a popup for COMPOUND-IDENTITY."
  (let ((results (atlas-interactive--get-similarity compound-identity)))
    (if (null results)
        (message "No similar entities found yet - keep adding aspects!")
      (with-output-to-temp-buffer "*Atlas: Similarity*"
        (with-current-buffer "*Atlas: Similarity*"
          (insert (propertize "Similar Entities (by current aspects)\n\n"
                              'face 'bold))
          (dolist (entry (seq-take results 10))
            (let ((dev-id (atlas--to-string (atlas--get entry 'dev-id)))
                  (similarity (atlas--get entry 'similarity))
                  (shared (atlas--to-list (atlas--get entry 'shared)))
                  (unique (atlas--to-list (atlas--get entry 'unique-to-entity))))
              (insert (format "%.2f  %s\n" similarity dev-id))
              (when shared
                (insert (format "  ✓ Shared: %s\n"
                               (mapconcat #'atlas--to-string shared " "))))
              (when unique
                (insert (format "  + Extra: %s\n"
                               (mapconcat #'atlas--to-string unique " "))))
              (insert "\n"))))))))

;;; Properties Builder

(defun atlas-interactive--get-ontology-keys (entity-type)
  "Get ontology keys for ENTITY-TYPE."
  (let* ((form (format "(entity-type-ontology-keys %s)" entity-type))
         (result (atlas--eval form)))
    (when result
      (mapcar #'atlas--to-string (atlas--to-list result)))))

(defun atlas-interactive--build-properties (entity-type)
  "Build properties map for ENTITY-TYPE."
  (let ((ontology-keys (atlas-interactive--get-ontology-keys entity-type))
        (properties '()))
    (if ontology-keys
        (progn
          (message "Building properties for %s (found %d ontology keys)"
                   entity-type (length ontology-keys))
          ;; For now, return empty map - user can fill manually
          ;; Future: interactive property building
          properties)
      (message "No ontology keys found for %s - properties will be empty" entity-type)
      properties)))

;;; Template Generation

(defun atlas-interactive--format-entity (dev-id entity-type aspects properties)
  "Format complete entity registration form."
  (let ((aspects-str (if aspects
                         (format "#{%s}"
                                 (mapconcat (lambda (a) (format "%s" a))
                                           aspects
                                           "\n   "))
                       "#{}"))
        (props-str (if properties
                       (format "{%s}"
                               (mapconcat (lambda (p)
                                           (format "%s %s" (car p) (cdr p)))
                                         properties
                                         "\n  "))
                     "{}")))
    (format "(registry/register!\n %s\n %s\n %s\n %s)"
            dev-id
            entity-type
            aspects-str
            props-str)))

;;; Main Interactive Command

;;;###autoload
(defun atlas-interactive-author-entity ()
  "Interactively author a new Atlas entity with guided steps.

Steps:
1. Select entity type
2. Choose or create dev-id
3. Build compound identity (aspects) with similarity feedback
4. Configure properties (based on ontology)

The complete registration form is inserted at point."
  (interactive)
  (let* ((start-point (point))
         ;; Step 1: Entity type
         (entity-type (progn
                        (atlas-interactive--select-entity-type)))
         ;; Step 2: Dev-ID
         (dev-id (progn
                   (message "Entity type: %s" entity-type)
                   (atlas-interactive--choose-dev-id entity-type)))
         ;; Step 3: Aspects
         (aspects (progn
                    (message "Dev-id: %s - Building aspects..." dev-id)
                    (atlas-interactive--build-aspects-interactive)))
         ;; Step 4: Properties (placeholder for now)
         (properties (progn
                       (message "Aspects: %s" aspects)
                       (atlas-interactive--build-properties entity-type)))
         ;; Generate and insert
         (entity-form (atlas-interactive--format-entity dev-id entity-type aspects properties)))

    ;; Insert the form
    (insert entity-form)
    (insert "\n")

    ;; Final similarity check
    (when aspects
      (let* ((compound-identity (cons entity-type aspects))
             (similar (atlas-interactive--get-similarity compound-identity)))
        (when similar
          (message "Complete! Found %d similar entities (closest: %.2f similarity)"
                   (length similar)
                   (or (atlas--get (car similar) 'similarity) 0.0)))))

    ;; Position cursor for editing
    (goto-char start-point)
    (forward-line 1)))

;;; Quick Aspect Addition

;;;###autoload
(defun atlas-interactive-add-aspect-to-set ()
  "Add an aspect to the compound identity set at point.
Shows similarity feedback after each addition."
  (interactive)
  (save-excursion
    (let* ((bounds (atlas-interactive--find-aspect-set-bounds))
           (start (car bounds))
           (end (cdr bounds)))
      (if (not bounds)
          (message "No aspect set found at point (expected #{...})")
        ;; Get current aspects
        (let* ((current-str (buffer-substring-no-properties (+ start 2) (- end 1)))
               (current-aspects (unless (string-empty-p (string-trim current-str))
                                  (split-string current-str)))
               (new-aspect (atlas-interactive--choose-aspect-by-namespace)))
          (when new-aspect
            ;; Insert at end of set
            (goto-char (- end 1))
            (if current-aspects
                (insert (format "\n   %s" new-aspect))
              (insert (format "%s" new-aspect)))

            ;; Show similarity
            (let* ((all-aspects (cons new-aspect (mapcar #'intern current-aspects)))
                   (similar (atlas-interactive--get-similarity all-aspects)))
              (if similar
                  (message "Added %s - %d similar entities found (closest: %.2f)"
                           new-aspect
                           (length similar)
                           (or (atlas--get (car similar) 'similarity) 0.0))
                (message "Added %s - no similar entities yet" new-aspect)))))))))

(defun atlas-interactive--find-aspect-set-bounds ()
  "Find bounds of aspect set #{...} at or near point."
  (save-excursion
    (let ((start (search-backward "#{" nil t))
          (end (search-forward "}" nil t)))
      (when (and start end)
        (cons start end)))))

(provide 'atlas-interactive)
;;; atlas-interactive.el ends here
