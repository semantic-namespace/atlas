;;; atlas-authoring.el --- Authoring helpers for Atlas entity development -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; Author: @tangrammer + LLMs

;;; Commentary:
;;
;; Interactive helpers for developing Atlas entities:
;; - Aspect statistics (how connected is this entity?)
;; - Smart aspect insertion with namespace selection
;; - Entity template scaffolding

;;; Code:

(require 'atlas-core)
(require 'atlas-completion)

;;; Aspect Statistics

(defun atlas-authoring--aspect-stats (entity-id)
  "Get aspect sharing statistics for ENTITY-ID.
Returns alist of (aspect . count) showing how many entities share each aspect."
  (let* ((form (format "(let [entity-id %s
                             entity-identity (atlas.registry.lookup/identity-for entity-id)
                             all-entities (keys @atlas.registry/registry)
                             aspect-counts (into {}
                                             (map (fn [aspect]
                                                    [aspect
                                                     (->> all-entities
                                                          (filter #(atlas.registry.lookup/has-aspect? %% aspect))
                                                          count)])
                                                  entity-identity))]
                         aspect-counts)"
                       (atlas--to-keyword entity-id)))
         (result (atlas--eval form)))
    (when result
      (let ((entries (atlas--map-entries result)))
        ;; Sort by count descending
        (sort entries (lambda (a b) (> (cdr a) (cdr b))))))))

(defun atlas-authoring--format-aspect-stats (entity-id stats)
  "Format aspect statistics for display."
  (with-temp-buffer
    (insert (propertize (format "Aspect Statistics for %s\n\n" entity-id)
                        'face 'atlas-header-face))
    (insert (propertize "How many entities share each aspect:\n\n"
                        'face 'default))

    (if (null stats)
        (insert (propertize "No aspects found for this entity.\n"
                            'face 'atlas-warning-face))
      (let ((total-aspects (length stats))
            (max-count (apply #'max (mapcar #'cdr stats))))

        ;; Summary
        (insert (propertize (format "Total aspects: %d\n" total-aspects)
                            'face 'atlas-annotation-face))
        (insert (propertize (format "Most shared: %d entities\n\n"
                                    max-count)
                            'face 'atlas-annotation-face))

        ;; Table header
        (insert (propertize (format "%-40s %8s  %s\n"
                                    "Aspect" "Shared" "Bar")
                            'face 'atlas-subheader-face))
        (insert (propertize (make-string 70 ?-) 'face 'atlas-subheader-face))
        (insert "\n")

        ;; Each aspect
        (dolist (entry stats)
          (let* ((aspect (atlas--to-string (car entry)))
                 (count (cdr entry))
                 (bar-width (/ (* count 30) max-count))
                 (bar (make-string bar-width ?█))
                 (face (cond
                        ((= count 1) 'atlas-warning-face)  ; unique aspect
                        ((> count (* total-aspects 0.5)) 'atlas-success-face) ; very common
                        (t 'atlas-aspect-face))))
            (insert (propertize (format "%-40s" aspect) 'face 'atlas-aspect-face))
            (insert (propertize (format " %7d  " count) 'face 'atlas-annotation-face))
            (insert (propertize bar 'face face))
            (insert "\n")))

        ;; Legend
        (insert "\n")
        (insert (propertize "Legend:\n" 'face 'atlas-subheader-face))
        (insert (propertize "  Yellow" 'face 'atlas-warning-face))
        (insert " - Unique to this entity (1)\n")
        (insert (propertize "  Green" 'face 'atlas-success-face))
        (insert " - Very common (>50% of aspects)\n")))

    (buffer-string)))

;;;###autoload
(defun atlas-authoring-aspect-stats (entity-id)
  "Show how many entities share each aspect with ENTITY-ID.

This helps understand:
- Which aspects make this entity unique (count=1)
- Which aspects connect it to many others (high count)
- The overall connectivity of this entity in the semantic graph"
  (interactive
   (list (atlas--completing-read-entity "Entity: ")))
  (let ((stats (atlas-authoring--aspect-stats entity-id)))
    (atlas--display-in-popup
     (atlas-authoring--format-aspect-stats entity-id stats)
     "*Atlas: Aspect Stats*")))

;;; Aspect Insertion

(defun atlas-authoring--get-aspect-namespaces ()
  "Get list of all aspect namespaces currently in use."
  (let* ((form "(list-aspect-namespaces)")
         (result (atlas--eval form)))
    (when result
      (mapcar (lambda (ns-info)
                (atlas--to-string (atlas--get ns-info 'namespace/name)))
              (atlas--to-list result)))))

(defun atlas-authoring--get-aspect-names-in-namespace (ns)
  "Get all aspect names (without namespace) in namespace NS."
  (let* ((form (format "(list-aspect-names-in-namespace \"%s\")" ns))
         (result (atlas--eval form)))
    (when result
      (mapcar #'atlas--to-string (atlas--to-list result)))))

(defun atlas-authoring--format-namespace-counts ()
  "Get namespaces with usage counts for annotation."
  (let* ((form "(list-aspect-namespaces)")
         (result (atlas--eval form)))
    (when result
      (mapcar (lambda (ns-info)
                (cons (intern (atlas--to-string (atlas--get ns-info 'namespace/name)))
                      (atlas--get ns-info 'namespace/count)))
              (atlas--to-list result)))))

(defun atlas-authoring--annotate-namespace (ns)
  "Annotate namespace NS with usage count."
  (let* ((counts (atlas-authoring--format-namespace-counts))
         (count (or (cdr (assoc (intern ns) counts)) 0)))
    (propertize (format " (%d)" count)
                'face 'atlas-annotation-face)))

(defun atlas-authoring--annotate-aspect-name (name)
  "Annotate aspect name with count of entities using it."
  ;; For now, just show it's available
  (propertize " ✓" 'face 'atlas-success-face))

;;;###autoload
(defun atlas-authoring-insert-aspect ()
  "Insert an aspect at point using two-step completion.

First selects the namespace (domain/, tier/, operation/, etc.),
then selects the specific name within that namespace.

The full :namespace/name keyword is inserted at point."
  (interactive)
  (let* ((namespaces (atlas-authoring--get-aspect-namespaces))
         (ns (completing-read
              "Aspect namespace: "
              (lambda (string pred action)
                (if (eq action 'metadata)
                    '(metadata
                      (annotation-function . atlas-authoring--annotate-namespace))
                  (complete-with-action action namespaces string pred)))
              nil t))
         (names (atlas-authoring--get-aspect-names-in-namespace ns))
         (name (completing-read
                (format "Name in %s/: " ns)
                (lambda (string pred action)
                  (if (eq action 'metadata)
                      '(metadata
                        (annotation-function . atlas-authoring--annotate-aspect-name))
                    (complete-with-action action names string pred)))
                nil t))
         (keyword (format ":%s/%s" ns name)))
    (insert keyword)
    (message "Inserted %s" keyword)))

;;; Aspect Statistics at Point

(defun atlas-authoring--entity-at-point ()
  "Try to extract entity-id at point.
Looks for :keyword/form patterns."
  (save-excursion
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (when bounds
        (let ((text (buffer-substring-no-properties (car bounds) (cdr bounds))))
          (when (string-match "^:?\\([a-z-]+/[a-z-]+\\)" text)
            (concat ":" (match-string 1 text))))))))

;;;###autoload
(defun atlas-authoring-aspect-stats-at-point ()
  "Show aspect statistics for entity at point, or prompt if none found."
  (interactive)
  (let ((entity-id (or (atlas-authoring--entity-at-point)
                       (atlas--completing-read-entity "Entity: "))))
    (atlas-authoring-aspect-stats entity-id)))

;;; Create New Aspect

(defun atlas-authoring--suggest-namespace ()
  "Suggest namespace based on common patterns."
  (let ((suggestions '("domain" "tier" "operation" "effect"
                       "compliance" "security" "observability")))
    (completing-read "New aspect namespace: " suggestions nil nil)))

;;;###autoload
(defun atlas-authoring-create-aspect ()
  "Create a new aspect keyword with guided prompts.

Prompts for namespace and name, shows preview, then inserts.
Useful for creating new aspects that don't exist yet."
  (interactive)
  (let* ((ns (atlas-authoring--suggest-namespace))
         (name (read-string (format "Name in %s/: " ns)))
         (keyword (format ":%s/%s" ns name))
         (confirm (yes-or-no-p (format "Insert new aspect %s? " keyword))))
    (when confirm
      (insert keyword)
      (message "Inserted new aspect: %s" keyword))))

;;; Aspect Palette (Quick Reference)

(defun atlas-authoring--format-aspect-palette ()
  "Format all aspects grouped by namespace as a reference palette."
  (let ((namespaces (atlas-authoring--get-aspect-namespaces)))
    (with-temp-buffer
      (insert (propertize "Aspect Palette - All Available Aspects\n\n"
                          'face 'atlas-header-face))
      (insert "Click any aspect to copy to clipboard\n\n")

      (when namespaces
        (dolist (ns namespaces)
          (let ((names (atlas-authoring--get-aspect-names-in-namespace ns)))
            (when names
              ;; Namespace header
              (insert (propertize (format "%s/ (%d)\n" ns (length names))
                                  'face 'atlas-subheader-face))
              ;; Aspect list
              (dolist (name names)
                (let ((full-kw (format ":%s/%s" ns name)))
                  (insert "  ")
                  (insert-button full-kw
                                 'action (lambda (_btn)
                                           (kill-new full-kw)
                                           (message "Copied: %s" full-kw))
                                 'face 'atlas-aspect-face
                                 'help-echo "Click to copy")
                  (insert "\n")))
              (insert "\n")))))
      (buffer-string))))

;;;###autoload
(defun atlas-authoring-aspect-palette ()
  "Show a palette of all available aspects grouped by namespace.

Click any aspect to copy it to the clipboard.
Useful reference when authoring entity definitions."
  (interactive)
  (atlas--display-in-popup
   (atlas-authoring--format-aspect-palette)
   "*Atlas: Aspect Palette*"))

;;; Entity Template Scaffolding — ontology-driven

;;;###autoload
(defun atlas-authoring-scaffold-entity ()
  "Insert a register! template for any Atlas entity type.

Reads entity types from the live registry (including custom types),
then generates the skeleton from ontology keys. No hardcoded templates."
  (interactive)
  (let* ((types-result (atlas--eval "(list-entity-types)"))
         (types (when types-result (atlas--to-list types-result)))
         (type-strings (mapcar (lambda (t)
                                 (let ((s (atlas--to-string t)))
                                   (if (string-prefix-p ":" s)
                                       (substring s 1) s)))
                               types))
         (choice (completing-read "Entity type: " type-strings nil t))
         (type-kw (if (string-prefix-p ":" choice) choice
                    (concat ":" choice)))
         ;; Prompt for dev-id prefix
         (prefix (read-string (format "Dev-id prefix (e.g. fn, component): ")
                              nil nil (car (last (split-string (substring type-kw 1) "/")))))
         (form (format "(scaffold-entity %s {:dev-id-prefix \"%s\"})" type-kw prefix))
         (template (atlas--eval form)))
    (if template
        (let ((text (atlas--unescape-template template)))
          (insert text)
          (insert "\n")
          (message "Inserted %s scaffold (ontology-driven)" type-kw))
      (message "Failed to generate scaffold for %s" type-kw))))

;;; Dev-ID Insertion

(defun atlas-authoring--suggest-dev-id-namespace ()
  "Suggest namespace for a new dev-id based on common patterns."
  (let ((suggestions '("endpoint" "fn" "component" "schema" "protocol"
                       "pattern" "constraint" "failure-mode" "value" "role")))
    (completing-read "Dev-id namespace: " suggestions nil nil)))

;;;###autoload
(defun atlas-authoring-insert-dev-id ()
  "Insert an existing dev-id at point using completion.

Provides completion with entity type annotations.
The full :namespace/name keyword is inserted at point."
  (interactive)
  (let* ((dev-id (atlas--completing-read-entity "Dev-id: "))
         (keyword (if (string-prefix-p ":" dev-id)
                      dev-id
                    (format ":%s" dev-id))))
    (insert keyword)
    (message "Inserted %s" keyword)))

;;;###autoload
(defun atlas-authoring-create-dev-id ()
  "Create a new dev-id with guided prompts.

Prompts for namespace and name, shows preview, then inserts.
Useful for creating new dev-ids that don't exist yet."
  (interactive)
  (let* ((ns (atlas-authoring--suggest-dev-id-namespace))
         (name (read-string (format "Name in %s/: " ns)))
         (keyword (format ":%s/%s" ns name))
         (confirm (yes-or-no-p (format "Insert new dev-id %s? " keyword))))
    (when confirm
      (insert keyword)
      (message "Inserted new dev-id: %s" keyword))))

;;; Similar Entities with Aspect Diff

(defun atlas-authoring--extract-compound-identity-at-point ()
  "Extract compound identity set at point (between #{...})."
  (save-excursion
    (let ((start (search-backward "#{" nil t))
          (end (search-forward "}" nil t)))
      (when (and start end)
        (buffer-substring-no-properties (+ start 2) (1- end))))))

(defun atlas-authoring--format-similar-with-diff (compound-identity-str results)
  "Format similar entities showing aspect differences."
  (with-temp-buffer
    (insert (propertize (format "Similar Entities to %s\n\n" compound-identity-str)
                        'face 'atlas-header-face))
    (insert (propertize "Sorted by semantic distance (closest first)\n\n"
                        'face 'default))

    (if (null results)
        (insert (propertize "No similar entities found.\n"
                            'face 'atlas-warning-face))
      (dolist (entry results)
        (let* ((dev-id (atlas--to-string (atlas--get entry 'dev-id)))
               (similarity (atlas--get entry 'similarity))
               (shared (atlas--to-list (atlas--get entry 'shared)))
               (unique-to-query (atlas--to-list (atlas--get entry 'unique-to-query)))
               (unique-to-entity (atlas--to-list (atlas--get entry 'unique-to-entity))))

          ;; Entity header with similarity score
          (insert (propertize (format "%.2f  " similarity)
                              'face 'atlas-annotation-face))
          (atlas--insert-entity dev-id)
          (insert "\n")

          ;; Shared aspects
          (when shared
            (insert (propertize "  ✓ Shared: " 'face 'atlas-success-face))
            (dolist (aspect shared)
              (insert (format "%s " (atlas--to-string aspect))))
            (insert "\n"))

          ;; Aspects only in query (missing from entity)
          (when unique-to-query
            (insert (propertize "  - Missing: " 'face 'atlas-warning-face))
            (dolist (aspect unique-to-query)
              (insert (format "%s " (atlas--to-string aspect))))
            (insert "\n"))

          ;; Aspects only in entity (extra aspects)
          (when unique-to-entity
            (insert (propertize "  + Extra: " 'face 'atlas-aspect-face))
            (dolist (aspect unique-to-entity)
              (insert (format "%s " (atlas--to-string aspect))))
            (insert "\n"))

          (insert "\n"))))
    (buffer-string)))

;;;###autoload
(defun atlas-authoring-similar-at-point ()
  "Show similar entities to the compound identity at point.
Shows entities sorted by semantic distance with aspect differences highlighted:
  ✓ Shared aspects (overlap)
  - Missing aspects (in query but not in entity)
  + Extra aspects (in entity but not in query)"
  (interactive)
  (let ((compound-id-str (atlas-authoring--extract-compound-identity-at-point)))
    (if (not compound-id-str)
        (message "No compound identity found at point (expected #{...})")
      (let* ((form (format "(similar-with-diff #{%s})" compound-id-str))
             (result (atlas--eval form))
             (results (when result (atlas--to-list result))))
        (atlas--display-in-popup
         (atlas-authoring--format-similar-with-diff compound-id-str results)
         "*Atlas: Similar Entities*")))))

;;; ======================================================================
;;; TEMPORARY — Defmethod-to-Atlas scaffold (remove after yorba migration)
;;; ======================================================================

(defun atlas-authoring--enclosing-defmethod ()
  "Return the source string of the defmethod form enclosing point, or nil."
  (save-excursion
    (let ((start nil))
      ;; Search backward for (defmethod
      (when (re-search-backward "(defmethod\\b" nil t)
        (setq start (point))
        ;; Move to matching close paren
        (forward-sexp 1)
        (buffer-substring-no-properties start (point))))))

(defun atlas-authoring--buffer-ns-form ()
  "Return the (ns …) form string from the buffer's top, or nil.
Reads the first ns form regardless of point position so alias resolution
uses the actual source file's namespace, not a ns the defmethod happens
to live under at the REPL."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^(ns\\b" nil t)
      (let ((start (match-beginning 0)))
        (goto-char start)
        (forward-sexp 1)
        (buffer-substring-no-properties start (point))))))

;;;###autoload
(defun atlas-authoring-scaffold-from-defmethod ()
  "TEMPORARY — Generate atlas execution-function + workflow-producer
registrations from the defmethod at point.

Place cursor inside a (defmethod ig/init-key ...) form and run this.
Inserts the register! calls above the defmethod.

Remove this command after the yorba migration is complete."
  (interactive)
  (let ((source (atlas-authoring--enclosing-defmethod))
        (ns-form (atlas-authoring--buffer-ns-form)))
    (if (not source)
        (message "No defmethod form found at point")
      (let* ((escape (lambda (s) (replace-regexp-in-string "\"" "\\\\\"" s)))
             (escaped-src (funcall escape source))
             (ns-arg (if ns-form
                         (format "\"%s\"" (funcall escape ns-form))
                       "nil"))
             (form (format "(do (require '[atlas.migration.defmethod-scaffold :as scaffold])
                               (scaffold/scaffold-from-source \"%s\" %s))"
                           escaped-src ns-arg))
             (result (atlas--eval form)))
        (if (not result)
            (message "Failed to generate scaffold from defmethod")
          (let ((text (atlas--unescape-template result)))
            ;; Go to beginning of defmethod and insert above
            (save-excursion
              (re-search-backward "(defmethod\\b" nil t)
              (insert text)
              (insert "\n"))
            (message "Inserted atlas scaffold above defmethod")))))))

(provide 'atlas-authoring)
;;; atlas-authoring.el ends here
