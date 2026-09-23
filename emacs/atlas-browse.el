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
(require 'atlas-theme)

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
(defun atlas-browse-by-type ()
  "Browse entities by type: select type → select entity → show info.
Step 1: completing-read over ontology types (with entity counts as annotations).
Step 2: completing-read over entities of the selected type.
Step 3: display entity info for the selected entity."
  (interactive)
  ;; Step 1: fetch types with counts and completing-read
  (let* ((types-raw  (atlas--eval-safe "(list-entity-types-with-counts)" []))
         (types-list (atlas--to-list types-raw))
         (type->count (let ((tbl (make-hash-table :test 'equal)))
                        (dolist (entry types-list)
                          (let* ((type  (atlas--get entry 'entity-type/type))
                                 (count (atlas--get entry 'entity-type/count))
                                 (s     (if (symbolp type) (symbol-name type) (format "%s" type)))
                                 (key   (if (string-prefix-p ":" s) (substring s 1) s)))
                            (puthash key count tbl)))
                        tbl))
         (type-strs   (sort (hash-table-keys type->count) #'string<))
         (annotate-type (lambda (candidate)
                          (when-let ((n (gethash candidate type->count)))
                            (propertize (format " (%d)" n) 'face 'atlas-annotation-face))))
         (selected-type
          (let ((completion-extra-properties `(:annotation-function ,annotate-type)))
            (completing-read "Entity type: " type-strs nil t)))
         (selected-type-kw (atlas--to-keyword selected-type))

         ;; Step 2: fetch entities of that type and completing-read
         (entities-raw  (atlas--eval-safe
                         (format "(list-entities-of-type %s)" selected-type-kw) []))
         (entities-list (atlas--to-list entities-raw))
         (entity-strs   (mapcar (lambda (e)
                                  (let ((s (if (symbolp e) (symbol-name e) (format "%s" e))))
                                    (if (string-prefix-p ":" s) (substring s 1) s)))
                                entities-list))
         (selected-entity
          (completing-read (format "[%s] Entity: " selected-type) entity-strs nil t)))

    ;; Step 3: display entity info
    (atlas-browse-entity-info selected-entity)))

(defconst atlas-browse--aspect-ns-order '("domain" "tier" "operation" "effect")
  "Aspect namespaces shown first in the Identity section, in this order.")

(defconst atlas-browse--footer-hints
  '(("RET" . "open") ("TAB" . "next") ("b" . "blast radius") ("r" . "used by")
    ("M-." . "source") ("g" . "refresh") ("?" . "menu"))
  "Key hints shown at the bottom of entity views (all bound in `atlas-mode-map').")

(defun atlas-browse--prop (info key)
  "Look up property KEY in INFO's definition values, then its extra props."
  (or (atlas--get (atlas--get info 'entity/definition-values) key)
      (atlas--get (atlas--get info 'entity/extra-props) key)))

(defun atlas-browse--insert-aspect-name (aspect)
  "Insert ASPECT's name, without namespace, as a button.
The button opens the survey of the full aspect."
  (let* ((full (atlas--to-string aspect))
         (ns (and (string-match "\\`:?\\([^/]+\\)/\\(.*\\)\\'" full) (match-string 1 full)))
         (name (if ns (match-string 2 full) full))
         (effect (and (equal ns "effect") (not (equal name "read")))))
    (insert-text-button name
                        'face (if effect 'atlas-theme-effect-face 'atlas-aspect-face)
                        'follow-link t
                        'help-echo (format "All entities with %s" full)
                        'action (lambda (_) (atlas-browse-aspect-entities full)))))

(defun atlas-browse--insert-identity (aspects)
  "Insert the Identity section: ASPECTS grouped into one row per namespace."
  (let ((groups (make-hash-table :test 'equal)) namespaces)
    (dolist (a (atlas--to-list aspects))
      (let* ((s (atlas--to-string a))
             (ns (if (string-match "\\`:?\\([^/]+\\)/" s) (match-string 1 s) "")))
        (unless (gethash ns groups) (push ns namespaces))
        (puthash ns (append (gethash ns groups) (list a)) groups)))
    (setq namespaces
          (append (seq-filter (lambda (n) (member n namespaces)) atlas-browse--aspect-ns-order)
                  (sort (seq-remove (lambda (n) (member n atlas-browse--aspect-ns-order)) namespaces)
                        #'string<)))
    (atlas-theme-section "Identity")
    (let ((width (max 11 (1+ (apply #'max 0 (mapcar #'length namespaces))))))
      (dolist (ns namespaces)
        (atlas-theme-row ns
                         (lambda ()
                           (let ((first t))
                             (dolist (a (gethash ns groups))
                               (unless first (insert "  "))
                               (setq first nil)
                               (atlas-browse--insert-aspect-name a))))
                         width)))
    (insert "\n")))

(defun atlas-browse--insert-data-keys (keys)
  "Insert KEYS inline as data-key buttons, wrapping at the rule width."
  (insert "  ")
  (let ((first t))
    (dolist (k (atlas--to-list keys))
      (let ((s (atlas--to-string k)))
        (unless first
          (if (> (+ (current-column) 3 (length s)) atlas-theme-rule-width)
              (insert "\n  ")
            (insert "   ")))
        (setq first nil)
        (atlas--insert-data-key s))))
  (insert "\n\n"))

(defun atlas-browse--property-role (key)
  "Classify property KEY by its name: `needs', `produces', `deps' or nil."
  (let ((name (replace-regexp-in-string "\\`.*/" "" (atlas--to-string key))))
    (pcase name
      ("context" 'needs)
      ("response" 'produces)
      ("deps" 'deps))))

;;;###autoload
(defun atlas-browse-entity-info (entity)
  "Show detailed info for ENTITY: identity, what it needs/produces, dependencies."
  (interactive
   (list (atlas--completing-read-entity "Entity: ")))
  (let* ((entity-kw (atlas--to-keyword entity))
         (info (atlas--eval-safe (format "(entity-info %s)" entity-kw)))
         (buf (atlas--buffer (format "entity:%s" entity))))
    (with-current-buffer buf
      (setq atlas--last-command (lambda () (atlas-browse-entity-info entity)))
      (if (not info)
          (progn
            (atlas-theme-title nil entity-kw "not found")
            (insert "  ")
            (atlas-theme-dim "No entity with this dev-id in the connected registry.")
            (insert "\n"))
        (let* ((type (atlas-browse--prop info 'atlas/type))
               (props (seq-remove
                       (lambda (pair)
                         (or (null (cdr pair))
                             (member (atlas--to-string (car pair)) '(":atlas/dev-id" ":atlas/type"))))
                       (atlas--map-entries (atlas--get info 'entity/definition-values))))
               (dep-ids (apply #'append
                               (mapcar (lambda (pair)
                                         (when (eq (atlas-browse--property-role (car pair)) 'deps)
                                           (atlas--to-list (cdr pair))))
                                       props)))
               (types (atlas-theme-entity-types dep-ids)))
          (atlas-theme-title type entity-kw (atlas-theme--type-name type))
          (atlas-browse--insert-identity (atlas--get info 'entity/aspects))
          (dolist (pair props)
            (let ((items (atlas--to-list (cdr pair))))
              (pcase (atlas-browse--property-role (car pair))
                ('needs    (atlas-theme-section "Needs" (length items))
                           (atlas-browse--insert-data-keys items))
                ('produces (atlas-theme-section "Produces" (length items))
                           (atlas-browse--insert-data-keys items))
                ('deps     (atlas-theme-section "Depends on" (length items))
                           (atlas-theme-entity-list items types 10 "no dependencies")
                           (insert "\n"))
                (_         (atlas-theme-section (atlas--to-string (car pair)))
                           (atlas--insert-property-value (cdr pair))
                           (insert "\n")))))))
      (atlas-theme-footer atlas-browse--footer-hints)
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun atlas-browse-data-flow (entity)
  "Show where each input of ENTITY comes from."
  (interactive
   (list (atlas--completing-read-entity "Function: ")))
  (let* ((entity-kw (atlas--to-keyword entity))
         (flow (atlas--to-list (atlas--eval-safe (format "(data-flow %s)" entity-kw))))
         (buf (atlas--buffer (format "flow:%s" entity))))
    (with-current-buffer buf
      (setq atlas--last-command (lambda () (atlas-browse-data-flow entity)))
      (atlas-theme-section "Inputs come from" (length flow))
      (if (null flow)
          (progn (insert "  ") (atlas-theme-dim "no inputs declared") (insert "\n"))
        (let* ((key-width (apply #'max 0 (mapcar (lambda (i) (length (atlas--to-string
                                                                       (atlas--get i 'dataflow/needs))))
                                                  flow))))
          (dolist (item flow)
            (let* ((needs (atlas--to-string (atlas--get item 'dataflow/needs)))
                   (producers (atlas--to-list (atlas--get item 'dataflow/produced-by)))
                   (satisfied (atlas--get item 'dataflow/satisfied?)))
              (insert "  ")
              (atlas--insert-data-key needs)
              (insert (make-string (- (+ key-width 2) (length needs)) ?\s))
              (atlas-theme-mark (cond ((and producers satisfied) 'ok)
                                      (producers 'warn)
                                      (t 'none)))
              (insert " ")
              (if (null producers)
                  (atlas-theme-dim "external input (no producer in registry)")
                (atlas--insert-entity (car producers))
                (when (cdr producers)
                  (insert " ")
                  (atlas-theme-dim (format "+%d more" (length (cdr producers))))))
              (insert "\n")))))
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
         (deps (atlas--to-list (atlas--eval-safe (format "(dependents-of %s)" entity-kw) [])))
         (types (atlas-theme-entity-types deps))
         (buf (atlas--buffer (format "dependents:%s" entity))))
    (with-current-buffer buf
      (setq atlas--last-command (lambda () (atlas-browse-dependents entity)))
      (atlas-theme-section "Used by" (length deps))
      (atlas-theme-entity-list deps types 20 "nothing depends on this")
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
(defun atlas-browse-recursive-deps (entity)
  "Show all transitive (recursive) dependencies for ENTITY.
Traverses execution-functions, structure-components and their deps recursively.
Displays results in BFS order with indentation showing dependency depth."
  (interactive
   (list (atlas--completing-read-entity "Entity: ")))
  (let* ((entity-kw (atlas--to-keyword entity))
         (deps (atlas--eval-safe (format "(recursive-dependencies-of %s)" entity-kw) []))
         (buf (atlas--buffer (format "rdeps:%s" entity))))
    (with-current-buffer buf
      (setq atlas--last-command (lambda () (atlas-browse-recursive-deps entity)))
      (atlas--insert-header (format "Recursive Dependencies of %s" entity))
      (insert (propertize "All transitive dependencies (BFS order):\n\n"
                          'face 'font-lock-comment-face))
      (let ((deps-list (atlas--to-list deps)))
        (if (and deps-list (> (length deps-list) 0))
            (dolist (dep deps-list)
              (let* ((dep-id    (atlas--get dep 'dep/dev-id))
                     (dep-type  (atlas--get dep 'dep/type))
                     (depth     (or (atlas--get dep 'dep/depth) 1))
                     (via       (atlas--get dep 'dep/via))
                     (indent    (make-string (* 2 (1- depth)) ?\s))
                     (type-str  (when dep-type
                                  (let ((s (if (symbolp dep-type)
                                              (symbol-name dep-type)
                                            (format "%s" dep-type))))
                                    (replace-regexp-in-string "^:?atlas/" "" s)))))
                (insert indent)
                (insert (propertize "→ " 'face 'atlas-annotation-face))
                (atlas--insert-entity dep-id)
                (when type-str
                  (insert (propertize (format " [%s]" type-str)
                                      'face 'atlas-annotation-face)))
                (when (atlas--get dep 'dep/already-seen?)
                  (insert (propertize " ↑" 'face 'font-lock-comment-face)))
                (when (and via (> depth 1) (not (atlas--get dep 'dep/already-seen?)))
                  (insert (propertize (format "  ← %s" via)
                                      'face 'font-lock-comment-face)))
                ;; Show context-deps (entities found in context/input)
                (let ((ctx-deps (atlas--to-list (atlas--get dep 'dep/context-deps))))
                  (when (and ctx-deps (> (length ctx-deps) 0)
                             (not (atlas--get dep 'dep/already-seen?)))
                    (insert "\n" indent "  ")
                    (insert (propertize "deps via context: " 'face 'atlas-warning-face))
                    (dolist (d ctx-deps)
                      (atlas--insert-entity d)
                      (insert " "))))
                ;; Show pure input data keys
                (let ((input (atlas--to-list (atlas--get dep 'dep/input))))
                  (when (and input (> (length input) 0)
                             (not (atlas--get dep 'dep/already-seen?)))
                    (insert "\n" indent "  ")
                    (insert (propertize "input: " 'face 'font-lock-comment-face))
                    (insert (propertize
                             (mapconcat (lambda (k)
                                          (let ((s (if (symbolp k) (symbol-name k) (format "%s" k))))
                                            (if (string-prefix-p ":" s) s (concat ":" s))))
                                        input " ")
                             'face 'font-lock-doc-face))))
                (insert "\n")))
          (insert "  (no dependencies)\n")))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun atlas-browse-deps-summary (entity)
  "Show flat summary of all transitive deps and data keys for ENTITY.
What do I need to test/run this entity? Two lists:
1. All entity deps (components, functions)
2. All data keys needed (context/input across the full tree)"
  (interactive
   (list (atlas--completing-read-entity "Entity: ")))
  (let* ((entity-kw (atlas--to-keyword entity))
         (summary (atlas--eval-safe
                   (format "(recursive-dependencies-summary %s)" entity-kw)))
         (buf (atlas--buffer (format "summary:%s" entity))))
    (with-current-buffer buf
      (setq atlas--last-command (lambda () (atlas-browse-deps-summary entity)))
      (atlas--insert-header (format "Test/Dev Summary: %s" entity))

      ;; Root context
      (when-let ((ctx (atlas--to-list (atlas--get summary 'summary/context))))
        (atlas--insert-subheader (format "Root context (%d)" (length ctx)))
        (dolist (k ctx)
          (insert "  ")
          (atlas--insert-data-key k)
          (insert "\n"))
        (insert "\n"))

      ;; All entity deps
      (let ((deps (atlas--to-list (atlas--get summary 'summary/deps))))
        (atlas--insert-subheader (format "All entity deps (%d)" (length deps)))
        (if deps
            (dolist (d deps)
              (insert "  ")
              (atlas--insert-entity d)
              (insert "\n"))
          (insert "  (none)\n"))
        (insert "\n"))

      ;; All data keys
      (let ((keys (atlas--to-list (atlas--get summary 'summary/data-keys))))
        (atlas--insert-subheader (format "All data keys needed (%d)" (length keys)))
        (if keys
            (dolist (k keys)
              (insert "  ")
              (atlas--insert-data-key k)
              (insert "\n"))
          (insert "  (none)\n")))

      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun atlas-browse-recursive-dependents (entity)
  "Show all transitive dependents for ENTITY, grouped by distance.
If I change this entity, what is transitively affected?  Entities reached
again by another path are folded into a per-level count, not repeated."
  (interactive
   (list (atlas--completing-read-entity "Entity: ")))
  (let* ((entity-kw (atlas--to-keyword entity))
         (deps (atlas--to-list
                (atlas--eval-safe (format "(recursive-dependents-of %s)" entity-kw) [])))
         (buf (atlas--buffer (format "rdependents:%s" entity))))
    (with-current-buffer buf
      (setq atlas--last-command (lambda () (atlas-browse-recursive-dependents entity)))
      (if (null deps)
          (progn (atlas-theme-section "Ripple")
                 (insert "  ") (atlas-theme-dim "nothing depends on this entity") (insert "\n"))
        (let ((by-depth (seq-group-by (lambda (d) (or (atlas--get d 'dep/depth) 1)) deps))
              (repeats 0))
          (dolist (level (sort (mapcar #'car by-depth) #'<))
            (let* ((rows (alist-get level by-depth))
                   (fresh (seq-remove (lambda (d) (atlas--get d 'dep/already-seen?)) rows)))
              (setq repeats (+ repeats (- (length rows) (length fresh))))
              ;; a level reached only through entities already listed adds nothing new
              (when fresh
                (atlas-theme-section (if (= level 1) "Direct"
                                       (format "%d steps away" level))
                                     (length fresh))
                (dolist (d fresh)
                  (let ((via (atlas--get d 'dep/via)))
                    (insert "  " (atlas-theme-badge (atlas--get d 'dep/type)) " ")
                    (atlas--insert-entity (atlas--to-string (atlas--get d 'dep/dev-id)))
                    (when (and via (> level 1))
                      (insert "  ")
                      (atlas-theme-dim (format "via %s" (atlas--to-string via))))
                    (insert "\n")))
                (insert "\n"))))
          (when (> repeats 0)
            (insert "  ")
            (atlas-theme-dim (format "+%d more path%s reach entities already listed"
                                     repeats (if (= repeats 1) "" "s")))
            (insert "\n"))))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun atlas-browse-dependents-summary (entity)
  "Show the blast radius of ENTITY: total, count per type, affected entities."
  (interactive
   (list (atlas--completing-read-entity "Entity: ")))
  (let* ((entity-kw (atlas--to-keyword entity))
         (summary (atlas--eval-safe
                   (format "(recursive-dependents-summary %s)" entity-kw)))
         (affected (atlas--to-list (atlas--get summary 'summary/affected)))
         (by-type (sort (atlas--map-entries (atlas--get summary 'summary/by-type))
                        (lambda (a b) (> (cdr a) (cdr b)))))
         (types (atlas-theme-entity-types affected))
         (buf (atlas--buffer (format "blast:%s" entity))))
    (with-current-buffer buf
      (setq atlas--last-command (lambda () (atlas-browse-dependents-summary entity)))
      (atlas-theme-banner "Blast radius"
                          (format "%d affected" (or (atlas--get summary 'summary/affected-count) 0)))
      (atlas-theme-section "By type" (length by-type))
      (if (null by-type)
          (progn (insert "  ") (atlas-theme-dim "nothing is affected") (insert "\n"))
        (let ((top (cdar by-type))
              (name-width (apply #'max 0 (mapcar (lambda (p) (length (atlas-theme--type-name (car p))))
                                                  by-type))))
          (dolist (pair by-type)
            (let ((name (atlas-theme--type-name (car pair))))
              (insert "  " (atlas-theme-badge (car pair)) " "
                      (propertize (format (format "%%-%ds" name-width) name)
                                  'face 'atlas-theme-label-face)
                      " " (atlas-theme-count (cdr pair)) "  "
                      (atlas-theme-bar (cdr pair) top 20) "\n")))))
      (insert "\n")
      (atlas-theme-section "Affected" (length affected))
      (atlas-theme-entity-list affected types 40 "none")
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun atlas-browse-producers (data-key)
  "Find entities that produce DATA-KEY."
  (interactive
   (list (atlas--completing-read-data-key "Data key: ")))
  (let* ((data-kw (atlas--to-keyword data-key))
         (producers (atlas--to-list (atlas--eval-safe (format "(producers-of %s)" data-kw) [])))
         (types (atlas-theme-entity-types producers))
         (buf (atlas--buffer (format "producers:%s" data-key))))
    (with-current-buffer buf
      (setq atlas--last-command (lambda () (atlas-browse-producers data-key)))
      (atlas-theme-title "data-key" data-kw "data key")
      (atlas-theme-section "Produced by" (length producers))
      (atlas-theme-entity-list producers types 20
                               "nothing in the registry produces this — external input")
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun atlas-browse-consumers (data-key)
  "Find entities that consume DATA-KEY."
  (interactive
   (list (atlas--completing-read-data-key "Data key: ")))
  (let* ((data-kw (atlas--to-keyword data-key))
         (consumers (atlas--to-list (atlas--eval-safe (format "(consumers-of %s)" data-kw) [])))
         (types (atlas-theme-entity-types consumers))
         (buf (atlas--buffer (format "consumers:%s" data-key))))
    (with-current-buffer buf
      (setq atlas--last-command (lambda () (atlas-browse-consumers data-key)))
      (atlas-theme-section "Consumed by" (length consumers))
      (atlas-theme-entity-list consumers types 20 "nothing consumes this")
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun atlas-browse-execution-order ()
  "Show entities in topological order of their data flow."
  (interactive)
  (let* ((order (atlas--to-list (atlas--eval-safe "(execution-order)" [])))
         (types (atlas-theme-entity-types order))
         (width (length (number-to-string (length order))))
         (buf (atlas--buffer "execution-order")))
    (with-current-buffer buf
      (setq atlas--last-command #'atlas-browse-execution-order)
      (atlas-theme-section "Execution order (by data flow)" (length order))
      (if (null order)
          (progn (insert "  ") (atlas-theme-dim "no execution order available") (insert "\n"))
        (let ((n 0))
          (dolist (e order)
            (setq n (1+ n))
            (insert "  " (propertize (format (format "%%%dd" width) n) 'face 'atlas-theme-label-face) "  ")
            (atlas-theme-entity-row e types ""))))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun atlas-browse-system-summary ()
  "Show the system at a glance: counts by kind and the domains."
  (interactive)
  (let* ((summary (atlas--eval-safe "(system-summary)"))
         (buf (atlas--buffer "summary")))
    (with-current-buffer buf
      (setq atlas--last-command #'atlas-browse-system-summary)
      (if (not summary)
          (progn (atlas-theme-banner "System")
                 (insert "  ") (atlas-theme-dim "no summary available") (insert "\n"))
        (let* ((domains (atlas--to-list (atlas--get summary 'domains)))
               (rows `((":atlas/structure-component" "components" ,(atlas--get summary 'components))
                       (":atlas/execution-function"  "functions"  ,(atlas--get summary 'functions))
                       (":atlas/interface-endpoint"  "endpoints"  ,(atlas--get summary 'endpoints))
                       (":atlas/data-schema"         "schemas"    ,(atlas--get summary 'schemas))))
               (top (apply #'max 1 (mapcar (lambda (r) (length (atlas--to-list (nth 2 r)))) rows))))
          (atlas-theme-banner "System" (format "%d domains" (length domains)))
          (atlas-theme-section "At a glance")
          (dolist (r rows)
            (let ((n (length (atlas--to-list (nth 2 r)))))
              (insert "  " (atlas-theme-badge (nth 0 r)) " "
                      (propertize (format "%-11s" (nth 1 r)) 'face 'atlas-theme-label-face)
                      (atlas-theme-count n) "  " (atlas-theme-bar n top 20) "\n")))
          (insert "\n")
          (atlas-theme-section "Domains" (length domains))
          (insert "  ")
          (let ((first t))
            (dolist (d domains)
              (let ((name (replace-regexp-in-string "\\`:?domain/" "" (atlas--to-string d))))
                (unless first
                  (if (> (+ (current-column) 3 (length name)) atlas-theme-rule-width)
                      (insert "\n  ")
                    (insert "   ")))
                (setq first nil)
                (atlas-browse--insert-aspect-name d))))
          (insert "\n")))
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

;;; Prop drilling - follow references interactively

(defun atlas--drill-entity (start-dev-id)
  "Iteratively navigate entity props starting from START-DEV-ID.
Loop: select prop → select value → if value is an entity, repeat with it.
Exit with C-g at any completing-read, or when a non-entity value is selected."
  (let ((current start-dev-id))
    (catch 'done
      (while t
        ;; Fetch prop keys for the current entity
        (let* ((raw-keys  (atlas--eval-safe
                           (format "(entity-prop-keys %s)" (atlas--to-keyword current)) []))
               (keys-list (atlas--to-list raw-keys)))
          (unless keys-list
            (message "No props found for %s" current)
            (throw 'done nil))

          ;; Step 1: completing-read over prop keys
          (let* ((key-strs (mapcar (lambda (k)
                                     (let ((s (if (symbolp k) (symbol-name k) (format "%s" k))))
                                       (if (string-prefix-p ":" s) (substring s 1) s)))
                                   keys-list))
                 (selected-key (completing-read
                                (format "[%s] prop: " current)
                                key-strs nil t))
                 (selected-key-kw (atlas--to-keyword selected-key))

                 ;; Fetch items of selected prop
                 (raw-items  (atlas--eval-safe
                              (format "(entity-prop-items %s %s)"
                                      (atlas--to-keyword current) selected-key-kw) []))
                 (items-list (atlas--to-list raw-items)))

            (unless items-list
              (message "[%s › %s] (empty)" current selected-key)
              (throw 'done nil))

            ;; Step 2: completing-read over prop values, annotated with [entity]
            (let* ((item-strs  (mapcar (lambda (item)
                                         (let* ((val (atlas--get item 'item/value))
                                                (s   (if (symbolp val) (symbol-name val)
                                                       (format "%s" val))))
                                           (if (string-prefix-p ":" s) (substring s 1) s)))
                                       items-list))
                   (item-flags (mapcar (lambda (item)
                                         (atlas--get item 'item/is-dev-id?))
                                       items-list))
                   (annotate-fn
                    (lambda (candidate)
                      (let* ((idx  (seq-position item-strs candidate #'equal))
                             (flag (and idx (nth item-flags idx))))
                        (when flag
                          (propertize " [entity]" 'face 'atlas-annotation-face)))))
                   (completion-extra-properties `(:annotation-function ,annotate-fn))
                   (selected-val (completing-read
                                  (format "[%s › %s] value: " current selected-key)
                                  item-strs nil t))
                   (selected-idx       (seq-position item-strs selected-val #'equal))
                   (selected-is-entity (and selected-idx (nth item-flags selected-idx))))

              (if selected-is-entity
                  ;; It's a registered entity — drill into it
                  (setq current selected-val)
                ;; Leaf value — show info for current entity and stop
                (message "[%s › %s] = :%s  (not an entity — showing %s)"
                         current selected-key selected-val current)
                (atlas-browse-entity-info current)
                (throw 'done nil)))))))))

;;;###autoload
(defun atlas-drill-entity-at-point ()
  "Drill into props of the dev-id keyword at point.
Reads the keyword under cursor, checks it's a registered entity, then
opens an interactive prop navigator: select prop → select value → follow
entity references recursively.  C-g exits at any step."
  (interactive)
  (let ((kw (atlas--keyword-at-point)))
    (if (not kw)
        (message "No namespaced keyword at point (need :ns/name)")
      (let* ((is-entity (atlas--eval-safe (format "(registered-entity? %s)" kw))))
        (if (not is-entity)
            (message "%s is not a registered entity" kw)
          ;; Strip leading colon for display / internal use
          (atlas--drill-entity (if (string-prefix-p ":" kw) (substring kw 1) kw)))))))

;;;###autoload
(defun atlas-browse-home ()
  "Show registry overview: total entity count and breakdown by type.
Each type is a clickable entry — click to open domain-survey for that type."
  (interactive)
  (let* ((result (atlas--eval-safe
                  "(let [reg @atlas.registry/registry]
                     {:total (count reg)
                      :by-type (->> reg
                                    (map (fn [[_ props]] (:atlas/type props)))
                                    (remove nil?)
                                    frequencies
                                    (map (fn [[t cnt]] {:type (str t) :count cnt}))
                                    (sort-by (comp - :count))
                                    vec)})"))
         (buf (atlas--buffer "home")))
    (with-current-buffer buf
      (setq atlas--last-command #'atlas-browse-home)
      (if (not result)
          (progn (atlas-theme-banner "Atlas registry")
                 (insert "  ") (atlas-theme-dim "registry unavailable") (insert "\n"))
        (let* ((by-type (atlas--to-list (atlas--get result 'by-type)))
               (top (apply #'max 1 (mapcar (lambda (e) (atlas--get e 'count)) by-type)))
               (name-width (apply #'max 0 (mapcar (lambda (e) (length (atlas--get e 'type))) by-type))))
          (atlas-theme-banner "Atlas registry" (format "%d entities" (atlas--get result 'total)))
          (atlas-theme-section "By type" (length by-type))
          (dolist (entry by-type)
            (let ((type-str (atlas--get entry 'type))
                  (cnt (atlas--get entry 'count)))
              (insert "  " (atlas-theme-badge type-str) " ")
              (atlas--insert-type type-str)
              (insert (make-string (max 1 (- (+ name-width 1) (length type-str))) ?\s)
                      (atlas-theme-count cnt) "  " (atlas-theme-bar cnt top 20) "\n")))))
      (atlas-theme-footer '(("RET" . "survey type") ("TAB" . "next") ("g" . "refresh") ("?" . "menu")))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun atlas-browse-aspect-entities (aspect)
  "Show all entities carrying ASPECT, grouped by type.
Each type group collapses to 10 entities with a '… N more' button to reveal
the rest — same collapsing model as the browser UI."
  (interactive (list (read-string "Aspect (e.g. :domain/auth): " ":")))
  (let* ((aspect-kw (if (string-prefix-p ":" aspect) aspect (concat ":" aspect)))
         (result (atlas--eval-safe
                  (format
                   "(let [reg @atlas.registry/registry
                          kw %s]
                      (->> reg
                           (filter (fn [[cid _]] (contains? cid kw)))
                           (map (fn [[_ props]] props))
                           (group-by :atlas/type)
                           (map (fn [[t entities]]
                                  {:type    (str t)
                                   :count   (count entities)
                                   :entities (vec (sort (map (fn [p] (str (:atlas/dev-id p)))
                                                             entities)))}))
                           (sort-by (comp - :count))
                           vec))"
                   aspect-kw)))
         (groups (atlas--to-list result))
         (total (apply #'+ (mapcar (lambda (g) (atlas--get g 'count)) groups)))
         (buf (atlas--buffer (format "aspect:%s" aspect))))
    (with-current-buffer buf
      (setq atlas--last-command (lambda () (atlas-browse-aspect-entities aspect)))
      (atlas-theme-banner aspect-kw
                          (format "%d entities · %d types" total (length groups)))
      (if (null groups)
          (progn (insert "  ") (atlas-theme-dim "no entities carry this aspect") (insert "\n"))
        (dolist (group groups)
          (let ((type-str (atlas--get group 'type)))
            (atlas-theme-section (atlas-theme--type-name type-str)
                                 (atlas--get group 'count) type-str)
            (atlas-theme-entity-list (atlas--to-list (atlas--get group 'entities)) nil 10)
            (insert "\n"))))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

(provide 'atlas-browse)
;;; atlas-browse.el ends here
