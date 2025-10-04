;;; semantic-ns.el --- IDE support for Semantic Namespace Framework (namespaced API) -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; Author: @tangrammer + LLMs
;; Version: 0.3.0
;; Package-Requires: ((emacs "27.1") (cider "1.0") (transient "0.4"))
;; Keywords: clojure, semantic, architecture

;;; Commentary:
;;
;; Provides interactive commands for querying and navigating
;; semantic namespace registries via CIDER REPL.
;;
;; Main entry point: M-x semantic-ns (transient menu)
;;
;; Features:
;; - Completing read with annotations (entity type, aspects)
;; - Transient menu for all commands
;; - Integration with ivy/selectrum for fuzzy search

;;; Code:

(require 'cider)
(require 'transient)
(require 'parseedn)
(require 'seq)  ; For seq, seq-group-by

(define-key transient-base-map (kbd "C-g") #'transient-quit-one)

(defgroup semantic-ns nil
  "Semantic Namespace IDE support."
  :group 'clojure
  :prefix "semantic-ns-")

(defvar semantic-ns-ide-ns "atlas.ide"
  "Clojure namespace containing IDE functions.")

(defvar semantic-ns-lsp-helpers-ns "atlas.tooling.lsp-helpers"
  "Clojure namespace containing LSP helper functions.")

;; Cache for completion data (avoid repeated REPL calls)
(defvar semantic-ns--entity-cache nil)
(defvar semantic-ns--aspect-cache nil)
(defvar semantic-ns--cache-time nil)
(defvar semantic-ns--cache-ttl 5 "Cache TTL in seconds.")
(defvar semantic-ns-debug nil "Enable debug messages for troubleshooting.")

;;; Faces

(defface semantic-ns-header-face
  '((t :inherit font-lock-keyword-face :weight bold :height 1.1))
  "Face for headers in semantic-ns buffers.")

(defface semantic-ns-subheader-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for subheaders in semantic-ns buffers.")

(defface semantic-ns-entity-face
  '((t :inherit font-lock-function-name-face))
  "Face for entity names.")

(defface semantic-ns-aspect-face
  '((t :inherit font-lock-type-face))
  "Face for aspect keywords.")

(defface semantic-ns-error-face
  '((t :inherit error :weight bold))
  "Face for errors.")

(defface semantic-ns-warning-face
  '((t :inherit warning))
  "Face for warnings.")

(defface semantic-ns-success-face
  '((t :inherit success :weight bold))
  "Face for success indicators.")

(defface semantic-ns-annotation-face
  '((t :inherit completions-annotations))
  "Face for annotations in completion.")

;;; Business Semantics Faces

(defface semantic-ns-pattern-face
  '((t :inherit font-lock-string-face :weight bold))
  "Face for business patterns.")

(defface semantic-ns-constraint-face
  '((t :inherit font-lock-warning-face :weight bold))
  "Face for constraints.")

(defface semantic-ns-failure-face
  '((t :inherit error :weight bold))
  "Face for failure modes.")

(defface semantic-ns-value-face
  '((t :inherit font-lock-constant-face :weight bold))
  "Face for value propositions.")

(defface semantic-ns-role-face
  '((t :inherit font-lock-builtin-face :weight bold))
  "Face for user roles.")

(defface semantic-ns-experience-face
  '((t :inherit font-lock-doc-face :weight bold))
  "Face for user experiences.")

;;; CIDER Evaluation

(defun semantic-ns--to-list (obj)
  "Convert OBJ to list if it's a vector, otherwise return as-is.
Parseedn converts Clojure vectors to Emacs vectors and sets to lists."
  (cond
   ((vectorp obj) (append obj nil))
   ((listp obj) obj)
   (t (list obj))))

(defun semantic-ns--to-string (obj)
  "Safely convert OBJ to string, handling symbols and strings."
  (cond
   ((stringp obj) obj)
   ((symbolp obj) (symbol-name obj))
   (t (format "%s" obj))))

(defun semantic-ns--map-entries (obj)
  "Convert OBJ into a list of (key . value) pairs when possible."
  (cond
   ((hash-table-p obj)
    (let (pairs)
      (maphash (lambda (k v)
                 (push (cons k v) pairs))
               obj)
      (nreverse pairs)))
   ((and (listp obj) (consp (car obj)))
    (mapcar (lambda (pair)
              (if (vectorp pair)
                  (cons (aref pair 0) (aref pair 1))
                pair))
            obj))
   ((and (vectorp obj) (> (length obj) 0)
         (or (consp (aref obj 0)) (vectorp (aref obj 0))))
    (semantic-ns--map-entries (append obj nil)))
   (t nil)))

(defun semantic-ns--key->names (k)
  "Return list of comparable names for keyword or symbol K.
Includes full name (without leading colon) and un-namespaced tail to support
both namespaced and non-namespaced callers."
  (let* ((raw (if (symbolp k) (symbol-name k) (format "%s" k)))
         (trimmed (if (string-prefix-p ":" raw) (substring raw 1) raw)))
    (delete-dups
     (cons trimmed
           (let ((tail (car (last (split-string trimmed "/")))))
             (when tail (list tail)))))))

(defun semantic-ns--get (map key)
  "Get KEY from MAP (hash-table or alist) while tolerating namespaced keywords.
Matches either the full keyword name or its namespace-stripped tail so older
callers still work when API responses switched to namespaced keys."
  (when semantic-ns-debug
    (message "[semantic-ns DEBUG] Getting key '%s' from map type: %s"
             key
             (cond ((hash-table-p map) "hash-table")
                   ((listp map) "alist")
                   (t "unknown"))))
  (let* ((key-names (semantic-ns--key->names key))
         (lookup (lambda (k)
                   (cond
                    ((hash-table-p map) (gethash k map))
                    ((listp map) (cdr (assq k map)))
                    (t nil))))
         (keys (cond
                ((hash-table-p map) (hash-table-keys map))
                ((listp map) (mapcar #'car map))
                (t nil)))
         (match (seq-find (lambda (candidate)
                            (seq-some (lambda (name)
                                        (member name (semantic-ns--key->names candidate)))
                                      key-names))
                          keys))
         (result (when match (funcall lookup match))))
    (when semantic-ns-debug
      (message "[semantic-ns DEBUG] Key '%s' -> key-names: %S, available keys: %S, match: %s, result: %S"
               key key-names keys match result))
    result))

(defun semantic-ns--to-keyword (s)
  "Convert S to a proper keyword format for Clojure.
Handles both string and symbol inputs, ensuring proper : prefix."
  (let ((str (if (symbolp s) (symbol-name s) s)))
    (if (string-prefix-p ":" str)
        str
      (concat ":" str))))

(defconst semantic-ns--non-ide-forms
  '("do" "let" "if" "when" "cond" "case" "fn" "binding" "->" "->>" "as->" "some->" "some->>" "doto")
  "Forms that should not be qualified with the IDE namespace.")

(defun semantic-ns--qualify-ide-form (form)
  "Qualify FORM with the IDE namespace when it looks like a simple function call."
  (if (string-match "\\`\\s-*(\\([[:alnum:]*+!\\-\\?_.]+\\)\\>" form)
      (let ((sym (match-string 1 form)))
        (if (or (string-match-p "/" sym)
                (member sym semantic-ns--non-ide-forms))
            form
          (replace-match (concat semantic-ns-ide-ns "/" sym) t t form 1)))
    form))

(defun semantic-ns--eval (form)
  "Evaluate FORM in CIDER and return parsed result.
Returns nil on error with message to user."
  (unless (cider-connected-p)
    (user-error "CIDER not connected. Run cider-jack-in first"))
  (let* ((qualified-form (semantic-ns--qualify-ide-form form))
         (code (format "(do (require '[%s]) %s)"
                       semantic-ns-ide-ns qualified-form))
         (result (cider-nrepl-sync-request:eval code)))
    (when semantic-ns-debug
      (message "[semantic-ns DEBUG] Evaluating: %s" form))
    (if-let ((err (nrepl-dict-get result "err")))
        (progn
          (message "Clojure error: %s" err)
          nil)
      (if-let ((value (nrepl-dict-get result "value")))
          (progn
            (when semantic-ns-debug
              (message "[semantic-ns DEBUG] Raw EDN response (first 500 chars): %s"
                       (substring value 0 (min 500 (length value)))))
            (condition-case err
                ;; Use parseedn to parse EDN (CIDER includes this)
                (let ((parsed (parseedn-read-str value)))
                  (when semantic-ns-debug
                    (message "[semantic-ns DEBUG] Parsed result type: %s, length: %s"
                             (type-of parsed)
                             (if (sequencep parsed) (length parsed) "N/A")))
                  parsed)
              (error
               (message "Failed to parse result: %s (raw: %s)"
                        (error-message-string err) value)
               nil)))
        (progn
          (message "No value returned from evaluation")
          nil)))))

(defun semantic-ns--eval-safe (form &optional default)
  "Evaluate FORM, returning DEFAULT (or nil) on any error."
  (or (semantic-ns--eval form) default))

(defun semantic-ns--invalidate-cache ()
  "Invalidate completion cache."
  (interactive)
  (setq semantic-ns--entity-cache nil
        semantic-ns--aspect-cache nil
        semantic-ns--cache-time nil)
  (message "Cache invalidated"))

;;;###autoload
(defun semantic-ns-toggle-debug ()
  "Toggle debug messages for semantic-ns."
  (interactive)
  (setq semantic-ns-debug (not semantic-ns-debug))
  (message "semantic-ns debug mode: %s" (if semantic-ns-debug "ENABLED" "DISABLED")))

(defun semantic-ns--cache-valid-p ()
  "Check if cache is still valid."
  (and semantic-ns--cache-time
       (< (- (float-time) semantic-ns--cache-time) semantic-ns--cache-ttl)))

;;; Buffer Helpers

(defun semantic-ns--buffer (name)
  "Get or create semantic-ns buffer with NAME."
  (let ((buf (get-buffer-create (format "*semantic-ns: %s*" name))))
    (with-current-buffer buf
      (semantic-ns-mode)
      (read-only-mode -1)
      (erase-buffer))
    buf))

(defun semantic-ns--insert-header (text)
  "Insert TEXT as a header."
  (insert (propertize text 'face 'semantic-ns-header-face) "\n")
  (insert (make-string (min 50 (length text)) ?-) "\n\n"))

(defun semantic-ns--insert-subheader (text)
  "Insert TEXT as a subheader."
  (insert (propertize text 'face 'semantic-ns-subheader-face) "\n"))

(defun semantic-ns-jump-to-definition-at-point ()
  "Jump to the definition of the entity at point."
  (interactive)
  (let* ((button (button-at (point)))
         (entity (when button
                  (or (button-get button 'entity)
                      (button-label button)))))
    (if entity
        (semantic-ns--jump-to-definition entity)
      (message "No entity at point"))))

(defun semantic-ns--jump-to-definition (entity)
  "Jump to the definition of ENTITY by searching for contract/def."
  (let* ((entity-str (if (symbolp entity) (symbol-name entity) entity))
         ;; Remove leading : if present
         (entity-kw (if (string-prefix-p ":" entity-str)
                        entity-str
                      (concat ":" entity-str))))
    (if (not (cider-connected-p))
        (message "CIDER not connected")
      (message "Searching for definition of %s..." entity-kw)
      ;; Use cider-sync-request:ns-path to find which namespace to search
      (let* ((search-cmd (format "grep -r \"contract/def.*%s\" --include=\"*.clj\" ."
                                (shell-quote-argument entity-kw)))
             (default-directory (cider-current-dir))
             (results (shell-command-to-string search-cmd)))
        (if (string-empty-p (string-trim results))
            (message "Definition not found for %s" entity-kw)
          ;; Parse first result: "file:line:content"
          (let* ((first-line (car (split-string results "\n" t)))
                 (parts (split-string first-line ":"))
                 (file (car parts))
                 (line (string-to-number (cadr parts))))
            (find-file (expand-file-name file default-directory))
            (goto-char (point-min))
            (forward-line (1- line))
            (recenter)
            (message "Found definition at %s:%d" file line)))))))

(defun semantic-ns--insert-entity (entity)
  "Insert ENTITY with proper face and make it clickable."
  (let ((entity-str (if (symbolp entity) (symbol-name entity) entity)))
    (insert-text-button entity-str
                        'face 'semantic-ns-entity-face
                        'entity entity-str
                        'action (let ((e entity-str))
                                  (lambda (_) (semantic-ns-entity-info e)))
                        'follow-link t
                        'help-echo "Click: show info, d: jump to definition")))

(defun semantic-ns--insert-aspect (aspect)
  "Insert ASPECT with proper face and make it clickable."
  (let ((aspect-str (if (symbolp aspect) (symbol-name aspect) aspect)))
    (insert-text-button aspect-str
                        'face 'semantic-ns-aspect-face
                        'action (let ((a aspect-str))
                                  (lambda (_) (semantic-ns-find-by-aspect a)))
                        'follow-link t
                        'help-echo "Click to find entities with this aspect")))

(defun semantic-ns--insert-data-key (key)
  "Insert data KEY with proper face and make it clickable."
  (let ((key-str (if (symbolp key) (symbol-name key) key)))
    (insert-text-button key-str
                        'face 'font-lock-variable-name-face
                        'action (let ((k key-str))
                                  (lambda (_) (semantic-ns-producers k)))
                        'follow-link t
                        'help-echo "Click to find producers")))

(defun semantic-ns--insert-set (set-obj)
  "Insert a set as clickable elements.
Handles edn-set representations from parseedn or other set formats."
  (let ((set-items
         (cond
          ;; If it's a string representation like (edn-set :a :b :c)
          ((stringp set-obj)
           (if (string-prefix-p "(edn-set " set-obj)
               ;; Extract items from (edn-set :a :b :c) format
               (let* ((content (substring set-obj 9 -1))  ; Remove "(edn-set " and ")"
                      (items (split-string content " " t)))
                 items)
             (list set-obj)))
          ;; If it's a vector
          ((vectorp set-obj) (append set-obj nil))
          ;; If it's a list
          ((listp set-obj) set-obj)
          ;; Single item
          (t (list set-obj)))))
    ;; Display items with #{} notation
    (insert "#{")
    (let ((first t))
      (dolist (item set-items)
        (unless first (insert " "))
        (setq first nil)
        (if (semantic-ns--keyword-like-p item)
            (semantic-ns--insert-keyword-value item)
          (insert (format "%s" item)))))
    (insert "}")))

;;; Business Semantics Display Helpers

(defun semantic-ns--entity-keyword-p (value)
  "Check if VALUE is an entity reference keyword.
Returns t if the namespace indicates it's an entity (endpoint, fn, component, etc.)."
  (let ((name (cond ((symbolp value) (symbol-name value))
                    ((stringp value) value)
                    (t nil))))
    (and name
         (string-prefix-p ":" name)
         (string-match-p "^:\\(endpoint\\|fn\\|function\\|component\\|schema\\|protocol\\|pattern\\|constraint\\|failure-mode\\|value\\|role\\|experience\\)/" name))))

(defun semantic-ns--keyword-like-p (value)
  "Return t if VALUE looks like a keyword string or symbol."
  (let ((name (cond ((symbolp value) (symbol-name value))
                    ((stringp value) value)
                    (t nil))))
    (and name (string-prefix-p ":" name))))

(defun semantic-ns--insert-keyword-value (value)
  "Insert keyword VALUE as entity or data-key link."
  (if (semantic-ns--entity-keyword-p value)
      (semantic-ns--insert-entity value)
    (semantic-ns--insert-data-key value)))

(defun semantic-ns--insert-value (value)
  "Insert VALUE with basic formatting and links when possible."
  (cond
   ((semantic-ns--keyword-like-p value)
    (semantic-ns--insert-keyword-value value))
   ((hash-table-p value)
    (insert "{")
    (let ((first t))
      (dolist (pair (semantic-ns--map-entries value))
        (unless first (insert ", "))
        (setq first nil)
        (semantic-ns--insert-value (car pair))
        (insert " ")
        (semantic-ns--insert-value (cdr pair))))
    (insert "}"))
   ((vectorp value)
    (insert "[")
    (let ((first t))
      (dolist (item (append value nil))
        (unless first (insert " "))
        (setq first nil)
        (semantic-ns--insert-value item)))
    (insert "]"))
   ((listp value)
    (semantic-ns--insert-set value))
   (t
    (insert (format "%s" value)))))

(defun semantic-ns--insert-property-value (value)
  "Insert a property VALUE with keyword collections displayed as clickable items."
  (let ((items (cond ((vectorp value) (append value nil))
                     ((listp value) value)
                     (t nil))))
    (cond
     ((and items (seq-every-p #'semantic-ns--keyword-like-p items))
      (dolist (item items)
        (insert "  ")
        (semantic-ns--insert-keyword-value item)
        (insert "\n")))
     ((hash-table-p value)
      (let ((entries (semantic-ns--map-entries value)))
        (if entries
            (dolist (pair entries)
              (insert "  ")
              (if (semantic-ns--keyword-like-p (car pair))
                  (semantic-ns--insert-keyword-value (car pair))
                (semantic-ns--insert-value (car pair)))
              (insert " ")
              (semantic-ns--insert-value (cdr pair))
              (insert "\n"))
          (insert "  {}\n"))))
     (items
      (insert "  ")
      (semantic-ns--insert-value value)
      (insert "\n"))
     (t
      (insert "  ")
      (semantic-ns--insert-value value)
      (insert "\n")))))

(defun semantic-ns--insert-business-entity (entity)
  "Insert ENTITY with business-specific face."
  (let ((entity-str (if (symbolp entity) (symbol-name entity) entity)))
    (insert-text-button entity-str
                        'face 'semantic-ns-entity-face
                        'entity entity-str
                        'action (let ((e entity-str))
                                  (lambda (_) (semantic-ns-business-info e)))
                        'follow-link t
                        'help-echo "Click: show business info")))

(defun semantic-ns--insert-business-metadata (title value face)
  "Insert a business metadata field with TITLE and VALUE, styled with FACE.
Entity references (keywords) are rendered as clickable links."
  (when value
    (semantic-ns--insert-subheader title)
    (cond
      ((listp value)
       (dolist (item value)
         (insert "  • ")
         (if (and (symbolp item) (semantic-ns--entity-keyword-p item))
             (semantic-ns--insert-entity item)
           (insert (format "%s" item)))
         (insert "\n")))
      ((vectorp value)
       (dolist (item (append value nil))
         (insert "  • ")
         (if (and (symbolp item) (semantic-ns--entity-keyword-p item))
             (semantic-ns--insert-entity item)
           (insert (format "%s" item)))
         (insert "\n")))
      ((hash-table-p value)
       (maphash (lambda (k v)
                  (insert (format "  %s: %s\n" k v)))
                value))
      ((and (symbolp value) (semantic-ns--entity-keyword-p value))
       (insert "  ")
       (semantic-ns--insert-entity value)
       (insert "\n"))
      (t
       (insert (format "  %s\n" value))))
    (insert "\n")))

(defun semantic-ns--format-business-info (info)
  "Format and display business entity INFO in current buffer."
  (let ((biz-type (semantic-ns--get info 'entity/type))
        (metadata (semantic-ns--get info 'business/metadata)))
    (when biz-type
      (insert (format "Type: %s\n\n" biz-type)))
    (when metadata
      (pcase biz-type
        (:business-pattern
         (when-let ((p (semantic-ns--get metadata 'principle)))
           (semantic-ns--insert-business-metadata "Principle" p nil))
         (when-let ((j (semantic-ns--get metadata 'justification)))
           (semantic-ns--insert-business-metadata "Justification" j nil))
         (when-let ((ux (semantic-ns--get metadata 'user-experience)))
           (semantic-ns--insert-business-metadata "User Experience" ux nil))
         (when-let ((bv (semantic-ns--get metadata 'business-value)))
           (semantic-ns--insert-business-metadata "Business Value" bv nil)))

        (:constraint
         (when-let ((r (semantic-ns--get metadata 'rationale)))
           (semantic-ns--insert-business-metadata "Rationale" r nil))
         (when-let ((cr (semantic-ns--get metadata 'compliance-requirement)))
           (semantic-ns--insert-business-metadata "Compliance" cr nil))
         (when-let ((eb (semantic-ns--get metadata 'enforced-by)))
           (semantic-ns--insert-business-metadata "Enforced By" eb nil)))

        (:failure-mode
         (when-let ((tb (semantic-ns--get metadata 'triggered-by)))
           (semantic-ns--insert-business-metadata "Triggered By" tb nil))
         (when-let ((det (semantic-ns--get metadata 'detection)))
           (semantic-ns--insert-business-metadata "Detection" det nil))
         (when-let ((rp (semantic-ns--get metadata 'recovery-path)))
           (semantic-ns--insert-business-metadata "Recovery Path" rp nil))
         (when-let ((freq (semantic-ns--get metadata 'frequency)))
           (semantic-ns--insert-business-metadata "Frequency" freq nil)))

        (:value-proposition
         (when-let ((bp (semantic-ns--get metadata 'business-problem)))
           (semantic-ns--insert-business-metadata "Problem" bp nil))
         (when-let ((sol (semantic-ns--get metadata 'solution)))
           (semantic-ns--insert-business-metadata "Solution" sol nil))
         (when-let ((us (semantic-ns--get metadata 'user-segment)))
           (semantic-ns--insert-business-metadata "User Segment" us nil))
         (when-let ((bv (semantic-ns--get metadata 'business-value)))
           (semantic-ns--insert-business-metadata "Business Value" bv nil)))

        (:user-role
         (when-let ((desc (semantic-ns--get metadata 'description)))
           (semantic-ns--insert-business-metadata "Description" desc nil))
         (when-let ((resp (semantic-ns--get metadata 'responsibilities)))
           (semantic-ns--insert-business-metadata "Responsibilities" resp nil))
         (when-let ((da (semantic-ns--get metadata 'data-access)))
           (semantic-ns--insert-business-metadata "Data Access" da nil)))

        (:user-experience
         (when-let ((uj (semantic-ns--get metadata 'user-journey)))
           (semantic-ns--insert-business-metadata "User Journey" uj nil))
         (when-let ((ttc (semantic-ns--get metadata 'time-to-complete)))
           (semantic-ns--insert-business-metadata "Time to Complete" ttc nil))
         (when-let ((us (semantic-ns--get metadata 'user-sentiment)))
           (semantic-ns--insert-business-metadata "User Sentiment" us nil))
         (when-let ((fp (semantic-ns--get metadata 'friction-points)))
           (semantic-ns--insert-business-metadata "Friction Points" fp nil)))))))

;;; Completion with Annotations

(defun semantic-ns--get-entities-with-metadata ()
  "Get all entities with their type for completion (cached)."
  (if (semantic-ns--cache-valid-p)
      semantic-ns--entity-cache
    (let ((result (semantic-ns--eval "(list-all-entities)")))
      (setq semantic-ns--cache-time (float-time)
            semantic-ns--entity-cache (or result [])))))


(defun semantic-ns--get-aspects-with-counts ()
  "Get all aspects with usage counts (cached)."
  (if (semantic-ns--cache-valid-p)
      semantic-ns--aspect-cache
    (setq semantic-ns--cache-time (float-time)
          semantic-ns--aspect-cache
          (semantic-ns--eval-safe "(list-aspects)" []))))

(defun semantic-ns--completing-read-entity (prompt)
  "Read entity with completion and type annotations using PROMPT."
  (let* ((metadata (semantic-ns--get-entities-with-metadata))
         (metadata-list (semantic-ns--to-list metadata))
         (candidates (mapcar (lambda (e)
                               (let* ((dev-id (semantic-ns--get e 'entity/dev-id))
                                      (dev-id-str (if (symbolp dev-id) (symbol-name dev-id) dev-id)))
                                 ;; Parseedn returns symbols with : prefix, remove it
                                 (if (string-prefix-p ":" dev-id-str)
                                     (substring dev-id-str 1)
                                   dev-id-str)))
                             metadata-list))
         (annotate-fn (lambda (candidate)
                        (when-let* ((entity (seq-find
                                             (lambda (e)
                                               (let* ((dev-id (semantic-ns--get e 'entity/dev-id))
                                                      (dev-id-str (if (symbolp dev-id)
                                                                      (symbol-name dev-id)
                                                                    dev-id)))
                                                 (string= (if (string-prefix-p ":" dev-id-str)
                                                              (substring dev-id-str 1)
                                                            dev-id-str)
                                                          candidate)))
                                             metadata-list))
                                    (type (semantic-ns--get entity 'entity/type)))
                          (propertize (format " [%s]" type)
                                      'face 'semantic-ns-annotation-face))))
         (completion-extra-properties `(:annotation-function ,annotate-fn)))
    (completing-read prompt candidates nil t)))

(defun semantic-ns--completing-read-aspect (prompt)
  "Read aspect with completion and entity count annotations using PROMPT."
  (let* ((aspects-data (semantic-ns--get-aspects-with-counts))
         (aspects-list (semantic-ns--to-list aspects-data))
         (candidates (mapcar (lambda (a)
                               (let ((aspect (or (semantic-ns--get a 'aspect/aspect)
                                                 (semantic-ns--get a 'aspect))))
                                 (if (symbolp aspect) (symbol-name aspect) aspect)))
                            aspects-list))
         (annotate-fn (lambda (candidate)
                        (when-let* ((data (seq-find
                                           (lambda (a)
                                             (let ((aspect (or (semantic-ns--get a 'aspect/aspect)
                                                               (semantic-ns--get a 'aspect))))
                                               (string= (if (symbolp aspect)
                                                            (symbol-name aspect)
                                                          aspect)
                                                        candidate)))
                                           aspects-list))
                                    (count (or (semantic-ns--get data 'aspect/count)
                                               (semantic-ns--get data 'count))))
                          (propertize (format " (%d entities)" count)
                                      'face 'semantic-ns-annotation-face))))
        (completion-extra-properties `(:annotation-function ,annotate-fn)))
    (completing-read prompt candidates nil t)))

(defun semantic-ns--completing-read-data-key (prompt)
  "Read data key with completion and producer/consumer info using PROMPT."
  (let* ((keys-raw (semantic-ns--eval-safe "(complete-data-key \"\")" []))
         (keys (semantic-ns--to-list keys-raw))
         (annotate-fn (lambda (candidate)
                        ;; Candidate comes from complete-data-key as string without : prefix
                        ;; Clojure functions expect keyword with : prefix
                        (let ((kw (semantic-ns--to-keyword candidate)))
                          (let ((producers (or (semantic-ns--eval-safe
                                                (format "(count (producers-of %s))" kw)) 0))
                                (consumers (or (semantic-ns--eval-safe
                                                (format "(count (consumers-of %s))" kw)) 0)))
                            (propertize (format " [%d producers, %d consumers]"
                                                producers consumers)
                                        'face 'semantic-ns-annotation-face)))))
         (completion-extra-properties `(:annotation-function ,annotate-fn)))
    (completing-read prompt keys nil t)))

;;; Major Mode

(defvar semantic-ns-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "g") 'semantic-ns-refresh)
    (define-key map (kbd "?") 'semantic-ns)
    (define-key map (kbd "RET") 'push-button)
    (define-key map (kbd "<return>") 'push-button)
    (define-key map (kbd "d") 'semantic-ns-jump-to-definition-at-point)
    (define-key map (kbd "TAB") 'forward-button)
    (define-key map (kbd "<backtab>") 'backward-button)
    (define-key map (kbd "n") 'forward-button)
    (define-key map (kbd "p") 'backward-button)
    map)
  "Keymap for semantic-ns-mode.")

(define-derived-mode semantic-ns-mode special-mode "Semantic-NS"
  "Major mode for semantic namespace query results.

Navigation:
  RET/click - Show entity info
  d         - Jump to definition (grep for contract/def)
  TAB/n     - Next link
  S-TAB/p   - Previous link
  g         - Refresh
  ?         - Show semantic-ns menu
  q         - Quit window

\\{semantic-ns-mode-map}"
  (setq buffer-read-only t)
  (setq-local revert-buffer-function #'semantic-ns--revert-buffer))

(defvar-local semantic-ns--last-command nil
  "Last command used to populate this buffer.")

(defun semantic-ns--revert-buffer (&rest _)
  "Revert by re-running the last command."
  (when semantic-ns--last-command
    (funcall semantic-ns--last-command)))

(defun semantic-ns-refresh ()
  "Refresh current buffer and invalidate cache."
  (interactive)
  (semantic-ns--invalidate-cache)
  (revert-buffer))

;;; Interactive Commands

;;;###autoload
(defun semantic-ns-list-entities ()
  "List all registered semantic entities."
  (interactive)
  (semantic-ns--invalidate-cache)
  (let* ((entities (semantic-ns--get-entities-with-metadata))
         (buf (semantic-ns--buffer "entities")))
    (when semantic-ns-debug
      (message "[semantic-ns DEBUG] Received %d entities" (length entities))
      (when (> (length entities) 0)
        (message "[semantic-ns DEBUG] First entity: %S" (elt entities 0))
        (message "[semantic-ns DEBUG] First entity type: %s"
                 (type-of (elt entities 0)))))
    (with-current-buffer buf
      (setq semantic-ns--last-command #'semantic-ns-list-entities)
      (semantic-ns--insert-header "Registered Entities")
      (if (or (not entities) (= 0 (length entities)))
          (insert (propertize "  No entities found.\n  Make sure you've initialized the registry (e.g., (app/init-registry!))\n"
                              'face 'font-lock-comment-face))
        ;; Convert vector to list for seq-group-by
        (let* ((entities-list (semantic-ns--to-list entities))
               ;; Try to get type, falling back to checking multiple key formats
               (by-type (seq-group-by
                         (lambda (e)
                           (let ((entity-type (or (semantic-ns--get e 'entity/type)
                                                  (semantic-ns--get e 'type))))
                             (when semantic-ns-debug
                               (message "[semantic-ns DEBUG] Entity type result: %S for entity: %S"
                                        entity-type e))
                             (or entity-type 'unknown)))
                         entities-list)))
          (when semantic-ns-debug
            (message "[semantic-ns DEBUG] Grouped by type: %S" (mapcar #'car by-type)))
          ;; Display all entity types in a specific order
          (dolist (type '(endpoint function component protocol
                          business-pattern constraint failure-mode
                          value-proposition user-role user-experience
                          other unknown))
            ;; Look for both :type and type in the grouped result
            (let ((items (or (cdr (assq type by-type))
                            (cdr (assq (intern (concat ":" (symbol-name type))) by-type)))))
              (when items
                ;; Custom display names for better readability
                (let ((display-name (pcase type
                                      ('business-pattern "Business Patterns")
                                      ('constraint "Constraints")
                                      ('failure-mode "Failure Modes")
                                      ('value-proposition "Value Propositions")
                                      ('user-role "User Roles")
                                      ('user-experience "User Experiences")
                                      (_ (format "%ss" (capitalize (symbol-name type)))))))
                  (semantic-ns--insert-subheader
                   (format "%s (%d)" display-name (length items)))
                  (dolist (item items)
                    (insert "  ")
                    (let ((dev-id (semantic-ns--get item 'entity/dev-id)))
                      (when semantic-ns-debug
                        (message "[semantic-ns DEBUG] Extracted dev-id: %S from item: %S" dev-id item))
                      (semantic-ns--insert-entity dev-id))
                    (insert "\n"))
                  (insert "\n")))))))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))


;;;###autoload
(defun semantic-ns-list-aspects ()
  "List all semantic aspects with usage counts."
  (interactive)
  (semantic-ns--invalidate-cache)
  (let* ((aspects (semantic-ns--get-aspects-with-counts))
         (buf (semantic-ns--buffer "aspects")))
    (if (not aspects)
        (message "No aspects found or error retrieving aspects")
      (with-current-buffer buf
        (setq semantic-ns--last-command #'semantic-ns-list-aspects)
        (semantic-ns--insert-header "Semantic Aspects")
        ;; Handle vector or list
        (let ((aspects-list (if (vectorp aspects) (append aspects nil) aspects)))
          (dolist (a aspects-list)
            (insert "  ")
            (semantic-ns--insert-aspect (or (semantic-ns--get a 'aspect/aspect)
                                            (semantic-ns--get a 'aspect)))
            (insert (propertize (format " (%d)\n" (or (semantic-ns--get a 'aspect/count)
                                                       (semantic-ns--get a 'count) 0))
                                'face 'semantic-ns-annotation-face))))
        (goto-char (point-min))
        (read-only-mode 1))
      (pop-to-buffer buf))))

;;;###autoload
(defun semantic-ns-find-by-aspect (aspect)
  "Find all entities with ASPECT."
  (interactive
   (list (semantic-ns--completing-read-aspect "Aspect: ")))
  (let* ((aspect-kw (semantic-ns--to-keyword aspect))
         (entities (semantic-ns--eval-safe
                    (format "(entities-with-aspect %s)" aspect-kw) []))
         (buf (semantic-ns--buffer (format "aspect:%s" aspect))))
    (with-current-buffer buf
      (setq semantic-ns--last-command (lambda () (semantic-ns-find-by-aspect aspect)))
      (semantic-ns--insert-header (format "Entities with :%s" aspect))
      (let ((entities-list (if (vectorp entities) (append entities nil) entities)))
        (if (and entities-list (> (length entities-list) 0))
            (dolist (entity entities-list)
              (insert "  ")
              (semantic-ns--insert-entity entity)
              (insert "\n"))
          (insert "  (none found)\n")))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun semantic-ns-entity-info (entity)
  "Show detailed info for ENTITY."
  (interactive
   (list (semantic-ns--completing-read-entity "Entity: ")))
  (let* ((entity-kw (semantic-ns--to-keyword entity))
         (info (semantic-ns--eval-safe (format "(entity-info %s)" entity-kw)))
         (buf (semantic-ns--buffer (format "entity:%s" entity))))
    (with-current-buffer buf
      (setq semantic-ns--last-command (lambda () (semantic-ns-entity-info entity)))
      (semantic-ns--insert-header (format "Entity: %s" entity))
      (if (not info)
          (insert "  (no info available or error)\n")
        ;; Aspects - try both entity/aspects and aspects
        (when-let ((aspects (or (semantic-ns--get info 'entity/aspects)
                                (semantic-ns--get info 'aspects))))
          (semantic-ns--insert-subheader "Aspects")
          (let ((aspects-list (if (vectorp aspects) (append aspects nil) aspects)))
            (dolist (aspect aspects-list)
              (insert "  ")
              (semantic-ns--insert-aspect aspect)
              (insert "\n")))
          (insert "\n"))

        ;; Properties - one section per definition key (when present)
        (when-let ((definition-values (or (semantic-ns--get info 'entity/definition-values)
                                          (semantic-ns--get info 'definition-values)
                                          (semantic-ns--get info 'entity/props)
                                          (semantic-ns--get info 'props))))
          (let* ((entries (semantic-ns--map-entries definition-values))
                 (entries (seq-filter (lambda (pair) (not (null (cdr pair)))) entries)))
            (when entries
              (dolist (pair entries)
                (semantic-ns--insert-subheader (semantic-ns--to-string (car pair)))
                (semantic-ns--insert-property-value (cdr pair))
                (insert "\n"))))))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun semantic-ns-data-flow (entity)
  "Show data flow for ENTITY."
  (interactive
   (list (semantic-ns--completing-read-entity "Function: ")))
  (let* ((entity-kw (semantic-ns--to-keyword entity))
         (flow (semantic-ns--eval-safe (format "(data-flow %s)" entity-kw)))
         (buf (semantic-ns--buffer (format "flow:%s" entity))))
    (with-current-buffer buf
      (setq semantic-ns--last-command (lambda () (semantic-ns-data-flow entity)))
      (semantic-ns--insert-header (format "Data Flow: %s" entity))
      (let ((flow-list (semantic-ns--to-list flow)))
        (if (and flow-list (> (length flow-list) 0))
            (dolist (item flow-list)
              (let ((needs (or (semantic-ns--get item 'dataflow/needs)
                               (semantic-ns--get item 'needs)))
                    (produced-by (or (semantic-ns--get item 'dataflow/produced-by)
                                     (semantic-ns--get item 'produced-by)))
                    (satisfied (or (semantic-ns--get item 'dataflow/satisfied?)
                                   (semantic-ns--get item 'satisfied?))))
                (insert "  ")
                (semantic-ns--insert-data-key needs)
                (insert " <- ")
                (let ((produced-list (semantic-ns--to-list produced-by)))
                  (if (and produced-list (> (length produced-list) 0))
                      (progn
                        (semantic-ns--insert-entity (car produced-list))
                        (insert (if satisfied
                                    (propertize " [ok]" 'face 'semantic-ns-success-face)
                                  (propertize " [?]" 'face 'semantic-ns-warning-face))))
                    (insert (propertize "(endpoint input)" 'face 'font-lock-comment-face))))
                (insert "\n")))
          (insert "  (no data flow info)\n")))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun semantic-ns-check-invariants ()
  "Run invariant validation and show results."
  (interactive)
  (let* ((result (semantic-ns--eval-safe "(check-invariants)"))
         (buf (semantic-ns--buffer "invariants")))
    (with-current-buffer buf
      (setq semantic-ns--last-command #'semantic-ns-check-invariants)
      (semantic-ns--insert-header "Axiom Validation")
      (if (not result)
          (insert (propertize "[ERROR] Could not check invariants\n"
                              'face 'semantic-ns-error-face))
        (if (semantic-ns--get result 'valid?)
            (insert (propertize "[PASS] All invariants pass\n\n"
                                'face 'semantic-ns-success-face))
          (insert (propertize "[FAIL] Validation failed\n\n"
                              'face 'semantic-ns-error-face)))

        (when-let ((errors (semantic-ns--get result 'errors)))
          (let ((errors-list (semantic-ns--to-list errors)))
            (when (> (length errors-list) 0)
              (semantic-ns--insert-subheader "Errors")
              (dolist (e errors-list)
                (insert (propertize "  [X] " 'face 'semantic-ns-error-face))
                (insert (format "%s\n" (semantic-ns--get e 'invariant)))
                (insert (format "      %s\n" (semantic-ns--get e 'message))))
              (insert "\n"))))

        (when-let ((warnings (semantic-ns--get result 'warnings)))
          (let ((warnings-list (semantic-ns--to-list warnings)))
            (when (> (length warnings-list) 0)
              (semantic-ns--insert-subheader "Warnings")
              (dolist (w warnings-list)
                (insert (propertize "  [!] " 'face 'semantic-ns-warning-face))
                (insert (format "%s\n" (semantic-ns--get w 'invariant)))
                (insert (format "      %s\n" (semantic-ns--get w 'message))))
              (insert "\n")))))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun semantic-ns-dependents (entity)
  "Find what depends on ENTITY."
  (interactive
   (list (semantic-ns--completing-read-entity "Entity: ")))
  (let* ((entity-kw (semantic-ns--to-keyword entity))
         (deps (semantic-ns--eval-safe (format "(dependents-of %s)" entity-kw) []))
         (buf (semantic-ns--buffer (format "dependents:%s" entity))))
    (with-current-buffer buf
      (setq semantic-ns--last-command (lambda () (semantic-ns-dependents entity)))
      (semantic-ns--insert-header (format "Dependents of %s" entity))
      (insert (propertize "What depends on this entity:\n\n"
                          'face 'font-lock-comment-face))
      (let ((deps-list (semantic-ns--to-list deps)))
        (if (and deps-list (> (length deps-list) 0))
            (dolist (d deps-list)
              (insert "  ")
              (semantic-ns--insert-entity d)
              (insert "\n"))
          (insert "  (nothing depends on this)\n")))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun semantic-ns-dependencies (entity)
  "Find ENTITY's dependencies."
  (interactive
   (list (semantic-ns--completing-read-entity "Entity: ")))
  (let* ((entity-kw (semantic-ns--to-keyword entity))
         (deps (semantic-ns--eval-safe (format "(dependencies-of %s)" entity-kw) []))
         (buf (semantic-ns--buffer (format "deps:%s" entity))))
    (with-current-buffer buf
      (setq semantic-ns--last-command (lambda () (semantic-ns-dependencies entity)))
      (semantic-ns--insert-header (format "Dependencies of %s" entity))
      (insert (propertize "What this entity depends on:\n\n"
                          'face 'font-lock-comment-face))
      (let ((deps-list (semantic-ns--to-list deps)))
        (if (and deps-list (> (length deps-list) 0))
            (dolist (d deps-list)
              (insert "  ")
              (semantic-ns--insert-entity d)
              (insert "\n"))
          (insert "  (no dependencies)\n")))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun semantic-ns-producers (data-key)
  "Find functions that produce DATA-KEY."
  (interactive
   (list (semantic-ns--completing-read-data-key "Data key: ")))
  (let* ((data-kw (semantic-ns--to-keyword data-key))
         (producers (semantic-ns--eval-safe (format "(producers-of %s)" data-kw) []))
         (buf (semantic-ns--buffer (format "producers:%s" data-key))))
    (with-current-buffer buf
      (setq semantic-ns--last-command (lambda () (semantic-ns-producers data-key)))
      (semantic-ns--insert-header (format "Producers of %s" data-key))
      (let ((producers-list (semantic-ns--to-list producers)))
        (if (and producers-list (> (length producers-list) 0))
            (dolist (p producers-list)
              (insert "  ")
              (semantic-ns--insert-entity p)
              (insert "\n"))
          (insert "  (no producers - endpoint input?)\n")))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun semantic-ns-consumers (data-key)
  "Find functions that consume DATA-KEY."
  (interactive
   (list (semantic-ns--completing-read-data-key "Data key: ")))
  (let* ((data-kw (semantic-ns--to-keyword data-key))
         (consumers (semantic-ns--eval-safe (format "(consumers-of %s)" data-kw) []))
         (buf (semantic-ns--buffer (format "consumers:%s" data-key))))
    (with-current-buffer buf
      (setq semantic-ns--last-command (lambda () (semantic-ns-consumers data-key)))
      (semantic-ns--insert-header (format "Consumers of %s" data-key))
      (let ((consumers-list (semantic-ns--to-list consumers)))
        (if (and consumers-list (> (length consumers-list) 0))
            (dolist (c consumers-list)
              (insert "  ")
              (semantic-ns--insert-entity c)
              (insert "\n"))
          (insert "  (no consumers)\n")))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun semantic-ns-execution-order ()
  "Show topologically sorted execution order."
  (interactive)
  (let* ((order (semantic-ns--eval-safe "(execution-order)" []))
         (buf (semantic-ns--buffer "execution-order")))
    (with-current-buffer buf
      (setq semantic-ns--last-command #'semantic-ns-execution-order)
      (semantic-ns--insert-header "Execution Order (by data flow)")
      (let ((order-list (semantic-ns--to-list order)))
        (if (and order-list (> (length order-list) 0))
            (let ((n 1))
              (dolist (entity order-list)
                (insert (format "  %d. " n))
                (semantic-ns--insert-entity entity)
                (insert "\n")
                (setq n (1+ n))))
          (insert "  (no execution order available)\n")))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun semantic-ns-system-summary ()
  "Show system overview."
  (interactive)
  (let* ((summary (semantic-ns--eval-safe "(system-summary)"))
         (buf (semantic-ns--buffer "summary")))
    (with-current-buffer buf
      (setq semantic-ns--last-command #'semantic-ns-system-summary)
      (semantic-ns--insert-header "System Summary")
      (if (not summary)
          (insert "  (no summary available)\n")
        (insert (format "%s\n\n" (semantic-ns--get summary 'summary)))
        (when-let ((domains (semantic-ns--get summary 'domains)))
          (semantic-ns--insert-subheader "Domains")
          (let ((domains-list (semantic-ns--to-list domains)))
            (dolist (d domains-list)
              (insert "  ")
              (semantic-ns--insert-aspect d)
              (insert "\n")))))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun semantic-ns-generate-docs ()
  "Generate and display markdown documentation."
  (interactive)
  (let* ((md (semantic-ns--eval-safe "(generate-markdown)"))
         (buf (semantic-ns--buffer "docs.md")))
    (with-current-buffer buf
      (if (not md)
          (insert "Error generating documentation\n")
        (insert md))
      (goto-char (point-min))
      (when (fboundp 'markdown-mode)
        (markdown-mode))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;; Transient Menu

;;;###autoload
(defun semantic-ns-trace-data-flow (data-key)
  "Trace how DATA-KEY flows through the system."
  (interactive (list (semantic-ns--completing-read-data-key "Data key: ")))
  (let* ((data-kw (semantic-ns--to-keyword data-key))
         (result (semantic-ns--eval (format "(trace-data-flow %s)" data-kw)))
         (buf (semantic-ns--buffer (format "trace-flow-%s" data-key))))
    (with-current-buffer buf
      (semantic-ns--insert-header (format "Data Flow: %s" data-key))

      (if (not result)
          (insert "  (error retrieving data flow)\n")
        (semantic-ns--insert-subheader "Producers")
        (let ((producers (semantic-ns--to-list (or (semantic-ns--get result 'dataflow/produced-by)
                                                    (semantic-ns--get result 'produced-by)))))
          (if (and producers (> (length producers) 0))
              (dolist (prod producers)
                (insert "  ")
                (semantic-ns--insert-entity prod)
                (insert "\n"))
            (insert "  (none)\n")))
        (insert "\n")

        (semantic-ns--insert-subheader "Consumers")
        (let ((consumers (semantic-ns--to-list (or (semantic-ns--get result 'dataflow/consumed-by)
                                                    (semantic-ns--get result 'consumed-by)))))
          (if (and consumers (> (length consumers) 0))
              (dolist (consumer consumers)
                (insert "  ")
                (semantic-ns--insert-entity consumer)
                (insert "\n"))
            (insert "  (none)\n")))
        (insert "\n")

        (let ((connected (or (semantic-ns--get result 'dataflow/connected?)
                             (semantic-ns--get result 'connected?))))
          (insert (format "Connected: %s\n" (if connected "✓" "✗")))))

      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun semantic-ns-impact-of-change (entity-id)
  "Show what would be affected if ENTITY-ID changes."
  (interactive (list (semantic-ns--completing-read-entity "Entity: ")))
  (let* ((entity-kw (semantic-ns--to-keyword entity-id))
         (result (semantic-ns--eval (format "(impact-of-change %s)" entity-kw)))
         (buf (semantic-ns--buffer (format "impact-%s" entity-id))))
    (with-current-buffer buf
      (semantic-ns--insert-header (format "Impact Analysis: %s" entity-id))

      (if (not result)
          (insert "  (error retrieving impact data)\n")
        (semantic-ns--insert-subheader "Produces")
        (let ((produces (semantic-ns--to-list (or (semantic-ns--get result 'impact/produces)
                                                   (semantic-ns--get result 'entity/produces)))))
          (if (and produces (> (length produces) 0))
              (dolist (item produces)
                (insert (format "  %s\n" item)))
            (insert "  (nothing)\n")))
        (insert "\n")

        (semantic-ns--insert-subheader "Direct Dependents (would break)")
        (let ((deps (semantic-ns--to-list (or (semantic-ns--get result 'impact/direct-dependents)
                                               (semantic-ns--get result 'direct-dependents)))))
          (if (and deps (> (length deps) 0))
              (dolist (dep deps)
                (insert "  ")
                (semantic-ns--insert-entity dep)
                (insert "\n"))
            (insert "  (none)\n"))))

      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun semantic-ns-domain-coupling ()
  "Analyze inter-domain dependencies."
  (interactive)
  (let* ((result (semantic-ns--eval "(domain-coupling)"))
         (buf (semantic-ns--buffer "domain-coupling")))
    (with-current-buffer buf
      (semantic-ns--insert-header "Domain Coupling Analysis")

      (if (not result)
          (insert "  (error retrieving coupling data)\n")
        (let ((coupling-list (semantic-ns--to-list result)))
          (if (and coupling-list (> (length coupling-list) 0))
              (dolist (item coupling-list)
                (let ((domain (or (semantic-ns--get item 'coupling/domain)
                                  (semantic-ns--get item 'domain)))
                      (depends-on (semantic-ns--to-list (or (semantic-ns--get item 'coupling/depends-on)
                                                             (semantic-ns--get item 'depends-on))))
                      (entity-count (or (semantic-ns--get item 'coupling/entity-count)
                                        (semantic-ns--get item 'entity-count)
                                        0)))
                  (when domain
                    (semantic-ns--insert-subheader (format "%s (%d entities)" domain entity-count))
                    (if (and depends-on (> (length depends-on) 0))
                        (progn
                          (insert "  Depends on:\n")
                          (dolist (dep depends-on)
                            (insert (format "    %s\n" dep))))
                      (insert "  (no dependencies)\n"))
                    (insert "\n"))))
            (insert "  (no domain coupling data)\n"))))

      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun semantic-ns-pii-surface ()
  "Show all entities handling PII."
  (interactive)
  (let* ((result (semantic-ns--eval "(pii-surface)"))
         (buf (semantic-ns--buffer "pii-surface")))
    (with-current-buffer buf
      (semantic-ns--insert-header "PII Surface Analysis")

      (if (not result)
          (insert "  (error retrieving PII data)\n")
        (let ((items (semantic-ns--to-list result)))
          (if (and items (> (length items) 0))
              (dolist (item items)
                (let ((id (or (semantic-ns--get item 'pii/id)
                              (semantic-ns--get item 'id)))
                      (audited (or (semantic-ns--get item 'pii/audited?)
                                   (semantic-ns--get item 'audited?))))
                  (when id
                    (insert "  ")
                    (semantic-ns--insert-entity id)
                    (insert (format " [%s]\n" (if audited "audited ✓" "NOT AUDITED ✗"))))))
            (insert "  (no PII-handling entities found)\n"))))

      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun semantic-ns-error-handler-coverage ()
  "Check error handler coverage."
  (interactive)
  (let* ((result (semantic-ns--eval "(error-handler-coverage)"))
         (buf (semantic-ns--buffer "error-handlers")))
    (with-current-buffer buf
      (semantic-ns--insert-header "Error Handler Coverage")

      (semantic-ns--insert-subheader "Registered Handlers")
      (let ((handlers (semantic-ns--to-list (or (semantic-ns--get result 'error-handler/handlers)
                                                 (semantic-ns--get result 'handlers)))))
        (dolist (handler handlers)
          (let ((id (or (semantic-ns--get handler 'error-handler/id)
                        (semantic-ns--get handler 'id))))
            (insert "  ")
            (semantic-ns--insert-entity id)
            (insert "\n"))))
      (insert "\n")

      (semantic-ns--insert-subheader "Coverage Analysis")
      (let ((coverage (semantic-ns--to-list (or (semantic-ns--get result 'error-handler/coverage)
                                                 (semantic-ns--get result 'coverage)))))
        (dolist (item coverage)
          (let ((entity (or (semantic-ns--get item 'error-handler/entity)
                            (semantic-ns--get item 'entity)))
                (concern (or (semantic-ns--get item 'error-handler/concern)
                             (semantic-ns--get item 'concern)))
                (has-handler (or (semantic-ns--get item 'error-handler/has-handler?)
                                 (semantic-ns--get item 'has-handler?))))
            (insert (format "  %s - %s: %s\n"
                          entity concern
                          (if has-handler "✓" "✗"))))))

      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun semantic-ns-suggest-aspects (entity-id)
  "Suggest aspects for ENTITY-ID based on similar entries."
  (interactive (list (semantic-ns--completing-read-entity "Entity: ")))
  (let* ((entity-kw (semantic-ns--to-keyword entity-id))
         (result (semantic-ns--eval (format "(suggest-aspects %s)" entity-kw)))
         (buf (semantic-ns--buffer (format "suggest-%s" entity-id))))
    (with-current-buffer buf
      (semantic-ns--insert-header (format "Aspect Suggestions: %s" entity-id))

      (semantic-ns--insert-subheader "Similar Entries")
      (let ((similar (semantic-ns--to-list (or (semantic-ns--get result 'ontology/similar-entries)
                                                (semantic-ns--get result 'similar-entries)))))
        (dolist (sim similar)
          ;; Filter out edn-set wrapper representations
          (unless (and (stringp sim) (string-prefix-p "(edn-set" sim))
            (insert (format "  %s\n" sim)))))
      (insert "\n")

      (semantic-ns--insert-subheader "Suggested Aspects")
      (let ((suggested (semantic-ns--to-list (or (semantic-ns--get result 'ontology/suggested-aspects)
                                                  (semantic-ns--get result 'suggested-aspects)))))
        (if (and suggested (> (length suggested) 0))
            (dolist (asp suggested)
              (insert "  ")
              (semantic-ns--insert-aspect asp)
              (insert "\n"))
          (insert "  (no suggestions)\n")))
      (insert "\n")

      (insert (format "Rationale: %s\n" (or (semantic-ns--get result 'ontology/rationale)
                                             (semantic-ns--get result 'rationale))))

      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun semantic-ns-inspect-entity (entity-id)
  "Quick inspection of ENTITY-ID."
  (interactive (list (semantic-ns--completing-read-entity "Entity: ")))
  (let* ((entity-kw (semantic-ns--to-keyword entity-id))
         (result (semantic-ns--eval (format "(inspect-entity %s)" entity-kw)))
         (buf (semantic-ns--buffer (format "inspect-%s" entity-id))))
    (with-current-buffer buf
      (semantic-ns--insert-header (format "Inspect: %s" entity-id))

      (insert (format "Dev ID: %s\n" (or (semantic-ns--get result 'inspection/dev-id)
                                          (semantic-ns--get result 'entity/dev-id))))
      (when-let ((aspect-count (or (semantic-ns--get result 'inspection/aspect-count)
                                   (semantic-ns--get result 'aspect-count))))
        (insert (format "Aspects: %d\n" aspect-count)))
      (insert "\n")

      (semantic-ns--insert-subheader "Semantic Identity")
      (let ((identity (semantic-ns--to-list (or (semantic-ns--get result 'inspection/semantic-identity)
                                                 (semantic-ns--get result 'semantic-identity)))))
        (dolist (asp identity)
          (insert "  ")
          (semantic-ns--insert-aspect asp)
          (insert "\n")))
      (insert "\n")

      (when-let ((docs (or (semantic-ns--get result 'inspection/docs)
                           (semantic-ns--get result 'docs))))
        (semantic-ns--insert-subheader "Documentation")
        (insert (format "%s\n" docs)))

      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun semantic-ns-aspect-catalog ()
  "Show all aspects with usage statistics."
  (interactive)
  (let* ((result (semantic-ns--eval "(aspect-catalog)"))
         (buf (semantic-ns--buffer "aspect-catalog")))
    (with-current-buffer buf
      (semantic-ns--insert-header "Aspect Catalog")

      (let ((catalog (semantic-ns--to-list result)))
        (dolist (ns-group catalog)
          (let* ((ns-name (or (semantic-ns--get ns-group 'aspect-catalog/namespace)
                              (semantic-ns--get ns-group 'namespace)))
                 (items (or (semantic-ns--get ns-group 'aspect-catalog/items)
                            (semantic-ns--get ns-group 'items))))
            (when ns-name
              (semantic-ns--insert-subheader (format "Namespace: %s" ns-name))
              (dolist (asp-data (semantic-ns--to-list items))
                (let ((aspect (or (semantic-ns--get asp-data 'aspect-catalog/aspect)
                                  (semantic-ns--get asp-data 'aspect)))
                      (usage (or (semantic-ns--get asp-data 'aspect-catalog/usage-count)
                                 (semantic-ns--get asp-data 'usage-count)
                                 0)))
                  (insert "  ")
                  (semantic-ns--insert-aspect aspect)
                  (insert (format " (%d uses)\n" usage))))
              (insert "\n")))))

      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun semantic-ns-list-templates ()
  "List available entity templates."
  (interactive)
  (let* ((result (semantic-ns--eval "(list-templates)"))
         (buf (semantic-ns--buffer "templates")))
    (with-current-buffer buf
      (semantic-ns--insert-header "Entity Templates")

      (let ((templates (semantic-ns--to-list result)))
        (dolist (template templates)
          (let ((name (or (semantic-ns--get template 'template/name)
                          (semantic-ns--get template 'name)))
                (params (semantic-ns--to-list (or (semantic-ns--get template 'template/params)
                                                   (semantic-ns--get template 'params))))
                (desc (or (semantic-ns--get template 'template/description)
                          (semantic-ns--get template 'description))))
            ;; name can be string or symbol, safely convert
            (when name
              (semantic-ns--insert-subheader (semantic-ns--to-string name))
              (when desc
                (insert (format "  Description: %s\n" desc)))
              (when params
                (insert "  Parameters: ")
                ;; params can be strings or symbols, safely convert each
                (insert (mapconcat #'semantic-ns--to-string params ", ")))
              (insert "\n\n")))))

      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun semantic-ns-similar-entities (entity-id)
  "Find entities with similar semantic profiles to ENTITY-ID."
  (interactive (list (semantic-ns--completing-read-entity "Entity: ")))
  (let* ((entity-kw (semantic-ns--to-keyword entity-id))
         (result (semantic-ns--eval (format "(semantic-similarity %s)" entity-kw)))
         (buf (semantic-ns--buffer (format "similar:%s" entity-id))))
    (with-current-buffer buf
      (setq semantic-ns--last-command (lambda () (semantic-ns-similar-entities entity-id)))
      (semantic-ns--insert-header (format "Similar to %s" entity-id))

      (let ((items (semantic-ns--to-list result)))
        (if (and items (> (length items) 0))
            (dolist (item items)
              (let ((entity (or (semantic-ns--get item 'similarity/entity)
                                (semantic-ns--get item 'entity)))
                    (score (or (semantic-ns--get item 'similarity/score)
                               (semantic-ns--get item 'score)))
                    (shared (semantic-ns--to-list (or (semantic-ns--get item 'similarity/shared-aspects)
                                                       (semantic-ns--get item 'shared-aspects)))))
                (insert "  ")
                (semantic-ns--insert-entity entity)
                (insert (format " (%.0f%% similar)\n" (* 100 (or score 0))))
                (when shared
                  (insert "    Shared: ")
                  (let ((first t))
                    (dolist (asp shared)
                      (unless first (insert ", "))
                      (setq first nil)
                      (semantic-ns--insert-aspect asp)))
                  (insert "\n"))
                (insert "\n")))
          (insert "  (no similar entities found)\n")))

      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun semantic-ns-by-tier ()
  "Show all entities grouped by architectural tier."
  (interactive)
  (let* ((result (semantic-ns--eval "(by-tier)"))
         (buf (semantic-ns--buffer "by-tier")))
    (with-current-buffer buf
      (setq semantic-ns--last-command #'semantic-ns-by-tier)
      (semantic-ns--insert-header "Entities by Tier")

      (let ((tiers (semantic-ns--to-list result)))
        (dolist (tier-data tiers)
          (let ((tier-name (or (semantic-ns--get tier-data 'tier/name)
                               (semantic-ns--get tier-data 'name)))
                (entities (semantic-ns--to-list (or (semantic-ns--get tier-data 'tier/entities)
                                                     (semantic-ns--get tier-data 'entities))))
                (count (or (semantic-ns--get tier-data 'tier/count)
                           (semantic-ns--get tier-data 'count)
                           0)))
            (when tier-name
              (semantic-ns--insert-subheader (format "%s (%d)" tier-name count))
              (if entities
                  (dolist (entity entities)
                    (insert "  ")
                    (semantic-ns--insert-entity entity)
                    (insert "\n"))
                (insert "  (none)\n"))
              (insert "\n")))))

      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun semantic-ns-architecture-view ()
  "Show architecture documentation: entities by tier and domain."
  (interactive)
  (let* ((result (semantic-ns--eval "(architecture-view)"))
         (buf (semantic-ns--buffer "architecture")))
    (with-current-buffer buf
      (setq semantic-ns--last-command #'semantic-ns-architecture-view)
      (semantic-ns--insert-header "Architecture View")

      ;; Tiers section
      (semantic-ns--insert-subheader "By Tier")
      (let ((tiers (semantic-ns--to-list (or (semantic-ns--get result 'architecture/tiers)
                                              (semantic-ns--get result 'tiers)))))
        (dolist (tier-data tiers)
          (let ((tier-name (or (semantic-ns--get tier-data 'tier/name)
                               (semantic-ns--get tier-data 'name)))
                (count (or (semantic-ns--get tier-data 'tier/count)
                           (semantic-ns--get tier-data 'count)
                           0)))
            (when tier-name
              (insert (format "  %s: %d entities\n" tier-name count))))))
      (insert "\n")

      ;; Domains section
      (semantic-ns--insert-subheader "By Domain")
      (let ((domains (semantic-ns--to-list (or (semantic-ns--get result 'architecture/domains)
                                                (semantic-ns--get result 'domains)))))
        (dolist (domain-data domains)
          (let ((domain-name (or (semantic-ns--get domain-data 'domain/name)
                                 (semantic-ns--get domain-data 'name)))
                (entities (semantic-ns--to-list (or (semantic-ns--get domain-data 'domain/entities)
                                                     (semantic-ns--get domain-data 'entities))))
                (count (or (semantic-ns--get domain-data 'domain/count)
                           (semantic-ns--get domain-data 'count)
                           0)))
            (when domain-name
              (insert "\n")
              (insert (format "  %s (%d)\n" domain-name count))
              (dolist (entity entities)
                (insert "    ")
                (semantic-ns--insert-entity entity)
                (insert "\n"))))))

      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun semantic-ns-operations-view ()
  "Show operations documentation: external integrations, pure functions, OAuth."
  (interactive)
  (let* ((result (semantic-ns--eval "(operations-view)"))
         (buf (semantic-ns--buffer "operations")))
    (with-current-buffer buf
      (setq semantic-ns--last-command #'semantic-ns-operations-view)
      (semantic-ns--insert-header "Operations View")

      ;; External integrations
      (let ((ext (or (semantic-ns--get result 'operations/external-integrations)
                     (semantic-ns--get result 'external-integrations))))
        (semantic-ns--insert-subheader "External Integrations")
        (insert (format "  %s\n\n" (or (semantic-ns--get ext 'description) "")))
        (let ((entities (semantic-ns--to-list (semantic-ns--get ext 'entities))))
          (dolist (entity entities)
            (insert "  ")
            (semantic-ns--insert-entity entity)
            (insert "\n")))
        (insert "\n  Operational Concerns:\n")
        (let ((concerns (semantic-ns--to-list (semantic-ns--get ext 'concerns))))
          (dolist (concern concerns)
            (insert (format "    - %s\n" concern)))))
      (insert "\n")

      ;; Pure functions
      (let ((pure (or (semantic-ns--get result 'operations/pure-functions)
                      (semantic-ns--get result 'pure-functions))))
        (semantic-ns--insert-subheader "Pure Functions")
        (insert (format "  %s\n\n" (or (semantic-ns--get pure 'description) "")))
        (let ((entities (semantic-ns--to-list (semantic-ns--get pure 'entities))))
          (dolist (entity entities)
            (insert "  ")
            (semantic-ns--insert-entity entity)
            (insert "\n")))
        (insert "\n  Operational Concerns:\n")
        (let ((concerns (semantic-ns--to-list (semantic-ns--get pure 'concerns))))
          (dolist (concern concerns)
            (insert (format "    - %s\n" concern)))))
      (insert "\n")

      ;; OAuth dependencies
      (let ((oauth (or (semantic-ns--get result 'operations/oauth-dependencies)
                       (semantic-ns--get result 'oauth-dependencies))))
        (semantic-ns--insert-subheader "OAuth Dependencies")
        (insert (format "  %s\n\n" (or (semantic-ns--get oauth 'description) "")))
        (let ((entities (semantic-ns--to-list (semantic-ns--get oauth 'entities))))
          (if entities
              (dolist (entity entities)
                (insert "  ")
                (semantic-ns--insert-entity entity)
                (insert "\n"))
            (insert "  (none)\n")))
        (insert "\n  Operational Concerns:\n")
        (let ((concerns (semantic-ns--to-list (semantic-ns--get oauth 'concerns))))
          (dolist (concern concerns)
            (insert (format "    - %s\n" concern)))))

      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun semantic-ns-aspect-impact (aspect)
  "Analyze what would be affected by changes to ASPECT."
  (interactive (list (semantic-ns--completing-read-aspect "Aspect: ")))
  (let* ((aspect-kw (semantic-ns--to-keyword aspect))
         (result (semantic-ns--eval (format "(aspect-impact %s)" aspect-kw)))
         (buf (semantic-ns--buffer (format "aspect-impact:%s" aspect))))
    (with-current-buffer buf
      (setq semantic-ns--last-command (lambda () (semantic-ns-aspect-impact aspect)))
      (semantic-ns--insert-header (format "Impact of changing %s" aspect))

      (let ((total (or (semantic-ns--get result 'aspect-impact/total)
                       (semantic-ns--get result 'total)
                       0)))
        (insert (format "Total affected: %d entities\n\n" total)))

      ;; By tier
      (semantic-ns--insert-subheader "By Tier")
      (let ((by-tier (or (semantic-ns--get result 'aspect-impact/by-tier)
                         (semantic-ns--get result 'by-tier))))
        (if (hash-table-p by-tier)
            (maphash (lambda (k v)
                       (insert (format "  %s: %d\n" k v)))
                     by-tier)
          (when (listp by-tier)
            (dolist (pair by-tier)
              (insert (format "  %s: %s\n" (car pair) (cdr pair)))))))
      (insert "\n")

      ;; By type
      (semantic-ns--insert-subheader "By Type")
      (let ((by-type (or (semantic-ns--get result 'aspect-impact/by-type)
                         (semantic-ns--get result 'by-type))))
        (if (hash-table-p by-type)
            (maphash (lambda (k v)
                       (insert (format "  %s: %d\n" k v)))
                     by-type)
          (when (listp by-type)
            (dolist (pair by-type)
              (insert (format "  %s: %s\n" (car pair) (cdr pair)))))))
      (insert "\n")

      ;; Examples
      (semantic-ns--insert-subheader "Example Entities")
      (let ((examples (semantic-ns--to-list (or (semantic-ns--get result 'aspect-impact/examples)
                                                 (semantic-ns--get result 'examples)))))
        (if examples
            (dolist (entity examples)
              (insert "  ")
              (semantic-ns--insert-entity entity)
              (insert "\n"))
          (insert "  (none)\n")))

      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun semantic-ns-preview-refactor (old-aspect new-aspect)
  "Preview what would happen if OLD-ASPECT is renamed to NEW-ASPECT."
  (interactive
   (list (semantic-ns--completing-read-aspect "Old aspect: ")
         (read-string "New aspect: ")))
  (let* ((old-kw (semantic-ns--to-keyword old-aspect))
         (new-kw (semantic-ns--to-keyword new-aspect))
         (result (semantic-ns--eval (format "(preview-refactor-aspect %s %s)" old-kw new-kw)))
         (buf (semantic-ns--buffer (format "refactor:%s->%s" old-aspect new-aspect))))
    (with-current-buffer buf
      (setq semantic-ns--last-command (lambda () (semantic-ns-preview-refactor old-aspect new-aspect)))
      (semantic-ns--insert-header (format "Refactor Preview: %s → %s" old-aspect new-aspect))

      (let ((affected (or (semantic-ns--get result 'refactor/affected-count)
                          (semantic-ns--get result 'affected-count)
                          0))
            (conflicts (semantic-ns--to-list (or (semantic-ns--get result 'refactor/conflicts)
                                                  (semantic-ns--get result 'conflicts)))))
        (insert (format "Affected entities: %d\n" affected))
        (if (and conflicts (> (length conflicts) 0))
            (insert (propertize (format "Conflicts: %d (would collide!)\n" (length conflicts))
                                'face 'semantic-ns-error-face))
          (insert (propertize "Conflicts: none\n" 'face 'semantic-ns-success-face))))
      (insert "\n")

      (semantic-ns--insert-subheader "Migrations")
      (let ((migrations (semantic-ns--to-list (or (semantic-ns--get result 'refactor/migrations)
                                                   (semantic-ns--get result 'migrations)))))
        (if migrations
            (dolist (mig migrations)
              (let ((dev-id (or (semantic-ns--get mig 'refactor/dev-id)
                                (semantic-ns--get mig 'dev-id)))
                    (will-collide (or (semantic-ns--get mig 'refactor/will-collide?)
                                      (semantic-ns--get mig 'will-collide?))))
                (insert "  ")
                (semantic-ns--insert-entity dev-id)
                (when will-collide
                  (insert (propertize " [COLLISION]" 'face 'semantic-ns-error-face)))
                (insert "\n")))
          (insert "  (no migrations needed)\n")))

      (insert "\n")
      (insert (propertize "Note: This is a dry-run preview. No changes have been made.\n"
                          'face 'font-lock-comment-face))

      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun semantic-ns-llm-context ()
  "Export full documentation context for LLM consumption."
  (interactive)
  (let* ((result (semantic-ns--eval "(llm-context)"))
         (buf (semantic-ns--buffer "llm-context")))
    (with-current-buffer buf
      (semantic-ns--insert-header "LLM Documentation Context")
      (insert (propertize "Full system context optimized for LLM consumption.\n\n"
                          'face 'font-lock-comment-face))
      (if result
          (let ((pp-str (pp-to-string result)))
            (insert pp-str))
        (insert "(error retrieving context)\n"))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;; LSP Helpers Integration
;; These functions use lsp_helpers.clj for source-level operations

;;;###autoload
(defun semantic-ns-hover-at-point ()
  "Show semantic hover info for keyword at point.
Uses lsp-helpers/hover-info for markdown-formatted display."
  (interactive)
  (let* ((sym (thing-at-point 'symbol t))
         (kw (if (and sym (string-prefix-p ":" sym)) sym (concat ":" sym)))
         (code (format "(do (require '[%s :as lsp]) (lsp/hover-info %s))"
                       semantic-ns-lsp-helpers-ns kw))
         (result (cider-nrepl-sync-request:eval code))
         (value (nrepl-dict-get result "value")))
    (if (and value (not (string= value "nil")))
        (let ((buf (semantic-ns--buffer (format "hover:%s" sym))))
          (with-current-buffer buf
            ;; Parse the string and display
            (insert (read value))
            (goto-char (point-min))
            (when (fboundp 'markdown-mode)
              (markdown-mode))
            (read-only-mode 1))
          (display-buffer buf))
      (message "No semantic info for %s" sym))))

;;;###autoload
(defun semantic-ns-find-usages (dev-id)
  "Find all usages of DEV-ID in source files.
Uses lsp-helpers/find-dev-id-usages to search src/ and test/ directories."
  (interactive (list (semantic-ns--completing-read-entity "Entity: ")))
  (let* ((kw (semantic-ns--to-keyword dev-id))
         (code (format "(do (require '[%s :as lsp]) (vec (lsp/find-dev-id-usages %s)))"
                       semantic-ns-lsp-helpers-ns kw))
         (usages (semantic-ns--eval code))
         (buf (semantic-ns--buffer (format "usages:%s" dev-id))))
    (with-current-buffer buf
      (setq semantic-ns--last-command (lambda () (semantic-ns-find-usages dev-id)))
      (semantic-ns--insert-header (format "Usages of %s" dev-id))
      (let ((usages-list (semantic-ns--to-list usages)))
        (if (and usages-list (> (length usages-list) 0))
            (dolist (usage usages-list)
              (let ((file (semantic-ns--get usage 'file))
                    (line (semantic-ns--get usage 'line))
                    (content (semantic-ns--get usage 'content)))
                (insert "  ")
                ;; Make file:line clickable
                (insert-text-button (format "%s:%d" file line)
                                    'face 'link
                                    'action (lambda (_)
                                              (find-file file)
                                              (goto-char (point-min))
                                              (forward-line (1- line)))
                                    'follow-link t)
                (insert "\n")
                (insert (format "    %s\n\n" content))))
          (insert "  (no usages found)\n")))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun semantic-ns-export-for-lsp ()
  "Export registry to JSON files for external tools.
Writes to .clj-kondo/.cache/semantic-registry.json"
  (interactive)
  (let* ((code "(do (require '[atlas.tooling.lsp-export :as exp]) (exp/export-now!))")
         (result (semantic-ns--eval code)))
    (if result
        (message "Exported %d entities to %s"
                 (semantic-ns--get result 'exported-identities)
                 (car (semantic-ns--to-list (semantic-ns--get result 'files))))
      (message "Export failed"))))

;;;###autoload
(defun semantic-ns-write-search-index ()
  "Write EDN search index for external tools.
Writes to .clj-kondo/.cache/semantic-search-index.edn"
  (interactive)
  (let* ((code (format "(do (require '[%s :as lsp]) (lsp/write-search-index))"
                       semantic-ns-lsp-helpers-ns))
         (result (semantic-ns--eval code)))
    (if result
        (message "Wrote %d entries to %s"
                 (semantic-ns--get result 'entries)
                 (semantic-ns--get result 'file))
      (message "Write failed"))))

;;; Business Semantics Commands

;;;###autoload
(defun semantic-ns-list-business-entities ()
  "List all business-layer entities grouped by type."
  (interactive)
  (semantic-ns--invalidate-cache)
  (let* ((entities (semantic-ns--eval-safe "(list-business-entities)" nil))
         (buf (semantic-ns--buffer "business-entities")))
    (with-current-buffer buf
      (setq semantic-ns--last-command #'semantic-ns-list-business-entities)
      (semantic-ns--insert-header "Business Semantics")

      (let ((patterns (semantic-ns--to-list (semantic-ns--get entities 'business/patterns)))
            (constraints (semantic-ns--to-list (semantic-ns--get entities 'business/constraints)))
            (failures (semantic-ns--to-list (semantic-ns--get entities 'business/failure-modes)))
            (values (semantic-ns--to-list (semantic-ns--get entities 'business/values)))
            (roles (semantic-ns--to-list (semantic-ns--get entities 'business/roles)))
            (experiences (semantic-ns--to-list (semantic-ns--get entities 'business/experiences))))

        (when (> (length patterns) 0)
          (semantic-ns--insert-subheader (format "Patterns (%d)" (length patterns)))
          (dolist (p patterns)
            (insert "  ")
            (semantic-ns--insert-business-entity p)
            (insert "\n"))
          (insert "\n"))

        (when (> (length constraints) 0)
          (semantic-ns--insert-subheader (format "Constraints (%d)" (length constraints)))
          (dolist (c constraints)
            (insert "  ")
            (semantic-ns--insert-business-entity c)
            (insert "\n"))
          (insert "\n"))

        (when (> (length failures) 0)
          (semantic-ns--insert-subheader (format "Failure Modes (%d)" (length failures)))
          (dolist (f failures)
            (insert "  ")
            (semantic-ns--insert-business-entity f)
            (insert "\n"))
          (insert "\n"))

        (when (> (length values) 0)
          (semantic-ns--insert-subheader (format "Value Propositions (%d)" (length values)))
          (dolist (v values)
            (insert "  ")
            (semantic-ns--insert-business-entity v)
            (insert "\n"))
          (insert "\n"))

        (when (> (length roles) 0)
          (semantic-ns--insert-subheader (format "User Roles (%d)" (length roles)))
          (dolist (r roles)
            (insert "  ")
            (semantic-ns--insert-business-entity r)
            (insert "\n"))
          (insert "\n"))

        (when (> (length experiences) 0)
          (semantic-ns--insert-subheader (format "User Experiences (%d)" (length experiences)))
          (dolist (e experiences)
            (insert "  ")
            (semantic-ns--insert-business-entity e)
            (insert "\n"))
          (insert "\n")))

      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun semantic-ns-business-info (entity)
  "Show detailed business semantics info for ENTITY."
  (interactive
   (list (semantic-ns--completing-read-entity "Business entity: ")))
  (let* ((entity-kw (semantic-ns--to-keyword entity))
         (info (semantic-ns--eval-safe (format "(business-entity-info %s)" entity-kw)))
         (buf (semantic-ns--buffer (format "business:%s" entity))))
    (with-current-buffer buf
      (setq semantic-ns--last-command (lambda () (semantic-ns-business-info entity)))
      (semantic-ns--insert-header (format "Business: %s" entity))
      (if (not info)
          (insert "  (no info available)\n")
        (semantic-ns--format-business-info info)

        ;; Show implementations
        (let ((impls (semantic-ns--to-list (semantic-ns--get info 'business/implements-in))))
          (when (and impls (> (length impls) 0))
            (semantic-ns--insert-subheader "Implemented In")
            (dolist (impl impls)
              (insert "  ")
              (semantic-ns--insert-entity impl)
              (insert "\n"))
            (insert "\n"))))

      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun semantic-ns-implementations (business-aspect)
  "Show all technical entities implementing BUSINESS-ASPECT."
  (interactive
   (list (semantic-ns--completing-read-aspect "Business aspect: ")))
  (let* ((aspect-kw (semantic-ns--to-keyword business-aspect))
         (entities (semantic-ns--eval-safe (format "(entities-implementing %s)" aspect-kw) []))
         (buf (semantic-ns--buffer (format "impl:%s" business-aspect))))
    (with-current-buffer buf
      (setq semantic-ns--last-command (lambda () (semantic-ns-implementations business-aspect)))
      (semantic-ns--insert-header (format "Entities implementing %s" business-aspect))
      (let ((entities-list (semantic-ns--to-list entities)))
        (if (and entities-list (> (length entities-list) 0))
            (dolist (entity entities-list)
              (insert "  ")
              (semantic-ns--insert-entity entity)
              (insert "\n"))
          (insert "  (no implementations found)\n")))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun semantic-ns-business-of (entity)
  "Show all business aspects applied to technical ENTITY."
  (interactive
   (list (semantic-ns--completing-read-entity "Entity: ")))
  (let* ((entity-kw (semantic-ns--to-keyword entity))
         (business (semantic-ns--eval-safe (format "(business-aspects-of %s)" entity-kw) {}))
         (buf (semantic-ns--buffer (format "business-of:%s" entity))))
    (with-current-buffer buf
      (setq semantic-ns--last-command (lambda () (semantic-ns-business-of entity)))
      (semantic-ns--insert-header (format "Business aspects of %s" entity))

      (let ((patterns (semantic-ns--to-list (semantic-ns--get business 'business/patterns)))
            (constraints (semantic-ns--to-list (semantic-ns--get business 'business/constraints)))
            (failures (semantic-ns--to-list (semantic-ns--get business 'business/failure-modes)))
            (values (semantic-ns--to-list (semantic-ns--get business 'business/values)))
            (roles (semantic-ns--to-list (semantic-ns--get business 'business/roles)))
            (experiences (semantic-ns--to-list (semantic-ns--get business 'business/experiences))))

        (if (and (= 0 (length patterns))
                 (= 0 (length constraints))
                 (= 0 (length failures))
                 (= 0 (length values))
                 (= 0 (length roles))
                 (= 0 (length experiences)))
            (insert "  (no business aspects)\n")
          (progn
            (when (> (length patterns) 0)
              (semantic-ns--insert-subheader "Patterns")
              (dolist (p patterns)
                (insert "  ")
                (semantic-ns--insert-aspect p)
                (insert "\n"))
              (insert "\n"))

            (when (> (length constraints) 0)
              (semantic-ns--insert-subheader "Constraints")
              (dolist (c constraints)
                (insert "  ")
                (semantic-ns--insert-aspect c)
                (insert "\n"))
              (insert "\n"))

            (when (> (length failures) 0)
              (semantic-ns--insert-subheader "Failure Modes")
              (dolist (f failures)
                (insert "  ")
                (semantic-ns--insert-aspect f)
                (insert "\n"))
              (insert "\n"))

            (when (> (length values) 0)
              (semantic-ns--insert-subheader "Values")
              (dolist (v values)
                (insert "  ")
                (semantic-ns--insert-aspect v)
                (insert "\n"))
              (insert "\n"))

            (when (> (length roles) 0)
              (semantic-ns--insert-subheader "Roles")
              (dolist (r roles)
                (insert "  ")
                (semantic-ns--insert-aspect r)
                (insert "\n"))
              (insert "\n"))

            (when (> (length experiences) 0)
              (semantic-ns--insert-subheader "Experiences")
              (dolist (e experiences)
                (insert "  ")
                (semantic-ns--insert-aspect e)
                (insert "\n"))
              (insert "\n")))))

      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;; Protocol Commands

;;;###autoload
(defun semantic-ns-list-protocols ()
  "List all registered protocols."
  (interactive)
  (let* ((protocols (semantic-ns--eval-safe "(list-protocols)" []))
         (buf (semantic-ns--buffer "protocols")))
    (with-current-buffer buf
      (setq semantic-ns--last-command #'semantic-ns-list-protocols)
      (semantic-ns--insert-header "Registered Protocols")
      (let ((protocols-list (semantic-ns--to-list protocols)))
        (if (and protocols-list (> (length protocols-list) 0))
            (dolist (proto protocols-list)
              (let ((proto-id (or (semantic-ns--get proto 'protocol/id)
                                  (semantic-ns--get proto 'id)))
                    (functions (semantic-ns--to-list (or (semantic-ns--get proto 'protocol/functions)
                                                          (semantic-ns--get proto 'functions)))))
                (insert "  ")
                (semantic-ns--insert-entity proto-id)
                (insert "\n")
                (when functions
                  (dolist (fn functions)
                    (insert "    ")
                    (semantic-ns--insert-data-key fn)
                    (insert "\n")))
                (insert "\n")))
          (insert "  (no protocols found)\n")))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun semantic-ns-protocol-info (protocol-id)
  "Show detailed information about PROTOCOL-ID."
  (interactive
   (list (semantic-ns--completing-read-entity "Protocol: ")))
  (let* ((protocol-kw (semantic-ns--to-keyword protocol-id))
         (info (semantic-ns--eval-safe (format "(protocol-info %s)" protocol-kw)))
         (buf (semantic-ns--buffer (format "protocol:%s" protocol-id))))
    (with-current-buffer buf
      (setq semantic-ns--last-command (lambda () (semantic-ns-protocol-info protocol-id)))
      (semantic-ns--insert-header (format "Protocol: %s" protocol-id))
      (if (not info)
          (insert "  (no info available)\n")
        (semantic-ns--insert-subheader "Required Functions")
        (let ((functions (semantic-ns--to-list (or (semantic-ns--get info 'protocol/functions)
                                                    (semantic-ns--get info 'functions)))))
          (if (and functions (> (length functions) 0))
              (dolist (fn functions)
                (insert "  ")
                (semantic-ns--insert-data-key fn)
                (insert "\n"))
            (insert "  (none)\n")))
        (insert "\n")

        (semantic-ns--insert-subheader "Implementers")
        (let ((implementers (semantic-ns--to-list (or (semantic-ns--get info 'protocol/implementers)
                                                       (semantic-ns--get info 'implementers)))))
          (if (and implementers (> (length implementers) 0))
              (dolist (impl implementers)
                (insert "  ")
                (semantic-ns--insert-entity impl)
                (insert "\n"))
            (insert "  (no components implement this protocol)\n"))))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun semantic-ns-component-protocols (component-id)
  "Show all protocols implemented by COMPONENT-ID."
  (interactive
   (list (semantic-ns--completing-read-entity "Component: ")))
  (let* ((component-kw (semantic-ns--to-keyword component-id))
         (protocols (semantic-ns--eval-safe (format "(component-protocols %s)" component-kw) []))
         (buf (semantic-ns--buffer (format "component-protocols:%s" component-id))))
    (with-current-buffer buf
      (setq semantic-ns--last-command (lambda () (semantic-ns-component-protocols component-id)))
      (semantic-ns--insert-header (format "Protocols implemented by %s" component-id))
      (let ((protocols-list (semantic-ns--to-list protocols)))
        (if (and protocols-list (> (length protocols-list) 0))
            (dolist (proto protocols-list)
              (insert "  ")
              (semantic-ns--insert-aspect proto)
              (insert "\n"))
          (insert "  (no protocols)\n")))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun semantic-ns-components-implementing (protocol-id)
  "Find all components implementing PROTOCOL-ID."
  (interactive
   (list (semantic-ns--completing-read-entity "Protocol: ")))
  (let* ((protocol-kw (semantic-ns--to-keyword protocol-id))
         (components (semantic-ns--eval-safe (format "(components-implementing %s)" protocol-kw) []))
         (buf (semantic-ns--buffer (format "implementing:%s" protocol-id))))
    (with-current-buffer buf
      (setq semantic-ns--last-command (lambda () (semantic-ns-components-implementing protocol-id)))
      (semantic-ns--insert-header (format "Components implementing %s" protocol-id))
      (let ((components-list (semantic-ns--to-list components)))
        (if (and components-list (> (length components-list) 0))
            (dolist (comp components-list)
              (insert "  ")
              (semantic-ns--insert-entity comp)
              (insert "\n"))
          (insert "  (no components)\n")))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;; Main Menu (Beginner/Daily Use)
(transient-define-prefix semantic-ns ()
  "Atlas: Compound Identity Registry Explorer.

This is the main menu for daily use. Press 'z' for advanced features."
  [["Browse"
    ("e" "List entities" semantic-ns-list-entities)
    ("a" "Find by aspect" semantic-ns-find-by-aspect)
    ("A" "List aspects" semantic-ns-list-aspects)]
   ["Entity Details"
    ("i" "Entity info" semantic-ns-entity-info)
    ("D" "Dependencies" semantic-ns-dependencies)
    ("R" "Dependents" semantic-ns-dependents)]
   ["Business Semantics"
    ("b" "List business entities" semantic-ns-list-business-entities)
    ("B" "Business info" semantic-ns-business-info)
    ("M" "Business aspects of" semantic-ns-business-of)]
   ["Protocols"
    ("@" "List protocols" semantic-ns-list-protocols)
    ("#" "Protocol info" semantic-ns-protocol-info)
    ("$" "Component protocols" semantic-ns-component-protocols)]
   ["Data Flow"
    ("d" "Data flow trace" semantic-ns-data-flow)
    ("p" "Producers" semantic-ns-producers)
    ("u" "Consumers" semantic-ns-consumers)
    ("x" "Execution order" semantic-ns-execution-order)]
   ["Validation"
    ("c" "Check invariants" semantic-ns-check-invariants)
    ("G" "Refresh cache" semantic-ns--invalidate-cache)
    ("!" "Toggle debug" semantic-ns-toggle-debug)]
   ["Navigation"
    ("z" "Advanced tools →" semantic-ns-advanced)]])

;;; Advanced Menu
(transient-define-prefix semantic-ns-advanced ()
  "Advanced analysis and tooling for Atlas."
  [["Architecture Analysis"
    ("t" "By tier" semantic-ns-by-tier)
    ("V" "Architecture view" semantic-ns-architecture-view)
    ("O" "Operations view" semantic-ns-operations-view)
    ("C" "Domain coupling" semantic-ns-domain-coupling)]
   ["Business Semantics"
    ("N" "Implementations of aspect" semantic-ns-implementations)]
   ["Impact & Refactoring"
    ("I" "Impact of change" semantic-ns-impact-of-change)
    ("Y" "Aspect impact" semantic-ns-aspect-impact)
    ("r" "Preview refactor" semantic-ns-preview-refactor)
    ("~" "Similar entities" semantic-ns-similar-entities)]
   ["Compliance & Quality"
    ("P" "PII surface" semantic-ns-pii-surface)
    ("E" "Error handler coverage" semantic-ns-error-handler-coverage)
    ("T" "Trace data flow" semantic-ns-trace-data-flow)]
   ["Ontology Tools"
    ("S" "Suggest aspects" semantic-ns-suggest-aspects)
    ("X" "Inspect entity" semantic-ns-inspect-entity)
    ("K" "Aspect catalog" semantic-ns-aspect-catalog)
    ("L" "List templates" semantic-ns-list-templates)]
   ["LSP & Source Code"
    ("h" "Hover at point" semantic-ns-hover-at-point)
    ("f" "Find usages" semantic-ns-find-usages)
    ("w" "Export JSON" semantic-ns-export-for-lsp)
    ("W" "Write search index" semantic-ns-write-search-index)]
   ["Documentation & Export"
    ("g" "Generate docs" semantic-ns-generate-docs)
    ("l" "LLM context" semantic-ns-llm-context)
    ("s" "System summary" semantic-ns-system-summary)]]
  [["Navigation"
    ("q" "<< Back to main menu" semantic-ns)]])

(provide 'semantic-ns)
;;; semantic-ns.el ends here
(define-key global-map (kbd "M-F") 'semantic-ns)
