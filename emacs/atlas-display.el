;;; atlas-display.el --- Display helpers for Atlas IDE support -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; Author: @tangrammer + LLMs

;;; Commentary:
;;
;; Display helpers for Atlas Emacs integration:
;; - Buffer creation and management
;; - Entity/aspect display with clickable links
;; - Business semantics formatting
;; - Major mode definition

;;; Code:

(require 'atlas-core)

;; Forward declarations for functions defined in other modules
(declare-function atlas-entity-info "atlas-browse")
(declare-function atlas-find-by-aspect "atlas-browse")
(declare-function atlas-producers "atlas-browse")
(declare-function atlas-business-info "atlas-business")

;;; Major Mode

(defvar atlas-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "g") 'atlas-refresh)
    (define-key map (kbd "?") 'atlas)
    (define-key map (kbd "RET") 'push-button)
    (define-key map (kbd "TAB") 'forward-button)
    (define-key map (kbd "<backtab>") 'backward-button)
    (define-key map (kbd "d") 'atlas-jump-to-definition-at-point)
    (define-key map (kbd "i") 'atlas-entity-info)
    (define-key map (kbd "a") 'atlas-find-by-aspect)
    map)
  "Keymap for `atlas-mode'.")

(define-derived-mode atlas-mode special-mode "Atlas"
  "Major mode for Atlas semantic registry browsing.

\\{atlas-mode-map}"
  :group 'atlas
  (setq-local revert-buffer-function #'atlas--revert-buffer)
  (setq-local truncate-lines t))

(defvar-local atlas--last-command nil
  "Last command used to populate this buffer, for refreshing.")

(defun atlas--revert-buffer (&rest _)
  "Revert an atlas buffer by re-running the last command."
  (when atlas--last-command
    (funcall atlas--last-command)))

(defun atlas-refresh ()
  "Refresh the current atlas buffer."
  (interactive)
  (atlas--revert-buffer))

;;; Buffer Helpers

(defun atlas--buffer (name)
  "Get or create atlas buffer with NAME."
  (let ((buf (get-buffer-create (format "*atlas: %s*" name))))
    (with-current-buffer buf
      (atlas-mode)
      (read-only-mode -1)
      (erase-buffer))
    buf))

(defun atlas--insert-header (text)
  "Insert TEXT as a header."
  (insert (propertize text 'face 'atlas-header-face) "\n")
  (insert (make-string (min 50 (length text)) ?-) "\n\n"))

(defun atlas--insert-subheader (text)
  "Insert TEXT as a subheader."
  (insert (propertize text 'face 'atlas-subheader-face) "\n"))

;;; Jump to Definition

(defun atlas-jump-to-definition-at-point ()
  "Jump to the definition of the entity at point."
  (interactive)
  (let* ((button (button-at (point)))
         (entity (when button
                  (or (button-get button 'entity)
                      (button-label button)))))
    (if entity
        (atlas--jump-to-definition entity)
      (message "No entity at point"))))

(defun atlas--jump-to-definition (entity)
  "Jump to the definition of ENTITY by searching for contract/def."
  (let* ((entity-str (if (symbolp entity) (symbol-name entity) entity))
         (entity-kw (if (string-prefix-p ":" entity-str)
                        entity-str
                      (concat ":" entity-str))))
    (if (not (cider-connected-p))
        (message "CIDER not connected")
      (message "Searching for definition of %s..." entity-kw)
      (let* ((search-cmd (format "grep -r \"contract/def.*%s\" --include=\"*.clj\" ."
                                (shell-quote-argument entity-kw)))
             (default-directory (cider-current-dir))
             (results (shell-command-to-string search-cmd)))
        (if (string-empty-p (string-trim results))
            (message "Definition not found for %s" entity-kw)
          (let* ((first-line (car (split-string results "\n" t)))
                 (parts (split-string first-line ":"))
                 (file (car parts))
                 (line (string-to-number (cadr parts))))
            (find-file (expand-file-name file default-directory))
            (goto-char (point-min))
            (forward-line (1- line))
            (recenter)
            (message "Found definition at %s:%d" file line)))))))

;;; Entity/Aspect Display

(defun atlas--insert-entity (entity)
  "Insert ENTITY with proper face and make it clickable."
  (let ((entity-str (if (symbolp entity) (symbol-name entity) entity)))
    (insert-text-button entity-str
                        'face 'atlas-entity-face
                        'entity entity-str
                        'action (let ((e entity-str))
                                  (lambda (_) (atlas-entity-info e)))
                        'follow-link t
                        'help-echo "Click: show info, d: jump to definition")))

(defun atlas--insert-aspect (aspect)
  "Insert ASPECT with proper face and make it clickable.
Handles edn-set objects by unwrapping and displaying as a set."
  (cond
   ;; Handle edn-set object
   ((atlas--edn-set-p aspect)
    (insert "#{")
    (let ((first t)
          (items (atlas--unwrap-edn-set aspect)))
      (dolist (item items)
        (unless first (insert " "))
        (setq first nil)
        (atlas--insert-aspect item)))
    (insert "}"))

   ;; Regular aspect handling
   (t
    (let ((aspect-str
           (cond
            ((symbolp aspect) (symbol-name aspect))
            ((stringp aspect) aspect)
            (t (format "%s" aspect)))))
      (insert-text-button aspect-str
                          'face 'atlas-aspect-face
                          'action (let ((a aspect-str))
                                    (lambda (_) (atlas-find-by-aspect a)))
                          'follow-link t
                          'help-echo "Click to find entities with this aspect")))))

(defun atlas--insert-data-key (key)
  "Insert data KEY with proper face and make it clickable."
  (let ((key-str (if (symbolp key) (symbol-name key) key)))
    (insert-text-button key-str
                        'face 'font-lock-variable-name-face
                        'action (let ((k key-str))
                                  (lambda (_) (atlas-producers k)))
                        'follow-link t
                        'help-echo "Click to find producers")))

(defun atlas--insert-set (set-obj)
  "Insert a set as clickable elements.
Handles edn-set representations from parseedn or other set formats."
  (let ((set-items
         (cond
          ((atlas--edn-set-p set-obj)
           (atlas--unwrap-edn-set set-obj))
          ((stringp set-obj)
           (if (string-prefix-p "(edn-set " set-obj)
               (let* ((content (substring set-obj 9 -1))
                      (items (split-string content " " t)))
                 items)
             (list set-obj)))
          ((vectorp set-obj) (append set-obj nil))
          ((listp set-obj) set-obj)
          (t (list set-obj)))))
    (insert "#{")
    (let ((first t))
      (dolist (item set-items)
        (unless first (insert " "))
        (setq first nil)
        (if (atlas--keyword-like-p item)
            (atlas--insert-keyword-value item)
          (insert (format "%s" item)))))
    (insert "}")))

;;; Business Semantics Display Helpers

(defun atlas--entity-keyword-p (value)
  "Check if VALUE is an entity reference keyword.
Returns t if the namespace indicates it's an entity (endpoint, fn, component, etc.)."
  (let ((name (cond ((symbolp value) (symbol-name value))
                    ((stringp value) value)
                    (t nil))))
    (and name
         (string-prefix-p ":" name)
         (string-match-p "^:\\(endpoint\\|fn\\|function\\|component\\|schema\\|protocol\\|pattern\\|constraint\\|failure-mode\\|value\\|role\\|experience\\)/" name))))

(defun atlas--keyword-like-p (value)
  "Return t if VALUE looks like a keyword string or symbol."
  (let ((name (cond ((symbolp value) (symbol-name value))
                    ((stringp value) value)
                    (t nil))))
    (and name (string-prefix-p ":" name))))

(defun atlas--insert-keyword-value (value)
  "Insert keyword VALUE as entity or data-key link."
  (if (atlas--entity-keyword-p value)
      (atlas--insert-entity value)
    (atlas--insert-data-key value)))

(defun atlas--insert-value (value)
  "Insert VALUE with basic formatting and links when possible."
  (cond
   ((atlas--keyword-like-p value)
    (atlas--insert-keyword-value value))
   ((hash-table-p value)
    (insert "{")
    (let ((first t))
      (dolist (pair (atlas--map-entries value))
        (unless first (insert ", "))
        (setq first nil)
        (atlas--insert-value (car pair))
        (insert " ")
        (atlas--insert-value (cdr pair))))
    (insert "}"))
   ((vectorp value)
    (insert "[")
    (let ((first t))
      (dolist (item (append value nil))
        (unless first (insert " "))
        (setq first nil)
        (atlas--insert-value item)))
    (insert "]"))
   ((listp value)
    (atlas--insert-set value))
   (t
    (insert (format "%s" value)))))

(defun atlas--insert-property-value (value)
  "Insert a property VALUE with keyword collections displayed as clickable items."
  (let ((items (cond ((vectorp value) (append value nil))
                     ((listp value) value)
                     (t nil))))
    (cond
     ((and items (seq-every-p #'atlas--keyword-like-p items))
      (dolist (item items)
        (insert "  ")
        (atlas--insert-keyword-value item)
        (insert "\n")))
     ((hash-table-p value)
      (let ((entries (atlas--map-entries value)))
        (if entries
            (dolist (pair entries)
              (insert "  ")
              (if (atlas--keyword-like-p (car pair))
                  (atlas--insert-keyword-value (car pair))
                (atlas--insert-value (car pair)))
              (insert " ")
              (atlas--insert-value (cdr pair))
              (insert "\n"))
          (insert "  {}\n"))))
     (items
      (insert "  ")
      (atlas--insert-value value)
      (insert "\n"))
     (t
      (insert "  ")
      (atlas--insert-value value)
      (insert "\n")))))

(defun atlas--insert-business-entity (entity)
  "Insert ENTITY with business-specific face."
  (let ((entity-str (if (symbolp entity) (symbol-name entity) entity)))
    (insert-text-button entity-str
                        'face 'atlas-entity-face
                        'entity entity-str
                        'action (let ((e entity-str))
                                  (lambda (_) (atlas-business-info e)))
                        'follow-link t
                        'help-echo "Click: show business info")))

(defun atlas--insert-business-metadata (title value face)
  "Insert a business metadata field with TITLE and VALUE, styled with FACE.
Entity references (keywords) are rendered as clickable links."
  (when value
    (atlas--insert-subheader title)
    (cond
      ((listp value)
       (dolist (item value)
         (insert "  - ")
         (if (and (symbolp item) (atlas--entity-keyword-p item))
             (atlas--insert-entity item)
           (insert (format "%s" item)))
         (insert "\n")))
      ((vectorp value)
       (dolist (item (append value nil))
         (insert "  - ")
         (if (and (symbolp item) (atlas--entity-keyword-p item))
             (atlas--insert-entity item)
           (insert (format "%s" item)))
         (insert "\n")))
      ((hash-table-p value)
       (maphash (lambda (k v)
                  (insert (format "  %s: %s\n" k v)))
                value))
      ((and (symbolp value) (atlas--entity-keyword-p value))
       (insert "  ")
       (atlas--insert-entity value)
       (insert "\n"))
      (t
       (insert (format "  %s\n" value))))
    (insert "\n")))

(defun atlas--format-business-info (info)
  "Format and display business entity INFO in current buffer."
  (let ((biz-type (atlas--get info 'entity/type))
        (metadata (atlas--get info 'business/metadata)))
    (when biz-type
      (insert (format "Type: %s\n\n" biz-type)))
    (when metadata
      (pcase biz-type
        (:business-pattern
         (when-let ((p (atlas--get metadata 'principle)))
           (atlas--insert-business-metadata "Principle" p nil))
         (when-let ((j (atlas--get metadata 'justification)))
           (atlas--insert-business-metadata "Justification" j nil))
         (when-let ((ux (atlas--get metadata 'user-experience)))
           (atlas--insert-business-metadata "User Experience" ux nil))
         (when-let ((bv (atlas--get metadata 'business-value)))
           (atlas--insert-business-metadata "Business Value" bv nil)))

        (:constraint
         (when-let ((r (atlas--get metadata 'rationale)))
           (atlas--insert-business-metadata "Rationale" r nil))
         (when-let ((cr (atlas--get metadata 'compliance-requirement)))
           (atlas--insert-business-metadata "Compliance" cr nil))
         (when-let ((eb (atlas--get metadata 'enforced-by)))
           (atlas--insert-business-metadata "Enforced By" eb nil)))

        (:failure-mode
         (when-let ((tb (atlas--get metadata 'triggered-by)))
           (atlas--insert-business-metadata "Triggered By" tb nil))
         (when-let ((det (atlas--get metadata 'detection)))
           (atlas--insert-business-metadata "Detection" det nil))
         (when-let ((rp (atlas--get metadata 'recovery-path)))
           (atlas--insert-business-metadata "Recovery Path" rp nil))
         (when-let ((freq (atlas--get metadata 'frequency)))
           (atlas--insert-business-metadata "Frequency" freq nil)))

        (:value-proposition
         (when-let ((bp (atlas--get metadata 'business-problem)))
           (atlas--insert-business-metadata "Problem" bp nil))
         (when-let ((sol (atlas--get metadata 'solution)))
           (atlas--insert-business-metadata "Solution" sol nil))
         (when-let ((us (atlas--get metadata 'user-segment)))
           (atlas--insert-business-metadata "User Segment" us nil))
         (when-let ((bv (atlas--get metadata 'business-value)))
           (atlas--insert-business-metadata "Business Value" bv nil)))

        (:user-role
         (when-let ((desc (atlas--get metadata 'description)))
           (atlas--insert-business-metadata "Description" desc nil))
         (when-let ((resp (atlas--get metadata 'responsibilities)))
           (atlas--insert-business-metadata "Responsibilities" resp nil))
         (when-let ((da (atlas--get metadata 'data-access)))
           (atlas--insert-business-metadata "Data Access" da nil)))

        (:user-experience
         (when-let ((uj (atlas--get metadata 'user-journey)))
           (atlas--insert-business-metadata "User Journey" uj nil))
         (when-let ((ttc (atlas--get metadata 'time-to-complete)))
           (atlas--insert-business-metadata "Time to Complete" ttc nil))
         (when-let ((us (atlas--get metadata 'user-sentiment)))
           (atlas--insert-business-metadata "User Sentiment" us nil))
         (when-let ((fp (atlas--get metadata 'friction-points)))
           (atlas--insert-business-metadata "Friction Points" fp nil)))))))

(provide 'atlas-display)
;;; atlas-display.el ends here
