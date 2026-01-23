;;; atlas-core.el --- Core infrastructure for Atlas IDE support -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; Author: @tangrammer + LLMs

;;; Commentary:
;;
;; Core infrastructure for Atlas Emacs integration:
;; - Configuration variables
;; - Face definitions
;; - CIDER evaluation helpers
;; - EDN parsing utilities
;; - Cache management

;;; Code:

(require 'cider)
(require 'parseedn)
(require 'seq)

;;; Configuration

(defgroup atlas nil
  "Atlas IDE support."
  :group 'clojure
  :prefix "atlas-")

(defvar atlas-ide-ns "atlas.ide"
  "Clojure namespace containing IDE functions.")

(defvar atlas-lsp-helpers-ns "atlas.tooling.lsp-helpers"
  "Clojure namespace containing LSP helper functions.")

;; Cache for completion data
(defvar atlas--entity-cache nil)
(defvar atlas--aspect-cache nil)
(defvar atlas--cache-time nil)
(defvar atlas--cache-ttl 5 "Cache TTL in seconds.")
(defvar atlas-debug nil "Enable debug messages for troubleshooting.")

;;; Faces

(defface atlas-header-face
  '((t :inherit font-lock-keyword-face :weight bold :height 1.1))
  "Face for headers in atlas buffers.")

(defface atlas-subheader-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for subheaders in atlas buffers.")

(defface atlas-entity-face
  '((t :inherit font-lock-function-name-face))
  "Face for entity names.")

(defface atlas-aspect-face
  '((t :inherit font-lock-type-face))
  "Face for aspect keywords.")

(defface atlas-error-face
  '((t :inherit error :weight bold))
  "Face for errors.")

(defface atlas-warning-face
  '((t :inherit warning))
  "Face for warnings.")

(defface atlas-success-face
  '((t :inherit success :weight bold))
  "Face for success indicators.")

(defface atlas-annotation-face
  '((t :inherit completions-annotations))
  "Face for annotations in completion.")

;;; Business Semantics Faces

(defface atlas-pattern-face
  '((t :inherit font-lock-string-face :weight bold))
  "Face for business patterns.")

(defface atlas-constraint-face
  '((t :inherit font-lock-warning-face :weight bold))
  "Face for constraints.")

(defface atlas-failure-face
  '((t :inherit error :weight bold))
  "Face for failure modes.")

(defface atlas-value-face
  '((t :inherit font-lock-constant-face :weight bold))
  "Face for value propositions.")

(defface atlas-role-face
  '((t :inherit font-lock-builtin-face :weight bold))
  "Face for user roles.")

(defface atlas-experience-face
  '((t :inherit font-lock-doc-face :weight bold))
  "Face for user experiences.")

;;; EDN Parsing Utilities

(defun atlas--edn-set-p (obj)
  "Check if OBJ is an edn-set representation from parseedn."
  (and (consp obj)
       (symbolp (car obj))
       (or (eq (car obj) 'edn-set)
           (equal (symbol-name (car obj)) "edn-set"))))

(defun atlas--unwrap-edn-set (obj)
  "Unwrap edn-set to get list of elements.
Handles both (edn-set elem1 elem2 ...) and (edn-set (elem1 elem2 ...))."
  (let ((contents (cdr obj)))
    (if (and (= (length contents) 1) (listp (car contents)))
        (car contents)
      contents)))

(defun atlas--to-list (obj)
  "Convert OBJ to list if it's a vector, otherwise return as-is.
Parseedn converts Clojure vectors to Emacs vectors and sets to (edn-set ...)."
  (cond
   ((vectorp obj) (append obj nil))
   ((atlas--edn-set-p obj)
    (atlas--unwrap-edn-set obj))
   ((listp obj) obj)
   (t (list obj))))

(defun atlas--to-string (obj)
  "Safely convert OBJ to string, handling symbols and strings."
  (cond
   ((stringp obj) obj)
   ((symbolp obj) (symbol-name obj))
   (t (format "%s" obj))))

(defun atlas--map-entries (obj)
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
    (atlas--map-entries (append obj nil)))
   (t nil)))

(defun atlas--key->names (k)
  "Return list of comparable names for keyword or symbol K.
Includes full name (without leading colon) and un-namespaced tail to support
both namespaced and non-namespaced callers."
  (let* ((raw (if (symbolp k) (symbol-name k) (format "%s" k)))
         (trimmed (if (string-prefix-p ":" raw) (substring raw 1) raw)))
    (delete-dups
     (cons trimmed
           (let ((tail (car (last (split-string trimmed "/")))))
             (when tail (list tail)))))))

(defun atlas--get (map key)
  "Get KEY from MAP (hash-table or alist) while tolerating namespaced keywords.
Matches either the full keyword name or its namespace-stripped tail so older
callers still work when API responses switched to namespaced keys."
  (when atlas-debug
    (message "[atlas DEBUG] Getting key '%s' from map type: %s"
             key
             (cond ((hash-table-p map) "hash-table")
                   ((listp map) "alist")
                   (t "unknown"))))
  (let* ((key-names (atlas--key->names key))
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
                                        (member name (atlas--key->names candidate)))
                                      key-names))
                          keys))
         (result (when match (funcall lookup match))))
    (when atlas-debug
      (message "[atlas DEBUG] Available keys in map: %S" keys)
      (message "[atlas DEBUG] Looking for key-names: %S" key-names)
      (message "[atlas DEBUG] Key lookup: %s -> match: %s -> result type: %s"
               key match (type-of result)))
    result))

(defun atlas--to-keyword (s)
  "Convert S to a proper keyword format for Clojure.
Handles both string and symbol inputs, ensuring proper : prefix."
  (let ((str (if (symbolp s) (symbol-name s) s)))
    (if (string-prefix-p ":" str)
        str
      (concat ":" str))))

;;; CIDER Evaluation

(defconst atlas--non-ide-forms
  '("do" "let" "if" "when" "cond" "case" "fn" "binding" "->" "->>" "as->" "some->" "some->>" "doto")
  "Forms that should not be qualified with the IDE namespace.")

(defun atlas--qualify-ide-form (form)
  "Qualify FORM with the IDE namespace when it looks like a simple function call."
  (if (string-match "\\`\\s-*(\\([[:alnum:]*+!\\-\\?_.]+\\)\\>" form)
      (let ((sym (match-string 1 form)))
        (if (or (string-match-p "/" sym)
                (member sym atlas--non-ide-forms))
            form
          (replace-match (concat atlas-ide-ns "/" sym) t t form 1)))
    form))

(defun atlas--eval (form)
  "Evaluate FORM in CIDER and return parsed result.
Returns nil on error with message to user."
  (unless (cider-connected-p)
    (user-error "CIDER not connected. Run cider-jack-in first"))
  (let* ((qualified-form (atlas--qualify-ide-form form))
         (code (format "(do 
(set! *print-namespace-maps* false)
(require '[%s]) %s)"
                       atlas-ide-ns qualified-form))
         (result (cider-nrepl-sync-request:eval code)))
    (when atlas-debug
      (message "[atlas DEBUG] Evaluating: %s" form))
    (if-let ((err (nrepl-dict-get result "err")))
        (progn
          (message "Clojure error: %s" err)
          nil)
      (if-let ((value (nrepl-dict-get result "value")))
          (progn
            (when atlas-debug
              (message "[atlas DEBUG] Raw EDN response (first 500 chars): %s"
                       (substring value 0 (min 500 (length value)))))
            (condition-case err
                (let ((parsed (parseedn-read-str value)))
                  (when atlas-debug
                    (message "[atlas DEBUG] Parsed result type: %s, length: %s"
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

(defun atlas--eval-safe (form &optional default)
  "Evaluate FORM, returning DEFAULT (or nil) on any error."
  (or (atlas--eval form) default))

;;; Cache Management

(defun atlas--invalidate-cache ()
  "Invalidate completion cache."
  (interactive)
  (setq atlas--entity-cache nil
        atlas--aspect-cache nil
        atlas--cache-time nil)
  (message "Cache invalidated"))

(defun atlas--cache-valid-p ()
  "Check if cache is still valid."
  (and atlas--cache-time
       (< (- (float-time) atlas--cache-time) atlas--cache-ttl)))

;;;###autoload
(defun atlas-toggle-debug ()
  "Toggle debug messages for atlas."
  (interactive)
  (setq atlas-debug (not atlas-debug))
  (message "Atlas debug mode: %s" (if atlas-debug "ENABLED" "DISABLED")))

(provide 'atlas-core)
;;; atlas-core.el ends here
