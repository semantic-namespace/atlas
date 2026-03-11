;;; atlas-at-point.el --- Context-aware at-point commands -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; Author: @tangrammer + LLMs

;;; Commentary:
;;
;; At-point commands for Atlas: use entity/data-key under cursor,
;; fall back to completing-read when nothing at point.
;;
;; Used by:
;; - atlas-mode-map (result buffers): single-key access (d, t, w, r, p, u)
;; - atlas-clj-mode (clojure buffers): C-c a prefix

;;; Code:

(require 'atlas-core)
(require 'atlas-completion)

;; Forward declarations
(declare-function atlas-browse-entity-info "atlas-browse")
(declare-function atlas-browse-dependencies "atlas-browse")
(declare-function atlas-browse-recursive-deps "atlas-browse")
(declare-function atlas-browse-deps-summary "atlas-browse")
(declare-function atlas-browse-dependents "atlas-browse")
(declare-function atlas-browse-recursive-dependents "atlas-browse")
(declare-function atlas-browse-dependents-summary "atlas-browse")
(declare-function atlas-browse-producers "atlas-browse")
(declare-function atlas-browse-consumers "atlas-browse")
(declare-function atlas-drill-entity-at-point "atlas-browse")
(declare-function atlas-browse-check-invariants "atlas-browse")
(declare-function atlas-lens-mode "atlas-lens")
(declare-function atlas "atlas")

(defun atlas--entity-or-read ()
  "Return entity at point or prompt with completing-read.
In atlas buffers, reads from button property.
In clojure buffers, reads keyword at point.
Falls back to completing-read."
  (or (when-let* ((kw (atlas--keyword-at-point)))
        (let ((s (if (string-prefix-p ":" kw) (substring kw 1) kw)))
          s))
      (atlas--completing-read-entity "Entity: ")))

(defun atlas--data-key-or-read ()
  "Return data-key at point or prompt with completing-read."
  (or (when-let* ((kw (atlas--keyword-at-point)))
        (let ((s (if (string-prefix-p ":" kw) (substring kw 1) kw)))
          s))
      (atlas--completing-read-data-key "Data key: ")))

;;;###autoload
(defun atlas-at-point-info ()
  "Show entity info for keyword at point, or prompt."
  (interactive)
  (atlas-browse-entity-info (atlas--entity-or-read)))

;;;###autoload
(defun atlas-at-point-deps ()
  "Show dependencies for keyword at point, or prompt."
  (interactive)
  (atlas-browse-dependencies (atlas--entity-or-read)))

;;;###autoload
(defun atlas-at-point-transitive-deps ()
  "Show transitive deps for keyword at point, or prompt."
  (interactive)
  (atlas-browse-recursive-deps (atlas--entity-or-read)))

;;;###autoload
(defun atlas-at-point-deps-summary ()
  "Show 'what do I need?' summary for keyword at point, or prompt."
  (interactive)
  (atlas-browse-deps-summary (atlas--entity-or-read)))

;;;###autoload
(defun atlas-at-point-dependents ()
  "Show dependents for keyword at point, or prompt."
  (interactive)
  (atlas-browse-dependents (atlas--entity-or-read)))

;;;###autoload
(defun atlas-at-point-transitive-dependents ()
  "Show all transitive dependents (blast radius) for keyword at point, or prompt."
  (interactive)
  (atlas-browse-recursive-dependents (atlas--entity-or-read)))

;;;###autoload
(defun atlas-at-point-blast-radius ()
  "Show blast radius summary for keyword at point, or prompt."
  (interactive)
  (atlas-browse-dependents-summary (atlas--entity-or-read)))

;;;###autoload
(defun atlas-at-point-producers ()
  "Show producers for data-key at point, or prompt."
  (interactive)
  (atlas-browse-producers (atlas--data-key-or-read)))

;;;###autoload
(defun atlas-at-point-consumers ()
  "Show consumers for data-key at point, or prompt."
  (interactive)
  (atlas-browse-consumers (atlas--data-key-or-read)))

;;; xref backend — M-. for Atlas keywords

(require 'xref)
(require 'cl-lib)

(defun atlas--project-root ()
  "Return project root directory."
  (or (when (fboundp 'project-root)
        (when-let* ((proj (project-current)))
          (project-root proj)))
      (when (fboundp 'projectile-project-root)
        (projectile-project-root))
      default-directory))

(defun atlas--scan-file-for-register (file keyword-str)
  "Scan FILE for a register! call containing KEYWORD-STR.
Returns (file line-of-register!) or nil. Handles multiline calls."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (catch 'found
      (while (search-forward keyword-str nil t)
        (let ((kw-line (line-number-at-pos)))
          (save-excursion
            (beginning-of-line)
            (dotimes (i 4)  ; current line + up to 3 lines back
              (when (looking-at ".*register!")
                (throw 'found (list file (line-number-at-pos))))
              (unless (bobp) (forward-line -1))))))
      nil)))

(defun atlas--find-register-locations (keyword-str)
  "Find register! call sites for KEYWORD-STR.
Uses grep/rg to find candidate files, then scans in Elisp.
Handles same-line and multiline register! calls."
  (let* ((root (atlas--project-root))
         (default-directory root)
         (escaped (regexp-quote keyword-str))
         (cmd (if (executable-find "rg")
                  (format "rg -l --type clojure '%s' 2>/dev/null" escaped)
                (format "grep -rlE '%s' --include='*.clj' --include='*.cljc' . 2>/dev/null" escaped)))
         (_ (message "[atlas-xref] candidate files cmd: %s" cmd))
         (rel-files (split-string (shell-command-to-string cmd) "\n" t)))
    (message "[atlas-xref] candidate files: %s" rel-files)
    (delq nil
          (mapcar (lambda (rel-file)
                    (atlas--scan-file-for-register
                     (expand-file-name rel-file root)
                     keyword-str))
                  rel-files))))

(defun atlas-xref-backend ()
  "Atlas xref backend identifier."
  (message "[atlas-xref] backend function called, returning 'atlas")
  'atlas)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql atlas)))
  "Return keyword at point if it looks like an Atlas dev-id."
  (message "[atlas-xref] identifier-at-point called")
  (let ((kw (atlas--keyword-at-point)))
    (message "[atlas-xref] keyword-at-point returned: %s" kw)
    (when kw
      (let ((connected (cider-connected-p)))
        (message "[atlas-xref] cider-connected-p: %s" connected)
        (if (not connected)
            (progn (message "[atlas-xref] CIDER not connected, skipping") nil)
          (let ((registered (atlas--eval-safe (format "(registered-entity? %s)" kw))))
            (message "[atlas-xref] registered-entity? %s => %s" kw registered)
            (when registered kw)))))))

(cl-defmethod xref-backend-definitions ((_backend (eql atlas)) identifier)
  "Find register! definition for Atlas keyword IDENTIFIER."
  (message "[atlas-xref] definitions called for: %s" identifier)
  (let* ((root (atlas--project-root))
         (_ (message "[atlas-xref] project root: %s" root))
         (locations (atlas--find-register-locations identifier)))
    (message "[atlas-xref] found %d locations" (length locations))
    (dolist (loc locations)
      (message "[atlas-xref]   location: %s:%d" (nth 0 loc) (nth 1 loc)))
    (mapcar (lambda (loc)
              (xref-make identifier
                         (xref-make-file-location (nth 0 loc) (nth 1 loc) 0)))
            locations)))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql atlas)))
  "No completion table."
  nil)

(defun atlas-xref-setup ()
  "Add Atlas xref backend (checked before CIDER/LSP).
Ensures atlas is first by removing and re-adding at the front."
  ;; Remove first in case it's already there in wrong position
  (remove-hook 'xref-backend-functions #'atlas-xref-backend t)
  ;; Add to front of the buffer-local list
  (add-hook 'xref-backend-functions #'atlas-xref-backend nil t)
  ;; Force to absolute front, ahead of cider/lsp
  (when (local-variable-p 'xref-backend-functions)
    (setq-local xref-backend-functions
                (cons 'atlas-xref-backend
                      (remq 'atlas-xref-backend xref-backend-functions))))
  (message "[atlas-xref] setup complete, xref-backend-functions: %s"
           xref-backend-functions))

;;; atlas-clj-mode — minor mode for Clojure buffers

(defun atlas-goto-definition ()
  "Go to register! definition for Atlas keyword at point.
Falls through to `xref-find-definitions' for non-Atlas keywords."
  (interactive)
  (let ((kw (atlas--keyword-at-point)))
    (if (and kw
             (cider-connected-p)
             (atlas--eval-safe (format "(registered-entity? %s)" kw)))
        (let ((locations (atlas--find-register-locations kw)))
          (if locations
              (let* ((loc (car locations))
                     (file (nth 0 loc))
                     (line (nth 1 loc)))
                (xref-push-marker-stack)
                (find-file file)
                (goto-char (point-min))
                (forward-line (1- line)))
            (user-error "Atlas: register! call not found for %s" kw)))
      ;; Not an Atlas entity — fall through to default M-.
      (let ((xref-backend-functions (remq 'atlas-xref-backend xref-backend-functions)))
        (call-interactively 'xref-find-definitions)))))

(defvar atlas-clj-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-.") 'atlas-goto-definition)
    (define-key map (kbd "C-c a a") 'atlas)
    (define-key map (kbd "C-c a i") 'atlas-at-point-info)
    (define-key map (kbd "C-c a t") 'atlas-at-point-transitive-deps)
    (define-key map (kbd "C-c a w") 'atlas-at-point-deps-summary)
    (define-key map (kbd "C-c a d") 'atlas-at-point-deps)
    (define-key map (kbd "C-c a r") 'atlas-at-point-dependents)
    (define-key map (kbd "C-c a R") 'atlas-at-point-transitive-dependents)
    (define-key map (kbd "C-c a b") 'atlas-at-point-blast-radius)
    (define-key map (kbd "C-c a f") 'atlas-drill-entity-at-point)
    (define-key map (kbd "C-c a l") 'atlas-lens-mode)
    (define-key map (kbd "C-c a c") 'atlas-browse-check-invariants)
    map)
  "Keymap for `atlas-clj-mode'.")

;;;###autoload
(define-minor-mode atlas-clj-mode
  "Atlas keybindings for Clojure buffers.

Provides at-point access to Atlas registry queries via C-c a prefix.
All commands use the keyword under cursor, falling back to completing-read.

M-. on registered Atlas keywords jumps to the register! call site.
Falls through to CIDER/LSP for non-Atlas keywords.

\\{atlas-clj-mode-map}"
  :lighter " Atlas"
  :keymap atlas-clj-mode-map
  (if atlas-clj-mode
      (atlas-xref-setup)
    (remove-hook 'xref-backend-functions #'atlas-xref-backend t)))

;;;###autoload
(defun atlas-clj-mode-enable ()
  "Enable `atlas-clj-mode' in current buffer."
  (atlas-clj-mode 1))

;;;###autoload
(defun atlas-xref-diagnose ()
  "Diagnose Atlas xref setup. Run this in the buffer where M-. fails."
  (interactive)
  (message "=== Atlas xref diagnosis ===")
  (message "1. atlas-clj-mode active: %s" (bound-and-true-p atlas-clj-mode))
  (message "2. xref-backend-functions (local): %s"
           (buffer-local-value 'xref-backend-functions (current-buffer)))
  (message "3. atlas-xref-backend in list: %s"
           (memq 'atlas-xref-backend
                 (buffer-local-value 'xref-backend-functions (current-buffer))))
  (message "4. keyword-at-point: %s" (atlas--keyword-at-point))
  (message "5. cider-connected-p: %s" (cider-connected-p))
  (when-let* ((kw (atlas--keyword-at-point)))
    (let ((reg (atlas--eval-safe (format "(registered-entity? %s)" kw))))
      (message "6. registered-entity? %s => %s" kw reg))
    (let ((locs (atlas--find-register-locations kw)))
      (message "7. grep locations: %s" locs)))
  (message "=== end diagnosis ==="))

(provide 'atlas-at-point)
;;; atlas-at-point.el ends here
