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

;;; Clojure spec helpers

(defun atlas--spec-exists-p (kw)
  "Return non-nil if KW is a registered clojure.spec."
  (when (cider-connected-p)
    (let* ((code (format "(do (require 'clojure.spec.alpha) (some? (clojure.spec.alpha/get-spec %s)))" kw))
           (result (cider-nrepl-sync-request:eval code))
           (value (nrepl-dict-get result "value")))
      (and value (not (string= value "false"))))))

(defun atlas--spec-exercise-pretty (kw &optional n)
  "Return a pretty-printed string of spec/exercise results for KW.
Uses clojure.pprint on the server side. Returns nil on error."
  (when (cider-connected-p)
    (let* ((count (or n 5))
           (code (format
                  (concat "(do (require 'clojure.spec.alpha)"
                          "    (require 'clojure.pprint)"
                          "    (with-out-str"
                          "      (run! (fn [[gen conformed]]"
                          "              (println \"── generated ──\")"
                          "              (clojure.pprint/pprint gen)"
                          "              (println \"── conformed ──\")"
                          "              (clojure.pprint/pprint conformed)"
                          "              (println))"
                          "            (clojure.spec.alpha/exercise %s %d))))")
                  kw count))
           (result (cider-nrepl-sync-request:eval code))
           (value  (nrepl-dict-get result "value"))
           (err    (nrepl-dict-get result "err")))
      (cond
       (err   (message "spec/exercise error: %s" err) nil)
       (value (condition-case e
                  (read value)          ; unquote the returned EDN string
                (error (message "spec unquote error: %s" (error-message-string e))
                       nil)))))))

(defun atlas--show-spec-popup (kw pretty-str)
  "Display pretty-printed spec examples for KW in a side-window popup."
  (let* ((buf-name (format "*Atlas Spec: %s*" kw))
         (buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (format "Spec examples: %s\n" kw) 'face 'atlas-header-face))
        (insert (make-string 60 ?─))
        (insert "\n\n")
        (insert pretty-str)
        (insert (propertize "(q to close)" 'face 'font-lock-comment-face)))
      (special-mode)
      (local-set-key (kbd "q") #'quit-window)
      (goto-char (point-min)))
    (display-buffer buf '(display-buffer-in-side-window
                          (side . bottom)
                          (window-height . 0.35)))))

(defun atlas-goto-definition ()
  "Go to register! definition for Atlas keyword at point.
Falls through to spec examples popup for clojure.spec keywords,
then to default `xref-find-definitions' for everything else."
  (interactive)
  (let ((kw (atlas--keyword-at-point)))
    (cond
     ;; Case 1: registered Atlas entity → jump to register! site
     ((and kw
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
          (user-error "Atlas: register! call not found for %s" kw))))
     ;; Case 2: clojure.spec def → show generated examples in popup
     ((and kw (atlas--spec-exists-p kw))
      (let ((pretty (atlas--spec-exercise-pretty kw)))
        (if pretty
            (atlas--show-spec-popup kw pretty)
          (user-error "Atlas: could not generate spec examples for %s" kw))))
     ;; Case 3: neither — fall through to CIDER/LSP xref
     (t
      (let ((xref-backend-functions (remq 'atlas-xref-backend xref-backend-functions)))
        (call-interactively 'xref-find-definitions))))))

;;; atlas-clj-mode — minor mode for Clojure buffers

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
