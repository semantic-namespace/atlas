;;; atlas-lens.el --- Template-driven overlays for register! forms -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; Author: @tangrammer + LLMs

;;; Commentary:
;;
;; Atlas Lens Mode overlays `register!' forms with template-driven views.
;; Modes cycle with C-c C-l m — available modes are derived from
;; `atlas-render-specs' plus the special :raw (no-overlay) mode:
;;
;;   :raw      — no overlay, show the actual register! source
;;   :semantic — signature card (aspects, context, response, deps)
;;   :impl     — implementation view (impl fn, context, deps)
;;
;; Rendering is fully delegated to atlas-render.el.  This file owns only:
;;   - register! form scanning (LSP + regex fallback)
;;   - batch REPL entity fetch
;;   - Emacs overlay apply/clear/cursor-reveal lifecycle
;;   - minor mode + keybindings
;;
;; The overlay is purely visual — buffer content is untouched, so
;; LSP/CIDER/paredit work normally.  When the cursor enters a form,
;; the overlay auto-reveals the real code.
;;
;; Uses clojure-lsp (via lsp-mode) to find register! call sites, so it
;; works regardless of the namespace alias used (r/, registry/, cid/, etc.).

;;; Code:

(require 'atlas-core)
(require 'atlas-render)

;;; Lens Mode State
;;
;; atlas-lens--mode is intentionally global (not buffer-local) so the
;; active mode is consistent across all Clojure buffers in a session.

(defvar atlas-lens--mode :semantic
  "Current lens mode: :raw, or any mode declared in `atlas-render-specs'.")

(defvar atlas-lens--overlays nil
  "List of active lens overlays in the current buffer.")
(make-variable-buffer-local 'atlas-lens--overlays)

(defvar atlas-lens--revealed-overlay nil
  "The currently revealed overlay (cursor is inside it).")
(make-variable-buffer-local 'atlas-lens--revealed-overlay)

(defvar atlas-lens--debounce-timer nil
  "Timer for debounced cursor reveal/hide.")
(make-variable-buffer-local 'atlas-lens--debounce-timer)

(defvar atlas-lens--entities-cache nil
  "Cached entity info from last fetch, reused across mode switches.")
(make-variable-buffer-local 'atlas-lens--entities-cache)

(defvar atlas-lens--forms-cache nil
  "Cached form positions from last scan, reused across mode switches.")
(make-variable-buffer-local 'atlas-lens--forms-cache)

;;; Form Filter Hook

(defvar atlas-lens-form-filter-functions nil
  "Abnormal hook to selectively overlay register! forms.
Each function is called with two args: FORM (plist :start :end :dev-id)
and ENTITY-INFO (hash-table from registry).  Return non-nil to include
the form, nil to skip it.  All functions must return non-nil to include.
When nil (default), all forms with resolved entity info are overlaid.")

(defun atlas-lens--form-included-p (form info)
  "Return non-nil if FORM should receive an overlay.
Checks all functions in `atlas-lens-form-filter-functions'."
  (or (null atlas-lens-form-filter-functions)
      (seq-every-p (lambda (fn) (funcall fn form info))
                   atlas-lens-form-filter-functions)))

;;; Scanning — find register! forms via LSP references

(defun atlas-lens--lsp-find-register-refs ()
  "Use clojure-lsp to find all references to `atlas.registry/register!' in the current buffer.
Returns list of (LINE . COL) positions (0-indexed) for each reference in this file."
  (when (and (fboundp 'lsp-request) (fboundp 'lsp-workspaces) (lsp-workspaces))
    (let* ((buf-uri (lsp--buffer-uri))
           (ref-pos (atlas-lens--find-any-register-pos))
           (refs (when ref-pos
                   (lsp-request
                    "textDocument/references"
                    (list :textDocument (list :uri buf-uri)
                          :position (list :line (car ref-pos)
                                          :character (cdr ref-pos))
                          :context (list :includeDeclaration :json-false))))))
      (let (positions)
        (dolist (ref refs)
          (let* ((loc-uri (gethash "uri" (gethash "location" ref
                                                   (gethash "Location" ref ref))
                                   (gethash "uri" ref)))
                 (range (gethash "range" (gethash "location" ref
                                                   (gethash "Location" ref ref))
                                 (gethash "range" ref)))
                 (start (gethash "start" range))
                 (line (gethash "line" start))
                 (col (gethash "character" start)))
            (when (and loc-uri (string= loc-uri buf-uri))
              (push (cons line col) positions))))
        (nreverse positions)))))

(defun atlas-lens--find-any-register-pos ()
  "Find position of any register! symbol in the buffer for LSP querying.
Returns (LINE . COL) 0-indexed, or nil."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "\\bregister!" nil t)
      (let ((pos (match-beginning 0)))
        (goto-char pos)
        (cons (1- (line-number-at-pos))
              (current-column))))))

(defun atlas-lens--find-register-forms ()
  "Find all register! forms in the current buffer.
Uses LSP references when available, falls back to regex scanning.
Returns list of plists (:start N :end M :dev-id \"keyword-string\")."
  (let ((lsp-refs (atlas-lens--lsp-find-register-refs)))
    (if lsp-refs
        (atlas-lens--forms-from-lsp-refs lsp-refs)
      (atlas-lens--forms-from-regex))))

(defun atlas-lens--forms-from-lsp-refs (refs)
  "Build form list from LSP reference positions REFS.
Each ref is (LINE . COL) 0-indexed pointing to `register!'."
  (let (forms)
    (save-excursion
      (dolist (ref refs)
        (goto-char (point-min))
        (forward-line (car ref))
        (move-to-column (cdr ref))
        (let ((reg-pos (point)))
          (when (re-search-backward "(" (line-beginning-position 0) t)
            (let ((form-start (point)))
              (condition-case nil
                  (let ((form-end (save-excursion (forward-sexp 1) (point))))
                    (save-excursion
                      (goto-char reg-pos)
                      (forward-sexp 1)
                      (when-let ((kw (atlas-lens--extract-dev-id form-end)))
                        (push (list :start form-start :end form-end :dev-id kw)
                              forms))))
                (scan-error nil)))))))
    (nreverse forms)))

(defun atlas-lens--extract-dev-id (form-end)
  "Extract the dev-id keyword from point to FORM-END.
Returns nil when the first keyword is :atlas/* (3-arity call without explicit dev-id)."
  (save-excursion
    (skip-chars-forward " \t\n\r")
    (when (and (< (point) form-end)
               (re-search-forward ":[a-zA-Z][a-zA-Z0-9_.*+!-]*/" form-end t))
      (goto-char (match-beginning 0))
      (let ((kw-start (point)))
        (forward-sexp 1)
        (let ((kw (buffer-substring-no-properties kw-start (point))))
          (unless (string-prefix-p ":atlas/" kw)
            kw))))))

(defun atlas-lens--forms-from-regex ()
  "Fallback: find register! forms using regex scanning."
  (save-excursion
    (goto-char (point-min))
    (let (forms)
      (while (re-search-forward
              "(\\(?:[a-zA-Z][a-zA-Z0-9_.*+-]*/\\)?register!?"
              nil t)
        (let ((form-start (match-beginning 0)))
          (goto-char form-start)
          (condition-case nil
              (let ((form-end (save-excursion (forward-sexp 1) (point))))
                (save-excursion
                  (goto-char (match-end 0))
                  (when-let ((kw (atlas-lens--extract-dev-id form-end)))
                    (push (list :start form-start :end form-end :dev-id kw)
                          forms)))
                (goto-char form-end))
            (scan-error (forward-char 1)))))
      (nreverse forms))))

;;; Batch fetch

(defun atlas-lens--fetch-entities (dev-ids)
  "Fetch entity info for DEV-IDS (list of keyword strings) via CIDER.
Returns hash-table of dev-id symbol -> entity-info hash-table, or nil."
  (when dev-ids
    (let* ((ids-str (mapconcat #'identity dev-ids " "))
           (result (atlas--eval
                    (format "(do (require '[atlas.ide :as ide]) (ide/entities-info [%s]))"
                            ids-str))))
      (or result (make-hash-table :test 'equal)))))

;;; Overlay management

(defun atlas-lens--clear-overlays ()
  "Remove all lens overlays from the current buffer."
  (dolist (ov atlas-lens--overlays)
    (when (overlay-buffer ov)
      (delete-overlay ov)))
  (setq atlas-lens--overlays nil
        atlas-lens--revealed-overlay nil))

(defun atlas-lens--apply-overlays (forms entities mode)
  "Create overlays for FORMS using ENTITIES info in lens MODE."
  (atlas-lens--clear-overlays)
  (dolist (form forms)
    (let* ((start (plist-get form :start))
           (end   (plist-get form :end))
           (dev-id-str (plist-get form :dev-id))
           (info (atlas-render--lookup-entity entities dev-id-str)))
      (when (and info (atlas-lens--form-included-p form info))
        (let* ((card (atlas-render-form info mode))
               (ov (make-overlay start end)))
          (overlay-put ov 'display card)
          (overlay-put ov 'atlas-lens t)
          (overlay-put ov 'atlas-lens-card card)
          (overlay-put ov 'evaporate t)
          (push ov atlas-lens--overlays))))))

;;; Cursor reveal/hide

(defun atlas-lens--cursor-update ()
  "Debounced check for cursor entering/leaving an overlay."
  (when atlas-lens--debounce-timer
    (cancel-timer atlas-lens--debounce-timer))
  (setq atlas-lens--debounce-timer
        (run-with-idle-timer 0.05 nil #'atlas-lens--do-cursor-update)))

(defun atlas-lens--do-cursor-update ()
  "Reveal overlay at cursor; restore previous revealed overlay if cursor left."
  (let ((pos (point))
        found)
    (dolist (ov atlas-lens--overlays)
      (when (and (overlay-buffer ov)
                 (<= (overlay-start ov) pos)
                 (< pos (overlay-end ov)))
        (setq found ov)))
    (cond
     ((and found (not (eq found atlas-lens--revealed-overlay)))
      (when (and atlas-lens--revealed-overlay
                 (overlay-buffer atlas-lens--revealed-overlay))
        (overlay-put atlas-lens--revealed-overlay 'display
                     (overlay-get atlas-lens--revealed-overlay 'atlas-lens-card)))
      (overlay-put found 'display nil)
      (setq atlas-lens--revealed-overlay found))
     ((and (not found) atlas-lens--revealed-overlay)
      (when (overlay-buffer atlas-lens--revealed-overlay)
        (overlay-put atlas-lens--revealed-overlay 'display
                     (overlay-get atlas-lens--revealed-overlay 'atlas-lens-card)))
      (setq atlas-lens--revealed-overlay nil)))))

;;; Mode cycling

(defun atlas-lens-cycle-mode ()
  "Cycle lens mode: :raw → :semantic → :impl → :raw.
Re-renders from cache when possible; falls back to full refresh.
For prose narrative, use `atlas-slice' instead."
  (interactive)
  (let* ((modes (cons :raw (seq-remove (lambda (m) (eq m :narrative))
                                       (atlas-render--available-modes))))
         (pos   (or (seq-position modes atlas-lens--mode #'eq) 0))
         (next  (nth (mod (1+ pos) (length modes)) modes)))
    (setq atlas-lens--mode next)
    (if (eq next :raw)
        (progn
          (atlas-lens--clear-overlays)
          (message "Atlas Lens: raw (overlays off)"))
      (if (and atlas-lens--forms-cache atlas-lens--entities-cache)
          (progn
            (atlas-lens--apply-overlays atlas-lens--forms-cache
                                        atlas-lens--entities-cache
                                        next)
            (message "Atlas Lens: %s (%d overlays)"
                     next (length atlas-lens--overlays)))
        (atlas-lens-refresh)))))

;;; Refresh

(defun atlas-lens-refresh ()
  "Re-scan buffer and refresh all lens overlays."
  (interactive)
  (when atlas-lens-mode
    (cond
     ((eq atlas-lens--mode :raw)
      (atlas-lens--clear-overlays)
      (message "Atlas Lens: raw (overlays off)"))
     ((not (cider-connected-p))
      (message "Atlas Lens: REPL unavailable — connect CIDER first"))
     (t
      (let ((forms (atlas-lens--find-register-forms)))
        (if (null forms)
            (progn
              (atlas-lens--clear-overlays)
              (setq atlas-lens--forms-cache nil
                    atlas-lens--entities-cache nil)
              (message "Atlas Lens: no register! forms found"))
          (let* ((dev-ids (mapcar (lambda (f) (plist-get f :dev-id)) forms))
                 (entities (atlas-lens--fetch-entities dev-ids)))
            (setq atlas-lens--forms-cache forms
                  atlas-lens--entities-cache entities)
            (atlas-lens--apply-overlays forms entities atlas-lens--mode)
            (message "Atlas Lens: %s (%d overlays)"
                     atlas-lens--mode (length atlas-lens--overlays)))))))))

(defun atlas-lens--after-save ()
  "Refresh overlays after saving the buffer."
  (when atlas-lens-mode
    (atlas-lens-refresh)))

;;; Minor mode

(defvar atlas-lens-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-l r") #'atlas-lens-refresh)
    (define-key map (kbd "C-c C-l m") #'atlas-lens-cycle-mode)
    map)
  "Keymap for `atlas-lens-mode'.")

;;;###autoload
(define-minor-mode atlas-lens-mode
  "Toggle Atlas Lens mode.
Overlays `register!' forms with template-driven semantic views.

Modes cycle with C-c C-l m (derived from `atlas-render-specs'):
  :raw       — no overlay, show actual source
  :semantic  — signature card (aspects, context, response, deps)
  :impl      — implementation view (impl fn, context, deps)

For prose narrative view use `atlas-slice' (generates an org-mode buffer).
Templates are customizable via `atlas-render-specs'.
When no template exists for a type+mode, a minimal default is shown.

Uses clojure-lsp to find register! calls (alias-independent),
with regex fallback when LSP is unavailable.

Cursor entering a form reveals the real code; leaving restores the card.

\\{atlas-lens-mode-map}"
  :lighter " Lens"
  :keymap atlas-lens-mode-map
  (if atlas-lens-mode
      (progn
        (atlas-lens-refresh)
        (add-hook 'post-command-hook #'atlas-lens--cursor-update nil t)
        (add-hook 'after-save-hook #'atlas-lens--after-save nil t))
    (atlas-lens--clear-overlays)
    (setq atlas-lens--forms-cache nil
          atlas-lens--entities-cache nil)
    (when atlas-lens--debounce-timer
      (cancel-timer atlas-lens--debounce-timer)
      (setq atlas-lens--debounce-timer nil))
    (remove-hook 'post-command-hook #'atlas-lens--cursor-update t)
    (remove-hook 'after-save-hook #'atlas-lens--after-save t)))

(provide 'atlas-lens)
;;; atlas-lens.el ends here
