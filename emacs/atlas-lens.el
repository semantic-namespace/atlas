;;; atlas-lens.el --- Signature card overlays for register! forms -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; Author: @tangrammer + LLMs

;;; Commentary:
;;
;; Atlas Lens Mode overlays `register!` forms with compact signature cards
;; showing each entity's semantic identity. The overlay is purely visual —
;; buffer content is untouched, so LSP/CIDER/paredit work normally.
;; When the cursor enters a form, the overlay auto-reveals the real code.
;;
;; Uses clojure-lsp (via lsp-mode) to find register! call sites, so it
;; works regardless of the namespace alias used (r/, registry/, cid/, etc.).

;;; Code:

(require 'atlas-core)

;;; Customization

(defvar atlas-lens--overlays nil
  "List of active lens overlays in the current buffer.")
(make-variable-buffer-local 'atlas-lens--overlays)

(defvar atlas-lens--revealed-overlay nil
  "The currently revealed overlay (cursor is inside it).")
(make-variable-buffer-local 'atlas-lens--revealed-overlay)

(defvar atlas-lens--debounce-timer nil
  "Timer for debounced cursor reveal/hide.")
(make-variable-buffer-local 'atlas-lens--debounce-timer)

;;; Scanning — find register! forms via LSP references

(defun atlas-lens--lsp-find-register-refs ()
  "Use clojure-lsp to find all references to `atlas.registry/register!` in the current buffer.
Returns list of (LINE . COL) positions (0-indexed) for each reference in this file."
  (when (and (fboundp 'lsp-request) (fboundp 'lsp-workspaces) (lsp-workspaces))
    (let* ((buf-uri (lsp--buffer-uri))
           ;; Ask LSP for the definition location of register! so we can get references.
           ;; We need a position where register! is used. Instead, we'll search
           ;; for any register! call in the buffer and ask LSP for references from there.
           (ref-pos (atlas-lens--find-any-register-pos))
           (refs (when ref-pos
                   (lsp-request
                    "textDocument/references"
                    (list :textDocument (list :uri buf-uri)
                          :position (list :line (car ref-pos)
                                          :character (cdr ref-pos))
                          :context (list :includeDeclaration :json-false))))))
      ;; Filter to only references in this file
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
  "Find the position of any register! symbol in the buffer for LSP querying.
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
Each ref is (LINE . COL) 0-indexed pointing to `register!`."
  (let (forms)
    (save-excursion
      (dolist (ref refs)
        (goto-char (point-min))
        (forward-line (car ref))
        (move-to-column (cdr ref))
        ;; We're on `register!` — go back to the opening `(` of the call
        (let ((reg-pos (point)))
          (when (re-search-backward "(" (line-beginning-position 0) t)
            (let ((form-start (point)))
              (condition-case nil
                  (let ((form-end (save-excursion (forward-sexp 1) (point))))
                    ;; Extract dev-id: first keyword after register!
                    (save-excursion
                      (goto-char reg-pos)
                      (forward-sexp 1) ;; skip `register!` symbol
                      (when (atlas-lens--extract-dev-id form-end)
                        (let ((kw (atlas-lens--extract-dev-id form-end)))
                          (push (list :start form-start
                                      :end form-end
                                      :dev-id kw)
                                forms)))))
                (scan-error nil)))))))
    (nreverse forms)))

(defun atlas-lens--extract-dev-id (form-end)
  "Extract the dev-id keyword from point to FORM-END.
Skips whitespace, finds first keyword. Returns nil if first keyword is :atlas/* (3-arity)."
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
  "Fallback: find register! forms using regex scanning.
Matches any namespace-qualified or bare register! call."
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
                    (push (list :start form-start
                                :end form-end
                                :dev-id kw)
                          forms)))
                (goto-char form-end))
            (scan-error (forward-char 1)))))
      (nreverse forms))))

;;; Batch fetch — get entity info for all dev-ids

(defun atlas-lens--fetch-entities (dev-ids)
  "Fetch entity info for DEV-IDS (list of keyword strings) via CIDER.
Returns hash-table of dev-id symbol -> entity-info hash-table."
  (when dev-ids
    (let* ((ids-str (mapconcat #'identity dev-ids " "))
           (form (format "(entities-info [%s])" ids-str))
           (result (atlas--eval form)))
      (or result (make-hash-table :test 'equal)))))

;;; Card rendering

(defun atlas-lens--format-kw-list (items)
  "Format a list of keyword ITEMS as a compact string."
  (mapconcat (lambda (item)
               (let ((s (atlas--to-string item)))
                 (if (string-prefix-p ":" s) (substring s 1) s)))
             (atlas--to-list items)
             " · "))

(defun atlas-lens--format-set (items)
  "Format a set/vector of ITEMS as #{...} string."
  (let ((formatted (mapconcat (lambda (item)
                                (let ((s (atlas--to-string item)))
                                  (if (string-prefix-p ":" s) s (concat ":" s))))
                              (atlas--to-list items)
                              " ")))
    (format "#{%s}" formatted)))

(defun atlas-lens--render-card (dev-id info)
  "Render a signature card for DEV-ID with entity INFO.
Returns a propertized string."
  (let* ((dev-id-str (let ((s (atlas--to-string dev-id)))
                       (if (string-prefix-p ":" s) (substring s 1) s)))
         (identity-vec (atlas--get info :entity/identity))
         (aspects (atlas--get info :entity/aspects))
         (def-values (atlas--get info :entity/definition-values))
         (context (atlas--get info :interface-endpoint/context))
         (response (atlas--get info :interface-endpoint/response))
         (deps (atlas--get info :execution-function/deps))
         (fields (atlas--get info :atlas/fields))
         ;; Determine entity type from identity
         (entity-type
          (let ((type-kw nil))
            (mapc (lambda (k)
                    (let ((s (atlas--to-string k)))
                      (when (string-prefix-p ":atlas/" s)
                        (setq type-kw s))))
                  (atlas--to-list identity-vec))
            (or type-kw "")))
         (lines '()))

    ;; Header line
    (push (concat
           (propertize (format "── %s ──" dev-id-str) 'face 'atlas-header-face)
           " "
           (propertize entity-type 'face 'atlas-subheader-face))
          lines)

    ;; Aspects line
    (when (and aspects (> (length (atlas--to-list aspects)) 0))
      (push (concat
             "  "
             (propertize "aspects: " 'face 'atlas-subheader-face)
             (propertize (atlas-lens--format-kw-list aspects) 'face 'atlas-aspect-face))
            lines))

    ;; Context line (execution-function / interface-endpoint)
    (when (and context (> (length (atlas--to-list context)) 0))
      (push (concat
             "  "
             (propertize "context: " 'face 'atlas-subheader-face)
             (propertize (format "[%s]" (atlas-lens--format-kw-list context))
                         'face 'atlas-entity-face))
            lines))

    ;; Response line
    (when (and response (> (length (atlas--to-list response)) 0))
      (push (concat
             "  "
             (propertize "response: " 'face 'atlas-subheader-face)
             (propertize (format "[%s]" (atlas-lens--format-kw-list response))
                         'face 'atlas-entity-face))
            lines))

    ;; Deps line
    (when (and deps (> (length (atlas--to-list deps)) 0))
      (push (concat
             "  "
             (propertize "deps: " 'face 'atlas-subheader-face)
             (propertize (atlas-lens--format-set deps) 'face 'atlas-entity-face))
            lines))

    ;; Fields line (data-schema)
    (when (and fields (> (length (atlas--to-list fields)) 0))
      (push (concat
             "  "
             (propertize "fields: " 'face 'atlas-subheader-face)
             (propertize (format "[%s]" (atlas-lens--format-kw-list fields))
                         'face 'atlas-entity-face))
            lines))

    ;; Definition values for custom types (fallback)
    (when (and def-values
               (not context) (not response) (not deps) (not fields))
      (let ((entries (atlas--map-entries def-values)))
        (dolist (entry entries)
          (let ((k (atlas--to-string (car entry)))
                (v (cdr entry)))
            (when v
              (push (concat
                     "  "
                     (propertize (format "%s: " (if (string-prefix-p ":" k)
                                                    (substring k 1) k))
                                 'face 'atlas-subheader-face)
                     (propertize (format "%s" v) 'face 'atlas-entity-face))
                    lines))))))

    (concat (string-join (nreverse lines) "\n") "\n")))

;;; Overlay management

(defun atlas-lens--clear-overlays ()
  "Remove all lens overlays from the current buffer."
  (dolist (ov atlas-lens--overlays)
    (when (overlay-buffer ov)
      (delete-overlay ov)))
  (setq atlas-lens--overlays nil)
  (setq atlas-lens--revealed-overlay nil))

(defun atlas-lens--apply-overlays (forms entities)
  "Create overlays for FORMS using ENTITIES info hash-table."
  (atlas-lens--clear-overlays)
  (dolist (form forms)
    (let* ((start (plist-get form :start))
           (end (plist-get form :end))
           (dev-id-str (plist-get form :dev-id))
           (dev-id-sym (intern (if (string-prefix-p ":" dev-id-str)
                                   (substring dev-id-str 1)
                                 dev-id-str)))
           (info (atlas-lens--lookup-entity entities dev-id-str)))
      (when info
        (let* ((card (atlas-lens--render-card dev-id-sym info))
               (ov (make-overlay start end)))
          (overlay-put ov 'display card)
          (overlay-put ov 'atlas-lens t)
          (overlay-put ov 'atlas-lens-card card)
          (overlay-put ov 'evaporate t)
          (push ov atlas-lens--overlays))))))

(defun atlas-lens--lookup-entity (entities dev-id-str)
  "Look up DEV-ID-STR in ENTITIES hash-table.
Tries both with and without leading colon."
  (when (hash-table-p entities)
    (let ((bare (if (string-prefix-p ":" dev-id-str)
                    (substring dev-id-str 1)
                  dev-id-str)))
      (or (gethash (intern dev-id-str) entities)
          (gethash (intern bare) entities)
          (gethash (intern (concat ":" bare)) entities)
          ;; Try atlas--get for flexible key matching
          (atlas--get entities dev-id-str)
          (atlas--get entities bare)))))

;;; Cursor reveal/hide

(defun atlas-lens--cursor-update ()
  "Check cursor position and reveal/hide overlays accordingly."
  (when atlas-lens--debounce-timer
    (cancel-timer atlas-lens--debounce-timer))
  (setq atlas-lens--debounce-timer
        (run-with-idle-timer
         0.05 nil
         #'atlas-lens--do-cursor-update)))

(defun atlas-lens--do-cursor-update ()
  "Perform the actual cursor reveal/hide check."
  (let ((pos (point))
        (found nil))
    ;; Find overlay at cursor position
    (dolist (ov atlas-lens--overlays)
      (when (and (overlay-buffer ov)
                 (<= (overlay-start ov) pos)
                 (< pos (overlay-end ov)))
        (setq found ov)))
    ;; Reveal/hide logic
    (cond
     ;; Cursor entered a new overlay
     ((and found (not (eq found atlas-lens--revealed-overlay)))
      ;; Hide previously revealed overlay
      (when (and atlas-lens--revealed-overlay
                 (overlay-buffer atlas-lens--revealed-overlay))
        (overlay-put atlas-lens--revealed-overlay 'display
                     (overlay-get atlas-lens--revealed-overlay 'atlas-lens-card)))
      ;; Reveal current overlay
      (overlay-put found 'display nil)
      (setq atlas-lens--revealed-overlay found))
     ;; Cursor left all overlays
     ((and (not found) atlas-lens--revealed-overlay)
      (when (overlay-buffer atlas-lens--revealed-overlay)
        (overlay-put atlas-lens--revealed-overlay 'display
                     (overlay-get atlas-lens--revealed-overlay 'atlas-lens-card)))
      (setq atlas-lens--revealed-overlay nil)))))

;;; Refresh

(defun atlas-lens-refresh ()
  "Re-scan buffer and refresh all lens overlays."
  (interactive)
  (when atlas-lens-mode
    (let ((forms (atlas-lens--find-register-forms)))
      (if (null forms)
          (progn
            (atlas-lens--clear-overlays)
            (message "Atlas Lens: no register! forms found"))
        (let ((dev-ids (mapcar (lambda (f) (plist-get f :dev-id)) forms)))
          (let ((entities (atlas-lens--fetch-entities dev-ids)))
            (atlas-lens--apply-overlays forms entities)
            (message "Atlas Lens: %d overlays applied" (length atlas-lens--overlays))))))))

(defun atlas-lens--after-save ()
  "Refresh overlays after saving the buffer."
  (when atlas-lens-mode
    (atlas-lens-refresh)))

;;; Minor mode

(defvar atlas-lens-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-l r") #'atlas-lens-refresh)
    map)
  "Keymap for `atlas-lens-mode'.")

;;;###autoload
(define-minor-mode atlas-lens-mode
  "Toggle Atlas Lens mode.
Overlays `register!' forms with compact signature cards showing
each entity's semantic identity. Buffer content is untouched.

Uses clojure-lsp to find register! calls (alias-independent),
with regex fallback when LSP is unavailable.

When the cursor enters a form, the overlay reveals the real code.
When the cursor leaves, the card overlay is restored.

\\{atlas-lens-mode-map}"
  :lighter " Lens"
  :keymap atlas-lens-mode-map
  (if atlas-lens-mode
      (progn
        (atlas-lens-refresh)
        (add-hook 'post-command-hook #'atlas-lens--cursor-update nil t)
        (add-hook 'after-save-hook #'atlas-lens--after-save nil t))
    (atlas-lens--clear-overlays)
    (when atlas-lens--debounce-timer
      (cancel-timer atlas-lens--debounce-timer)
      (setq atlas-lens--debounce-timer nil))
    (remove-hook 'post-command-hook #'atlas-lens--cursor-update t)
    (remove-hook 'after-save-hook #'atlas-lens--after-save t)))

(provide 'atlas-lens)
;;; atlas-lens.el ends here
