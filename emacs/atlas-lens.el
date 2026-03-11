;;; atlas-lens.el --- Template-driven overlays for register! forms -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; Author: @tangrammer + LLMs

;;; Commentary:
;;
;; Atlas Lens Mode overlays `register!` forms with template-driven views.
;; Three lens modes cycle with C-c C-l m:
;;
;;   :raw      — no overlay, show the actual register! source
;;   :semantic — signature card (aspects, context, response, deps)
;;   :impl     — implementation view (impl fn, context, deps)
;;
;; Templates are user-customizable per entity type via `atlas-lens-specs'.
;; Placeholders like ${execution-function/context} are substituted with
;; entity values fetched from the registry.
;;
;; When no user template exists for a type, the ontology's :ontology/keys
;; are fetched from the registry as a fallback.
;;
;; The overlay is purely visual — buffer content is untouched, so
;; LSP/CIDER/paredit work normally. When the cursor enters a form,
;; the overlay auto-reveals the real code.
;;
;; Uses clojure-lsp (via lsp-mode) to find register! call sites, so it
;; works regardless of the namespace alias used (r/, registry/, cid/, etc.).

;;; Code:

(require 'atlas-core)

;;; Lens Mode State

(defvar atlas-lens--mode :semantic
  "Current lens mode. One of :raw, :semantic, :impl.")

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

;;; Template Specs — user-customizable per entity type, per lens mode
;;
;; Each entry maps an entity type keyword to an alist of (mode . template).
;; Templates use ${key} placeholders substituted with entity values.
;; Special keys: ${dev-id}, ${entity-type}, ${entity/aspects}.

(defvar atlas-lens-specs
  '((:atlas/execution-function
     (:semantic . "── ${dev-id} ── ${entity-type}\n  aspects: ${entity/aspects}\n  context: ${execution-function/context}\n  response: ${execution-function/response}\n  deps: ${execution-function/deps}")
     (:impl . ";; ${dev-id}\n${execution-function/impl}\n  :context ${execution-function/context}\n  :deps ${execution-function/deps}"))

    (:atlas/interface-endpoint
     (:semantic . "── ${dev-id} ── ${entity-type}\n  aspects: ${entity/aspects}\n  context: ${interface-endpoint/context}\n  response: ${interface-endpoint/response}\n  deps: ${interface-endpoint/deps}")
     (:impl . ";; ${dev-id}\n${interface-endpoint/impl}\n  :context ${interface-endpoint/context}\n  :deps ${interface-endpoint/deps}"))

    (:atlas/structure-component
     (:semantic . "── ${dev-id} ── ${entity-type}\n  aspects: ${entity/aspects}\n  deps: ${structure-component/deps}")
     (:impl . ";; ${dev-id}\n  :deps ${structure-component/deps}"))

    (:atlas/data-schema
     (:semantic . "── ${dev-id} ── ${entity-type}\n  aspects: ${entity/aspects}\n  fields: ${atlas/fields}")
     (:impl . ";; ${dev-id}\n  :fields ${atlas/fields}")))

  "Template specs for lens rendering, per entity type and lens mode.
Each entry is (ENTITY-TYPE (MODE . TEMPLATE) ...).
Templates use ${key} placeholders.

Special keys:
  ${dev-id}          — the entity's dev-id
  ${entity-type}     — the :atlas/* entity type
  ${entity/aspects}  — semantic aspects (computed from identity)

All other keys are looked up from the entity's properties.

Users can customize this to control what each lens mode shows.
When no template is found for a type+mode, the ontology's :ontology/keys
are fetched from the registry and rendered as key: value lines.")

;;; Type Badges

(defvar atlas-lens-type-badges
  '(("execution-function"  . (" fn " . atlas-lens-badge-fn-face))
    ("interface-endpoint"   . (" ep " . atlas-lens-badge-ep-face))
    ("structure-component"  . (" co " . atlas-lens-badge-co-face))
    ("data-schema"          . (" ds " . atlas-lens-badge-ds-face)))
  "Mapping of entity type short names to (LABEL . FACE) for badge rendering.
Users can customize badge labels and faces here.")

(defun atlas-lens--type-badge (entity-type)
  "Return a propertized badge string for ENTITY-TYPE (e.g. \":atlas/execution-function\").
Badge is a short colored label like ` fn ` with a type-specific background."
  (let* ((short (if (string-prefix-p ":atlas/" entity-type)
                    (substring entity-type 7)
                  entity-type))
         (entry (assoc short atlas-lens-type-badges))
         (label (if entry (car (cdr entry)) (format " %s " (truncate-string-to-width short 2))))
         (face (if entry (cdr (cdr entry)) 'atlas-lens-badge-default-face)))
    (propertize label 'face face)))

;;; Template Engine

(defun atlas-lens--get-entity-type (info)
  "Extract the :atlas/* entity type keyword from entity INFO."
  (let ((identity-vec (atlas--get info :entity/identity))
        (type-kw nil))
    (mapc (lambda (k)
            (let ((s (atlas--to-string k)))
              (when (string-prefix-p ":atlas/" s)
                (setq type-kw s))))
          (atlas--to-list identity-vec))
    (or type-kw "")))

(defun atlas-lens--get-template (entity-type mode)
  "Get template for ENTITY-TYPE and MODE from user specs.
Returns template string or nil."
  (let ((type-spec (assoc (intern entity-type) atlas-lens-specs)))
    (when type-spec
      (cdr (assoc mode (cdr type-spec))))))

(defun atlas-lens--ontology-fallback-template (entity-type mode)
  "Build a fallback template from ontology keys for ENTITY-TYPE and MODE.
Fetches :ontology/keys from the registry via REPL."
  (let* ((keys (atlas--eval-safe
                (format "(mapv str (:ontology/keys (atlas.ontology/ontology-for %s)))"
                        entity-type)))
         (key-list (when keys (atlas--to-list keys))))
    (if key-list
        (concat
         (format "── ${dev-id} ── %s\n" entity-type)
         (if (eq mode :semantic)
             (concat "  aspects: ${entity/aspects}\n"
                     (mapconcat (lambda (k)
                                  (let ((ks (atlas--to-string k)))
                                    (format "  %s: ${%s}" ks ks)))
                                key-list "\n"))
           ;; :impl mode — show impl key first if present, then others
           (mapconcat (lambda (k)
                        (let ((ks (atlas--to-string k)))
                          (format "  %s: ${%s}" ks ks)))
                      key-list "\n")))
      ;; Ultimate fallback — just dev-id and aspects
      "── ${dev-id} ── ${entity-type}\n  aspects: ${entity/aspects}")))

(defun atlas-lens--resolve-value (key info)
  "Resolve KEY to a display string from entity INFO."
  (cond
   ((string= key "dev-id")
    (let ((s (atlas--to-string (or (atlas--get info :atlas/dev-id)
                                   (atlas--get info :entity/dev-id) ""))))
      (if (string-prefix-p ":" s) (substring s 1) s)))

   ((string= key "entity-type")
    (atlas-lens--get-entity-type info))

   ((string= key "entity/aspects")
    (let ((aspects (atlas--get info :entity/aspects)))
      (if aspects
          (atlas-lens--format-kw-list aspects)
        "")))

   (t
    (let ((val (atlas--get info key)))
      (cond
       ((null val) "")
       ((vectorp val) (format "[%s]" (atlas-lens--format-kw-list val)))
       ((atlas--edn-set-p val) (atlas-lens--format-set val))
       ((listp val) (atlas-lens--format-kw-list val))
       (t (format "%s" val)))))))

(defun atlas-lens--render-template (template info)
  "Render TEMPLATE string by substituting ${key} with values from INFO.
Returns a propertized string."
  (let ((result (replace-regexp-in-string
                 "\\${\\([^}]+\\)}"
                 (lambda (match)
                   (let ((key (match-string 1 match)))
                     (save-match-data
                       (atlas-lens--resolve-value key info))))
                 template t t)))
    ;; Apply faces to the rendered result
    (atlas-lens--propertize-rendered result)))

(defun atlas-lens--propertize-rendered (text)
  "Apply faces to rendered TEXT with visual hierarchy.
Dev-id: large/bold. Entity type: dim/italic. Labels: dim. Values: bright."
  (let ((lines (split-string text "\n"))
        (result-lines '()))
    (dolist (line lines)
      (push
       (cond
        ;; Header line: ── dev-id ──  :atlas/type → badge dev-id  :atlas/type
        ((string-match "^\\(── \\)\\([^ ]+\\)\\( ── *\\)\\(.*\\)" line)
         (let ((entity-type (string-trim (match-string 4 line))))
           (concat (atlas-lens--type-badge entity-type)
                   " "
                   (propertize (match-string 2 line) 'face 'atlas-lens-dev-id-face)
                   "  "
                   (propertize entity-type 'face 'atlas-lens-type-face))))
        ;; Comment line (;; impl mode)
        ((string-match "^;;" line)
         (propertize line 'face 'font-lock-comment-face))
        ;; aspects: line — color each aspect individually
        ((string-match "^\\(\\s-*aspects:\\s-*\\)\\(.*\\)" line)
         (concat (propertize (match-string 1 line) 'face 'atlas-lens-label-face)
                 (atlas-lens--propertize-aspects (match-string 2 line))))
        ;; Key: value line
        ((string-match "^\\(\\s-*\\)\\([^ \t\n:]+:\\)\\(.*\\)" line)
         (concat (match-string 1 line)
                 (propertize (match-string 2 line) 'face 'atlas-lens-label-face)
                 (propertize (match-string 3 line) 'face 'atlas-lens-value-face)))
        ;; Separator line (just dashes)
        ((string-match "^─+$" line)
         (propertize line 'face 'atlas-lens-separator-face))
        (t line))
       result-lines))
    (concat (string-join (nreverse result-lines) "\n") "\n")))

(defun atlas-lens--propertize-aspects (aspects-str)
  "Apply per-aspect coloring to ASPECTS-STR (aspects separated by \" · \")."
  (if (string-empty-p aspects-str)
      aspects-str
    (let* ((aspects (split-string aspects-str " · " t))
           (colored (mapcar (lambda (a) (propertize a 'face 'atlas-aspect-face))
                            aspects)))
      (mapconcat #'identity colored
                 (propertize " · " 'face 'atlas-lens-separator-face)))))

;;; Card Rendering (dispatches to template engine)

(defun atlas-lens--render-card (dev-id info mode)
  "Render a card for DEV-ID with entity INFO in lens MODE.
Uses template from user specs, falling back to ontology keys.
Appends a thin separator line at the bottom."
  (let* ((entity-type (atlas-lens--get-entity-type info))
         (template (or (atlas-lens--get-template entity-type mode)
                       (atlas-lens--ontology-fallback-template entity-type mode)))
         (card (atlas-lens--render-template template info)))
    (concat card (propertize "────────────────────────────────\n"
                             'face 'atlas-lens-separator-face))))

;;; Formatting Helpers (kept from original for template value resolution)

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

;;; Scanning — find register! forms via LSP references

(defun atlas-lens--lsp-find-register-refs ()
  "Use clojure-lsp to find all references to `atlas.registry/register!` in the current buffer.
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

;;; Overlay management

(defun atlas-lens--clear-overlays ()
  "Remove all lens overlays from the current buffer."
  (dolist (ov atlas-lens--overlays)
    (when (overlay-buffer ov)
      (delete-overlay ov)))
  (setq atlas-lens--overlays nil)
  (setq atlas-lens--revealed-overlay nil))

(defun atlas-lens--apply-overlays (forms entities mode)
  "Create overlays for FORMS using ENTITIES info in lens MODE."
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
        (let* ((card (atlas-lens--render-card dev-id-sym info mode))
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

;;; Mode cycling

(defun atlas-lens-cycle-mode ()
  "Cycle lens mode: :raw → :semantic → :impl → :raw.
Re-renders overlays without re-fetching entity data."
  (interactive)
  (setq atlas-lens--mode
        (pcase atlas-lens--mode
          (:raw :semantic)
          (:semantic :impl)
          (:impl :raw)))
  (if (eq atlas-lens--mode :raw)
      (progn
        (atlas-lens--clear-overlays)
        (message "Atlas Lens: raw (overlays off)"))
    ;; Re-render with cached data
    (if (and atlas-lens--forms-cache atlas-lens--entities-cache)
        (progn
          (atlas-lens--apply-overlays atlas-lens--forms-cache
                                      atlas-lens--entities-cache
                                      atlas-lens--mode)
          (message "Atlas Lens: %s (%d overlays)"
                   atlas-lens--mode (length atlas-lens--overlays)))
      ;; No cache — do a full refresh
      (atlas-lens-refresh))))

;;; Refresh

(defun atlas-lens-refresh ()
  "Re-scan buffer and refresh all lens overlays."
  (interactive)
  (when atlas-lens-mode
    (if (eq atlas-lens--mode :raw)
        (progn
          (atlas-lens--clear-overlays)
          (message "Atlas Lens: raw (overlays off)"))
      (let ((forms (atlas-lens--find-register-forms)))
        (if (null forms)
            (progn
              (atlas-lens--clear-overlays)
              (setq atlas-lens--forms-cache nil
                    atlas-lens--entities-cache nil)
              (message "Atlas Lens: no register! forms found"))
          (let* ((dev-ids (mapcar (lambda (f) (plist-get f :dev-id)) forms))
                 (entities (atlas-lens--fetch-entities dev-ids)))
            ;; Cache for mode switching
            (setq atlas-lens--forms-cache forms
                  atlas-lens--entities-cache entities)
            (atlas-lens--apply-overlays forms entities atlas-lens--mode)
            (message "Atlas Lens: %s (%d overlays)"
                     atlas-lens--mode (length atlas-lens--overlays))))))))

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
Overlays `register!' forms with template-driven views.

Three lens modes (cycle with C-c C-l m):
  :raw      — no overlay, show actual source
  :semantic — signature card (aspects, context, response, deps)
  :impl     — implementation view (impl fn, context, deps)

Templates are customizable via `atlas-lens-specs'.
When no template exists, ontology keys are used as fallback.

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
    (setq atlas-lens--forms-cache nil
          atlas-lens--entities-cache nil)
    (when atlas-lens--debounce-timer
      (cancel-timer atlas-lens--debounce-timer)
      (setq atlas-lens--debounce-timer nil))
    (remove-hook 'post-command-hook #'atlas-lens--cursor-update t)
    (remove-hook 'after-save-hook #'atlas-lens--after-save t)))

(provide 'atlas-lens)
;;; atlas-lens.el ends here
