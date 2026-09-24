;;; atlas-theme.el --- Stylesheet and layout primitives for atlas buffers -*- lexical-binding: t -*-

;; Copyright (C) 2026
;; Author: @tangrammer + LLMs

;;; Commentary:
;;
;; The "stylesheet" for atlas-mode buffers: semantic faces with light and dark
;; variants (Emacs picks per frame from its `background-mode'), plus a few
;; building blocks that views compose instead of hand-formatting text:
;;
;;   atlas-theme-title        full-width band: type badge, dev-id, meta
;;   atlas-theme-section      "── Label (n) ─────" rule
;;   atlas-theme-row          aligned label column + value
;;   atlas-theme-badge        colored type badge (FN EP CO DS …)
;;   atlas-theme-entity-row   badge + clickable entity
;;   atlas-theme-entity-list  badged entity list, collapsing to "… N more"
;;   atlas-theme-mark         ✓ / ⚠ / ✗ / · status marks
;;   atlas-theme-footer       key-hint bar
;;
;; Terminal-safe by design: only foreground, background, weight and slant —
;; no :height, fonts or boxes, which tty frames ignore.

;;; Code:

(require 'atlas-core)
(require 'atlas-display)

(defgroup atlas-theme nil
  "Visual style of atlas buffers."
  :group 'atlas)

(defcustom atlas-theme-rule-width 72
  "Maximum width of section rules."
  :type 'integer
  :group 'atlas-theme)

;;; Faces

(defface atlas-theme-title-face
  '((((background dark))  :background "#1e3a8a" :foreground "#dbeafe" :weight bold :extend t)
    (t                    :background "#dbeafe" :foreground "#1e3a8a" :weight bold :extend t))
  "Full-width title band at the top of an entity view.")

(defface atlas-theme-section-face
  '((((background dark))  :foreground "#93c5fd" :weight bold)
    (t                    :foreground "#1e3a8a" :weight bold))
  "Section label in a section rule.")

(defface atlas-theme-rule-face
  '((((background dark))  :foreground "#334155")
    (t                    :foreground "#cbd5e1"))
  "Horizontal rule after a section label.")

(defface atlas-theme-label-face
  '((((background dark))  :foreground "#94a3b8")
    (t                    :foreground "#64748b"))
  "Left-column labels in rows (aspect namespaces, property names).")

(defface atlas-theme-effect-face
  '((((background dark))  :foreground "#f87171" :weight bold)
    (t                    :foreground "#dc2626" :weight bold))
  "Aspects that signal side effects (effect/write, effect/delete …).")

(defface atlas-theme-ok-face
  '((((background dark))  :foreground "#4ade80" :weight bold)
    (t                    :foreground "#16a34a" :weight bold))
  "Satisfied / healthy status mark.")

(defface atlas-theme-warn-face
  '((((background dark))  :foreground "#fbbf24" :weight bold)
    (t                    :foreground "#d97706" :weight bold))
  "Warning status mark.")

(defface atlas-theme-bad-face
  '((((background dark))  :foreground "#f87171" :weight bold)
    (t                    :foreground "#dc2626" :weight bold))
  "Missing / broken status mark.")

(defface atlas-theme-dim-face
  '((((background dark))  :foreground "#64748b" :slant italic)
    (t                    :foreground "#94a3b8" :slant italic))
  "Secondary text: empty states, \"… N more\", external inputs.")

(defface atlas-theme-footer-face
  '((((background dark))  :background "#1e293b" :foreground "#cbd5e1" :extend t)
    (t                    :background "#f1f5f9" :foreground "#475569" :extend t))
  "Key-hint bar at the bottom of a view.")

(defface atlas-theme-footer-key-face
  '((((background dark))  :inherit atlas-theme-footer-face :foreground "#93c5fd" :weight bold)
    (t                    :inherit atlas-theme-footer-face :foreground "#1e3a8a" :weight bold))
  "Key names inside the footer bar.")

(defface atlas-theme-bar-face
  '((((background dark))  :foreground "#3b82f6")
    (t                    :foreground "#93c5fd"))
  "Proportional count bars.")

(defface atlas-theme-count-face
  '((((background dark))  :foreground "#e2e8f0" :weight bold)
    (t                    :foreground "#0f172a" :weight bold))
  "Numbers in count rows.")

;;; Badges

(defface atlas-theme-badge-dk-face
  '((((background dark))  :background "#6b21a8" :foreground "#f3e8ff" :weight bold)
    (t                    :background "#c084fc" :foreground "#3b0764" :weight bold))
  "Badge for data keys (not an entity type, so distinct from the DS badge).")

(defconst atlas-theme--badges
  '(("execution-function"  "FN" atlas-lens-badge-fn-face)
    ("interface-endpoint"  "EP" atlas-lens-badge-ep-face)
    ("structure-component" "CO" atlas-lens-badge-co-face)
    ("data-schema"         "DS" atlas-lens-badge-ds-face)
    ("data-key"            "DK" atlas-theme-badge-dk-face))
  "Entity type name → (label face).  Other types get initials + default face.")

(defun atlas-theme--type-name (type)
  "Strip TYPE (\":atlas/execution-function\", symbol or nil) to its name."
  (when type
    (let ((s (atlas--to-string type)))
      (if (string-match "/\\(.*\\)\\'" s) (match-string 1 s) (string-remove-prefix ":" s)))))

(defun atlas-theme-badge (type)
  "Return a colored badge string for entity TYPE (e.g. \" FN \")."
  (let* ((name (atlas-theme--type-name type))
         (known (assoc name atlas-theme--badges))
         (label (cond (known (nth 1 known))
                      (name (let ((words (split-string name "-" t)))
                              (upcase (if (cdr words)
                                          (mapconcat (lambda (w) (substring w 0 1))
                                                     (seq-take words 2) "")
                                        (substring name 0 (min 2 (length name)))))))
                      (t "??"))))
    (propertize (format " %-2s " label)
                'face (if known (nth 2 known) 'atlas-lens-badge-default-face)
                'help-echo (or name "unknown type"))))

;;; Type lookup (one round-trip per view)

(defun atlas-theme-entity-types (entities)
  "Return a hash-table dev-id string → type name for ENTITIES, in one eval."
  (let ((types (make-hash-table :test 'equal))
        (ids (seq-uniq (mapcar #'atlas--to-string (atlas--to-list entities)))))
    (when ids
      (let ((result (atlas--eval
                     (format "(let [ids [%s]] (into {} (for [d ids] [d (some-> (atlas.registry.lookup/props-for d) :atlas/type name)])))"
                             (mapconcat #'atlas--to-keyword ids " ")))))
        (dolist (pair (atlas--map-entries result))
          (puthash (atlas--to-string (car pair)) (cdr pair) types))))
    types))

;;; Building blocks

(defun atlas-theme--width ()
  "Width to lay out against: the selected window (used for rules only).
Title bands align at display time instead, so they fit any pane width."
  (window-width))

(defun atlas-theme--band (left-fn meta)
  "Insert a full-width band: LEFT-FN inserts the left part, META is right-aligned.
META is aligned to the window's right edge at display time (`:align-to'),
so the band stays correct in a narrower pane or after a resize."
  (let ((right (concat (or meta "") " "))
        (start (point)))
    (funcall left-fn)
    (insert (propertize " " 'display `(space :align-to (- right ,(length right)))))
    (insert right)
    ;; band under everything; faces already on the text (badges) take precedence
    (add-face-text-property start (point) 'atlas-theme-title-face t)
    (insert (propertize "\n" 'face 'atlas-theme-title-face) "\n")))

(defun atlas-theme-title (type id &optional meta)
  "Insert an entity title band: badge for TYPE, dev-id ID, META on the right.
The whole band carries ID as `atlas-entity', so M-. anywhere on it means ID."
  (let ((start (point)))
    (atlas-theme--band (lambda () (insert " " (atlas-theme-badge type) "  " id)) meta)
    (put-text-property start (point) 'atlas-entity id)))

(defun atlas-theme-banner (text &optional meta)
  "Insert a title band without a badge: TEXT on the left, META right-aligned."
  (atlas-theme--band (lambda () (insert " " text)) meta))

(defun atlas-theme-section (label &optional count badge-type)
  "Insert a section rule: \"── [BADGE] LABEL (COUNT) ─────\".
BADGE-TYPE, when given, puts that entity type's badge before LABEL."
  (let* ((badge (when badge-type (concat (atlas-theme-badge badge-type) " ")))
         (head (concat "── " label (if count (format "  (%d)" count) "") " "))
         (width (min atlas-theme-rule-width
                     (- (atlas-theme--width) 2))))
    (insert (propertize "── " 'face 'atlas-theme-section-face)
            (or badge "")
            (propertize (substring head 3) 'face 'atlas-theme-section-face)
            (propertize (make-string (max 3 (- width (length head) (length badge))) ?─)
                        'face 'atlas-theme-rule-face)
            "\n")))

(defun atlas-theme-bar (n max &optional width)
  "Return a bar string for N out of MAX, at most WIDTH (default 24) cells."
  (let ((cells (if (and max (> max 0))
                   (max (if (> n 0) 1 0) (round (* (or width 24) (/ (float n) max))))
                 0)))
    (propertize (make-string cells ?█) 'face 'atlas-theme-bar-face)))

(defun atlas-theme-count (n &optional width)
  "Return N right-aligned in WIDTH (default 4) columns, in the count face."
  (propertize (format (format "%%%dd" (or width 4)) n) 'face 'atlas-theme-count-face))

(defun atlas-theme-row (label value-fn &optional label-width)
  "Insert an aligned row: LABEL in the label column, then VALUE-FN's output.
LABEL-WIDTH defaults to 11 columns."
  (insert "  " (propertize (format (format "%%-%ds" (or label-width 11)) label)
                           'face 'atlas-theme-label-face))
  (funcall value-fn)
  (insert "\n"))

(defun atlas-theme-mark (status &optional note)
  "Insert a status mark: STATUS is `ok', `warn', `bad' or `none'; NOTE follows it."
  (pcase-let ((`(,glyph ,face) (pcase status
                                 ('ok   '("✓" atlas-theme-ok-face))
                                 ('warn '("⚠" atlas-theme-warn-face))
                                 ('bad  '("✗" atlas-theme-bad-face))
                                 (_     '("·" atlas-theme-dim-face)))))
    (insert (propertize glyph 'face face))
    (when note (insert " " (propertize note 'face face)))))

(defun atlas-theme-dim (text)
  "Insert TEXT in the secondary style."
  (insert (propertize text 'face 'atlas-theme-dim-face)))

(defun atlas-theme-entity-row (entity types &optional indent)
  "Insert one row: type badge (from TYPES hash) + clickable ENTITY.
With TYPES nil, the row has no badge (e.g. inside a section that has one).
An id TYPES knows nothing about is not a registry entity (e.g. an integrant
component key): it gets a dim marker and plain text, not a dead button."
  (let* ((e (atlas--to-string entity))
         (type (and types (gethash e types))))
    (insert (or indent "  "))
    (cond
     ((null types) (atlas--insert-entity e))
     (type (insert (atlas-theme-badge type) " ")
           (atlas--insert-entity e))
     (t (insert (propertize "  · " 'face 'atlas-theme-dim-face) " "
                (propertize e 'face 'atlas-theme-label-face) "  ")
        (atlas-theme-dim "not in registry")))
    (insert "\n")))

(defun atlas-theme-entity-list (entities types &optional threshold empty-text)
  "Insert ENTITIES as badged rows, collapsing after THRESHOLD (default 10).
TYPES is a hash from `atlas-theme-entity-types', or nil for no badges.
EMPTY-TEXT shows when there are none."
  (let* ((all (atlas--to-list entities))
         (threshold (or threshold 10))
         (hidden (seq-drop all threshold)))
    (if (null all)
        (progn (insert "  ") (atlas-theme-dim (or empty-text "none")) (insert "\n"))
      (dolist (e (seq-take all threshold))
        (atlas-theme-entity-row e types))
      (when hidden
        (insert "  ")
        (insert-text-button
         (format "… %d more" (length hidden))
         'face 'atlas-theme-dim-face
         'follow-link t
         'help-echo "Show the rest"
         'action (let ((rest hidden) (tys types))
                   (lambda (_btn)
                     (let ((inhibit-read-only t))
                       (delete-region (line-beginning-position) (1+ (line-end-position)))
                       (dolist (e rest) (atlas-theme-entity-row e tys))))))
        (insert "\n")))))

(defun atlas-theme-footer (hints)
  "Insert a key-hint bar.  HINTS is a list of (KEY . LABEL) strings."
  (insert "\n")
  (let ((start (point)))
    (insert " ")
    (dolist (h hints)
      (insert (propertize (car h) 'face 'atlas-theme-footer-key-face)
              " " (cdr h) "   "))
    (add-face-text-property start (point) 'atlas-theme-footer-face t)
    (insert (propertize "\n" 'face 'atlas-theme-footer-face))))

;;; Tab bar on dark frames

(defconst atlas-theme--tab-bar-dark
  '((tab-bar              :background "#0f172a" :foreground "#94a3b8")
    (tab-bar-tab          :background "#1e3a8a" :foreground "#dbeafe" :weight bold)
    (tab-bar-tab-inactive :background "#1e293b" :foreground "#94a3b8"))
  "Dark variants for the built-in tab-bar faces, which only define light colors.")

(defun atlas-theme-apply-tab-bar-dark ()
  "Put a dark variant in front of each tab-bar face's default spec.
Light frames keep Emacs's own spec untouched; a theme that styles the tab bar
still takes precedence over both.  Idempotent."
  (dolist (entry atlas-theme--tab-bar-dark)
    (let* ((face (car entry))
           (dark-clause (cons '((background dark)) (cdr entry)))
           (spec (seq-remove (lambda (clause) (equal (car clause) '((background dark))))
                             (face-default-spec face))))
      (face-spec-set face (cons dark-clause spec) 'face-defface-spec))))

(atlas-theme-apply-tab-bar-dark)

(provide 'atlas-theme)
;;; atlas-theme.el ends here
