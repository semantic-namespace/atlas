;;; atlas-render.el --- Channel-agnostic rendering for Atlas entity forms -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; Author: @tangrammer + LLMs

;;; Commentary:
;;
;; Channel-agnostic template engine for rendering Atlas entity info.
;; Consumed by atlas-lens (overlay-on-source) and any future buffer
;; that assembles entities from the registry (e.g. atlas-slice).
;;
;; Customization surface:
;;   `atlas-render-specs'        — templates per entity type, per mode
;;   `atlas-render-type-badges'  — badge label + face per entity type
;;
;; Public API:
;;   `atlas-render-form'             — (info mode) → propertized string
;;   `atlas-render--available-modes' — modes declared in current specs
;;
;; No ontology fallback: when no template matches a type+mode, a minimal
;; default (dev-id + entity-type + aspects) is rendered without a REPL call.

;;; Code:

(require 'atlas-core)

;;; Specs

(defvar atlas-render-specs
  '((:atlas/execution-function
     (:semantic . "── ${dev-id} ── ${entity-type}\n  aspects: ${entity/aspects}\n  context: ${execution-function/context}\n  response: ${execution-function/response}\n  deps: ${execution-function/deps}")
     (:impl . ";; ${dev-id}\n${execution-function/impl}\n  :context ${execution-function/context}\n  :deps ${execution-function/deps}")
     (:narrative . "*${dev-id}*\n\nAn execution function in the *${domain}* domain, carrying intent *${intent}*. Context: `${context-list}` (${context-producers-prose}). Response: `${response-list}` (${consumers-prose}).\n\n${deps-prose}"))

    (:atlas/interface-endpoint
     (:semantic . "── ${dev-id} ── ${entity-type}\n  aspects: ${entity/aspects}\n  context: ${interface-endpoint/context}\n  response: ${interface-endpoint/response}\n  deps: ${interface-endpoint/deps}")
     (:impl . ";; ${dev-id}\n${interface-endpoint/impl}\n  :context ${interface-endpoint/context}\n  :deps ${interface-endpoint/deps}")
     (:narrative . "*${dev-id}*\n\nAn interface endpoint in the *${domain}* domain, operation *${operation}*. Context: `${context-list}` (${context-producers-prose}). Response: `${response-list}` (${consumers-prose}).\n\n${dependents-prose}. ${deps-prose}"))

    (:atlas/structure-component
     (:semantic . "── ${dev-id} ── ${entity-type}\n  aspects: ${entity/aspects}\n  deps: ${structure-component/deps}")
     (:impl . ";; ${dev-id}\n  :deps ${structure-component/deps}")
     (:narrative . "*${dev-id}*\n\nStructural component in the *${domain}* domain. It carries ${deps-count}. ${deps-prose}\n\n${dependents-prose}."))

    (:atlas/data-schema
     (:semantic . "── ${dev-id} ── ${entity-type}\n  aspects: ${entity/aspects}\n  fields: ${atlas/fields}")
     (:impl . ";; ${dev-id}\n  :fields ${atlas/fields}"))

    (:atlas/llm-prompt
     (:narrative . "*${dev-id}*\n\n${summary}\n\nFile: `${file}`. Depends on ${mcp-deps-count}: `${mcp-deps-list}`.")))

  "Template specs per entity type and mode.
Each entry is (ENTITY-TYPE-KEYWORD (MODE . TEMPLATE-STRING) ...).
Templates use ${key} placeholders.

Special keys (:semantic, :impl modes):
  ${dev-id}          — entity dev-id without leading colon
  ${entity-type}     — the :atlas/* entity type keyword string
  ${entity/aspects}  — semantic aspects, dot-separated
  All other keys are looked up from entity properties via `atlas--get'.

In :narrative mode, keys come from atlas.ide.narrative/narrative-context
(a flat string map: domain, intent, context-list, deps-prose, etc.).
Markup: *word* → `atlas-book-entity-face', `word' → `atlas-book-key-face'.

When no template is found for a type+mode, `atlas-render--minimal-template'
is used — no REPL call is made.")

;;; Type Badges
;;
;; Keyed by full entity type keyword (:atlas/execution-function etc.).
;; Add entries for custom entity types here.

(defvar atlas-render-type-badges
  '((:atlas/execution-function  . (" fn " . atlas-lens-badge-fn-face))
    (:atlas/interface-endpoint  . (" ep " . atlas-lens-badge-ep-face))
    (:atlas/structure-component . (" co " . atlas-lens-badge-co-face))
    (:atlas/data-schema         . (" ds " . atlas-lens-badge-ds-face)))
  "Mapping of entity type keywords to (LABEL . FACE) for badge rendering.
Extend this for custom entity types.")

;;; Book Typography Faces (narrative mode only)
;;
;; Applied by `atlas-render--propertize-narrative' when rendering :narrative cards.
;; All other modes use `atlas-lens-*-face' faces defined in atlas-core.el.

(defface atlas-book-entity-face
  '((((class color) (background dark))
     :weight bold :slant italic :foreground "#ddd0a8")
    (t :weight bold :slant italic))
  "Face for entity dev-id references (*word*) in :narrative mode.")

(defface atlas-book-key-face
  '((((class color) (background dark))
     :family "Monospace" :height 0.85 :foreground "#8899cc" :background "#1c1e30")
    (((class color) (background light))
     :family "Monospace" :height 0.85 :foreground "#4a5490" :background "#eceef8")
    (t :family "Monospace" :height 0.85))
  "Face for data key references (`word`) in :narrative mode.")

(defface atlas-book-separator-face
  '((((class color) (background dark))
     :foreground "#484038")
    (t :foreground "#c0b8ac"))
  "Face for the `· · ·' separator between narrative cards.")

(defun atlas-render--type-badge (entity-type)
  "Return a propertized badge for ENTITY-TYPE string like \":atlas/execution-function\"."
  (let* ((kw (intern entity-type))
         (entry (assq kw atlas-render-type-badges))
         (label (if entry
                    (car (cdr entry))
                  (let ((tail (car (last (split-string entity-type "/")))))
                    (format " %s " (truncate-string-to-width (or tail entity-type) 2)))))
         (face (if entry (cdr (cdr entry)) 'atlas-lens-badge-default-face)))
    (propertize label 'face face)))

;;; Available Modes

(defun atlas-render--available-modes ()
  "Return ordered list of content modes declared in `atlas-render-specs'.
Does not include :raw — that is a no-overlay mode handled by the caller."
  (delete-dups
   (mapcar #'car
           (apply #'append (mapcar #'cdr atlas-render-specs)))))

;;; Template Engine

(defconst atlas-render--minimal-template
  "── ${dev-id} ── ${entity-type}\n  aspects: ${entity/aspects}"
  "Fallback template when no user template matches a type+mode.
No REPL call is made to produce this.")

(defconst atlas-render--minimal-narrative-template
  "*${dev-id}*\n\n/${entity-type}/. Aspects: ${aspects}."
  "Fallback org narrative template when no :narrative entry matches a type.")

(defun atlas-render--get-entity-type (info)
  "Extract the :atlas/* entity type keyword string from entity INFO."
  (let (type-kw)
    (mapc (lambda (k)
            (let ((s (atlas--to-string k)))
              (when (string-prefix-p ":atlas/" s)
                (setq type-kw s))))
          (atlas--to-list (atlas--get info :entity/identity)))
    (or type-kw "")))

(defun atlas-render--get-template (entity-type mode)
  "Return template string for ENTITY-TYPE string and MODE symbol.
For :narrative mode falls back to `atlas-render--minimal-narrative-template';
all other modes fall back to `atlas-render--minimal-template'."
  (let* ((type-spec (assoc (intern entity-type) atlas-render-specs))
         (tmpl (when type-spec (cdr (assoc mode (cdr type-spec))))))
    (or tmpl
        (if (eq mode :narrative)
            atlas-render--minimal-narrative-template
          atlas-render--minimal-template))))

;;; Formatting Helpers

(defun atlas-render--format-kw-list (items)
  "Format a list of keyword ITEMS as a \" · \"-separated string."
  (mapconcat (lambda (item)
               (let ((s (atlas--to-string item)))
                 (if (string-prefix-p ":" s) (substring s 1) s)))
             (atlas--to-list items)
             " · "))

(defun atlas-render--format-set (items)
  "Format a set/vector ITEMS as #{...} string."
  (let ((formatted (mapconcat (lambda (item)
                                (let ((s (atlas--to-string item)))
                                  (if (string-prefix-p ":" s) s (concat ":" s))))
                              (atlas--to-list items)
                              " ")))
    (format "#{%s}" formatted)))

;;; Value Resolution

(defun atlas-render--resolve-value (key info)
  "Resolve template KEY to a display string from entity INFO."
  (cond
   ((string= key "dev-id")
    (let ((s (atlas--to-string (or (atlas--get info :atlas/dev-id)
                                   (atlas--get info :entity/dev-id) ""))))
      (if (string-prefix-p ":" s) (substring s 1) s)))

   ((string= key "entity-type")
    (atlas-render--get-entity-type info))

   ((string= key "entity/aspects")
    (let ((aspects (atlas--get info :entity/aspects)))
      (if aspects (atlas-render--format-kw-list aspects) "")))

   (t
    (let ((val (atlas--get info key)))
      (cond
       ((null val) "")
       ((vectorp val) (format "[%s]" (atlas-render--format-kw-list val)))
       ((atlas--edn-set-p val) (atlas-render--format-set val))
       ((listp val) (atlas-render--format-kw-list val))
       (t (format "%s" val)))))))

(defun atlas-render--resolve-narrative-value (key info)
  "Resolve KEY from a narrative context INFO (flat string map from atlas.ide.narrative).
Tries :keyword, 'symbol, and string key forms to handle EDN parse variations."
  (let ((val (or (gethash (intern (concat ":" key)) info)
                 (gethash (intern key) info)
                 (gethash key info))))
    (cond
     ((null val) "")
     ((stringp val) val)
     (t (format "%s" val)))))

;;; Rendering

(defun atlas-render--propertize-aspects (aspects-str)
  "Apply per-aspect coloring to ASPECTS-STR (\" · \"-separated)."
  (if (string-empty-p aspects-str)
      aspects-str
    (mapconcat (lambda (a) (propertize a 'face 'atlas-aspect-face))
               (split-string aspects-str " · " t)
               (propertize " · " 'face 'atlas-lens-separator-face))))

(defun atlas-render--propertize-rendered (text)
  "Apply faces to rendered TEXT line by line."
  (let ((lines (split-string text "\n"))
        result-lines)
    (dolist (line lines)
      (push
       (cond
        ;; Header: ── dev-id ──  :atlas/type
        ((string-match "^\\(── \\)\\([^ ]+\\)\\( ── *\\)\\(.*\\)" line)
         (let ((entity-type (string-trim (match-string 4 line))))
           (concat (atlas-render--type-badge entity-type)
                   " "
                   (propertize (match-string 2 line) 'face 'atlas-lens-dev-id-face)
                   "  "
                   (propertize entity-type 'face 'atlas-lens-type-face))))
        ;; Comment line (impl mode)
        ((string-match "^;;" line)
         (propertize line 'face 'font-lock-comment-face))
        ;; aspects: line
        ((string-match "^\\(\\s-*aspects:\\s-*\\)\\(.*\\)" line)
         (concat (propertize (match-string 1 line) 'face 'atlas-lens-label-face)
                 (atlas-render--propertize-aspects (match-string 2 line))))
        ;; key: value line
        ((string-match "^\\(\\s-*\\)\\([^ \t\n:]+:\\)\\(.*\\)" line)
         (concat (match-string 1 line)
                 (propertize (match-string 2 line) 'face 'atlas-lens-label-face)
                 (propertize (match-string 3 line) 'face 'atlas-lens-value-face)))
        ;; separator line
        ((string-match "^─+$" line)
         (propertize line 'face 'atlas-lens-separator-face))
        (t line))
       result-lines))
    (concat (string-join (nreverse result-lines) "\n") "\n")))

(defun atlas-render--substitute-template (template info &optional narrative-p)
  "Substitute ${key} placeholders in TEMPLATE with values from INFO.
When NARRATIVE-P, uses `atlas-render--resolve-narrative-value' (flat string map).
Returns a plain string with no text properties."
  (replace-regexp-in-string
   "\\${\\([^}]+\\)}"
   (lambda (match)
     (let ((key (match-string 1 match)))
       (save-match-data
         (if narrative-p
             (atlas-render--resolve-narrative-value key info)
           (atlas-render--resolve-value key info)))))
   template t t))

(defun atlas-render--propertize-narrative (text)
  "Apply book faces to narrative TEXT by consuming *entity* and `key` markup.
Scans from end to start so deletions do not shift earlier positions."
  (with-temp-buffer
    (insert text)
    ;; `key` spans → atlas-book-key-face
    (goto-char (point-max))
    (while (re-search-backward "`\\([^`\n]+\\)`" nil t)
      (let ((content (match-string 1)))
        (delete-region (match-beginning 0) (match-end 0))
        (insert (propertize content 'face 'atlas-book-key-face))))
    ;; *entity* spans → atlas-book-entity-face
    (goto-char (point-max))
    (while (re-search-backward "\\*\\([^*\n]+\\)\\*" nil t)
      (let ((content (match-string 1)))
        (delete-region (match-beginning 0) (match-end 0))
        (insert (propertize content 'face 'atlas-book-entity-face))))
    (buffer-string)))

(defun atlas-render--render-template (template info)
  "Substitute ${key} in TEMPLATE from INFO and apply :semantic/:impl faces."
  (atlas-render--propertize-rendered
   (atlas-render--substitute-template template info)))

;;; Public API

(defun atlas-render--narrative-entity-type (info)
  "Extract entity type string from a narrative context map INFO.
Narrative maps carry `:entity-type' as a plain string value."
  (let ((val (or (gethash (intern ":entity-type") info)
                 (gethash (intern "entity-type") info)
                 (gethash "entity-type" info)
                 "")))
    (if (stringp val) val (format "%s" val))))

(defun atlas-render-form (info mode)
  "Render entity INFO in lens MODE as a propertized string.

In :semantic and :impl modes, INFO is an entity properties hash-table
from the registry (as returned by atlas-lens--fetch-entities).

In :narrative mode, INFO is a narrative context map from
atlas.ide.narrative/narrative-contexts — a flat string map with keys
like :dev-id, :domain, :context-list, :deps-prose, etc.

Returns a propertized string with a separator appended."
  (if (eq mode :narrative)
      (let* ((entity-type (atlas-render--narrative-entity-type info))
             (template (atlas-render--get-template entity-type mode))
             (substituted (atlas-render--substitute-template template info t)))
        (concat (atlas-render--propertize-narrative substituted)
                "\n"
                (propertize "· · ·\n" 'face 'atlas-book-separator-face)))
    (let* ((entity-type (atlas-render--get-entity-type info))
           (template (atlas-render--get-template entity-type mode))
           (card (atlas-render--render-template template info)))
      (concat card (propertize "────────────────────────────────\n"
                               'face 'atlas-lens-separator-face)))))

;;; Shared Lookup Helper

(defun atlas-render--lookup-entity (entities dev-id-str)
  "Look up DEV-ID-STR in ENTITIES hash-table.
Tries :keyword, bare-symbol, and :bare-symbol key forms to handle
EDN parse variations."
  (when (hash-table-p entities)
    (let ((bare (if (string-prefix-p ":" dev-id-str)
                    (substring dev-id-str 1)
                  dev-id-str)))
      (or (gethash (intern dev-id-str) entities)
          (gethash (intern bare) entities)
          (gethash (intern (concat ":" bare)) entities)
          (atlas--get entities dev-id-str)
          (atlas--get entities bare)))))

;;; Org Rendering (for atlas-slice)

(defun atlas-render--org-properties (info)
  "Return alist of (PROP-NAME . VALUE) strings from narrative context map INFO.
Only non-empty values are useful; callers filter with `seq-filter'."
  (list (cons "TYPE"   (atlas-render--resolve-narrative-value "entity-type" info))
        (cons "DOMAIN" (atlas-render--resolve-narrative-value "domain"      info))
        (cons "INTENT" (atlas-render--resolve-narrative-value "intent"      info))))

(defun atlas-render--org-tags (info)
  "Return an org tag string derived from narrative context map INFO.
Extracts the entity-type name, domain, intent, and operation and
formats them as `:tag1:tag2:'.  Org tags only allow [a-zA-Z0-9_]."
  (let* ((sanitize (lambda (s)
                     (when (and s (not (string-empty-p s)))
                       (replace-regexp-in-string "[^a-zA-Z0-9]" "_" s))))
         (entity-type (atlas-render--resolve-narrative-value "entity-type" info))
         (type-tag (when (string-match "/\\(.+\\)$" entity-type)
                     (funcall sanitize (match-string 1 entity-type))))
         (domain    (funcall sanitize (atlas-render--resolve-narrative-value "domain"    info)))
         (intent    (funcall sanitize (atlas-render--resolve-narrative-value "intent"    info)))
         (operation (funcall sanitize (atlas-render--resolve-narrative-value "operation" info)))
         (tags (seq-filter #'identity (list type-tag domain intent operation))))
    (if tags
        (concat "  :" (string-join tags ":") ":")
      "")))

(defun atlas-render--linkify-dev-ids (text)
  "Replace bare ns/name patterns in TEXT with org internal links.
Only processes plain text — callers must ensure TEXT contains no
org markup spans that should be protected."
  (replace-regexp-in-string
   "\\b\\([a-zA-Z][a-zA-Z0-9._-]*/[a-zA-Z][a-zA-Z0-9._-]*\\)\\b"
   "[[*\\1][\\1]]"
   text t))

(defun atlas-render--org-linkify-body (text)
  "Linkify bare dev-id refs in TEXT, skipping =verbatim= and *bold* spans."
  (let ((result "")
        (remaining text)
        (skip-re "\\(=\\([^=\n]+\\)=\\|\\*\\([^*\n]+\\)\\*\\)"))
    (while (string-match skip-re remaining)
      (let ((before (substring remaining 0 (match-beginning 0)))
            (span   (match-string 0 remaining)))
        (setq result    (concat result (atlas-render--linkify-dev-ids before) span)
              remaining (substring remaining (match-end 0)))))
    (concat result (atlas-render--linkify-dev-ids remaining))))

(defun atlas-render--org-body (info)
  "Render entity INFO as an org-mode prose string (no text properties).
Uses the :narrative template from `atlas-render-specs'.
Strips the leading *dev-id* line (already present as the org heading).
Converts backtick spans to =verbatim=, then linkifies bare ns/name refs
(e.g. in deps-prose) to org internal links."
  (let* ((entity-type (atlas-render--narrative-entity-type info))
         (template    (atlas-render--get-template entity-type :narrative))
         (substituted (atlas-render--substitute-template template info t))
         ;; Drop leading *dev-id*\n\n — already present as the org heading
         (no-header   (replace-regexp-in-string "^\\*[^*\n]+\\*\n\n" "" substituted))
         ;; Remove empty bold markers from empty template values, e.g.:
         ;;   ", carrying intent **" → "" (comma-led clause with empty bold)
         (no-empty-bold (replace-regexp-in-string ",\\s-*[a-z][a-z ]+\\*\\*" "" no-header))
         ;; Suppress uninformative producer/consumer clauses
         (stripped    (replace-regexp-in-string
                       " (with no known consumer in the registry)" ""
                       (replace-regexp-in-string
                        " (no producer is registered for these keys)" ""
                        no-empty-bold)))
         ;; Convert all `...` spans → =org-verbatim=
         ;; (backtick-to-link was removed: data keys with dotted namespaces
         ;;  like com.example.spec.*/x are indistinguishable from entity dev-ids)
         (verbatim    (replace-regexp-in-string "`\\([^`\n]+\\)`" "=\\1=" stripped t)))
    ;; Linkify bare entity dev-id refs outside protected spans
    (atlas-render--org-linkify-body verbatim)))

(provide 'atlas-render)
;;; atlas-render.el ends here
