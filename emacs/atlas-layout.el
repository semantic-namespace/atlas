;;; atlas-layout.el --- Intent-driven window layouts for Atlas -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; Author: @tangrammer + LLMs

;;; Commentary:
;;
;; Five layout functions that arrange Emacs windows based on what the user
;; is trying to understand about the registry.  Each function clears the
;; frame to a single window, opens the appropriate atlas buffers, and
;; arranges them.
;;
;; Designed to be driven by an LLM (e.g. from an ECA chat or Claude Code
;; conversation) via emacsclient --eval, but all functions are interactive
;; and can also be called directly via M-x.
;;
;; Entry points:
;;   atlas-layout/entity-focus   ENTITY   — narrative + type-appropriate side
;;   atlas-layout/dataflow-focus DATA-KEY — producers top / consumers bottom
;;   atlas-layout/blast-radius   ENTITY   — dependents tree + summary
;;   atlas-layout/domain-survey  ASPECT   — full-width aspect slice
;;   atlas-layout/arch-overview           — system summary + execution order

;;; Code:

(require 'atlas-core)
(require 'atlas-browse)
(require 'atlas-lsp)

;;; Helpers

(defun atlas-layout--find-cider-repl ()
  "Return any live CIDER REPL buffer, independent of current buffer context."
  (seq-find (lambda (buf)
              (with-current-buffer buf
                (eq major-mode 'cider-repl-mode)))
            (buffer-list)))

(defun atlas-layout--in-window (win thunk)
  "Select WIN and call THUNK, forcing any buffer display into WIN.
When the current buffer has no CIDER connection (e.g. an *atlas:* buffer),
wraps THUNK in the CIDER REPL buffer so cider-connected-p returns non-nil.
The selected window stays WIN throughout, so display-buffer-same-window
still targets the right pane."
  (select-window win)
  (let ((display-buffer-overriding-action '(display-buffer-same-window . nil)))
    (if (cider-connected-p)
        (funcall thunk)
      (let ((repl-buf (atlas-layout--find-cider-repl)))
        (if repl-buf
            (with-current-buffer repl-buf
              (funcall thunk))
          (funcall thunk))))))

(defun atlas-layout--entity-type (entity)
  "Return entity-type string for ENTITY, e.g. \":atlas/execution-function\".
Reads :atlas/type directly from the registry — the same field atlas.datalog uses.
Returns nil if the entity is not found or the REPL is unavailable."
  (ignore-errors
    (let* ((kw (if (string-prefix-p ":" entity) entity (concat ":" entity)))
           (result (atlas--eval
                    (format "(let [props (get @atlas.registry/registry
                                             (get @atlas.registry/dev-id-index %s))]
                               (str (:atlas/type props)))"
                            kw))))
      (when (and result (not (string-empty-p result)))
        result))))

(defvar atlas-layout--nrepl-root-cache nil
  "Cached nREPL working directory for resolving relative source paths.")

(defvar atlas-layout--dataflow-cache (make-hash-table :test 'equal)
  "Cache of entity-type string → has-dataflow-p boolean.
Ontology is stable per session so one lookup per type suffices.")

(defun atlas-layout--nrepl-root ()
  "Return the nREPL process working directory (cached per session).
Uses cider-nrepl-sync-request:eval directly to avoid the atlas.ide qualifier
wrapping that atlas--eval applies, which breaks Java interop forms."
  (or atlas-layout--nrepl-root-cache
      (setq atlas-layout--nrepl-root-cache
            (let* ((result (cider-nrepl-sync-request:eval
                            "(System/getProperty \"user.dir\")"))
                   (value (nrepl-dict-get result "value")))
              (when value
                ;; nREPL returns Clojure printed string e.g. "\"/path\""
                (string-trim value "\"" "\""))))))

(defun atlas-layout--has-dataflow-p (etype)
  "Return non-nil if ETYPE's ontology declares dataflow context or response verbs.
Result is cached — ontology is stable for the lifetime of the nREPL session.
ETYPE is a string like \":atlas/execution-function\"."
  (let ((cached (gethash etype atlas-layout--dataflow-cache 'miss)))
    (if (not (eq cached 'miss))
        cached
      (let ((result
             (atlas--eval
              (format
               "(let [reg @atlas.registry/registry
                      ont (->> reg
                               (filter (fn [[cid _]] (contains? cid :atlas/ontology)))
                               (map (fn [[_ p]] p))
                               (filter (fn [p] (= %s (:ontology/for p))))
                               first)]
                  (boolean (or (:dataflow/context-verb ont)
                               (:dataflow/response-verb ont))))"
               etype))))
        (puthash etype result atlas-layout--dataflow-cache)
        result))))

(defun atlas-layout--definition-location (entity)
  "Return (abs-file line) for ENTITY's register! call, or nil.
Searches via nREPL so paths resolve relative to the nREPL project root."
  (let* ((kw (if (string-prefix-p ":" entity) entity (concat ":" entity)))
         (code (format "(do (require '[%s :as lsp]) (vec (lsp/find-dev-id-usages %s)))"
                       atlas-lsp-helpers-ns kw))
         (usages-list (atlas--to-list (atlas--eval code)))
         (root (atlas-layout--nrepl-root))
         ;; Registration line contains the entity-type keyword e.g. ":atlas/execution-function"
         (reg (or (seq-find (lambda (u)
                              (string-match-p ":atlas/" (or (atlas--get u 'content) "")))
                            usages-list)
                  (car usages-list))))
    (when (and reg root)
      (list (expand-file-name (atlas--get reg 'file) root)
            (atlas--get reg 'line)))))

(defun atlas-layout--open-definition (entity)
  "Open source file at ENTITY's register! line in the selected window.
Falls back to a plain message if the file cannot be resolved."
  (let ((loc (atlas-layout--definition-location entity)))
    (if loc
        (let ((file (car loc))
              (line (cadr loc)))
          (if (file-exists-p file)
              (progn
                (find-file file)
                (goto-char (point-min))
                (forward-line (1- line))
                (recenter 10))
            (message "Definition file not found: %s" file)))
      (message "No definition found for %s" entity))))

;;; Layout functions

;;;###autoload
(defun atlas-layout/entity-focus (entity)
  "Open entity understanding layout for ENTITY.

Top-left:  entity details (aspects + all properties).
Top-right: ontology-driven semantic pane — queries the live registry to check
  whether this entity type declares dataflow verbs (context-verb/response-verb)
  in its ontology entry. Types that do get data-flow; all others get dependents.
  New entity types are handled automatically as their ontology modules register.
Bottom (full width): source file opened at the register! line."
  (interactive (list (atlas--completing-read-entity "Entity: ")))
  (delete-other-windows)
  (let* ((top    (selected-window))
         (bottom (split-window-below (/ (* (window-height) 2) 3)))
         ;; Pre-compute both while the initial buffer has CIDER context —
         ;; after the first atlas-layout--in-window call the current buffer
         ;; may be an *atlas:* buffer which has no CIDER project association.
         (etype  (atlas-layout--entity-type entity))
         (has-df (atlas-layout--has-dataflow-p etype)))
    ;; Top-left: entity details
    (atlas-layout--in-window top
      (lambda () (atlas-browse-entity-info entity)))
    ;; Top-right: ontology-driven semantic pane.
    (let ((top-right (split-window-right nil top)))
      (atlas-layout--in-window top-right
        (lambda ()
          (if has-df
              (atlas-browse-data-flow entity)
            (atlas-browse-dependents entity))))
      ;; Bottom: source at register! line
      (atlas-layout--in-window bottom
        (lambda () (atlas-layout--open-definition entity)))
      ;; Wire linked-pane navigation: entity clicks in either top pane update
      ;; entity-info (top-left) and source (bottom) without breaking the layout.
      ;; The nav-fn is set buffer-locally so it is resolved from the right buffer
      ;; at click time (atlas-layout--entity-nav-fn is a dynamic/special variable).
      (let ((nav-fn (let ((ew top) (sw bottom))
                      (lambda (e)
                        (when (window-live-p ew)
                          (atlas-layout--in-window ew
                            (lambda () (atlas-browse-entity-info e))))
                        (when (window-live-p sw)
                          (atlas-layout--in-window sw
                            (lambda () (atlas-layout--open-definition e))))))))
        (with-current-buffer (window-buffer top)
          (setq-local atlas-layout--entity-nav-fn nav-fn))
        (with-current-buffer (window-buffer top-right)
          (setq-local atlas-layout--entity-nav-fn nav-fn))))
    (select-window top)))

;;;###autoload
(defun atlas-layout/dataflow-focus (data-key)
  "Open data flow layout for DATA-KEY.
Top window: producers.  Bottom window: consumers."
  (interactive (list (read-string "Data key (e.g. :user/id): " ":")))
  (delete-other-windows)
  (let ((top (selected-window)))
    (atlas-layout--in-window top
      (lambda () (atlas-browse-producers data-key)))
    (let ((bottom (split-window-below)))
      (atlas-layout--in-window bottom
        (lambda () (atlas-browse-consumers data-key)))
      (select-window top))))

;;;###autoload
(defun atlas-layout/blast-radius (entity)
  "Open blast radius layout for ENTITY.
Left window: full recursive dependents tree.
Right window: dependents summary (count by type)."
  (interactive (list (atlas--completing-read-entity "Entity: ")))
  (delete-other-windows)
  (let ((left (selected-window)))
    (atlas-layout--in-window left
      (lambda () (atlas-browse-recursive-dependents entity)))
    (let ((right (split-window-right (/ (window-width) 3))))
      (atlas-layout--in-window right
        (lambda () (atlas-browse-dependents-summary entity)))
      (select-window left))))

;;;###autoload
(defun atlas-layout/home ()
  "Open registry home: total count and type breakdown.
Each type is clickable — opens domain-survey for that type."
  (interactive)
  (delete-other-windows)
  (atlas-layout--in-window (selected-window)
    (lambda () (atlas-browse-home))))

;;;###autoload
(defun atlas-layout/domain-survey (aspect)
  "Show all entities carrying ASPECT, grouped by type with collapsible lists.
Each type group shows up to 10 entities; click '… N more' to reveal the rest."
  (interactive (list (read-string "Aspect (e.g. :domain/auth): " ":")))
  (delete-other-windows)
  (atlas-layout--in-window (selected-window)
    (lambda () (atlas-browse-aspect-entities aspect))))

;;;###autoload
(defun atlas-layout/arch-overview ()
  "Open architecture overview layout.
Top window: system summary.  Bottom window: topological execution order."
  (interactive)
  (delete-other-windows)
  (let ((top (selected-window)))
    (atlas-layout--in-window top
      (lambda () (atlas-browse-system-summary)))
    (let ((bottom (split-window-below (/ (window-height) 2))))
      (atlas-layout--in-window bottom
        (lambda () (atlas-browse-execution-order)))
      (select-window top))))

(provide 'atlas-layout)
;;; atlas-layout.el ends here
