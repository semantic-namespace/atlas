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
;; LLM-driven sessions run in a dedicated daemon (see atlas-llm-emacs.sh):
;; the LLM starts it and connects CIDER; the human attaches a frame with
;; `emacsclient -t -s <socket>'.  Layouts are drawn into that attached
;; client frame, each in its own tab, so earlier views stay reachable.
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

(defgroup atlas-layout nil
  "Intent-driven window layouts for Atlas."
  :group 'atlas)

(defcustom atlas-layout-use-tabs t
  "When non-nil, each layout opens in its own tab named after the layout."
  :type 'boolean
  :group 'atlas-layout)

(defcustom atlas-layout-narrow-width 140
  "Frames narrower than this stack entity-focus panes vertically."
  :type 'integer
  :group 'atlas-layout)

;;; Client frame + tabs

(defvar atlas-layout--last-client-frame nil
  "Most recently attached client frame (set by `server-after-make-frame-hook').")

(defun atlas-layout--remember-client-frame ()
  "Record the selected frame as the latest attach target when it is a client frame."
  (when (frame-parameter (selected-frame) 'client)
    (setq atlas-layout--last-client-frame (selected-frame))))

(add-hook 'server-after-make-frame-hook #'atlas-layout--remember-client-frame)

(defcustom atlas-layout-tty-background-mode 'auto
  "Background mode for terminal frames attached to an atlas daemon.
`auto' reads the attaching terminal's environment: ATLAS_EMACS_BACKGROUND
\(dark or light), then the COLORFGBG hint many terminals set; with neither,
Emacs's own guess stands.  `dark' or `light' force it; nil never touches it.
Emacs usually cannot query a terminal's background and guesses `light', so
on a dark terminal every face with light/dark variants picks the wrong one."
  :type '(choice (const auto) (const dark) (const light) (const :tag "Emacs's guess" nil))
  :group 'atlas-layout)

(defun atlas-layout--atlas-daemon-p ()
  "Non-nil in a daemon started by atlas-llm-daemon.sh (socket atlas-<project>).
Guards per-person preferences so they never touch the user's own Emacs server."
  (and (daemonp) (boundp 'server-name) (stringp server-name)
       (string-prefix-p "atlas-" (file-name-nondirectory server-name))))

(defun atlas-layout--tty-background (frame)
  "Background mode (`dark', `light' or nil) for FRAME's terminal."
  (pcase atlas-layout-tty-background-mode
    ((or 'dark 'light) atlas-layout-tty-background-mode)
    ('auto
     (let ((explicit (getenv "ATLAS_EMACS_BACKGROUND" frame))
           (fgbg (getenv "COLORFGBG" frame)))
       (cond ((member explicit '("dark" "light")) (intern explicit))
             ;; "fg;bg" (or "fg;default;bg"): colour indexes 0-6 and 8 are dark
             ((and fgbg (string-match "\\([0-9]+\\)\\'" fgbg))
              (let ((bg (string-to-number (match-string 1 fgbg))))
                (if (or (< bg 7) (= bg 8)) 'dark 'light))))))))

(defun atlas-layout--apply-client-preferences (&optional frame)
  "Apply the attaching person's preferences to FRAME (terminal client frames).
Background comes from `atlas-layout--tty-background'; ATLAS_EMACS_THEMES=off
disables that person's Emacs themes in this daemon.  Atlas daemons only."
  (let ((frame (or frame (selected-frame))))
    (when (and (atlas-layout--atlas-daemon-p)
               (frame-parameter frame 'client)
               (not (display-graphic-p frame)))
      (when (equal (getenv "ATLAS_EMACS_THEMES" frame) "off")
        (mapc #'disable-theme custom-enabled-themes))
      (when-let* ((mode (atlas-layout--tty-background frame)))
        ;; frame-set-background-mode derives the mode from the terminal
        ;; parameter (a frame parameter alone gets overwritten); this scopes
        ;; it to the one terminal.
        (set-terminal-parameter (frame-terminal frame) 'background-mode mode)
        (frame-set-background-mode frame)))))

(add-hook 'server-after-make-frame-hook #'atlas-layout--apply-client-preferences)

(defun atlas-layout--client-frames ()
  "Live client frames, most recently attached first."
  (let ((all (seq-filter (lambda (f) (frame-parameter f 'client)) (frame-list))))
    (if (memq atlas-layout--last-client-frame all)
        (cons atlas-layout--last-client-frame
              (delq atlas-layout--last-client-frame all))
      all)))

(defun atlas-layout--client-frame ()
  "Return the frame a human attached via emacsclient, or signal.
Prefers the selected frame when it is a client frame (M-x usage);
otherwise the most recently attached one (emacsclient --eval usage, where
the selected frame is the daemon's invisible initial frame, and an older
suspended client may still hold a frame on the same terminal)."
  (or (and (frame-parameter (selected-frame) 'client) (selected-frame))
      (car (atlas-layout--client-frames))
      (and (not (daemonp)) (selected-frame))
      (user-error "No frame attached. Run: atlas-llm-daemon.sh attach (or your `em' alias)")))

(defun atlas-layout--enter-tab (name)
  "Switch to (or create) the tab NAME when `atlas-layout-use-tabs' is set."
  ;; tab-bar is preloaded in Emacs 27+; no `require' (it breaks some inits).
  (when (and atlas-layout-use-tabs (fboundp 'tab-bar-switch-to-tab))
    (tab-bar-mode 1)
    (tab-bar-switch-to-tab name)))

(defmacro atlas-layout--with-layout (name &rest body)
  "Run BODY in the attached client frame, inside the tab NAME."
  (declare (indent 1))
  `(with-selected-frame (atlas-layout--client-frame)
     ;; Don't rearrange windows under a human who is mid-command.
     (when (active-minibuffer-window)
       (user-error "The attached frame's minibuffer is active; try again when it's closed"))
     (atlas-layout--enter-tab ,name)
     ,@body))

(defun atlas-layout--require-entity (entity)
  "Return ENTITY's type string, or signal if it is not in the registry."
  (or (atlas-layout--entity-type entity)
      (user-error "Entity %s not found in the connected registry" entity)))

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
Delegates to lsp-helpers/find-definition, which disambiguates re-registrations
(e.g. test fixtures reusing the dev-id) by the loaded entity's aspects.
Paths resolve relative to the nREPL project root."
  (let* ((kw (if (string-prefix-p ":" entity) entity (concat ":" entity)))
         (code (format "(do (require '[%s :as lsp]) (lsp/find-definition %s))"
                       atlas-lsp-helpers-ns kw))
         (loc (atlas--eval code))
         (root (atlas-layout--nrepl-root)))
    (when (and loc root)
      (list (expand-file-name (atlas--get loc 'file) root)
            (atlas--get loc 'line)))))

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
  (atlas-layout--with-layout (format "entity %s" entity)
  (delete-other-windows)
  (let* (;; Pre-compute both while the initial buffer has CIDER context —
         ;; after the first atlas-layout--in-window call the current buffer
         ;; may be an *atlas:* buffer which has no CIDER project association.
         (etype  (atlas-layout--require-entity entity))
         (has-df (atlas-layout--has-dataflow-p etype))
         (narrow (< (frame-width) atlas-layout-narrow-width))
         (top    (selected-window))
         (bottom (split-window-below (/ (* (window-height) 2) 3))))
    ;; Top-left: entity details
    (atlas-layout--in-window top
      (lambda () (atlas-browse-entity-info entity)))
    ;; Top-right: ontology-driven semantic pane.
    ;; Narrow (e.g. terminal) frames stack the semantic pane under the details.
    (let ((top-right (if narrow
                         (split-window-below nil top)
                       (split-window-right nil top))))
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
    (select-window top))))

;;;###autoload
(defun atlas-layout/dataflow-focus (data-key)
  "Open data flow layout for DATA-KEY.
Top window: producers.  Bottom window: consumers."
  (interactive (list (read-string "Data key (e.g. :user/id): " ":")))
  (atlas-layout--with-layout (format "flow %s" data-key)
    (delete-other-windows)
    (let* ((top (selected-window))
           (bottom (split-window-below)))
      (atlas-layout--in-window top
        (lambda () (atlas-browse-producers data-key)))
      (atlas-layout--in-window bottom
        (lambda () (atlas-browse-consumers data-key)))
      (select-window top))))

;;;###autoload
(defun atlas-layout/blast-radius (entity)
  "Open blast radius layout for ENTITY.
Left window: full recursive dependents tree.
Right window: dependents summary (count by type)."
  (interactive (list (atlas--completing-read-entity "Entity: ")))
  (atlas-layout--with-layout (format "blast %s" entity)
    (atlas-layout--require-entity entity)
    (delete-other-windows)
    (let* ((left (selected-window))
           (right (split-window-right (/ (* (window-width) 2) 3))))
      (atlas-layout--in-window left
        (lambda () (atlas-browse-recursive-dependents entity)))
      (atlas-layout--in-window right
        (lambda () (atlas-browse-dependents-summary entity)))
      (select-window left))))

;;;###autoload
(defun atlas-layout/home ()
  "Open registry home: total count and type breakdown.
Each type is clickable — opens domain-survey for that type."
  (interactive)
  (atlas-layout--with-layout "home"
    (delete-other-windows)
    (atlas-layout--in-window (selected-window)
      (lambda () (atlas-browse-home)))))

;;;###autoload
(defun atlas-layout/domain-survey (aspect)
  "Show all entities carrying ASPECT, grouped by type with collapsible lists.
Each type group shows up to 10 entities; click '… N more' to reveal the rest."
  (interactive (list (read-string "Aspect (e.g. :domain/auth): " ":")))
  (atlas-layout--with-layout (format "aspect %s" aspect)
    (delete-other-windows)
    (atlas-layout--in-window (selected-window)
      (lambda () (atlas-browse-aspect-entities aspect)))))

;;;###autoload
(defun atlas-layout/arch-overview ()
  "Open architecture overview layout.
Top window: system summary.  Bottom window: topological execution order."
  (interactive)
  (atlas-layout--with-layout "architecture"
    (delete-other-windows)
    (let* ((top (selected-window))
           (bottom (split-window-below)))
      (atlas-layout--in-window top
        (lambda () (atlas-browse-system-summary)))
      (atlas-layout--in-window bottom
        (lambda () (atlas-browse-execution-order)))
      (select-window top))))

;;; LLM session: connect + status

(defun atlas-layout--connected-endpoint ()
  "Return (HOST PORT) of the live CIDER REPL, or nil."
  (when-let* ((repl (atlas-layout--find-cider-repl)))
    (with-current-buffer repl
      (list (plist-get nrepl-endpoint :host)
            (plist-get nrepl-endpoint :port)))))

;;;###autoload
(defvar atlas-layout-llm-project nil
  "Project directory this daemon serves (set by `atlas-layout/llm-connect').")

(defun atlas-layout--cider-repls ()
  "All live CIDER REPL buffers in this Emacs."
  (seq-filter (lambda (buf)
                (with-current-buffer buf (eq major-mode 'cider-repl-mode)))
              (buffer-list)))

(defun atlas-layout/llm-connect (host port project-dir)
  "Connect CIDER to HOST:PORT for PROJECT-DIR, unless already connected there.
Idempotent, so the LLM can call it at the start of every session.  A daemon
talks to exactly one REPL: connections to any other port are closed first,
otherwise views could keep querying a stale or dead REPL.  Closing only drops
the client side; REPLs started outside this Emacs keep running."
  (setq atlas-layout-llm-project (file-name-as-directory project-dir))
  (let* ((repls (atlas-layout--cider-repls))
         (same (seq-filter (lambda (r)
                             (with-current-buffer r
                               (equal (plist-get nrepl-endpoint :port) port)))
                           repls))
         (others (seq-difference repls same)))
    (dolist (r others) (cider-quit r))
    (if same
        (if others "switched (closed other REPL connections)" "already-connected")
      (setq atlas-layout--nrepl-root-cache nil)
      (clrhash atlas-layout--dataflow-cache)
      (let ((default-directory (file-name-as-directory project-dir))
            (cider-repl-pop-to-buffer-on-connect nil))
        (cider-connect-clj (list :host host :port port
                                 :project-dir project-dir)))
      "connecting")))

;;;###autoload
(defun atlas-layout/llm-status ()
  "One-line status for the LLM: socket, CIDER endpoint, attached frames, tabs."
  (let* ((endpoint (atlas-layout--connected-endpoint))
         (frames (atlas-layout--client-frames)))
    (format "socket=%s project=%s cider=%s frames=%d size=%s tabs=%s"
            server-name
            (or atlas-layout-llm-project "-")
            (if endpoint (format "%s:%s" (car endpoint) (cadr endpoint)) "none")
            (length frames)
            (if frames
                (format "%dx%d" (frame-width (car frames)) (frame-height (car frames)))
              "-")
            (if frames
                (mapconcat (lambda (tab) (alist-get 'name tab))
                           (funcall tab-bar-tabs-function (car frames)) ",")
              "-"))))

;;;###autoload
(defun atlas-layout/llm-report ()
  "Describe what the attached frame currently shows: one line per window."
  (with-selected-frame (atlas-layout--client-frame)
    (mapconcat
     (lambda (w)
       (with-current-buffer (window-buffer w)
         (format "%s | %s" (buffer-name)
                 (replace-regexp-in-string
                  "\n+" " / "
                  (buffer-substring-no-properties
                   (point-min) (min (point-max) (+ (point-min) 300)))))))
     (window-list) "\n")))

;;;###autoload
(defun atlas-layout/llm-screen ()
  "Return what the human actually sees in the attached frame.
One block per window: header (buffer, size, point line) followed by the
visible text from window-start to window-end — so an LLM can check a layout
against the registry without asking the human to describe their screen."
  (with-selected-frame (atlas-layout--client-frame)
    (redisplay t)
    (mapconcat
     (lambda (w)
       (with-current-buffer (window-buffer w)
         (format "=== %s [%dx%d] point-line=%d%s\n%s"
                 (buffer-name) (window-width w) (window-height w)
                 (line-number-at-pos (window-point w))
                 (if (eq w (frame-selected-window)) " SELECTED" "")
                 (buffer-substring-no-properties
                  (window-start w) (window-end w t)))))
     (window-list) "\n")))

(provide 'atlas-layout)
;;; atlas-layout.el ends here
