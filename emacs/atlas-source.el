;;; atlas-source.el --- Hover and navigation for atlas keywords, no REPL needed -*- lexical-binding: t -*-

;;; Commentary:
;;
;; In a project with an `atlas-source.edn' at its root, point on any
;; `:ns/name' keyword — in a .clj file or a registry .edn file — and:
;;
;;   eldoc     shows what it is: an entity (type, aspects, contract, uses),
;;             a data key (producers, consumers) or an aspect (carriers)
;;   M-.       jumps to where it is defined: each registry file that
;;             describes it, and the var that implements it
;;   M-?       lists what uses it
;;   C-c .     opens a describe buffer with clickable file:line links
;;
;; Entities come from the project's registry files, from register! calls
;; written as data, and — when a REPL is connected through CIDER — from a
;; snapshot the REPL keeps in .atlas/runtime.edn, which fills in what no
;; file says (generated entities, register! calls in loops).
;;
;; Answers come from `bb source serve', one long-lived babashka process per
;; project that reads the registry files and the code (clj-kondo) without
;; loading anything, keeps the index in memory and refreshes it from disk
;; before each answer.  Saved files are seen; unsaved edits are not.
;;
;; For keywords atlas doesn't know, eldoc and xref fall through to the next
;; backend (clojure-lsp, CIDER).
;;
;; Setup:
;;   (load "<atlas checkout>/emacs/atlas-source.el")
;;   (atlas-source-setup)   ; enable in Clojure buffers of atlas projects

;;; Code:

(require 'cl-lib)
(require 'eldoc)
(require 'xref)
(require 'json)

(defgroup atlas-source nil
  "Atlas keywords from the project's source files."
  :group 'tools)

(defcustom atlas-source-bb-program (or (executable-find "bb") "/usr/local/bin/bb")
  "The babashka executable."
  :type 'string)

(defcustom atlas-source-bb-config
  (expand-file-name "../bb.edn" (file-name-directory (or load-file-name buffer-file-name)))
  "The atlas checkout's bb.edn, which defines the `source' task."
  :type 'file)

(defcustom atlas-source-watch-repl t
  "When non-nil, a CIDER connection in an atlas project starts
`atlas.source/watch!', so the REPL keeps .atlas/runtime.edn current."
  :type 'boolean)

(defcustom atlas-source-timeout 3
  "Seconds to wait for a synchronous answer (M-., M-?, describe)."
  :type 'number)

;; =============================================================================
;; Server: one `bb source serve' per project root
;; =============================================================================

(defvar atlas-source--servers (make-hash-table :test 'equal)
  "Project root -> server process.")

(defvar atlas-source--callbacks (make-hash-table :test 'eql)
  "Request id -> fn of the reply plist.")

(defvar atlas-source--next-id 0)

(defun atlas-source--root ()
  "The project root: the closest directory with an atlas-source.edn."
  (when-let* ((dir (locate-dominating-file default-directory "atlas-source.edn")))
    (expand-file-name dir)))

(defun atlas-source--filter (proc output)
  "Dispatch each complete JSON line PROC printed to its request's callback."
  (let ((pending (concat (or (process-get proc 'pending) "") output)))
    (while (string-match "\n" pending)
      (let ((line (substring pending 0 (match-beginning 0))))
        (setq pending (substring pending (match-end 0)))
        (when-let* ((reply (ignore-errors
                             (json-parse-string line :object-type 'plist :array-type 'list
                                                :null-object nil :false-object nil)))
                    (id (plist-get reply :id))
                    (cb (gethash id atlas-source--callbacks)))
          (remhash id atlas-source--callbacks)
          (funcall cb reply))))
    (process-put proc 'pending pending)))

(defun atlas-source--server (root)
  "The live server for ROOT, started if needed."
  (let ((proc (gethash root atlas-source--servers)))
    (if (process-live-p proc)
        proc
      (let* ((default-directory root)
             (proc (make-process
                    :name (format "atlas-source[%s]" (file-name-nondirectory (directory-file-name root)))
                    :command (list atlas-source-bb-program "--config" atlas-source-bb-config
                                   "source" "serve" "--root" root)
                    :connection-type 'pipe
                    :coding 'utf-8
                    :noquery t
                    :stderr (get-buffer-create " *atlas-source stderr*")
                    :filter #'atlas-source--filter)))
        (puthash root proc atlas-source--servers)
        proc))))

(defun atlas-source--request (root op keyword callback)
  "Send OP for KEYWORD to ROOT's server; call CALLBACK with the reply plist."
  (let ((id (cl-incf atlas-source--next-id)))
    (puthash id callback atlas-source--callbacks)
    (process-send-string (atlas-source--server root)
                         (concat (json-serialize (list :id id :op op :keyword (or keyword ""))) "\n"))
    id))

(defun atlas-source--request-sync (root op keyword)
  "OP for KEYWORD from ROOT's server, waiting up to `atlas-source-timeout'."
  (let* ((done nil) (reply nil)
         (proc (atlas-source--server root))
         (deadline (+ (float-time) atlas-source-timeout)))
    (atlas-source--request root op keyword (lambda (r) (setq reply r done t)))
    (while (and (not done) (< (float-time) deadline) (process-live-p proc))
      (accept-process-output proc 0.05))
    (or reply (user-error "atlas-source: no answer from %s (see buffer \" *atlas-source stderr*\")" root))))

(defun atlas-source-stop ()
  "Stop every atlas-source server."
  (interactive)
  (maphash (lambda (_ p) (when (process-live-p p) (delete-process p))) atlas-source--servers)
  (clrhash atlas-source--servers)
  (clrhash atlas-source--callbacks))

;; =============================================================================
;; Keyword at point
;; =============================================================================

(defun atlas-source--keyword-at-point ()
  "The qualified keyword at point, as a string like \":fn/get-cart\", or nil."
  (let ((s (thing-at-point 'symbol t)))
    (when (and s (string-match-p "\\`:[^:/[:space:]]+/[^/[:space:]]+\\'" s))
      s)))

(defun atlas-source--hover (keyword)
  "The hover result plist for KEYWORD, or nil when atlas doesn't know it."
  (when-let* ((root (atlas-source--root)))
    (let ((result (plist-get (atlas-source--request-sync root "hover" keyword) :result)))
      (unless (equal (plist-get result :kind) "none") result))))

;; =============================================================================
;; eldoc
;; =============================================================================

(defun atlas-source-eldoc (callback &rest _)
  "Eldoc for the atlas keyword at point; nil (falls through) otherwise."
  (let ((kw (atlas-source--keyword-at-point))
        (root (atlas-source--root)))
    (when (and kw root)
      (atlas-source--request
       root "hover" kw
       (lambda (reply)
         (let ((r (plist-get reply :result)))
           (if (and r (not (equal (plist-get r :kind) "none")))
               (funcall callback (plist-get r :eldoc) :thing kw :face 'font-lock-constant-face)
             (funcall callback nil)))))
      t)))

;; =============================================================================
;; xref (M-. and M-?)
;; =============================================================================

(defun atlas-source--xref-backend ()
  "The atlas xref backend, when point is on a keyword atlas knows."
  (when-let* ((kw (atlas-source--keyword-at-point)))
    (when (atlas-source--hover kw) 'atlas-source)))

(cl-defmethod xref-backend-identifier-at-point ((_ (eql atlas-source)))
  (atlas-source--keyword-at-point))

(cl-defmethod xref-backend-identifier-completion-table ((_ (eql atlas-source)))
  nil)

(defun atlas-source--xrefs (keyword key)
  (mapcar (lambda (l)
            (xref-make (plist-get l :summary)
                       (xref-make-file-location (plist-get l :file) (plist-get l :line) 0)))
          (plist-get (atlas-source--hover keyword) key)))

(cl-defmethod xref-backend-definitions ((_ (eql atlas-source)) identifier)
  (atlas-source--xrefs identifier :definitions))

(cl-defmethod xref-backend-references ((_ (eql atlas-source)) identifier)
  (atlas-source--xrefs identifier :references))

;; =============================================================================
;; Describe buffer
;; =============================================================================

(define-derived-mode atlas-source-describe-mode special-mode "Atlas"
  "Describes an atlas keyword. RET or click follows a file:line or keyword.")

(defun atlas-source--buttonize (root)
  "Make file:line references and qualified keywords in the buffer clickable."
  (goto-char (point-min))
  (while (re-search-forward "\\([^[:space:]]+\\.\\(?:edn\\|cljc?\\)\\):\\([0-9]+\\)\\(?:-[0-9]+\\)?" nil t)
    (let ((file (expand-file-name (match-string 1) root))
          (line (string-to-number (match-string 2))))
      (make-text-button (match-beginning 0) (match-end 0)
                        'action (lambda (_) (find-file-other-window file) (goto-char (point-min)) (forward-line (1- line)))
                        'help-echo (format "open %s:%d" file line))))
  (goto-char (point-min))
  (while (re-search-forward ":[[:alnum:]._*+!?<>=-]+/[[:alnum:]._*+!?<>=-]+" nil t)
    (unless (get-text-property (match-beginning 0) 'button)
      (let ((kw (match-string 0)))
        (make-text-button (match-beginning 0) (match-end 0)
                          'action (lambda (_) (atlas-source-describe kw))
                          'face 'font-lock-constant-face
                          'help-echo (format "describe %s" kw))))))

(defun atlas-source-describe (&optional keyword)
  "Describe KEYWORD (default: the keyword at point) in a buffer with links."
  (interactive)
  (let* ((kw (or keyword (atlas-source--keyword-at-point) (user-error "No qualified keyword at point")))
         (root (or (atlas-source--root) (user-error "No atlas-source.edn above %s" default-directory)))
         (r (or (atlas-source--hover kw) (user-error "atlas doesn't know %s" kw))))
    (with-current-buffer (get-buffer-create "*atlas*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (atlas-source-describe-mode)
        (setq default-directory root)
        (insert (plist-get r :describe) "\n")
        (atlas-source--buttonize root)
        (goto-char (point-min))
        (put-text-property (point-min) (line-end-position) 'face 'bold))
      (display-buffer (current-buffer)))))

;; =============================================================================
;; REPL snapshot (CIDER)
;; =============================================================================

(declare-function cider-nrepl-request:eval "cider-client")
(declare-function nrepl-dict-get "nrepl-dict")

(defun atlas-source-watch-repl ()
  "Make the connected REPL keep <project>/.atlas/runtime.edn current, so
hover sees entities only the running code knows about."
  (interactive)
  (when-let* ((root (atlas-source--root)))
    (let ((path (expand-file-name ".atlas/runtime.edn" root)))
      (cider-nrepl-request:eval
       (format "(do (require 'atlas.source) (atlas.source/watch! %S))" path)
       (lambda (response)
         (when-let* ((err (nrepl-dict-get response "err")))
           (message "atlas-source: REPL snapshot not started: %s" (string-trim err))))))))

(defun atlas-source--on-cider-connected ()
  (when (and atlas-source-watch-repl (atlas-source--root))
    (atlas-source-watch-repl)))

;; =============================================================================
;; Mode
;; =============================================================================

(defvar atlas-source-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c .") #'atlas-source-describe)
    m))

;;;###autoload
(define-minor-mode atlas-source-mode
  "Hover, M-. and M-? for atlas keywords, from the project's source files."
  :lighter " atlas"
  :keymap atlas-source-mode-map
  (if atlas-source-mode
      (progn
        ;; Ahead of CIDER (`cider-xref-fn-depth' -90) and lsp-mode: the
        ;; atlas backends answer only for keywords atlas knows, so the rest
        ;; still reaches them.
        (add-hook 'eldoc-documentation-functions #'atlas-source-eldoc -95 t)
        (add-hook 'xref-backend-functions #'atlas-source--xref-backend -95 t)
        (setq-local eldoc-echo-area-use-multiline-p t)
        (eldoc-mode 1))
    (remove-hook 'eldoc-documentation-functions #'atlas-source-eldoc t)
    (remove-hook 'xref-backend-functions #'atlas-source--xref-backend t)))

(defun atlas-source-maybe-enable ()
  "Enable `atlas-source-mode' when the buffer is in an atlas project."
  (when (and buffer-file-name (atlas-source--root))
    (atlas-source-mode 1)))

(defun atlas-source-setup ()
  "Enable `atlas-source-mode' in Clojure buffers of atlas projects, now and later."
  (interactive)
  (dolist (h '(clojure-mode-hook clojurec-mode-hook clojurescript-mode-hook
               clojure-ts-mode-hook clojure-ts-clojurec-mode-hook))
    (add-hook h #'atlas-source-maybe-enable))
  (add-hook 'cider-connected-hook #'atlas-source--on-cider-connected)
  (dolist (b (buffer-list))
    (with-current-buffer b
      (when (derived-mode-p 'clojure-mode 'clojure-ts-mode)
        (atlas-source-maybe-enable)))))

(provide 'atlas-source)
;;; atlas-source.el ends here
