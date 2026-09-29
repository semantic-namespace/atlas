;;; atlas-review.el --- File-by-file review of a branch, with an LLM's notes -*- lexical-binding: t -*-

;; Copyright (C) 2026
;; Author: @tangrammer + LLMs

;;; Commentary:
;;
;; A review session for one branch checked out in its own worktree:
;;
;;   notebook  an org file, one heading per changed file (TODO/DONE), holding
;;             the LLM's notes (what changed, atlas impact, suggestions) and the
;;             reviewer's own; each heading links to its file with an
;;             `atlas-review:' link.
;;   index     the notebook folded to one line per file ("review notes" tab,
;;             C-c r o); RET on a file opens it in the review tab.
;;   review    a tab per file: magit's diff of that file against the review
;;             base (including uncommitted edits); RET on any diff line opens
;;             the editable file at that line, C-c r b comes back to the diff,
;;             refreshed. Below, the file's notebook section (an indirect
;;             buffer, so edits land in the notebook).
;;
;; An LLM starts it with `atlas-review/start' (via emacsclient --eval); the
;; human moves with the keys of `atlas-review-mode':
;;   RET (in the diff)   edit the file at that line
;;   C-c r b             back to the diff (refreshed)
;;   C-c r s             magit status of the worktree: stage hunks (s), commit (c c)
;;   C-c r r             reset the review layout (diff | notes) for the current file
;;   s / u (in the diff) stage / unstage the hunk or region (squash review)
;;   C-c r t             toggle the diff between unstaged and staged changes
;;   RET (on a file heading in the index)   review that file
;;   C-c r n / C-c r p   next / previous file (notebook order)
;;   C-c r d             toggle the file's TODO/DONE
;;   C-c r g             refresh the diff
;;   C-c r o             back to the whole notebook

;;; Code:

(require 'org)
(require 'atlas-layout)

(defvar atlas-review-worktree nil "Worktree being reviewed (absolute, with trailing slash).")
(defvar atlas-review-base nil "Git revision the review diffs against (usually the merge base).")
(defvar atlas-review-notebook nil "Absolute path of the review notebook (org).")
(defvar atlas-review--current nil "Relative path of the file shown in the review tab.")

(defvar atlas-review-mode-map (make-sparse-keymap)
  "Keymap for `atlas-review-mode'.")

;; Bound outside the defvar so reloading this file updates an existing map.
(dolist (binding '(("C-c r n" . atlas-review/next)
                   ("C-c r p" . atlas-review/previous)
                   ("C-c r d" . atlas-review/toggle-done)
                   ("C-c r g" . atlas-review/refresh-diff)
                   ("C-c r o" . atlas-review/notebook)
                   ("C-c r b" . atlas-review/back-to-diff)
                   ("C-c r s" . atlas-review/stage)
                   ("C-c r r" . atlas-review/reset)
                   ("C-c r t" . atlas-review/toggle-staged)))
  (define-key atlas-review-mode-map (kbd (car binding)) (cdr binding)))

(defvar atlas-review-index-mode-map (make-sparse-keymap)
  "Keymap for the review index: RET on a file heading opens it.")
(define-key atlas-review-index-mode-map (kbd "RET") #'atlas-review/open-at-point)

(define-minor-mode atlas-review-index-mode
  "RET on a file heading of the review notebook opens that file for review."
  :keymap atlas-review-index-mode-map)

(defun atlas-review/open-at-point ()
  "Open the file of the notebook heading at point; elsewhere, a normal RET."
  (interactive)
  (let ((file (and (org-at-heading-p) (org-entry-get nil "REVIEW_FILE"))))
    (cond (file (atlas-review/open file))
          ((org-at-heading-p) (org-cycle))
          (t (call-interactively (or (lookup-key org-mode-map (kbd "RET")) #'newline))))))

(defun atlas-review--show-index (file)
  "Show the notebook folded to one line per file, point on FILE's heading."
  (switch-to-buffer (atlas-review--notebook-buffer))
  (atlas-review-index-mode 1)
  (hl-line-mode 1)
  (widen)
  (org-overview)
  (if (atlas-review--goto-heading file)
      (progn (beginning-of-line) (recenter 3))
    (goto-char (point-min))))

(defvar atlas-review-diff-mode-map (make-sparse-keymap)
  "Keymap for the review diff: RET edits the file at point; s / u stage and unstage.")
(define-key atlas-review-diff-mode-map (kbd "RET") #'atlas-review/visit)
(define-key atlas-review-diff-mode-map (kbd "s") #'atlas-review/stage-at-point)
(define-key atlas-review-diff-mode-map (kbd "u") #'atlas-review/unstage-at-point)

(defun atlas-review--new-file-p (file)
  "Non-nil when FILE is new in the review (intent-to-add or staged as added)."
  (let ((default-directory atlas-review-worktree))
    (or (member file (magit-git-lines "diff" "--name-only" "--diff-filter=A" "--" file))
        (member file (magit-git-lines "diff" "--cached" "--name-only" "--diff-filter=A" "--" file)))))

(defun atlas-review/stage-at-point ()
  "Stage the hunk or region at point.
New files can only be staged whole (git can't apply part of an intent-to-add
file), so on a new file this stages the file."
  (interactive)
  (let ((file (magit-file-at-point)))
    (if (and file (atlas-review--new-file-p file))
        (let ((default-directory atlas-review-worktree))
          (magit-run-git "add" "--" file)
          (message "%s is a new file: staged it whole" file))
      (call-interactively #'magit-stage))))

(defun atlas-review/unstage-at-point ()
  "Unstage the hunk or region at point; a new file is unstaged whole and kept
intent-to-add, so it stays in the review instead of becoming untracked."
  (interactive)
  (let ((file (magit-file-at-point)))
    (if (and file (atlas-review--new-file-p file))
        (let ((default-directory atlas-review-worktree))
          (magit-run-git "reset" "-q" "--" file)
          (magit-run-git "add" "-N" "--" file)
          (message "%s is a new file: unstaged it whole" file))
      (call-interactively #'magit-unstage))))

(define-minor-mode atlas-review-diff-mode
  "RET in a review diff visits the working-tree file at that line."
  :keymap atlas-review-diff-mode-map)

(define-minor-mode atlas-review-mode
  "Review keys in buffers of an atlas review session."
  :lighter " Review"
  :keymap atlas-review-mode-map)

;;; Notebook

(defun atlas-review--notebook-buffer ()
  "The notebook's buffer, visiting it if needed."
  (let ((buf (find-file-noselect atlas-review-notebook)))
    (with-current-buffer buf
      (atlas-review-mode 1)
      (unless atlas-review-reading-mode (atlas-review-reading-mode 1)))
    buf))

(defun atlas-review--files ()
  "Files of the review, in notebook order (from each heading's REVIEW_FILE property;
  FILE is reserved by org and always means the notebook itself)."
  (with-current-buffer (atlas-review--notebook-buffer)
    (org-with-wide-buffer
     (let (files)
       (org-map-entries (lambda () (when-let* ((f (org-entry-get nil "REVIEW_FILE"))) (push f files)))
                        "LEVEL=1")
       (nreverse files)))))

(defun atlas-review--goto-heading (file)
  "Move point to FILE's heading in the notebook buffer; return non-nil if found."
  (goto-char (point-min))
  (let ((found nil))
    (org-map-entries (lambda () (when (and (not found) (equal (org-entry-get nil "REVIEW_FILE") file))
                                  (setq found (point))))
                     "LEVEL=1")
    (when found (goto-char found))))

(defun atlas-review--note-buffer (file)
  "Indirect notebook buffer narrowed to FILE's section (edits go to the notebook)."
  (let* ((base (atlas-review--notebook-buffer))
         (name "*atlas-review: notes*"))
    (when (get-buffer name) (kill-buffer name))
    (let ((ind (make-indirect-buffer base name t)))
      (with-current-buffer ind
        (widen)
        (if (atlas-review--goto-heading file)
            (progn (org-narrow-to-subtree) (org-fold-show-subtree))
          (goto-char (point-min)))
        ;; the clone inherits the index's modes; the notes pane is for writing
        (atlas-review-index-mode -1)
        (hl-line-mode -1)
        (atlas-review-mode 1))
      ind)))

;;; Diff

(defvar atlas-review--show-staged nil
  "Non-nil when the review diff shows the file's staged changes instead of the unstaged ones.")

(defun atlas-review--squash-p ()
  "Non-nil when HEAD is the review base: every change is in the index or working tree,
so the diff can be magit's own unstaged/staged view and `s'/`u' stage hunk by hunk."
  (let ((default-directory atlas-review-worktree))
    (equal (magit-rev-parse "HEAD") (magit-rev-parse atlas-review-base))))

(defun atlas-review--diff-buffer (file)
  "Magit diff of FILE for the review, fully expanded.
When HEAD is the base (squash review): the unstaged changes, or the staged ones
with `atlas-review--show-staged', so staged hunks leave the view.  Otherwise the
working tree against `atlas-review-base'."
  (let* ((default-directory atlas-review-worktree)
         (display-buffer-overriding-action '(display-buffer-same-window))
         ;; after default-directory: magit reads its settings from the repository.
         ;; magit-diff-arguments returns (ARGS FILES); only the args are ours to reuse
         (args (car (magit-diff-arguments 'magit-diff-mode))))
    (cond ((not (atlas-review--squash-p))
           (magit-diff-setup-buffer atlas-review-base nil args (list file)))
          (atlas-review--show-staged
           (magit-diff-setup-buffer nil "--cached" args (list file)))
          (t
           (magit-diff-setup-buffer nil nil args (list file))))
    (atlas-review-mode 1)
    (atlas-review-diff-mode 1)
    (magit-section-show-level-4-all)
    (goto-char (point-min))
    (current-buffer)))

(defun atlas-review/toggle-staged ()
  "Show the current file's staged changes (unstage with `u'), or back to unstaged."
  (interactive)
  (setq atlas-review--show-staged (not atlas-review--show-staged))
  (atlas-review/open atlas-review--current)
  (message "Review diff: %s changes of %s" (if atlas-review--show-staged "STAGED" "unstaged")
           atlas-review--current))

(defvar-local atlas-review--diff-buffer nil
  "In a file visited from a review diff, the diff buffer to come back to.")

(defun atlas-review/visit ()
  "Open the working-tree file at the diff line under point, ready to edit.
Uses magit's worktree visit, so it is always the real file, never a blob."
  (interactive)
  (let ((diff (current-buffer)))
    (call-interactively #'magit-diff-visit-worktree-file)
    (atlas-review-mode 1)
    (setq atlas-review--diff-buffer diff)
    (add-hook 'after-save-hook #'atlas-review/refresh-diff nil t)))

(defun atlas-review/stage ()
  "Magit status of the review worktree in this window, to stage hunks and commit.
C-c r b returns to the review diff."
  (interactive)
  (let ((display-buffer-overriding-action '(display-buffer-same-window)))
    (magit-status-setup-buffer atlas-review-worktree))
  (atlas-review-mode 1))

(defun atlas-review/back-to-diff ()
  "Return from the file to its review diff, refreshed."
  (interactive)
  (let ((diff (or atlas-review--diff-buffer
                  (seq-find (lambda (b) (with-current-buffer b
                                          (bound-and-true-p atlas-review-diff-mode)))
                            (buffer-list)))))
    (if (not (buffer-live-p diff))
        (message "No review diff to go back to")
      (switch-to-buffer diff)
      (magit-refresh))))

(defun atlas-review/refresh-diff ()
  "Refresh the review diff (it also refreshes on every save of the file)."
  (interactive)
  (let ((diff (or (seq-find (lambda (w) (with-current-buffer (window-buffer w)
                                          (bound-and-true-p atlas-review-diff-mode)))
                            (window-list))
                  atlas-review--diff-buffer)))
    (cond ((windowp diff) (with-selected-window diff (magit-refresh)))
          ((buffer-live-p diff) (with-current-buffer diff (magit-refresh))))))

;;; Layout

;;;###autoload
(defun atlas-review/start (worktree base notebook)
  "Start reviewing WORKTREE against BASE, with the org NOTEBOOK.
Opens the notebook in its own tab; follow a file's link, or press C-c r n."
  (setq atlas-review-worktree (file-name-as-directory (expand-file-name worktree))
        atlas-review-base base
        atlas-review-notebook (expand-file-name notebook))
  (atlas-review/notebook)
  (let ((files (length (atlas-review--files))))
    (if (zerop files)
        (format "review of %s against %s: a notebook of sections (diff: links open the hunks)" atlas-review-worktree base)
      (format "review of %s against %s: %d files" atlas-review-worktree base files))))

(defun atlas-review/notebook ()
  "Show the review index: the notebook, one line per file, in the \"review notes\" tab.
RET on a file opens its diff and notes in the \"review\" tab."
  (interactive)
  (atlas-layout--with-layout "review notes"
    (delete-other-windows)
    (atlas-review--show-index (or atlas-review--current ""))))

;;;###autoload
(defun atlas-review/open (file)
  "Review FILE (relative to the worktree) in the \"review\" tab."
  (interactive (list (completing-read "File: " (atlas-review--files) nil t)))
  (setq atlas-review--current file)
  (atlas-layout--with-layout "review"
    (delete-other-windows)
    (let* ((diff (selected-window))
           (notes (split-window-below (/ (* (window-height) 7) 10))))
      (with-selected-window diff (atlas-review--diff-buffer file))
      (with-selected-window notes (switch-to-buffer (atlas-review--note-buffer file)))
      (select-window diff)))
  (format "reviewing %s" file))

(defun atlas-review/reset ()
  "Redraw the review layout for the file being reviewed.
When called from a file that is part of the review, review that file instead."
  (interactive)
  (let* ((here (and buffer-file-name atlas-review-worktree
                    (string-prefix-p atlas-review-worktree buffer-file-name)
                    (file-relative-name buffer-file-name atlas-review-worktree)))
         (file (if (member here (atlas-review--files)) here atlas-review--current)))
    (if file
        (atlas-review/open file)
      (message "No review in progress"))))

(defun atlas-review--step (delta)
  "Open the file DELTA steps from the current one in notebook order."
  (let* ((files (atlas-review--files))
         (i (or (seq-position files atlas-review--current) -1))
         (j (+ i delta)))
    (if (and (>= j 0) (< j (length files)))
        (atlas-review/open (nth j files))
      (message "No %s file" (if (> delta 0) "next" "previous")))))

(defun atlas-review/next () "Review the next file." (interactive) (atlas-review--step 1))
(defun atlas-review/previous () "Review the previous file." (interactive) (atlas-review--step -1))

(defun atlas-review/toggle-done ()
  "Toggle the current file's heading between TODO and DONE, and save the notebook.
In a notebook of sections rather than files, toggle the heading at point."
  (interactive)
  (if (and (null atlas-review--current) (derived-mode-p 'org-mode) (org-at-heading-p))
      (progn (org-todo (if (equal (org-get-todo-state) "DONE") "TODO" "DONE")) (save-buffer))
  (with-current-buffer (atlas-review--notebook-buffer)
    (org-with-wide-buffer
     (when (atlas-review--goto-heading atlas-review--current)
       (org-todo (if (equal (org-get-todo-state) "DONE") "TODO" "DONE"))))
    (save-buffer))
  (message "%s: %s" atlas-review--current
           (with-current-buffer (atlas-review--notebook-buffer)
             (org-with-wide-buffer (atlas-review--goto-heading atlas-review--current)
                                   (org-get-todo-state))))))

;;; Links: [[atlas-review:path/to/file.clj][open]]

(org-link-set-parameters "atlas-review" :follow #'atlas-review/open)

;;; Reading mode: the notebook as text to read

(defface atlas-review-label-what '((t :inherit font-lock-keyword-face :weight bold)) "The What: label.")
(defface atlas-review-label-look '((t :inherit warning :weight bold)) "The Look at: label.")
(defface atlas-review-label-atlas '((t :inherit font-lock-type-face :weight bold)) "The Atlas: label.")
(defface atlas-review-label-checked '((t :inherit success)) "The Checked, not a bug: label.")
(defface atlas-review-label-nit '((t :inherit shadow)) "The Nit: label.")
(defface atlas-review-src-code '((t :inherit font-lock-constant-face)) "The ‹code› tag.")
(defface atlas-review-src-atlas '((t :inherit font-lock-type-face :weight bold)) "The ‹atlas› tag.")
(defface atlas-review-src-repl '((t :inherit font-lock-function-name-face)) "The ‹repl› tag.")
(defface atlas-review-src-inferred '((t :inherit shadow :slant italic)) "The ‹inferred› tag.")

(defconst atlas-review--reading-keywords
  '(("^[ \t]*- \\(What:\\)" 1 'atlas-review-label-what prepend)
    ("^[ \t]*- \\(Look at:\\)" 1 'atlas-review-label-look prepend)
    ("^[ \t]*- \\(Atlas:\\)" 1 'atlas-review-label-atlas prepend)
    ("^[ \t]*- \\(Checked, not a bug:\\)" 1 'atlas-review-label-checked prepend)
    ("^[ \t]*- \\(Nit:\\)\\(.*\\)" (1 'atlas-review-label-nit prepend) (2 'shadow prepend))
    ("‹code›" 0 'atlas-review-src-code prepend)
    ("‹atlas›" 0 'atlas-review-src-atlas prepend)
    ("‹repl›" 0 'atlas-review-src-repl prepend)
    ("‹inferred›" 0 'atlas-review-src-inferred prepend)
    ("‹judgement›" 0 'atlas-review-src-inferred prepend)
    ("^\\*+ Before merging" 0 'warning prepend))
  "Font-lock for the labels and the source tags of a review notebook.")

(defun atlas-review--reading-overlays ()
  "Hide the [[atlas-review:…][open]] lines (RET on the heading opens the file) and
show each folded :EVIDENCE: drawer as one dim line, so the reader knows there is
something to open."
  (remove-overlays (point-min) (point-max) 'atlas-review-hidden t)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\[\\[atlas-review:[^]]*\\]\\[open\\]\\]\n" nil t)
      (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
        (overlay-put ov 'invisible t)
        (overlay-put ov 'atlas-review-hidden t)))
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*:EVIDENCE:[ \t]*$" nil t)
      (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
        (overlay-put ov 'atlas-review-hidden t)
        (overlay-put ov 'display (propertize "  ▸ evidence (TAB)" 'face 'shadow))))))

(define-minor-mode atlas-review-reading-mode
  "Show a review notebook as text to read: org markup and drawers hidden, labels
and source tags coloured, prose indented and wrapped, a folded :EVIDENCE: drawer
shown as one line.  Toggle off to see the raw org."
  :lighter " Read"
  (if atlas-review-reading-mode
      (progn
        (setq-local org-hide-emphasis-markers t
                    org-pretty-entities t
                    org-link-descriptive t)
        (font-lock-add-keywords nil atlas-review--reading-keywords 'append)
        (org-indent-mode 1)
        (visual-line-mode 1)
        (org-fold-hide-drawer-all)
        (atlas-review--reading-overlays)
        (add-hook 'after-revert-hook #'atlas-review--reading-overlays nil t))
    (kill-local-variable 'org-hide-emphasis-markers)
    (kill-local-variable 'org-pretty-entities)
    (font-lock-remove-keywords nil atlas-review--reading-keywords)
    (org-indent-mode -1)
    (visual-line-mode -1)
    (remove-overlays (point-min) (point-max) 'atlas-review-hidden t)
    (remove-hook 'after-revert-hook #'atlas-review--reading-overlays t))
  (font-lock-flush))

;;; Links: [[diff:path/to/file.clj::LINE][…]] — the review diff, point on the hunk holding LINE

(defun atlas-review--open-diff-at (spec)
  "Show the review diff of the file in SPEC (\"file::line\"), point on the hunk
that contains that line of the branch's version; the first hunk when none does."
  (let* ((parts (split-string spec "::"))
         (file (car parts))
         (line (string-to-number (or (cadr parts) "1")))
         (buf (atlas-review--diff-buffer file)))
    (pop-to-buffer buf)
    (goto-char (point-min))
    (let ((best nil))
      (while (re-search-forward "^@@ -[0-9,]+ \\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@" nil t)
        (let* ((start (string-to-number (match-string 1)))
               (len (if (match-string 2) (string-to-number (match-string 2)) 1)))
          (when (and (<= start line) (< line (+ start (max len 1))))
            (setq best (match-beginning 0)))))
      (goto-char (or best (point-min)))
      (recenter 2))))

(org-link-set-parameters "diff" :follow #'atlas-review--open-diff-at)

;;; Points: the human asks for "2.2", the LLM (or C-c r .) shows what backs it

(defun atlas-review--index-file ()
  "The notebook's index sidecar: <notebook>.index.org."
  (and atlas-review-notebook (concat (file-name-sans-extension atlas-review-notebook) ".index.org")))

(defun atlas-review/point (id)
  "Show what backs point ID (a heading of the index sidecar, e.g. \"2.2\"): its
evidence lines in the lower window, and the first diff: or file: link followed in
the upper one.  Returns the evidence text, for the LLM's own reading."
  (interactive "sPoint: ")
  (let ((index (atlas-review--index-file)))
    (unless (and index (file-exists-p index)) (user-error "No index sidecar for this notebook"))
    (let* ((buf (find-file-noselect index))
           (text (with-current-buffer buf
                   (org-with-wide-buffer
                    (goto-char (point-min))
                    ;; "2.2" is one heading; "2" is every heading 2.1, 2.2, … in order
                    (let ((re (concat "^\\* \\(" (regexp-quote id) "\\(?:\\.[0-9]+\\)*\\)[ \t]*$"))
                          (parts nil))
                      (while (re-search-forward re nil t)
                        (let ((name (match-string 1))
                              (body (buffer-substring-no-properties (line-beginning-position 2) (org-end-of-subtree t t))))
                          (push (if (equal name id) body (concat "** " name "\n" body)) parts)))
                      (unless parts
                        (user-error "No point %s in %s (points: %s)" id index
                                    (let (ids) (goto-char (point-min))
                                         (while (re-search-forward "^\\* \\([0-9.]+\\)[ \t]*$" nil t) (push (match-string 1) ids))
                                         (mapconcat #'identity (nreverse ids) " "))))
                      (mapconcat #'identity (nreverse parts) ""))))))
      (atlas-layout--with-layout "review"
        (delete-other-windows)
        (let* ((upper (selected-window))
               (lower (split-window-below (/ (* (window-height) 7) 10)))
               (ev (get-buffer-create (format "*atlas-review: point %s*" id))))
          (with-current-buffer ev
            (let ((inhibit-read-only t))
              (erase-buffer) (org-mode) (insert "* " id "\n" text)
              (atlas-review-mode 1) (atlas-review-reading-mode 1)
              (goto-char (point-min))))
          (set-window-buffer lower ev)
          (select-window upper)
          (with-current-buffer ev
            (goto-char (point-min))
            (when (re-search-forward "\\[\\[\\(diff\\|file\\):\\([^]]*\\)\\]" nil t)
              (let ((type (match-string 1)) (target (match-string 2)))
                (if (equal type "diff")
                    (atlas-review--open-diff-at target)
                  (let ((parts (split-string target "::")))
                    (find-file (car parts))
                    (when (cadr parts) (goto-char (point-min)) (forward-line (1- (string-to-number (cadr parts)))))
                    (atlas-review-mode 1))))))
          (select-window upper)))
      text)))

(define-key atlas-review-mode-map (kbd "C-c r .") #'atlas-review/point)

(defun atlas-review/open-points ()
  "The notebook's points that are not DONE, with anything the human wrote under
them: lines starting with \">\".  Returns ((id state notes) …), for the LLM to
answer only what is still open."
  (interactive)
  (let (out)
    (with-current-buffer (atlas-review--notebook-buffer)
      (org-with-wide-buffer
       (org-map-entries
        (lambda ()
          (let* ((title (substring-no-properties (org-get-heading t t t t)))
                 (id (and (string-match "^\\([0-9]+\\(?:\\.[0-9]+\\)*\\)\\.?[ \t]" title) (match-string 1 title)))
                 (state (org-get-todo-state)))
            (when (and id (not (equal state "DONE")))
              (let ((end (save-excursion (org-end-of-subtree t t)))
                    notes)
                (forward-line 1)
                (while (< (point) end)
                  (when (looking-at "^[ \t]*>[ \t]?\\(.*\\)$")
                    (push (match-string-no-properties 1) notes))
                  (forward-line 1))
                (push (list id (or state "TODO") (nreverse notes)) out)))))
        nil nil)))
    (setq out (nreverse out))
    (when (called-interactively-p 'any)
      (message "%d open point(s): %s" (length out) (mapconcat #'car out " ")))
    out))


(provide 'atlas-review)
;;; atlas-review.el ends here
