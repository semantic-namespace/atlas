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
    (if file
        (atlas-review/open file)
      (call-interactively (or (lookup-key org-mode-map (kbd "RET")) #'newline)))))

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
    (with-current-buffer buf (atlas-review-mode 1))
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
  (format "review of %s against %s: %d files" atlas-review-worktree base
          (length (atlas-review--files))))

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
  "Toggle the current file's heading between TODO and DONE, and save the notebook."
  (interactive)
  (with-current-buffer (atlas-review--notebook-buffer)
    (org-with-wide-buffer
     (when (atlas-review--goto-heading atlas-review--current)
       (org-todo (if (equal (org-get-todo-state) "DONE") "TODO" "DONE"))))
    (save-buffer))
  (message "%s: %s" atlas-review--current
           (with-current-buffer (atlas-review--notebook-buffer)
             (org-with-wide-buffer (atlas-review--goto-heading atlas-review--current)
                                   (org-get-todo-state)))))

;;; Links: [[atlas-review:path/to/file.clj][open]]

(org-link-set-parameters "atlas-review" :follow #'atlas-review/open)

(provide 'atlas-review)
;;; atlas-review.el ends here
