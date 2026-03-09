;;; atlas-recent.el --- Git-based recency suggestions for Atlas -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; Author: @tangrammer + LLMs

;;; Commentary:
;;
;; Suggests dev-ids and aspects based on recent git changes to register! calls.
;; Uses atlas.source-tracker on the Clojure side to intersect git diffs with
;; register! form line ranges.
;;
;; All calls go through (atlas.registry.lookup/handle-tool ...) — same path
;; as MCP and REPL.
;;
;; Commands:
;;   atlas-recent-entities    — browse recently touched entities
;;   atlas-recent-aspects     — browse aspects from recent work
;;   atlas-recent-insert-entity — insert a recently touched dev-id
;;   atlas-recent-insert-aspect — insert a recently used aspect
;;   atlas-recent-jump        — jump to a recently touched entity's source

;;; Code:

(require 'atlas-core)
(require 'atlas-display)

;;; Cache

(defvar atlas-recent--cache nil
  "Cached recent entities data.")

(defvar atlas-recent--cache-time nil
  "When the recent cache was last populated.")

(defvar atlas-recent--cache-ttl 30
  "Cache TTL in seconds for recent data.")

(defvar atlas-recent-commit-count 10
  "Number of recent commits to scan for suggestions.")

(defun atlas-recent--cache-valid-p ()
  "Check if the recent cache is still valid."
  (and atlas-recent--cache-time
       (< (- (float-time) atlas-recent--cache-time) atlas-recent--cache-ttl)))

(defun atlas-recent--invalidate ()
  "Invalidate the recent cache."
  (interactive)
  (setq atlas-recent--cache nil
        atlas-recent--cache-time nil)
  (message "Atlas recent cache invalidated"))

;;; Clojure-side queries (via handle-tool)

(defun atlas-recent--ensure-loaded ()
  "Ensure lookup alias is available in the current REPL namespace."
  (atlas--eval-safe "(alias 'lookup (find-ns 'atlas.registry.lookup))"))

(defun atlas-recent--call-tool (tool-name args-str)
  "Call a source-tracker tool via handle-tool and return the :tool/result."
  (atlas--eval-safe
   (format "(:tool/result (lookup/handle-tool {:tool/name %s :tool/args %s}))"
           tool-name args-str)))

(defun atlas-recent--fetch-entities ()
  "Fetch recently touched entities from source-tracker."
  (if (atlas-recent--cache-valid-p)
      atlas-recent--cache
    (atlas-recent--ensure-loaded)
    (let ((result (atlas-recent--call-tool
                   ":atlas.source-tracker/recent-entities"
                   (format "{:query/count %d}" atlas-recent-commit-count))))
      (setq atlas-recent--cache result
            atlas-recent--cache-time (float-time))
      result)))

(defun atlas-recent--fetch-aspects ()
  "Fetch aspects from recently touched entities."
  (atlas-recent--ensure-loaded)
  (atlas-recent--call-tool
   ":atlas.source-tracker/recent-aspects"
   (format "{:query/count %d}" atlas-recent-commit-count)))

(defun atlas-recent--fetch-context ()
  "Fetch combined recent context (entities + aspects + files)."
  (atlas-recent--ensure-loaded)
  (atlas-recent--call-tool
   ":atlas.source-tracker/recent-context"
   (format "{:query/count %d}" atlas-recent-commit-count)))

;;; Browse Commands

;;;###autoload
(defun atlas-recent-entities ()
  "Browse recently touched entities (from git history).
Shows dev-ids modified in the last N commits with commit info."
  (interactive)
  (let* ((entities (atlas-recent--fetch-entities))
         (items (atlas--to-list entities))
         (buf (atlas--buffer "recent:entities")))
    (with-current-buffer buf
      (setq atlas--last-command #'atlas-recent-entities)
      (atlas--insert-header
       (format "Recently Touched Entities (last %d commits)" atlas-recent-commit-count))
      (if (and items (> (length items) 0))
          (dolist (entry items)
            (let ((dev-id  (atlas--get entry :dev-id))
                  (file    (atlas--get entry :file))
                  (message (atlas--get entry :message))
                  (date    (atlas--get entry :date)))
              (insert "  ")
              (atlas--insert-entity dev-id)
              (insert "\n")
              (when message
                (insert (propertize
                         (format "    %s  %s\n"
                                 (if date
                                     (substring (atlas--to-string date) 0
                                                (min 10 (length (atlas--to-string date))))
                                   "")
                                 (atlas--to-string message))
                         'face 'font-lock-comment-face)))
              (when file
                (insert (propertize
                         (format "    %s\n" (atlas--to-string file))
                         'face 'font-lock-doc-face)))))
        (insert "  (no recently touched entities found)\n"))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun atlas-recent-aspects ()
  "Browse aspects from recently touched entities, ranked by frequency."
  (interactive)
  (let* ((aspects (atlas-recent--fetch-aspects))
         (items (atlas--to-list aspects))
         (buf (atlas--buffer "recent:aspects")))
    (with-current-buffer buf
      (setq atlas--last-command #'atlas-recent-aspects)
      (atlas--insert-header
       (format "Recent Aspects (from last %d commits)" atlas-recent-commit-count))
      (if (and items (> (length items) 0))
          (dolist (entry items)
            (let ((aspect  (atlas--get entry :aspect))
                  (count   (atlas--get entry :count))
                  (dev-ids (atlas--get entry :dev-ids)))
              (insert "  ")
              (atlas--insert-aspect aspect)
              (insert (propertize
                       (format "  (%d entities)" (or count 0))
                       'face 'atlas-annotation-face))
              (insert "\n")
              (when dev-ids
                (insert (propertize
                         (format "    %s\n"
                                 (mapconcat (lambda (id) (atlas--to-string id))
                                            (atlas--to-list dev-ids) ", "))
                         'face 'font-lock-comment-face)))))
        (insert "  (no recent aspects found)\n"))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;; Insert Commands (for authoring)

;;;###autoload
(defun atlas-recent-insert-entity ()
  "Insert a recently touched dev-id at point.
Offers completion from entities modified in recent commits."
  (interactive)
  (let* ((entities (atlas-recent--fetch-entities))
         (items (atlas--to-list entities))
         (candidates
          (mapcar (lambda (entry)
                    (let* ((dev-id (atlas--get entry :dev-id))
                           (dev-id-str (atlas--to-string dev-id))
                           (message (atlas--get entry :message)))
                      (cons dev-id-str
                            (when message (atlas--to-string message)))))
                  items))
         (annotate-fn (lambda (candidate)
                        (when-let* ((info (cdr (assoc candidate candidates))))
                          (propertize (format "  %s" info)
                                     'face 'atlas-annotation-face))))
         (completion-extra-properties `(:annotation-function ,annotate-fn))
         (chosen (completing-read "Recent entity: "
                                  (mapcar #'car candidates) nil nil)))
    (when chosen
      (insert (if (string-prefix-p ":" chosen) chosen (concat ":" chosen))))))

;;;###autoload
(defun atlas-recent-insert-aspect ()
  "Insert a recently used aspect at point.
Offers completion from aspects of recently modified entities, ranked by frequency."
  (interactive)
  (let* ((aspects (atlas-recent--fetch-aspects))
         (items (atlas--to-list aspects))
         (candidates
          (mapcar (lambda (entry)
                    (let ((aspect (atlas--to-string (atlas--get entry :aspect)))
                          (count  (atlas--get entry :count)))
                      (cons aspect (or count 0))))
                  items))
         (annotate-fn (lambda (candidate)
                        (when-let* ((count (cdr (assoc candidate candidates))))
                          (propertize (format "  (%d recent entities)" count)
                                     'face 'atlas-annotation-face))))
         (completion-extra-properties `(:annotation-function ,annotate-fn))
         (chosen (completing-read "Recent aspect: "
                                  (mapcar #'car candidates) nil nil)))
    (when chosen
      (insert (if (string-prefix-p ":" chosen) chosen (concat ":" chosen))))))

;;; Jump Command

;;;###autoload
(defun atlas-recent-jump ()
  "Jump to a recently touched entity's source file.
Offers completion from entities modified in recent commits."
  (interactive)
  (let* ((entities (atlas-recent--fetch-entities))
         (items (atlas--to-list entities))
         (candidates
          (mapcar (lambda (entry)
                    (let ((dev-id (atlas--to-string (atlas--get entry :dev-id)))
                          (file   (atlas--to-string (atlas--get entry :file))))
                      (cons dev-id file)))
                  items))
         (chosen (completing-read "Jump to recent entity: "
                                  (mapcar #'car candidates) nil t))
         (file (cdr (assoc chosen candidates))))
    (when file
      ;; Find the file and search for the dev-id
      (let ((dev-id-kw (if (string-prefix-p ":" chosen) chosen (concat ":" chosen))))
        (find-file file)
        (goto-char (point-min))
        (when (search-forward dev-id-kw nil t)
          (beginning-of-line)
          (recenter))))))

;;; Commit Summary Command

;;;###autoload
(defun atlas-recent-commit-entities (commit-ref)
  "Show which entity definitions were touched in COMMIT-REF."
  (interactive
   (list (read-string "Commit (default HEAD): " nil nil "HEAD")))
  (atlas-recent--ensure-loaded)
  (let* ((summary (atlas-recent--call-tool
                   ":atlas.source-tracker/commit-entities"
                   (format "{:git/commit-ref \"%s\"}" commit-ref)))
         (buf (atlas--buffer (format "recent:commit:%s" commit-ref))))
    (with-current-buffer buf
      (setq atlas--last-command (lambda () (atlas-recent-commit-entities commit-ref)))
      (atlas--insert-header
       (format "Entities in commit: %s" commit-ref))
      (if (not summary)
          (insert "  (could not analyze commit)\n")
        (let ((message (atlas--get summary :message))
              (entities (atlas--to-list (atlas--get summary :entities)))
              (by-file (atlas--get summary :by-file)))
          (when message
            (insert (propertize (format "  %s\n\n" (atlas--to-string message))
                                'face 'font-lock-comment-face)))
          (insert (format "  %d entities touched\n\n"
                          (or (atlas--get summary :entity-count) 0)))
          (if entities
              (dolist (dev-id entities)
                (insert "  ")
                (atlas--insert-entity dev-id)
                (insert "\n"))
            (insert "  (no entity definitions changed)\n"))))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;; Staged Changes Commands

;;;###autoload
(defun atlas-recent-staged-entities ()
  "Show which entity definitions are touched in currently staged changes.
Displays the list of dev-ids affected by `git diff --cached`."
  (interactive)
  (atlas-recent--ensure-loaded)
  (let* ((summary (atlas-recent--call-tool
                   ":atlas.source-tracker/staged-entities" "{}"))
         (buf (atlas--buffer "recent:staged")))
    (with-current-buffer buf
      (setq atlas--last-command #'atlas-recent-staged-entities)
      (atlas--insert-header "Entities in Staged Changes")
      (if (not summary)
          (insert "  (could not analyze staged changes)\n")
        (let ((entities (atlas--to-list (atlas--get summary :entities)))
              (count (or (atlas--get summary :entity-count) 0)))
          (insert (format "  %d entities touched\n\n" count))
          (if entities
              (dolist (dev-id entities)
                (insert "  ")
                (atlas--insert-entity dev-id)
                (insert "\n"))
            (insert "  (no entity definitions in staged changes)\n"))))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun atlas-recent-insert-staged-entities ()
  "Insert the list of dev-ids changed in staged files at point.
Useful for commit messages or documentation."
  (interactive)
  (atlas-recent--ensure-loaded)
  (let* ((summary (atlas-recent--call-tool
                   ":atlas.source-tracker/staged-entities" "{}"))
         (entities (when summary
                     (atlas--to-list (atlas--get summary :entities)))))
    (if (and entities (> (length entities) 0))
        (let ((dev-ids (mapcar (lambda (e)
                                 (let ((s (atlas--to-string e)))
                                   (if (string-prefix-p ":" s) s (concat ":" s))))
                               entities)))
          (insert (mapconcat #'identity dev-ids "\n"))
          (message "Inserted %d staged dev-ids" (length dev-ids)))
      (message "No entity definitions in staged changes"))))

(provide 'atlas-recent)
;;; atlas-recent.el ends here
