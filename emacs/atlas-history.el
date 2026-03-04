;;; atlas-history.el --- History browsing for Atlas IDE support -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; Author: @tangrammer + LLMs

;;; Commentary:
;;
;; History browsing commands for Atlas Emacs integration:
;; - Initialize and query the history database
;; - Browse entity timelines across versions
;; - Compare versions (snap diffs, vocabulary, edges)
;; - Transient menu for history navigation

;;; Code:

(require 'atlas-core)
(require 'atlas-display)
(require 'atlas-completion)

;;; Completion helpers

(defun atlas--history-completing-read-version (prompt)
  "Read a history version string with completion using PROMPT."
  (let* ((versions-raw (atlas--eval-safe "(history-versions)" []))
         (versions (atlas--to-list versions-raw))
         (candidates (mapcar #'atlas--to-string versions)))
    (completing-read prompt candidates nil t)))

(defun atlas--history-completing-read-two-versions (prompt-old prompt-new)
  "Read two version strings for diff comparisons.
PROMPT-OLD for the older version, PROMPT-NEW for the newer."
  (let* ((versions-raw (atlas--eval-safe "(history-versions)" []))
         (versions (atlas--to-list versions-raw))
         (candidates (mapcar #'atlas--to-string versions))
         (v-old (completing-read prompt-old candidates nil t))
         (v-new (completing-read prompt-new candidates nil t)))
    (list v-old v-new)))

;;; Commands

;;;###autoload
(defun atlas-history-init ()
  "Initialize the history database."
  (interactive)
  (let ((result (atlas--eval-safe "(history-init!)")))
    (if result
        (message "History initialized: %S" result)
      (message "Failed to initialize history"))))

;;;###autoload
(defun atlas-history-versions ()
  "List all snapshotted versions."
  (interactive)
  (let* ((versions (atlas--eval-safe "(history-versions)" []))
         (buf (atlas--buffer "history:versions")))
    (with-current-buffer buf
      (setq atlas--last-command #'atlas-history-versions)
      (atlas--insert-header "History Versions")
      (let ((versions-list (atlas--to-list versions)))
        (if (and versions-list (> (length versions-list) 0))
            (let ((n 1))
              (dolist (v versions-list)
                (insert (format "  %d. %s\n" n (atlas--to-string v)))
                (setq n (1+ n))))
          (insert "  (no versions snapshotted)\n")))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun atlas-history-entity-timeline (entity)
  "Show all versions of ENTITY (single dev-id)."
  (interactive
   (list (or (atlas--keyword-at-point)
             (atlas--to-keyword (atlas--completing-read-entity "Entity: ")))))
  (let* ((entity-kw (atlas--to-keyword entity))
         (timeline (atlas--eval-safe
                    (format "(history-entity-timeline \"%s\")" entity-kw) []))
         (buf (atlas--buffer (format "history:timeline:%s" entity))))
    (with-current-buffer buf
      (setq atlas--last-command (lambda () (atlas-history-entity-timeline entity)))
      (atlas--insert-header (format "Entity Timeline: %s" entity))
      (let ((items (atlas--to-list timeline)))
        (if (and items (> (length items) 0))
            (dolist (snap items)
              (let ((version  (atlas--get snap 'snap/version))
                    (type     (atlas--get snap 'snap/type))
                    (aspects  (atlas--get snap 'snap/aspect))
                    (added    (atlas--get snap 'snap/added))
                    (removed  (atlas--get snap 'snap/removed))
                    (prev-id  (atlas--get snap 'snap/prev-dev-id)))
                (atlas--insert-subheader
                 (format "%s%s"
                         (atlas--to-string version)
                         (if prev-id (format "  (was %s)" (atlas--to-string prev-id)) "")))
                (insert (format "  type: %s\n" (atlas--to-string type)))
                (when aspects
                  (insert "  aspects: ")
                  (let ((first t))
                    (dolist (a (atlas--to-list aspects))
                      (unless first (insert " "))
                      (setq first nil)
                      (atlas--insert-aspect a)))
                  (insert "\n"))
                (when added
                  (insert (propertize "  + " 'face 'atlas-success-face))
                  (let ((first t))
                    (dolist (a (atlas--to-list added))
                      (unless first (insert " "))
                      (setq first nil)
                      (atlas--insert-aspect a)))
                  (insert "\n"))
                (when removed
                  (insert (propertize "  - " 'face 'atlas-error-face))
                  (let ((first t))
                    (dolist (a (atlas--to-list removed))
                      (unless first (insert " "))
                      (setq first nil)
                      (atlas--insert-aspect a)))
                  (insert "\n"))
                (insert "\n")))
          (insert "  (no timeline data)\n")))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun atlas-history-full-timeline (entity)
  "Show complete history of ENTITY across dev-id renames."
  (interactive
   (list (or (atlas--keyword-at-point)
             (atlas--to-keyword (atlas--completing-read-entity "Entity: ")))))
  (let* ((entity-kw (atlas--to-keyword entity))
         (timeline (atlas--eval-safe
                    (format "(history-full-timeline \"%s\")" entity-kw) []))
         (buf (atlas--buffer (format "history:full-timeline:%s" entity))))
    (with-current-buffer buf
      (setq atlas--last-command (lambda () (atlas-history-full-timeline entity)))
      (atlas--insert-header (format "Full Timeline: %s" entity))
      (insert (propertize "Follows :snap/prev-dev-id chain back to original name\n\n"
                          'face 'font-lock-comment-face))
      (let ((items (atlas--to-list timeline)))
        (if (and items (> (length items) 0))
            (dolist (snap items)
              (let ((dev-id   (atlas--get snap 'snap/dev-id))
                    (version  (atlas--get snap 'snap/version))
                    (type     (atlas--get snap 'snap/type))
                    (added    (atlas--get snap 'snap/added))
                    (removed  (atlas--get snap 'snap/removed))
                    (prev-id  (atlas--get snap 'snap/prev-dev-id)))
                (atlas--insert-subheader
                 (format "%s  %s%s"
                         (atlas--to-string version)
                         (atlas--to-string dev-id)
                         (if prev-id (format "  (was %s)" (atlas--to-string prev-id)) "")))
                (insert (format "  type: %s\n" (atlas--to-string type)))
                (when added
                  (insert (propertize "  + " 'face 'atlas-success-face))
                  (let ((first t))
                    (dolist (a (atlas--to-list added))
                      (unless first (insert " "))
                      (setq first nil)
                      (atlas--insert-aspect a)))
                  (insert "\n"))
                (when removed
                  (insert (propertize "  - " 'face 'atlas-error-face))
                  (let ((first t))
                    (dolist (a (atlas--to-list removed))
                      (unless first (insert " "))
                      (setq first nil)
                      (atlas--insert-aspect a)))
                  (insert "\n"))
                (insert "\n")))
          (insert "  (no timeline data)\n")))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun atlas-history-version-diff (version)
  "Show all entities that changed in VERSION."
  (interactive
   (list (atlas--history-completing-read-version "Version: ")))
  (let* ((diffs (atlas--eval-safe
                 (format "(history-version-diff \"%s\")" version) []))
         (buf (atlas--buffer (format "history:diff:%s" version))))
    (with-current-buffer buf
      (setq atlas--last-command (lambda () (atlas-history-version-diff version)))
      (atlas--insert-header (format "Version Diff: %s" version))
      (let ((items (atlas--to-list diffs)))
        (if (and items (> (length items) 0))
            (progn
              (insert (format "  %d entities changed\n\n" (length items)))
              (dolist (snap items)
                (let ((dev-id  (atlas--get snap 'snap/dev-id))
                      (type    (atlas--get snap 'snap/type))
                      (added   (atlas--get snap 'snap/added))
                      (removed (atlas--get snap 'snap/removed))
                      (prev-id (atlas--get snap 'snap/prev-dev-id)))
                  (insert "  ")
                  (atlas--insert-entity dev-id)
                  (when prev-id
                    (insert (format " (was %s)" (atlas--to-string prev-id))))
                  (insert (format "  [%s]\n" (atlas--to-string type)))
                  (when removed
                    (insert (propertize "    - " 'face 'atlas-error-face))
                    (let ((first t))
                      (dolist (a (atlas--to-list removed))
                        (unless first (insert " "))
                        (setq first nil)
                        (atlas--insert-aspect a)))
                    (insert "\n"))
                  (when added
                    (insert (propertize "    + " 'face 'atlas-success-face))
                    (let ((first t))
                      (dolist (a (atlas--to-list added))
                        (unless first (insert " "))
                        (setq first nil)
                        (atlas--insert-aspect a)))
                    (insert "\n")))))
          (insert "  (no changes in this version)\n")))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun atlas-history-version-summary (version)
  "Show aggregate change counts per entity type for VERSION."
  (interactive
   (list (atlas--history-completing-read-version "Version: ")))
  (let* ((summary (atlas--eval-safe
                   (format "(history-version-summary \"%s\")" version) []))
         (buf (atlas--buffer (format "history:summary:%s" version))))
    (with-current-buffer buf
      (setq atlas--last-command (lambda () (atlas-history-version-summary version)))
      (atlas--insert-header (format "Version Summary: %s" version))
      (let ((items (atlas--to-list summary)))
        (if (and items (> (length items) 0))
            (dolist (pair items)
              (let ((pair-list (atlas--to-list pair)))
                (insert (format "  %-40s %d\n"
                                (atlas--to-string (nth 0 pair-list))
                                (nth 1 pair-list)))))
          (insert "  (no changes in this version)\n")))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun atlas-history-vocabulary-diff (v-old v-new)
  "Show aspect vocabulary changes between V-OLD and V-NEW."
  (interactive
   (atlas--history-completing-read-two-versions "Older version: " "Newer version: "))
  (let* ((diff (atlas--eval-safe
                (format "(history-vocabulary-diff \"%s\" \"%s\")" v-old v-new)))
         (buf (atlas--buffer (format "history:vocab:%s→%s" v-old v-new))))
    (with-current-buffer buf
      (setq atlas--last-command (lambda () (atlas-history-vocabulary-diff v-old v-new)))
      (atlas--insert-header (format "Vocabulary Diff: %s → %s" v-old v-new))
      (if (not diff)
          (insert "  (no data)\n")
        (let ((added   (atlas--to-list (atlas--get diff 'added)))
              (removed (atlas--to-list (atlas--get diff 'removed)))
              (grew    (atlas--to-list (atlas--get diff 'grew)))
              (shrank  (atlas--to-list (atlas--get diff 'shrank))))
          (when added
            (atlas--insert-subheader (format "New aspects (%d)" (length added)))
            (dolist (a added)
              (insert "  ")
              (atlas--insert-aspect a)
              (insert "\n"))
            (insert "\n"))
          (when removed
            (atlas--insert-subheader (format "Removed aspects (%d)" (length removed)))
            (dolist (a removed)
              (insert "  ")
              (atlas--insert-aspect a)
              (insert "\n"))
            (insert "\n"))
          (when grew
            (atlas--insert-subheader "Grew")
            (dolist (entry grew)
              (let ((aspect    (atlas--get entry 'aspect))
                    (old-count (atlas--get entry 'old-count))
                    (new-count (atlas--get entry 'new-count)))
                (insert "  ")
                (atlas--insert-aspect aspect)
                (insert (format "  %d → %d\n" old-count new-count))))
            (insert "\n"))
          (when shrank
            (atlas--insert-subheader "Shrank")
            (dolist (entry shrank)
              (let ((aspect    (atlas--get entry 'aspect))
                    (old-count (atlas--get entry 'old-count))
                    (new-count (atlas--get entry 'new-count)))
                (insert "  ")
                (atlas--insert-aspect aspect)
                (insert (format "  %d → %d\n" old-count new-count))))
            (insert "\n"))))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun atlas-history-edges-at (version entity)
  "Show all edges for ENTITY at VERSION."
  (interactive
   (let ((v (atlas--history-completing-read-version "Version: ")))
     (list v (atlas--to-keyword (atlas--completing-read-entity "Entity: ")))))
  (let* ((entity-kw (atlas--to-keyword entity))
         (edges (atlas--eval-safe
                 (format "(history-edges-at \"%s\" \"%s\")" version entity-kw) []))
         (buf (atlas--buffer (format "history:edges:%s@%s" entity version))))
    (with-current-buffer buf
      (setq atlas--last-command (lambda () (atlas-history-edges-at version entity)))
      (atlas--insert-header (format "Edges: %s @ %s" entity version))
      (let ((items (atlas--to-list edges)))
        (if (and items (> (length items) 0))
            (dolist (edge items)
              (let ((target   (atlas--get edge 'edge/target))
                    (verb     (atlas--get edge 'edge/verb))
                    (property (atlas--get edge 'edge/property)))
                (insert "  ")
                (atlas--insert-entity entity)
                (insert (format " —%s→ " (atlas--to-string verb)))
                (atlas--insert-entity target)
                (insert (format "  (%s)\n" (atlas--to-string property)))))
          (insert "  (no edges)\n")))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun atlas-history-dependents-of (version entity)
  "Show entities that reference ENTITY at VERSION (reverse lookup)."
  (interactive
   (let ((v (atlas--history-completing-read-version "Version: ")))
     (list v (atlas--to-keyword (atlas--completing-read-entity "Target entity: ")))))
  (let* ((entity-kw (atlas--to-keyword entity))
         (deps (atlas--eval-safe
                (format "(history-dependents-of \"%s\" \"%s\")" version entity-kw) []))
         (buf (atlas--buffer (format "history:dependents:%s@%s" entity version))))
    (with-current-buffer buf
      (setq atlas--last-command (lambda () (atlas-history-dependents-of version entity)))
      (atlas--insert-header (format "Dependents of %s @ %s" entity version))
      (let ((items (atlas--to-list deps)))
        (if (and items (> (length items) 0))
            (dolist (dep items)
              (let ((source   (atlas--get dep 'edge/source))
                    (verb     (atlas--get dep 'edge/verb))
                    (property (atlas--get dep 'edge/property)))
                (insert "  ")
                (atlas--insert-entity source)
                (insert (format " —%s→  (%s)\n"
                                (atlas--to-string verb)
                                (atlas--to-string property)))))
          (insert "  (nothing references this entity)\n")))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun atlas-history-edge-diff (v-old v-new)
  "Show edges added or removed between V-OLD and V-NEW."
  (interactive
   (atlas--history-completing-read-two-versions "Older version: " "Newer version: "))
  (let* ((diff (atlas--eval-safe
                (format "(history-edge-diff \"%s\" \"%s\")" v-old v-new)))
         (buf (atlas--buffer (format "history:edge-diff:%s→%s" v-old v-new))))
    (with-current-buffer buf
      (setq atlas--last-command (lambda () (atlas-history-edge-diff v-old v-new)))
      (atlas--insert-header (format "Edge Diff: %s → %s" v-old v-new))
      (if (not diff)
          (insert "  (no data)\n")
        (let ((added   (atlas--to-list (atlas--get diff 'added)))
              (removed (atlas--to-list (atlas--get diff 'removed))))
          (when removed
            (atlas--insert-subheader (format "Removed (%d)" (length removed)))
            (dolist (edge removed)
              (let ((source   (atlas--get edge 'edge/source))
                    (target   (atlas--get edge 'edge/target))
                    (verb     (atlas--get edge 'edge/verb))
                    (property (atlas--get edge 'edge/property)))
                (insert "  ")
                (atlas--insert-entity source)
                (insert (format " —%s→ " (atlas--to-string verb)))
                (atlas--insert-entity target)
                (insert (format "  (%s)\n" (atlas--to-string property)))))
            (insert "\n"))
          (when added
            (atlas--insert-subheader (format "Added (%d)" (length added)))
            (dolist (edge added)
              (let ((source   (atlas--get edge 'edge/source))
                    (target   (atlas--get edge 'edge/target))
                    (verb     (atlas--get edge 'edge/verb))
                    (property (atlas--get edge 'edge/property)))
                (insert "  ")
                (atlas--insert-entity source)
                (insert (format " —%s→ " (atlas--to-string verb)))
                (atlas--insert-entity target)
                (insert (format "  (%s)\n" (atlas--to-string property)))))
            (insert "\n"))
          (when (and (null added) (null removed))
            (insert "  No edge changes.\n"))))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun atlas-history-edge-summary (version)
  "Show edge counts by verb at VERSION."
  (interactive
   (list (atlas--history-completing-read-version "Version: ")))
  (let* ((summary (atlas--eval-safe
                   (format "(history-edge-summary \"%s\")" version) []))
         (buf (atlas--buffer (format "history:edge-summary:%s" version))))
    (with-current-buffer buf
      (setq atlas--last-command (lambda () (atlas-history-edge-summary version)))
      (atlas--insert-header (format "Edge Summary: %s" version))
      (let ((items (atlas--to-list summary)))
        (if (and items (> (length items) 0))
            (dolist (pair items)
              (let ((pair-list (atlas--to-list pair)))
                (insert (format "  %-30s %d\n"
                                (atlas--to-string (nth 0 pair-list))
                                (nth 1 pair-list)))))
          (insert "  (no edges at this version)\n")))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;; Transient menu

;;;###autoload
(transient-define-prefix atlas-history ()
  "Atlas History — browse semantic evolution across versions."
  [["Setup"
    ("!" "Initialize history db" atlas-history-init)
    ("v" "List versions" atlas-history-versions)]
   ["Entity History"
    ("t" "Entity timeline" atlas-history-entity-timeline)
    ("T" "Full timeline (across renames)" atlas-history-full-timeline)]
   ["Version Queries"
    ("d" "Version diff" atlas-history-version-diff)
    ("s" "Version summary" atlas-history-version-summary)
    ("V" "Vocabulary diff" atlas-history-vocabulary-diff)]
   ["Edge Queries"
    ("e" "Edges at version" atlas-history-edges-at)
    ("r" "Dependents of (reverse)" atlas-history-dependents-of)
    ("E" "Edge diff" atlas-history-edge-diff)
    ("S" "Edge summary" atlas-history-edge-summary)]
   ["Navigation"
    ("q" "Back to main menu" atlas)]])

(provide 'atlas-history)
;;; atlas-history.el ends here
