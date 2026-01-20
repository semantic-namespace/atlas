;;; atlas-explorer.el --- Explorer v2 with AND/OR filtering -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; Author: @tangrammer + LLMs

;;; Commentary:
;;
;; Explorer v2 - Dual map with AND/OR filtering for Atlas:
;; - Select aspects in AND mode (must have ALL)
;; - Select aspects in OR mode (must have ANY)
;; - Filter entities by combined criteria
;; - Visual aspect browser with namespace grouping

;;; Code:

(require 'atlas-core)
(require 'atlas-display)
(require 'atlas-completion)

(defvar atlas-explorer--aspects-and '()
  "List of aspects in AND mode (must have ALL).")

(defvar atlas-explorer--aspects-or '()
  "List of aspects in OR mode (must have ANY).")

(defun atlas-explorer--clear-selection ()
  "Clear all aspect selections."
  (interactive)
  (setq atlas-explorer--aspects-and '())
  (setq atlas-explorer--aspects-or '())
  (message "Selection cleared"))

(defun atlas-explorer--aspects-by-namespace ()
  "Get aspects grouped by namespace from the registry."
  (let ((aspects (atlas--get-aspects-with-counts)))
    (when aspects
      (let ((aspects-list (if (vectorp aspects) (append aspects nil) aspects))
            (grouped (make-hash-table :test 'equal)))
        (dolist (a aspects-list)
          (let* ((aspect-kw (atlas--to-string
                             (or (atlas--get a 'aspect/aspect)
                                 (atlas--get a 'aspect))))
                 (count (or (atlas--get a 'aspect/count)
                            (atlas--get a 'count) 0))
                 (parts (split-string aspect-kw ":"))
                 (ns-part (if (> (length parts) 1) (nth 1 parts) ""))
                 (ns-parts (split-string ns-part "/"))
                 (ns-name (car ns-parts))
                 (aspect-name (cadr ns-parts)))
            (when (and ns-name aspect-name)
              (let ((existing (gethash ns-name grouped)))
                (puthash ns-name
                         (cons (list aspect-name count aspect-kw) existing)
                         grouped)))))
        grouped))))

(defun atlas-explorer--cycle-aspect (aspect-kw)
  "Cycle ASPECT-KW through: not selected -> AND -> OR -> not selected."
  (let ((in-and (member aspect-kw atlas-explorer--aspects-and))
        (in-or (member aspect-kw atlas-explorer--aspects-or)))
    (cond
     ;; Not selected -> AND
     ((and (not in-and) (not in-or))
      (push aspect-kw atlas-explorer--aspects-and)
      (message "AND: %s" aspect-kw))
     ;; AND -> OR
     (in-and
      (setq atlas-explorer--aspects-and
            (delete aspect-kw atlas-explorer--aspects-and))
      (push aspect-kw atlas-explorer--aspects-or)
      (message "OR: %s" aspect-kw))
     ;; OR -> Not selected
     (in-or
      (setq atlas-explorer--aspects-or
            (delete aspect-kw atlas-explorer--aspects-or))
      (message "Deselected: %s" aspect-kw)))))

(defun atlas-explorer--matching-entities ()
  "Get entities matching current AND/OR selection."
  (when (or atlas-explorer--aspects-and
            atlas-explorer--aspects-or)
    (let* ((and-str (if atlas-explorer--aspects-and
                        (format "#{%s}"
                                (mapconcat #'identity
                                           atlas-explorer--aspects-and " "))
                      "#{}"))
           (or-str (if atlas-explorer--aspects-or
                       (format "#{%s}"
                               (mapconcat #'identity
                                          atlas-explorer--aspects-or " "))
                     "#{}"))
           (query (format "(explorer-filter-entities %s %s)" and-str or-str)))
      (atlas--eval-safe query []))))

;;;###autoload
(defun atlas-explorer-list-aspects ()
  "Show aspects grouped by namespace with selection state, in columns."
  (interactive)
  (let* ((grouped (atlas-explorer--aspects-by-namespace))
         (buf (atlas--buffer "explorer-aspects")))
    (if (not grouped)
        (message "No aspects found")
      (with-current-buffer buf
        (setq atlas--last-command #'atlas-explorer-list-aspects)
        (atlas--insert-header "Aspects by Namespace (click to toggle)")
        (insert "\n")
        (insert (propertize "  Legend: " 'face 'atlas-subheader-face))
        (insert (propertize "[A]" 'face '(:foreground "#4a9eff" :weight bold)))
        (insert "=AND (must have ALL)  ")
        (insert (propertize "[O]" 'face '(:foreground "#4aef7a" :weight bold)))
        (insert "=OR (must have ANY)\n")
        (when (or atlas-explorer--aspects-and atlas-explorer--aspects-or)
          (insert "\n")
          (when atlas-explorer--aspects-and
            (insert (propertize "  AND: " 'face '(:foreground "#4a9eff")))
            (insert (format "%d selected  " (length atlas-explorer--aspects-and))))
          (when atlas-explorer--aspects-or
            (insert (propertize "  OR: " 'face '(:foreground "#4aef7a")))
            (insert (format "%d selected" (length atlas-explorer--aspects-or))))
          (insert "\n"))
        (insert "\n")
        (maphash
         (lambda (ns-name aspects)
           (insert (propertize (format "  %s/ " ns-name)
                               'face 'atlas-subheader-face))
           (let ((sorted-aspects (sort aspects (lambda (a b) (string< (car a) (car b))))))
             (dolist (a sorted-aspects)
               (let* ((aspect-name (nth 0 a))
                      (aspect-kw (nth 2 a))
                      (in-and (member aspect-kw atlas-explorer--aspects-and))
                      (in-or (member aspect-kw atlas-explorer--aspects-or))
                      (prefix (cond
                               (in-and (propertize "[A]" 'face '(:foreground "#4a9eff" :weight bold)))
                               (in-or (propertize "[O]" 'face '(:foreground "#4aef7a" :weight bold)))
                               (t "   "))))
                 (insert prefix)
                 (insert-text-button
                  aspect-name
                  'action (lambda (_btn)
                            (atlas-explorer--cycle-aspect aspect-kw)
                            (atlas-explorer-list-aspects))
                  'follow-link t
                  'face (cond
                         (in-and '(:foreground "#4a9eff" :weight bold))
                         (in-or '(:foreground "#4aef7a" :weight bold))
                         (t 'atlas-aspect-face)))
                 (insert "  "))))
           (insert "\n"))
         grouped)
        (goto-char (point-min))
        (read-only-mode 1))
      (pop-to-buffer buf))))

;;;###autoload
(defun atlas-explorer-show-filtered ()
  "Show entities matching current AND/OR selection."
  (interactive)
  (if (and (null atlas-explorer--aspects-and)
           (null atlas-explorer--aspects-or))
      (message "No aspects selected. Use 'a' to select aspects first.")
    (let* ((entities (atlas-explorer--matching-entities))
           (buf (atlas--buffer "explorer-entities")))
      (with-current-buffer buf
        (setq atlas--last-command #'atlas-explorer-show-filtered)
        (atlas--insert-header "Filtered Entities")
        (insert "\n")
        (when atlas-explorer--aspects-and
          (insert (propertize "  AND: " 'face '(:foreground "#4a9eff")))
          (insert (format "%s\n" (mapconcat #'identity
                                            atlas-explorer--aspects-and ", "))))
        (when atlas-explorer--aspects-or
          (insert (propertize "  OR: " 'face '(:foreground "#4aef7a")))
          (insert (format "%s\n" (mapconcat #'identity
                                            atlas-explorer--aspects-or ", "))))
        (insert "\n")
        (let ((entities-list (atlas--to-list entities)))
          (if (and entities-list (> (length entities-list) 0))
              (let ((by-type (make-hash-table :test 'equal)))
                (dolist (e entities-list)
                  (let* ((dev-id (atlas--to-string
                                  (or (atlas--get e 'entity/dev-id)
                                      (atlas--get e 'dev-id)
                                      e)))
                         (etype (atlas--to-string
                                 (or (atlas--get e 'entity/type)
                                     (atlas--get e 'type)
                                     "unknown"))))
                    (puthash etype (cons dev-id (gethash etype by-type)) by-type)))
                (maphash
                 (lambda (etype dev-ids)
                   (insert (propertize (format "  %s (%d)\n" etype (length dev-ids))
                                       'face 'atlas-subheader-face))
                   (dolist (dev-id (sort dev-ids #'string<))
                     (insert "    ")
                     (atlas--insert-entity dev-id)
                     (insert "\n"))
                   (insert "\n"))
                 by-type))
            (insert "  (no matching entities)\n")))
        (goto-char (point-min))
        (read-only-mode 1))
      (pop-to-buffer buf))))

;;;###autoload
(defun atlas-explorer-select-aspect ()
  "Interactively select an aspect to toggle."
  (interactive)
  (let* ((aspect (atlas--completing-read-aspect "Toggle aspect: "))
         (aspect-kw (atlas--to-keyword aspect)))
    (atlas-explorer--cycle-aspect aspect-kw)
    (atlas-explorer-show-filtered)))

(provide 'atlas-explorer)
;;; atlas-explorer.el ends here
