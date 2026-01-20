;;; atlas-lsp.el --- LSP integration for Atlas IDE support -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; Author: @tangrammer + LLMs

;;; Commentary:
;;
;; LSP helpers integration for Atlas Emacs integration:
;; - Hover info at point
;; - Find usages in source files
;; - Export registry to JSON
;; - Write search index

;;; Code:

(require 'atlas-core)
(require 'atlas-display)
(require 'atlas-completion)

;;;###autoload
(defun atlas-lsp-hover-at-point ()
  "Show semantic hover info for keyword at point.
Uses lsp-helpers/hover-info for markdown-formatted display."
  (interactive)
  (let* ((sym (thing-at-point 'symbol t))
         (kw (if (and sym (string-prefix-p ":" sym)) sym (concat ":" sym)))
         (code (format "(do (require '[%s :as lsp]) (lsp/hover-info %s))"
                       atlas-lsp-helpers-ns kw))
         (result (cider-nrepl-sync-request:eval code))
         (value (nrepl-dict-get result "value")))
    (if (and value (not (string= value "nil")))
        (let ((buf (atlas--buffer (format "hover:%s" sym))))
          (with-current-buffer buf
            (insert (read value))
            (goto-char (point-min))
            (when (fboundp 'markdown-mode)
              (markdown-mode))
            (read-only-mode 1))
          (display-buffer buf))
      (message "No semantic info for %s" sym))))

;;;###autoload
(defun atlas-lsp-find-usages (dev-id)
  "Find all usages of DEV-ID in source files.
Uses lsp-helpers/find-dev-id-usages to search src/ and test/ directories."
  (interactive (list (atlas--completing-read-entity "Entity: ")))
  (let* ((kw (atlas--to-keyword dev-id))
         (code (format "(do (require '[%s :as lsp]) (vec (lsp/find-dev-id-usages %s)))"
                       atlas-lsp-helpers-ns kw))
         (usages (atlas--eval code))
         (buf (atlas--buffer (format "usages:%s" dev-id))))
    (with-current-buffer buf
      (setq atlas--last-command (lambda () (atlas-lsp-find-usages dev-id)))
      (atlas--insert-header (format "Usages of %s" dev-id))
      (let ((usages-list (atlas--to-list usages)))
        (if (and usages-list (> (length usages-list) 0))
            (dolist (usage usages-list)
              (let ((file (atlas--get usage 'file))
                    (line (atlas--get usage 'line))
                    (content (atlas--get usage 'content)))
                (insert "  ")
                (insert-text-button (format "%s:%d" file line)
                                    'face 'link
                                    'action (lambda (_)
                                              (find-file file)
                                              (goto-char (point-min))
                                              (forward-line (1- line)))
                                    'follow-link t)
                (insert "\n")
                (insert (format "    %s\n\n" content))))
          (insert "  (no usages found)\n")))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun atlas-lsp-export ()
  "Export registry to JSON files for external tools.
Writes to .clj-kondo/.cache/semantic-registry.json"
  (interactive)
  (let* ((code "(do (require '[atlas.tooling.lsp-export :as exp]) (exp/export-now!))")
         (result (atlas--eval code)))
    (if result
        (message "Exported %d entities to %s"
                 (atlas--get result 'exported-identities)
                 (car (atlas--to-list (atlas--get result 'files))))
      (message "Export failed"))))

;;;###autoload
(defun atlas-lsp-write-search-index ()
  "Write EDN search index for external tools.
Writes to .clj-kondo/.cache/semantic-search-index.edn"
  (interactive)
  (let* ((code (format "(do (require '[%s :as lsp]) (lsp/write-search-index))"
                       atlas-lsp-helpers-ns))
         (result (atlas--eval code)))
    (if result
        (message "Wrote %d entries to %s"
                 (atlas--get result 'entries)
                 (atlas--get result 'file))
      (message "Write failed"))))

(provide 'atlas-lsp)
;;; atlas-lsp.el ends here
