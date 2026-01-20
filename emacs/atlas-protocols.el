;;; atlas-protocols.el --- Protocol commands for Atlas IDE support -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; Author: @tangrammer + LLMs

;;; Commentary:
;;
;; Protocol commands for Atlas Emacs integration:
;; - List protocols
;; - Protocol info
;; - Find components implementing protocols

;;; Code:

(require 'atlas-core)
(require 'atlas-display)
(require 'atlas-completion)

;;;###autoload
(defun atlas-protocols-list ()
  "List all registered protocols."
  (interactive)
  (let* ((protocols (atlas--eval-safe "(list-protocols)" []))
         (buf (atlas--buffer "protocols")))
    (with-current-buffer buf
      (setq atlas--last-command #'atlas-protocols-list)
      (atlas--insert-header "Registered Protocols")
      (let ((protocols-list (atlas--to-list protocols)))
        (if (and protocols-list (> (length protocols-list) 0))
            (dolist (proto protocols-list)
              (let ((proto-id (or (atlas--get proto 'protocol/id) (atlas--get proto 'id)))
                    (functions (atlas--to-list (or (atlas--get proto 'protocol/functions)
                                                    (atlas--get proto 'functions)))))
                (insert "  ")
                (atlas--insert-entity proto-id)
                (insert "\n")
                (when functions
                  (dolist (fn functions)
                    (insert "    ")
                    (atlas--insert-data-key fn)
                    (insert "\n")))
                (insert "\n")))
          (insert "  (no protocols found)\n")))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun atlas-protocols-info (protocol-id)
  "Show detailed information about PROTOCOL-ID."
  (interactive
   (list (atlas--completing-read-entity "Protocol: ")))
  (let* ((protocol-kw (atlas--to-keyword protocol-id))
         (info (atlas--eval-safe (format "(protocol-info %s)" protocol-kw)))
         (buf (atlas--buffer (format "protocol:%s" protocol-id))))
    (with-current-buffer buf
      (setq atlas--last-command (lambda () (atlas-protocols-info protocol-id)))
      (atlas--insert-header (format "Protocol: %s" protocol-id))
      (if (not info)
          (insert "  (no info available)\n")
        (atlas--insert-subheader "Required Functions")
        (let ((functions (atlas--to-list (or (atlas--get info 'protocol/functions)
                                              (atlas--get info 'functions)))))
          (if (and functions (> (length functions) 0))
              (dolist (fn functions)
                (insert "  ")
                (atlas--insert-data-key fn)
                (insert "\n"))
            (insert "  (none)\n")))
        (insert "\n")
        (atlas--insert-subheader "Implementers")
        (let ((implementers (atlas--to-list (or (atlas--get info 'protocol/implementers)
                                                 (atlas--get info 'implementers)))))
          (if (and implementers (> (length implementers) 0))
              (dolist (impl implementers)
                (insert "  ")
                (atlas--insert-entity impl)
                (insert "\n"))
            (insert "  (no components implement this protocol)\n"))))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun atlas-protocols-of-component (component-id)
  "Show all protocols implemented by COMPONENT-ID."
  (interactive
   (list (atlas--completing-read-entity "Component: ")))
  (let* ((component-kw (atlas--to-keyword component-id))
         (protocols (atlas--eval-safe (format "(component-protocols %s)" component-kw) []))
         (buf (atlas--buffer (format "component-protocols:%s" component-id))))
    (with-current-buffer buf
      (setq atlas--last-command (lambda () (atlas-protocols-of-component component-id)))
      (atlas--insert-header (format "Protocols implemented by %s" component-id))
      (let ((protocols-list (atlas--to-list protocols)))
        (if (and protocols-list (> (length protocols-list) 0))
            (dolist (proto protocols-list)
              (insert "  ")
              (atlas--insert-aspect proto)
              (insert "\n"))
          (insert "  (no protocols)\n")))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun atlas-protocols-components-implementing (protocol-id)
  "Find all components implementing PROTOCOL-ID."
  (interactive
   (list (atlas--completing-read-entity "Protocol: ")))
  (let* ((protocol-kw (atlas--to-keyword protocol-id))
         (components (atlas--eval-safe (format "(components-implementing %s)" protocol-kw) []))
         (buf (atlas--buffer (format "implementing:%s" protocol-id))))
    (with-current-buffer buf
      (setq atlas--last-command (lambda () (atlas-protocols-components-implementing protocol-id)))
      (atlas--insert-header (format "Components implementing %s" protocol-id))
      (let ((components-list (atlas--to-list components)))
        (if (and components-list (> (length components-list) 0))
            (dolist (comp components-list)
              (insert "  ")
              (atlas--insert-entity comp)
              (insert "\n"))
          (insert "  (no components)\n")))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

(provide 'atlas-protocols)
;;; atlas-protocols.el ends here
