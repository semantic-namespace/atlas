;;; atlas-completion.el --- Completion helpers for Atlas IDE support -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; Author: @tangrammer + LLMs

;;; Commentary:
;;
;; Completion helpers for Atlas Emacs integration:
;; - Entity completion with type annotations
;; - Aspect completion with usage counts
;; - Data key completion with producer/consumer info

;;; Code:

(require 'atlas-core)

;;; Cached Data Retrieval

(defun atlas--get-entities-with-metadata ()
  "Get all entities with their type for completion (cached)."
  (if (atlas--cache-valid-p)
      atlas--entity-cache
    (let ((result (atlas--eval "(list-all-entities)")))
      (setq atlas--cache-time (float-time)
            atlas--entity-cache (or result [])))))

(defun atlas--get-aspects-with-counts ()
  "Get all aspects with usage counts (cached)."
  (if (atlas--cache-valid-p)
      atlas--aspect-cache
    (setq atlas--cache-time (float-time)
          atlas--aspect-cache
          (atlas--eval-safe "(list-aspects)" []))))

;;; Completing Read Functions

(defun atlas--completing-read-entity (prompt)
  "Read entity with completion and type annotations using PROMPT."
  (let* ((metadata (atlas--get-entities-with-metadata))
         (metadata-list (atlas--to-list metadata))
         (candidates (mapcar (lambda (e)
                               (let* ((dev-id (atlas--get e 'entity/dev-id))
                                      (dev-id-str (if (symbolp dev-id) (symbol-name dev-id) dev-id)))
                                 (if (string-prefix-p ":" dev-id-str)
                                     (substring dev-id-str 1)
                                   dev-id-str)))
                             metadata-list))
         (annotate-fn (lambda (candidate)
                        (when-let* ((entity (seq-find
                                             (lambda (e)
                                               (let* ((dev-id (atlas--get e 'entity/dev-id))
                                                      (dev-id-str (if (symbolp dev-id)
                                                                      (symbol-name dev-id)
                                                                    dev-id)))
                                                 (string= (if (string-prefix-p ":" dev-id-str)
                                                              (substring dev-id-str 1)
                                                            dev-id-str)
                                                          candidate)))
                                             metadata-list))
                                    (type (atlas--get entity 'entity/type)))
                          (propertize (format " [%s]" type)
                                      'face 'atlas-annotation-face))))
         (completion-extra-properties `(:annotation-function ,annotate-fn)))
    (completing-read prompt candidates nil t)))

(defun atlas--completing-read-aspect (prompt)
  "Read aspect with completion and entity count annotations using PROMPT."
  (let* ((aspects-data (atlas--get-aspects-with-counts))
         (aspects-list (atlas--to-list aspects-data))
         (candidates (mapcar (lambda (a)
                               (let ((aspect (or (atlas--get a 'aspect/aspect)
                                                 (atlas--get a 'aspect))))
                                 (if (symbolp aspect) (symbol-name aspect) aspect)))
                            aspects-list))
         (annotate-fn (lambda (candidate)
                        (when-let* ((data (seq-find
                                           (lambda (a)
                                             (let ((aspect (or (atlas--get a 'aspect/aspect)
                                                               (atlas--get a 'aspect))))
                                               (string= (if (symbolp aspect)
                                                            (symbol-name aspect)
                                                          aspect)
                                                        candidate)))
                                           aspects-list))
                                    (count (or (atlas--get data 'aspect/count)
                                               (atlas--get data 'count))))
                          (propertize (format " (%d entities)" count)
                                      'face 'atlas-annotation-face))))
        (completion-extra-properties `(:annotation-function ,annotate-fn)))
    (completing-read prompt candidates nil t)))

(defun atlas--completing-read-data-key (prompt)
  "Read data key with completion and producer/consumer info using PROMPT."
  (let* ((keys-raw (atlas--eval-safe "(complete-data-key \"\")" []))
         (keys (atlas--to-list keys-raw))
         (annotate-fn (lambda (candidate)
                        (let ((kw (atlas--to-keyword candidate)))
                          (let ((producers (or (atlas--eval-safe
                                                (format "(count (producers-of %s))" kw)) 0))
                                (consumers (or (atlas--eval-safe
                                                (format "(count (consumers-of %s))" kw)) 0)))
                            (propertize (format " [%d producers, %d consumers]"
                                                producers consumers)
                                        'face 'atlas-annotation-face)))))
         (completion-extra-properties `(:annotation-function ,annotate-fn)))
    (completing-read prompt keys nil t)))

(provide 'atlas-completion)
;;; atlas-completion.el ends here
