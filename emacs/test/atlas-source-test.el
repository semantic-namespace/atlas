;;; atlas-source-test.el --- Batch test of atlas-source.el against the cart example -*- lexical-binding: t -*-
;;
;; Run from the atlas checkout, with clojure-mode on the load path:
;;   emacs --batch -Q -L <dir with clojure-mode.el> -l emacs/test/atlas-source-test.el
;;
;; Starts a real `bb source serve' and drives eldoc, xref and the describe
;; buffer on the cart example's files.

(require 'clojure-mode)
(defvar atlas-root (expand-file-name "../../" (file-name-directory load-file-name)))
(setq default-directory atlas-root)
(add-to-list 'auto-mode-alist '("\\.edn\\'" . clojure-mode))
(load (expand-file-name "emacs/atlas-source.el" atlas-root))
;; this checkout's code, even when an atlas-source command is installed
(setq atlas-source-command (list "bb" "--config" (expand-file-name "bb.edn" atlas-root) "source"))
(defvar failures 0)
(defun check (what ok) (princ (format "%s %s\n" (if ok "ok  " "FAIL") what)) (unless ok (setq failures (1+ failures))))
(defun at (file needle)
  (find-file (expand-file-name file atlas-root)) (goto-char (point-min)) (search-forward needle) (backward-char 2) (atlas-source-mode 1))
(defun eldoc-at ()
  (let ((got 'none) (t0 (float-time)))
    (atlas-source-eldoc (lambda (s &rest _) (setq got s)))
    (while (and (eq got 'none) (< (- (float-time) t0) 5)) (accept-process-output nil 0.02))
    (list got (- (float-time) t0))))

;; in the defn metadata
(at "test/app/cart_impl.clj" ":fn/add-to-cart}")
(let ((r (eldoc-at)))
  (princ (format "  eldoc (first, starts server): %.0fms\n  %s\n" (* 1000 (cadr r)) (replace-regexp-in-string "\n" "\n  " (car r))))
  (check "eldoc on the defn's metadata keyword describes the entity" (string-prefix-p ":fn/add-to-cart  execution-function" (car r))))
(let ((r (eldoc-at)))
  (princ (format "  eldoc (warm): %.0fms\n" (* 1000 (cadr r))))
  (check "warm eldoc answers in well under 100ms" (< (cadr r) 0.1)))
(let ((defs (xref-backend-definitions 'atlas-source ":fn/add-to-cart")))
  (check "M-. offers the EDN entry and the defn"
         (equal (mapcar (lambda (x) (let ((l (xref-item-location x))) (cons (file-name-nondirectory (xref-file-location-file l)) (xref-file-location-line l)))) defs)
                '(("functions.edn" . 34) ("cart_impl.clj" . 39)))))
(check "the xref backend claims known keywords" (eq 'atlas-source (atlas-source--xref-backend)))

;; in an EDN file, on a dep reference
(at "test/app/cart_registry/functions.edn" ":fn/get-product}")
(check "eldoc in EDN on a dep" (string-prefix-p ":fn/get-product" (car (eldoc-at))))
(check "M-? lists who uses it"
       (equal (sort (mapcar (lambda (x) (xref-item-summary x)) (xref-backend-references 'atlas-source ":fn/get-product")) #'string<)
              '(":endpoint/cart-graph-api via :interface-endpoint/deps" ":fn/add-to-cart via :execution-function/deps")))

;; a data key and an aspect
(at "test/app/cart_registry/functions.edn" ":cart/subtotal]")
(check "eldoc on a data key" (string-match-p "data key" (car (eldoc-at))))
(at "test/app/cart_registry/functions.edn" ":compliance/pii")
(check "eldoc on an aspect" (string-match-p "aspect of 4 entities" (car (eldoc-at))))

;; a keyword atlas doesn't know falls through
(with-temp-buffer
  (clojure-mode) (setq default-directory atlas-root) (insert "(:foo/bar m)") (goto-char 4) (atlas-source-mode 1)
  (check "unknown keyword: eldoc gives nothing" (null (car (eldoc-at))))
  (check "unknown keyword: xref falls through" (null (atlas-source--xref-backend))))

;; describe buffer
(at "test/app/cart_impl.clj" ":fn/merge-carts}")
(atlas-source-describe)
(with-current-buffer "*atlas*"
  (princ (concat "  --- *atlas* ---\n  " (replace-regexp-in-string "\n" "\n  " (buffer-string)) "\n"))
  (goto-char (point-min)) (search-forward "functions.edn:")
  (check "file:line is a button" (button-at (point)))
  (push-button (point)))
(with-current-buffer (window-buffer (selected-window))
  (check "the button opens the entry at its line" (and (string-suffix-p "functions.edn" buffer-file-name) (looking-at " :fn/merge-carts"))))

(atlas-source-stop)
(princ (if (zerop failures) "OK\n" (format "%d failed\n" failures)))
(kill-emacs (if (zerop failures) 0 1))
