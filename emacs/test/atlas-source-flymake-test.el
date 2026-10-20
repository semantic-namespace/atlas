;;; atlas-source-flymake-test.el --- atlas-source.el's Flymake backend against a real `bb source serve' -*- lexical-binding: t -*-

;; Run from the atlas checkout:
;;   emacs --batch -Q -l emacs/test/atlas-source-flymake-test.el -f ert-run-tests-batch-and-exit
;; or `bb emacs-smoke', which also runs atlas-source-test.el.  Needs Emacs
;; 27+ and bb; no clojure-mode.  The fixture is test/fixtures/source-packs.

(require 'ert)
(defconst atlas-source-flymake-test--checkout
  (expand-file-name "../.." (file-name-directory (or load-file-name buffer-file-name))))
(load (expand-file-name "emacs/atlas-source.el" atlas-source-flymake-test--checkout) nil t)
(setq atlas-source-bb-config (expand-file-name "bb.edn" atlas-source-flymake-test--checkout))

(defun atlas-source-flymake-test--fixture (file)
  (expand-file-name (concat "test/fixtures/source-packs/" file) atlas-source-flymake-test--checkout))

(defun atlas-source-flymake-test--report (file &optional edit)
  "What the backend reports for FILE: (:called DIAGNOSTICS) or :not-called.
With EDIT, the buffer has an unsaved change first."
  (with-current-buffer (find-file-noselect (atlas-source-flymake-test--fixture file))
    (when edit (goto-char (point-max)) (insert " "))
    (let ((result :not-called)
          (deadline (+ (float-time) 120)))
      (atlas-source-flymake (lambda (diags &rest _) (setq result (list :called diags))))
      (unless edit
        (while (and (eq result :not-called) (< (float-time) deadline))
          (accept-process-output nil 0.1)))
      (when edit (set-buffer-modified-p nil) (revert-buffer t t))
      result)))

(defun atlas-source-flymake-test--marked (diag)
  "The text DIAG covers, its type and its message."
  (with-current-buffer (flymake-diagnostic-buffer diag)
    (list (buffer-substring-no-properties (flymake-diagnostic-beg diag) (flymake-diagnostic-end diag))
          (flymake-diagnostic-type diag)
          (flymake-diagnostic-text diag))))

(ert-deftest atlas-source-flymake-code ()
  "A key code uses outside its function's contract, underlined in the code."
  (let ((r (atlas-source-flymake-test--report "src/shop/handlers.clj")))
    (should (eq (car r) :called))
    (should (equal (mapcar #'atlas-source-flymake-test--marked (cadr r))
                   '((":order/discount" :note
                      "atlas: :order/discount is used here but is not in :fn/place-order's contract"))))))

(ert-deftest atlas-source-flymake-config ()
  "In an integrant config: the ref no config defines, and the id two packs declare."
  (let ((marked (mapcar #'atlas-source-flymake-test--marked (cadr (atlas-source-flymake-test--report "resources/system.edn")))))
    (should (member '(":shop/metrics" :warning "atlas: no config defines :shop/metrics") marked))
    (should (cl-some (lambda (m) (and (equal (car m) ":shop.resolvers/seed") (eq (cadr m) :warning))) marked))))

(ert-deftest atlas-source-flymake-destructured ()
  "A key only destructured is underlined at its name in the form."
  (let ((marked (mapcar #'atlas-source-flymake-test--marked (cadr (atlas-source-flymake-test--report "src/shop/resolvers.clj")))))
    (should (member '("lines" :note "atlas: nothing produces :order/lines") marked))
    (should (member '("loyalty-tier" :note "atlas: no schema shapes :user/loyalty-tier") marked))))

(ert-deftest atlas-source-flymake-unsaved ()
  "With unsaved changes the backend doesn't answer: the server reads the disk."
  (should (eq (atlas-source-flymake-test--report "src/shop/handlers.clj" t) :not-called)))

(ert-deftest atlas-source-flymake-mode ()
  "atlas-source-mode turns Flymake on with the backend, and Flymake shows its markers."
  (with-current-buffer (find-file-noselect (atlas-source-flymake-test--fixture "src/shop/spec.clj"))
    (atlas-source-mode 1)
    (should flymake-mode)
    (should (memq #'atlas-source-flymake flymake-diagnostic-functions))
    (flymake-start)
    (let ((deadline (+ (float-time) 120)))
      (while (and (null (flymake-diagnostics)) (< (float-time) deadline))
        (accept-process-output nil 0.1)))
    (should (equal (mapcar #'atlas-source-flymake-test--marked (flymake-diagnostics))
                   '((":user/emial" :note "atlas: :user/emial has no spec of its own"))))))

;;; atlas-source-flymake-test.el ends here
