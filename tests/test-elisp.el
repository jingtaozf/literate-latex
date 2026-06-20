;;; test-elisp.el --- E2E for the Emacs Lisp direct-load + tangle paths  -*- lexical-binding: t; -*-
;;
;; Run from the repo root:
;;   emacs -batch -Q -L . -l tests/test-elisp.el -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'literate-latex)

(defvar llt-tests-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory of this test file (.../tests/).")
(defvar llt-repo-dir
  (file-name-directory (directory-file-name llt-tests-dir))
  "Repository root.")

(defun llt-fixture (name)
  (expand-file-name (concat "fixtures/" name) llt-tests-dir))

;;; --- header parsing units ------------------------------------------------

(ert-deftest llt-parse-begin ()
  (should (equal (literate-latex--parse-begin "\\begin{chunk}{foo}")
                 '("foo")))
  (should (equal (literate-latex--parse-begin "  \\begin{chunk}{foo bar}")
                 '("foo bar")))
  (should (equal (literate-latex--parse-begin "\\begin{chunk}[load=no]{x}")
                 '("x" (load . "no"))))
  (should (equal (literate-latex--parse-begin "\\begin{chunk}[lang=elisp,load=test]{h}")
                 '("h" (lang . "elisp") (load . "test"))))
  (should-not (literate-latex--parse-begin "ordinary prose"))
  (should-not (literate-latex--parse-begin "\\section{not a chunk}")))

(ert-deftest llt-loadable-p ()
  (let ((literate-latex-language "elisp")
        (literate-latex-language-aliases '("elisp" "emacs-lisp"))
        (literate-latex-load-test-chunks nil))
    (should (literate-latex--loadable-p nil))
    (should (literate-latex--loadable-p '((load . "yes"))))
    (should-not (literate-latex--loadable-p '((load . "no"))))
    (should-not (literate-latex--loadable-p '((load . "test"))))
    (should (literate-latex--loadable-p '((lang . "emacs-lisp"))))
    (should-not (literate-latex--loadable-p '((lang . "python"))))
    (let ((literate-latex-load-test-chunks t))
      (should (literate-latex--loadable-p '((load . "test")))))))

;;; --- direct load (no tangle) ---------------------------------------------

(ert-deftest llt-direct-load ()
  (fmakunbound 'literate-latex-test-greet)
  (fmakunbound 'literate-latex-test-add)
  (fmakunbound 'this)                   ; would be bound if scratch leaked
  ;; Loading must not error even though the load=no chunk is invalid elisp.
  (literate-latex-load (llt-fixture "el-demo.pamphlet"))
  (should (fboundp 'literate-latex-test-greet))
  (should (fboundp 'literate-latex-test-add))
  (should (equal (literate-latex-test-greet "world") "Hello, world!"))
  (should (= (literate-latex-test-add 2 3) 5))
  (should-not (fboundp 'this)))         ; load=no chunk skipped

(ert-deftest llt-lexical-binding-honoured ()
  "The pamphlet modeline's `lexical-binding: t' must take effect."
  ;; The hook needs Emacs's lexical-binding default override (Emacs 30+).
  (skip-unless (boundp 'internal--get-default-lexical-binding-function))
  (fmakunbound 'literate-latex-test-make-adder)
  (literate-latex-load (llt-fixture "el-demo.pamphlet"))
  (should (fboundp 'literate-latex-test-make-adder))
  ;; A captured closure only returns 7 under lexical binding.
  (should (= (funcall (literate-latex-test-make-adder 3) 4) 7)))

;;; --- optional reorder (tangle) -------------------------------------------

(ert-deftest llt-tangle-reorder ()
  (let ((out (make-temp-file "llt-tangle" nil ".el")))
    (unwind-protect
        (progn
          (literate-latex-tangle (llt-fixture "tangle-demo.pamphlet") out)
          (should (equal (with-temp-buffer (insert-file-contents out)
                                           (buffer-string))
                         "(list\n  'b\n  'a\n  )\n")))
      (ignore-errors (delete-file out)))))

;;; --- noweb named-chunk semantics -----------------------------------------

(ert-deftest llt-noweb-concat-nest-reuse ()
  "Concatenation, multi-level nesting, reuse and compounding indentation."
  (let ((out (make-temp-file "llt-noweb" nil ".txt")))
    (unwind-protect
        (progn
          (literate-latex-tangle (llt-fixture "noweb.pamphlet") out)
          (should (equal (with-temp-buffer (insert-file-contents out)
                                           (buffer-string))
                         (concat "header\n"
                                 "a-begin\n  common-1\n  common-2\na-end\n"
                                 "common-1\ncommon-2\n"
                                 "middle\n"
                                 "a-begin\n  common-1\n  common-2\na-end\n"
                                 "footer\n"))))
      (ignore-errors (delete-file out)))))

(ert-deftest llt-noweb-cycle-errors ()
  (let ((out (make-temp-file "llt-cycle" nil ".txt")))
    (unwind-protect
        (should-error (literate-latex-tangle (llt-fixture "noweb-cycle.pamphlet") out))
      (ignore-errors (delete-file out)))))

(ert-deftest llt-noweb-undefined-errors ()
  (let ((out (make-temp-file "llt-undef" nil ".txt")))
    (unwind-protect
        (should-error (literate-latex-tangle (llt-fixture "noweb-undefined.pamphlet") out))
      (ignore-errors (delete-file out)))))

;;; --- editing major mode --------------------------------------------------

(ert-deftest llt-major-mode ()
  "`literate-latex-mode' activates with chunk imenu, keys and navigation."
  (with-temp-buffer
    (insert-file-contents (llt-fixture "cl-demo.pamphlet"))
    (literate-latex-mode)
    (should (eq major-mode 'literate-latex-mode))
    (should (derived-mode-p 'latex-mode))
    (let ((idx (literate-latex--imenu-index)))
      (should (assoc "greet" idx))
      (should (assoc "add" idx))
      (should (assoc "scratch" idx)))
    (should (eq (lookup-key literate-latex-mode-map (kbd "C-c C-j"))
                'literate-latex-goto-chunk))
    (should (eq (lookup-key literate-latex-mode-map (kbd "C-c C-c"))
                'literate-latex-compile))
    (goto-char (point-min))
    (literate-latex-goto-chunk "add")
    (should (looking-at "[ \t]*\\\\begin{chunk}{add}"))))

;;; --- typeset to PDF ------------------------------------------------------

(ert-deftest llt-compile-pdf ()
  (skip-unless (executable-find "pdflatex"))
  (let* ((tex (expand-file-name "tex" llt-repo-dir))
         (fixtures (expand-file-name "fixtures" llt-tests-dir))
         (build (expand-file-name "build" llt-tests-dir))
         (pdf (expand-file-name "cl-demo.pdf" build))
         (process-environment
          (cons (format "TEXINPUTS=%s:%s:" tex fixtures) process-environment)))
    (make-directory build t)
    (ignore-errors (delete-file pdf))
    (call-process "pdflatex" nil (get-buffer-create "*llt-pdflatex*") nil
                  "-interaction=nonstopmode" "-halt-on-error"
                  "-output-directory" build
                  (llt-fixture "cl-demo.pamphlet"))
    (should (file-exists-p pdf))))

(provide 'test-elisp)
;;; test-elisp.el ends here
