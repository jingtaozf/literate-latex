;;;; test-cl.lisp --- E2E for the Common Lisp direct-load + tangle paths.
;;;;
;;;; Run from the repo root:  sbcl --script tests/test-cl.lisp
;;;; Exit code is non-zero on any failure.

(let ((root (make-pathname
             :directory (pathname-directory
                         (truename *load-pathname*))
             :name nil :type nil)))
  ;; root = .../tests/ ; the system file is one level up.
  (load (merge-pathnames "../literate-latex.lisp" root)))

(defpackage #:literate-latex-test-runner (:use #:cl #:literate-latex))
(in-package #:literate-latex-test-runner)

(defvar *pass* 0)
(defvar *fail* 0)

(defmacro check (form &optional (desc (format nil "~s" form)))
  `(handler-case
       (if ,form
           (progn (incf *pass*) (format t "  ok   ~a~%" ,desc))
           (progn (incf *fail*) (format t "  FAIL ~a~%" ,desc)))
     (error (e)
       (incf *fail*)
       (format t "  FAIL ~a -- signalled ~a~%" ,desc e))))

(defun fixture (name)
  (merge-pathnames (format nil "fixtures/~a" name)
                   (make-pathname :directory (pathname-directory *load-pathname*))))

(defun build-path (name)
  (let ((p (merge-pathnames (format nil "build/~a" name)
                            (make-pathname :directory (pathname-directory *load-pathname*)))))
    (ensure-directories-exist p)
    p))

(defun slurp (path)
  (with-open-file (s path)
    (let ((buf (make-string (file-length s))))
      (subseq buf 0 (read-sequence buf s)))))

(defun lines (&rest ls)
  "Join LS with newlines, with a trailing newline (matches `tangle' output)."
  (format nil "~{~a~%~}" ls))

(defun tangle-errors-p (pamphlet)
  "T if tangling PAMPHLET signals an error."
  (handler-case (progn (tangle (fixture pamphlet) (build-path "err-out.lisp")) nil)
    (error () t)))

;;; --- direct load ---------------------------------------------------------

(format t "~&direct load (no tangle):~%")
(load-pamphlet (fixture "cl-demo.pamphlet"))

(let ((greet (find-symbol "GREET" :literate-latex-test))
      (add   (find-symbol "ADD"   :literate-latex-test)))
  (check (and greet (fboundp greet)) "greet chunk loaded")
  (check (and add (fboundp add)) "add chunk loaded")
  (check (string= (funcall greet "world") "Hello, world!")
         "greet returns the expected string")
  (check (= (funcall add 2 3) 5) "add computes 2+3=5")
  (check (null (find-symbol "SCRATCH" :literate-latex-test))
         "load=no chunk was skipped")
  (check (null (find-symbol "PY" :literate-latex-test))
         "lang=python chunk was skipped"))

;;; --- optional reorder (tangle) ------------------------------------------

(format t "~&optional reorder (tangle):~%")
(let* ((out (merge-pathnames "build/tangle-out.lisp"
                             (make-pathname :directory (pathname-directory *load-pathname*))))
       (expected (format nil "(list~%  'b~%  'a~%  )~%")))
  (ensure-directories-exist out)
  (tangle (fixture "tangle-demo.pamphlet") out)
  (let ((got (with-open-file (s out) (let ((buf (make-string (file-length s))))
                                       (subseq buf 0 (read-sequence buf s))))))
    (check (string= got expected)
           "tangle reorders helpers and preserves indentation")
    (unless (string= got expected)
      (format t "    expected: ~s~%    got:      ~s~%" expected got))))

;;; --- noweb named-chunk semantics -----------------------------------------

(format t "~&noweb named chunks:~%")
(let ((out (build-path "noweb-out.txt"))
      (expected (lines "header"
                       "a-begin" "  common-1" "  common-2" "a-end" ; nested + indent
                       "common-1" "common-2"                       ; direct ref, concat
                       "middle"
                       "a-begin" "  common-1" "  common-2" "a-end" ; reuse of section-a
                       "footer")))
  (tangle (fixture "noweb.pamphlet") out)
  (let ((got (slurp out)))
    (check (string= got expected)
           "concatenation + multi-level nesting + reuse + compounding indent")
    (unless (string= got expected)
      (format t "    expected:~%~a    got:~%~a" expected got))))

(check (tangle-errors-p "noweb-cycle.pamphlet")
       "cyclic chunk graph signals an error")
(check (tangle-errors-p "noweb-undefined.pamphlet")
       "reference to an undefined chunk signals an error")

;;; --- tangle from TeX (\lltangle) ------------------------------------------
;;;
;;; \lltangle in tex/literate-latex.sty must produce exactly what the CL
;;; `tangle' above produces, so the CL output is the reference.  Each engine
;;; found on PATH is run for real; a missing engine is reported and skipped.

(defparameter *tex-dir*
  (namestring (merge-pathnames "../tex/" (make-pathname :directory (pathname-directory *load-pathname*)))))

(defun run-tex-tangle (engine pamphlet root out-name)
  "Run ENGINE on \\lltangle{PAMPHLET}{ROOT}{OUT-NAME} inside tests/build/.
Return the exit code, or :missing when ENGINE is not installed."
  (let ((build (directory-namestring (build-path "x"))))
    (handler-case
        (sb-ext:process-exit-code
         (sb-ext:run-program
          engine
          (list "-interaction=nonstopmode" "-halt-on-error"
                (format nil "-jobname=~a" (pathname-name out-name))
                (format nil "\\RequirePackage{literate-latex}\\lltangle{~a}{~a}{~a}\\stop"
                        (namestring (fixture pamphlet)) root out-name))
          :search t :directory build :output nil :error nil :input nil
          :environment (cons (format nil "TEXINPUTS=~a:" *tex-dir*) (sb-ext:posix-environ))))
      (error () :missing))))

(defun tex-log (out-name)
  (slurp (build-path (format nil "~a.log" (pathname-name out-name)))))

(format t "~&tangle from TeX (\\lltangle):~%")
(dolist (engine '("pdflatex" "lualatex" "xelatex"))
  (if (eq (run-tex-tangle engine "tangle-demo.pamphlet" "*" "probe.out") :missing)
      (format t "  skip ~a not installed~%" engine)
      (progn
        (dolist (case '(("tangle-demo.pamphlet" "*") ("noweb.pamphlet" "*")
                        ("tex-tangle.pamphlet" "*")))
          (destructuring-bind (pamphlet root) case
            (let* ((stem (format nil "~a-~a" engine (pathname-name pamphlet)))
                   (ref (build-path (format nil "~a.ref" stem)))
                   (out (format nil "~a.out" stem))
                   (code (progn (tangle (fixture pamphlet) ref :root root)
                                (run-tex-tangle engine pamphlet root out))))
              (if (and (string= engine "xelatex") (string= pamphlet "tex-tangle.pamphlet"))
                  ;; the fixture has a tab: XeTeX must refuse rather than write ^^I
                  (check (and (/= code 0) (search "control character" (tex-log out)))
                         (format nil "~a refuses a chunk with a tab" engine))
                  (check (and (eql code 0) (string= (slurp (build-path out)) (slurp ref)))
                         (format nil "~a \\lltangle = CL tangle on ~a" engine pamphlet))))))
        (dolist (case '(("noweb-cycle.pamphlet" "reference cycle")
                        ("noweb-undefined.pamphlet" "undefined chunk")))
          (destructuring-bind (pamphlet message) case
            (let* ((out (format nil "~a-~a.out" engine (pathname-name pamphlet)))
                   (code (run-tex-tangle engine pamphlet "*" out)))
              (check (and (integerp code) (/= code 0) (search message (tex-log out)))
                     (format nil "~a \\lltangle stops on ~a" engine message))))))))

;;; --- summary -------------------------------------------------------------

(format t "~&~%CL: ~d passed, ~d failed~%" *pass* *fail*)
(sb-ext:exit :code (if (zerop *fail*) 0 1))
