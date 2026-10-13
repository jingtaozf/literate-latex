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
;;;
;;; XeTeX runs with -8bit: without it XeTeX writes a tab as ^^I, and the
;;; write probe in \lltangle must then refuse the fixture's tab line (checked
;;; separately below, together with pdfTeX stripped of its translation file).

(defparameter *tex-dir*
  (namestring (merge-pathnames "../tex/" (make-pathname :directory (pathname-directory *load-pathname*)))))

(defun run-tex-tangle (engine pamphlet root out-name &optional flags)
  "Run ENGINE with FLAGS on \\lltangle{PAMPHLET}{ROOT}{OUT-NAME} inside
tests/build/.  Return the exit code, or :missing when ENGINE is not installed."
  (let ((build (directory-namestring (build-path "x"))))
    (handler-case
        (sb-ext:process-exit-code
         (sb-ext:run-program
          engine
          (append flags
                  (list "-interaction=nonstopmode" "-halt-on-error"
                        (format nil "-jobname=~a" (pathname-name out-name))
                        (format nil "\\RequirePackage{literate-latex}\\lltangle{~a}{~a}{~a}\\stop"
                                (namestring (fixture pamphlet)) root out-name)))
          :search t :directory build :output nil :error nil :input nil
          :environment (cons (format nil "TEXINPUTS=~a:" *tex-dir*) (sb-ext:posix-environ))))
      (error () :missing))))

(defun tex-log (out-name)
  (slurp (build-path (format nil "~a.log" (pathname-name out-name)))))

(defun empty-file-p (out-name)
  (with-open-file (s (build-path out-name)) (zerop (file-length s))))

(defun check-refused (engine flags out-name message)
  "ENGINE with FLAGS must stop on tex-tangle.pamphlet with MESSAGE in the log
and leave OUT-NAME empty, never half-written."
  (let ((code (run-tex-tangle engine "tex-tangle.pamphlet" "*" out-name flags)))
    (if (eq code :missing)
        (format t "  skip ~a not installed~%" engine)
        (check (and (/= code 0) (search message (tex-log out-name)) (empty-file-p out-name))
               (format nil "~a ~{~a ~}refuses the fixture: ~a" engine flags message)))))

(format t "~&tangle from TeX (\\lltangle):~%")
(dolist (run '(("pdflatex") ("lualatex") ("xelatex" "-8bit")))
  (destructuring-bind (engine &rest flags) run
    (if (eq (run-tex-tangle engine "tangle-demo.pamphlet" "*" "probe.out" flags) :missing)
        (format t "  skip ~a not installed~%" engine)
        (progn
          (dolist (pamphlet '("tangle-demo.pamphlet" "noweb.pamphlet" "tex-tangle.pamphlet"))
            (let* ((stem (format nil "~a-~a" engine (pathname-name pamphlet)))
                   (ref (build-path (format nil "~a.ref" stem)))
                   (out (format nil "~a.out" stem))
                   (code (progn (tangle (fixture pamphlet) ref)
                                (run-tex-tangle engine pamphlet "*" out flags))))
              (check (and (eql code 0) (string= (slurp (build-path out)) (slurp ref)))
                     (format nil "~a ~{~a ~}\\lltangle = CL tangle on ~a" engine flags pamphlet))))
          (dolist (case '(("noweb-cycle.pamphlet" "reference cycle")
                          ("noweb-undefined.pamphlet" "undefined chunk")))
            (destructuring-bind (pamphlet message) case
              (let* ((out (format nil "~a-~a.out" engine (pathname-name pamphlet)))
                     (code (run-tex-tangle engine pamphlet "*" out flags)))
                (check (and (integerp code) (/= code 0) (search message (tex-log out))
                            (empty-file-p out))
                       (format nil "~a \\lltangle stops on ~a, output left empty"
                               engine message)))))))))

;; The write probe: an engine that would write ^^ notation is refused, and
;; -8bit (the flag the message recommends) makes the same run pass.
(check-refused "xelatex" nil "xelatex-plain.out" "rerun it with -8bit")
(check-refused "pdflatex" '("-translate-file=empty.tcx") "pdflatex-notcx.out"
               "non-ASCII character")
(let ((code (run-tex-tangle "pdflatex" "tex-tangle.pamphlet" "*" "pdflatex-notcx-8bit.out"
                            '("-translate-file=empty.tcx" "-8bit"))))
  (unless (eq code :missing)
    (check (and (eql code 0)
                (string= (slurp (build-path "pdflatex-notcx-8bit.out"))
                         (slurp (build-path "pdflatex-tex-tangle.ref"))))
           "pdflatex without a translation file but with -8bit = CL tangle")))

;;; --- summary -------------------------------------------------------------

(format t "~&~%CL: ~d passed, ~d failed~%" *pass* *fail*)
(sb-ext:exit :code (if (zerop *fail*) 0 1))
