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

;;; --- summary -------------------------------------------------------------

(format t "~&~%CL: ~d passed, ~d failed~%" *pass* *fail*)
(sb-ext:exit :code (if (zerop *fail*) 0 1))
