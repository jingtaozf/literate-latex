;;;; literate-latex.lisp --- load Common Lisp directly from a LaTeX pamphlet
;;;;
;;;; No tangle.  A pamphlet (.pamphlet / .tex) is read straight by the CL
;;;; reader: LaTeX prose is skipped, code inside `\begin{chunk}...\end{chunk}'
;;;; is read and evaluated in document order.
;;;;
;;;; Mechanism (mirrors literate-lisp, but the trigger char is `\' instead of
;;;; `#'): a private readtable makes `\' a macro character.  At top level the
;;;; only `\' the reader meets is the one starting `\end{chunk}', so the macro
;;;; flips back to prose-skipping and advances to the next loadable chunk.
;;;; The entry point `load-pamphlet' positions the stream at the first chunk
;;;; before the read/eval loop, so prose (and the `% -*-' modeline) is never
;;;; handed to the reader -- `%' therefore stays a normal constituent and code
;;;; containing `%' in symbol names is safe.
;;;;
;;;; Limitation: `\' loses its CL single-escape role inside this readtable, so
;;;; bare symbols written with `a\ b' style escapes are unsupported.  Use
;;;; `|a b|' multiple-escape instead.  `#\\' char literals and "a\\b" strings
;;;; are unaffected.

(defpackage #:literate-latex
  (:use #:cl)
  (:export #:load-pamphlet #:tangle #:*language* #:*language-aliases*
           #:*load-test-chunks*))

(in-package #:literate-latex)

(defparameter +chunk-begin+ "\\begin{chunk}")
(defparameter +chunk-end+ "\\end{chunk}")

(defvar *language* "lisp"
  "Language a loader claims.  A chunk with no explicit lang loads under any.")
(defvar *language-aliases* '("lisp" "common-lisp" "cl")
  "Names that a `lang=' option may use to mean the Common Lisp loader.")
(defvar *load-test-chunks* nil
  "When non-nil, chunks marked `load=test' are loaded too.")

;;; --- small string helpers (no external deps) -----------------------------

(defun trim-ws (s)
  (string-trim '(#\Space #\Tab #\Return #\Newline) s))

(defun prefixp (prefix s)
  (let ((p (length prefix)))
    (and (>= (length s) p) (string= prefix s :end2 p))))

(defun split-char (ch s)
  (loop with start = 0
        for pos = (position ch s :start start)
        collect (subseq s start (or pos (length s)))
        while pos do (setf start (1+ pos))))

;;; --- chunk header parsing ------------------------------------------------

(defun parse-opts (str)
  "\"load=no,lang=elisp\" -> plist (:LOAD \"no\" :LANG \"elisp\")."
  (loop for pair in (split-char #\, str)
        for eq = (position #\= pair)
        when eq
          append (list (intern (string-upcase (trim-ws (subseq pair 0 eq)))
                               :keyword)
                       (trim-ws (subseq pair (1+ eq))))))

(defun parse-chunk-begin (line)
  "If LINE is a chunk header return (values NAME OPTS-PLIST), else NIL."
  (let ((s (trim-ws line)))
    (when (prefixp +chunk-begin+ s)
      (let ((rest (subseq s (length +chunk-begin+)))
            (opts nil))
        (when (and (plusp (length rest)) (char= (char rest 0) #\[))
          (let ((close (position #\] rest)))
            (when close
              (setf opts (parse-opts (subseq rest 1 close))
                    rest (subseq rest (1+ close))))))
        (let ((open (position #\{ rest))
              (close (position #\} rest :from-end t)))
          (when (and open close (< open close))
            (values (subseq rest (1+ open) close) opts)))))))

(defun chunk-end-line-p (line)
  (prefixp +chunk-end+ (trim-ws line)))

(defun chunk-loadable-p (opts)
  (let ((load (or (getf opts :load) "yes"))
        (lang (getf opts :lang)))
    (and (cond ((string-equal load "no") nil)
               ((string-equal load "test") *load-test-chunks*)
               (t t))
         (or (null lang)
             (member lang *language-aliases* :test #'string-equal)))))

;;; --- the reader ----------------------------------------------------------

(defun skip-to-chunk-end (stream)
  (loop for line = (read-line stream nil nil)
        until (or (null line) (chunk-end-line-p line))))

(defun advance-to-code (stream)
  "Consume prose and non-loadable chunks; leave STREAM at the body of the next
loadable chunk.  Return T if positioned at code, NIL at end of file."
  (loop for line = (read-line stream nil nil)
        do (when (null line) (return nil))
           (multiple-value-bind (name opts) (parse-chunk-begin line)
             (when name
               (if (chunk-loadable-p opts)
                   (return t)
                   (skip-to-chunk-end stream))))))

(defun read-backslash (stream char)
  "Macro-character handler for `\\'.  At top level this fires on the line that
ends a chunk (or, defensively, on stray prose); it flips back to prose mode
and skips to the next loadable chunk, returning zero values so the reader
simply continues."
  (declare (ignore char))
  (let* ((rest (read-line stream nil ""))
         (line (concatenate 'string "\\" rest)))
    (multiple-value-bind (name opts) (parse-chunk-begin line)
      (cond
        ((and name (chunk-loadable-p opts)) (values))
        (name (skip-to-chunk-end stream) (advance-to-code stream) (values))
        (t (advance-to-code stream) (values))))))

(defvar *literate-readtable* nil)

(defun literate-readtable ()
  (or *literate-readtable*
      (setf *literate-readtable*
            (let ((rt (copy-readtable nil)))
              (set-macro-character #\\ #'read-backslash nil rt)
              rt))))

;;; --- public entry point --------------------------------------------------

(defun load-pamphlet (path &key (language *language*) (aliases *language-aliases*))
  "Load Common Lisp code from the pamphlet at PATH, in document order, with no
tangle step.  Chunks marked `load=no' (or a non-matching `lang=') are skipped.
Returns T."
  (with-open-file (stream path :external-format :utf-8)
    (let ((*readtable* (literate-readtable))
          (*package* *package*)
          (*language* language)
          (*language-aliases* aliases)
          (*load-pathname* (pathname path))
          (*load-truename* (ignore-errors (truename path)))
          (eof (list :eof)))
      (when (advance-to-code stream)
        (loop for form = (read stream nil eof)
              until (eq form eof)
              do (eval form)))
      t)))

;;; --- optional reorder (tangle) -------------------------------------------

(defun collect-chunks (path)
  "Return a hash table NAME -> list of body lines (same-named chunks append)."
  (let ((chunks (make-hash-table :test #'equal)))
    (with-open-file (stream path :external-format :utf-8)
      (loop for line = (read-line stream nil nil)
            while line
            do (multiple-value-bind (name opts) (parse-chunk-begin line)
                 (declare (ignore opts))
                 (when name
                   (let ((body (loop for l = (read-line stream nil nil)
                                     until (or (null l) (chunk-end-line-p l))
                                     collect l)))
                     (setf (gethash name chunks)
                           (append (gethash name chunks) body)))))))
    chunks))

(defun getchunk-ref (line)
  "If LINE is `<indent>\\getchunk{NAME}' return (values INDENT NAME), else NIL."
  (let* ((trimmed (string-left-trim '(#\Space #\Tab) line))
         (indent (subseq line 0 (- (length line) (length trimmed)))))
    (when (prefixp "\\getchunk{" trimmed)
      (let ((close (position #\} trimmed)))
        (when close
          (values indent (subseq trimmed (length "\\getchunk{") close)))))))

(defun emit-chunk (name chunks indent seen out)
  (when (member name seen :test #'equal)
    (error "literate-latex: chunk reference cycle through ~s" name))
  (let ((body (gethash name chunks)))
    (unless body
      (error "literate-latex: undefined chunk ~s" name))
    (dolist (line body)
      (multiple-value-bind (sub-indent sub-name) (getchunk-ref line)
        (if sub-name
            (emit-chunk sub-name chunks (concatenate 'string indent sub-indent)
                        (cons name seen) out)
            (write-line (concatenate 'string indent line) out))))))

(defun tangle (path out-path &key (root "*"))
  "Expand ROOT chunk (default \"*\") of pamphlet PATH into OUT-PATH, splicing
`\\getchunk{...}' references recursively in declared order.  This is the
optional reorder path -- it also serves languages whose reader cannot load a
pamphlet directly."
  (let ((chunks (collect-chunks path)))
    (with-open-file (out out-path :direction :output :if-exists :supersede
                                  :if-does-not-exist :create
                                  :external-format :utf-8)
      (emit-chunk root chunks "" nil out))
    out-path))
