# literate-latex --- E2E test driver
#
# Requires: sbcl, emacs, pdflatex (the PDF test self-skips if absent).

SBCL  ?= sbcl
EMACS ?= emacs

.PHONY: test test-cl test-el clean

test: test-cl test-el
	@echo "all literate-latex tests passed"

# Common Lisp: direct load (no tangle) + tangle reorder.
test-cl:
	$(SBCL) --script tests/test-cl.lisp

# Emacs Lisp: direct load + tangle reorder + header-parsing units + PDF typeset.
test-el:
	$(EMACS) -batch -Q -L . -l tests/test-elisp.el -f ert-run-tests-batch-and-exit

clean:
	rm -rf tests/build
	rm -f *.fasl *.elc
