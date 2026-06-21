# literate-latex --- E2E test driver
#
# Requires: sbcl, emacs, pdflatex (the PDF test self-skips if absent), git.
#
# The Emacs Lisp source and tests are .org files loaded via literate-elisp
# (there is no committed .el).  test-el uses a local literate-elisp checkout
# at ~/projects/literate-elisp if present, otherwise shallow-clones it into
# .deps/ so `make test' is self-contained (CI needs no extra step).

SBCL  ?= sbcl
EMACS ?= emacs

LITERATE_ELISP ?= $(HOME)/projects/literate-elisp
ifeq ($(wildcard $(LITERATE_ELISP)/literate-elisp.el),)
LITERATE_ELISP := .deps/literate-elisp
endif

.PHONY: test test-cl test-el clean

test: test-cl test-el
	@echo "all literate-latex tests passed"

# Common Lisp: direct load (no tangle) + tangle reorder.
test-cl:
	$(SBCL) --script tests/test-cl.lisp

$(LITERATE_ELISP)/literate-elisp.el:
	git clone --depth 1 https://github.com/jingtaozf/literate-elisp.git $(LITERATE_ELISP)

# Emacs Lisp: direct load + tangle reorder + header-parsing units + PDF typeset.
# Both literate-latex.org and the test .org are loaded through literate-elisp.
test-el: $(LITERATE_ELISP)/literate-elisp.el
	$(EMACS) -batch -Q -L . -L $(LITERATE_ELISP) -l literate-elisp \
	  --eval '(literate-elisp-load "literate-latex.org")' \
	  --eval '(literate-elisp-load "tests/test-elisp.org")' \
	  -f ert-run-tests-batch-and-exit

clean:
	rm -rf tests/build
	rm -f *.fasl *.elc
