EMACS ?= emacs
BATCH = $(EMACS) -Q --batch

.PHONY: test clean

test:
	$(BATCH) -L . -l tests/test-evil-tex-ts.el -f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc tests/*.elc
