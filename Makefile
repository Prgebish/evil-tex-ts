EMACS ?= emacs
BATCH = $(EMACS) -Q --batch

.PHONY: test clean

test:
	$(BATCH) \
		--eval "(require 'package)" \
		--eval "(push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives)" \
		--eval "(package-initialize)" \
		--eval "(unless (package-installed-p 'evil) (package-refresh-contents) (package-install 'evil))" \
		--eval "(require 'evil)" \
		-L . -l tests/test-evil-tex-ts.el -f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc tests/*.elc
