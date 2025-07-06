EMACS ?= emacs

all: compile test

compile: ucond.elc

ucond.elc: ucond.el
	$(EMACS) -Q -batch -f batch-byte-compile ucond.el

test:
	$(EMACS) -Q -batch -l ucond.el \
		-l ucond-tests.el \
		-l ucond-tests-llm.el \
		-f ert-run-tests-batch-and-exit

clean:
	rm -f ucond.elc

.PHONY: all compile test clean
