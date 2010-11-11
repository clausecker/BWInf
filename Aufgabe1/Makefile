#!/usr/bin/make -f

all: examples doc

doc:
	$(MAKE) -C doc/ doc

doc-clean:
	$(MAKE) -C doc/ clean

examples:
	$(MAKE) -C dist/ examples

clean: examples-clean doc-clean

examples-clean:
	$(MAKE) -C dist/ clean

.PHONY: all doc doc-clean examples clean examples-clean
