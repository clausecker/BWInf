#!/usr/bin/make -f

all: examples doc

doc: ; #Noch nicht fertig
doc-clean: ;

examples:
	$(MAKE) -C dist/ examples

clean: examples-clean doc-clean

examples-clean:
	$(MAKE) -C dist/ clean

.PHONY: all doc doc-clean examples clean examples-clean
