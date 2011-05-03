#!/usr/bin/make -f

CABAL = cabal

.PHONY: doc clean build-clean doc-clean

all: doc build

clean: build-clean doc-clean

build:
	$(CABAL) configure $(CABALFLAGS)
	$(CABAL) build $(CABALFLAGS)

build-jhc:
	$(MAKE) -C src
build-clean:
	$(CABAL) clean

doc:
	$(MAKE) -C doc

doc-clean:
	$(MAKE) -C doc clean
