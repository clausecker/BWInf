#!/usr/bin/make -f

HIGHLIGHTED_FILES = \
  Main.hs.tex\
  IO.hs.tex\
  Algorithmen.hs.tex\
  Datatypes.hs.tex

PYGMENTIZE = pygmentize
PYGMENTIZE_OPTS = -O style=bw

PDFLATEX = pdflatex

PDFLATEX_OPTS =

doc: doc.pdf

doc.pdf: doc.tex $(HIGHLIGHTED_FILES)
	$(PDFLATEX) $(PDFLATEX_OPTS) doc.tex

.PHONY: clean

clean:
	$(RM) *.pdf *.hs.tex *.aux *.log

Main.hs.tex: ../src/Main.hs
	$(PYGMENTIZE) $(PYGMENTIZE_OPTS) -o $@ $<

%.hs.tex: ../src/Aufgabe3/%.hs
	$(PYGMENTIZE) $(PYGMENTIZE_OPTS) -o $@ $<


