#!/usr/bin/make -f
doc: doc.pdf

HIGHLIGHTED_FILES = \
  Main.hs.tex\
  IO.hs.tex\
  Algorithmen.hs.tex\
  Datatypes.hs.tex
PYGMENTIZE = pygmentize
PYGMENTIZE_OPTS = -O style=bw
LN = ln -s

doc.pdf: doc.tex $(HIGHLIGHTED_FILES)
	pdflatex doc.tex

.PHONY: clean

clean:
	$(RM) *.pdf *.hs *.hs.tex *.aux *.log

%.hs.tex: %.hs
	$(PYGMENTIZE) $(PYGMENTIZE_OPTS) -o $@ $<

Main.hs:
	$(LN) ../src/Main.hs

%.hs:
	$(LN) ../src/Aufgabe3/$@


