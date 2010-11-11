#!/usr/bin/make -f

HIGHLIGHTED_FILES = bambus.cfdg.tex

PYGMENTIZE = pygmentize
STYLE = borland
PYGMENTIZE_OPTS = -O linenos -l text

PDFLATEX = pdflatex

PDFLATEX_OPTS = -halt-on-error

doc: doc.pdf

#Der log wird auch nach doc.log ausgegeben...
doc.pdf: doc.toc definitions.tex $(HIGHLIGHTED_FILES)
	$(PDFLATEX) $(PDFLATEX_OPTS) doc.tex > /dev/null

#Die Datei muss zweimal gebaut werden, um das Inhaltsverzeichnis darzustellen
doc.toc: doc.tex definitions.tex $(HIGHLIGHTED_FILES)
	$(PDFLATEX) -draftmode $(PDFLATEX_OPTS) doc.tex > /dev/null

#Enthält den Vorspann der Pygments Befehle.
definitions.tex:
	$(PYGMENTIZE) -S $(STYLE) -f tex > $@

.PHONY: clean doc

clean:
	$(RM) *.pdf *.hs.tex *.aux *.log definitions.tex *.toc

%.cfdg.tex: ../src/%.cfdg
	$(PYGMENTIZE) $(PYGMENTIZE_OPTS) -o $@ $<
