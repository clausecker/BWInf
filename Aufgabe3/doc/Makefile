#!/usr/bin/make -f

HIGHLIGHTED_FILES = \
  Main.hs.tex\
  IO.hs.tex\
  Algorithmen.hs.tex\
  Datatypes.hs.tex

PYGMENTIZE = pygmentize
STYLE = borland
PYGMENTIZE_OPTS = -O linenos

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

.PHONY: clean

clean:
	$(RM) *.pdf *.hs.tex *.aux *.log definitions.tex *.toc

#Da Main.hs im anderen Ordner liegt, ist ein extra Target notwendig.
Main.hs.tex: ../src/Main.hs
	$(PYGMENTIZE) $(PYGMENTIZE_OPTS) -o $@ $<

%.hs.tex: ../src/Aufgabe3/%.hs
	$(PYGMENTIZE) $(PYGMENTIZE_OPTS) -o $@ $<
