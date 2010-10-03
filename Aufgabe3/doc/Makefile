#!/usr/bin/make -f

HIGHLIGHTED_FILES = \
  Main.hs.tex\
  IO.hs.tex\
  Algorithmen.hs.tex\
  Datatypes.hs.tex

PYGMENTIZE = pygmentize
HIGHLIGHT_STYLE = bw
PYGMENTIZE_OPTS =

PDFLATEX = pdflatex

PDFLATEX_OPTS = -halt-on-error

doc: doc.pdf

#Der log wird auch nach doc.log ausgegeben...
doc.pdf: toc
	$(PDFLATEX) $(PDFLATEX_OPTS) doc.tex > /dev/null

#Die Datei muss zweimal gebaut werden, um das Inhaltsverzeichnis darzustellen
toc: doc.tex definitions.tex $(HIGHLIGHTED_FILES)
	$(PDFLATEX) -draftmode $(PDFLATEX_OPTS) doc.tex > /dev/null

#Enthält den Vorspann der Pygments Befehle.
definitions.tex:
	$(PYGMENTIZE) -S $(HIGHLIGHT_STYLE) -f tex > $@

.PHONY: clean toc

clean:
	$(RM) *.pdf *.hs.tex *.aux *.log definitions.tex

#Da Main.hs im anderen Ordner liegt, ist ein extra Target notwendig.
Main.hs.tex: ../src/Main.hs
	$(PYGMENTIZE) $(PYGMENTIZE_OPTS) -o $@ $<

%.hs.tex: ../src/Aufgabe3/%.hs
	$(PYGMENTIZE) $(PYGMENTIZE_OPTS) -o $@ $<
