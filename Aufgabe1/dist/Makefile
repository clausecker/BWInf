#!/usr/bin/make -f

SOURCE = ../src/bambus.cfdg
VARIATIONS = ../src/variations.list
CFDG = cfdg
#Die Variablen WIDTH und HEIGHT können optional gesetzt werden.
CFDGFLAGS = -q $(if $(WIDTH) , -w $(WIDTH)) $(if $(HEIGHT) , -h $(HEIGHT))
SHELL = /bin/sh
export WIDTH = 1000
export HEIGHT = 1000

#Flags zum erzeugen von PNG's oder SVG's
SVG_FLAG = -V
PNG_FLAG =

.PHONY: clean examples examples_svg examples_png random_svg random_png

examples: examples_svg examples_png

#Die folgenden Ziele lesen die Variantendatei aus und rufen make rekursiv auf.
examples_svg: $(VARIATIONS)
	sed 's/$$/\.svg/g' $< | xargs $(MAKE)

examples_png: $(VARIATIONS)
	sed 's/$$/\.png/g' $< | xargs $(MAKE)

#Erzeugt zufällige Ausgabe
random_svg: $(SOURCE)
	$(CFDG) $(SVG_FLAG) $(CFDGFLAGS) -o %V.svg $<

random_png: $(SOURCE)
	$(CFDG) $(PNG_FLAG) $(CFDGFLAGS) -o %V.png $<

#Erzeugt Ausgabe für eine Variante
%.svg: $(SOURCE)
	$(CFDG) $(SVG_FLAG) $(CFDGFLAGS) -v $* -o $@ $<

%.png: $(SOURCE)
	$(CFDG) $(PNG_FLAG) $(CFDGFLAGS) -v $* -o $@ $<

clean:
	rm -f *.png *.svg
