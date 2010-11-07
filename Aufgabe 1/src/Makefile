#!/usr/bin/make -f

SOURCE = bambus.cfdg
CFDG = cfdg
CFDGFLAGS = -q

#Flags zum erzeugen von PNG's oder SVG's
SVG_FLAG = -V
PNG_FLAG =

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
