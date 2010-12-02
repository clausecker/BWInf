#Makefile, um den Kram mit JHC zu bauen
JHC = jhc
OPTIONS_JHC = -f jgc
LIBS_JHC = -p containers
SOURCES = Main.hs \
          Aufgabe4/Datentypen.hs\
          Aufgabe4/IO.hs\
          Aufgabe4/Statistik.hs

Aufgabe4: Main.hs
	$(JHC) $(OPTIONS_JHC) $(LIBS_JHC) -o $@ $<
