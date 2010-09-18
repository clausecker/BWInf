Aufgabe 3 - Logistisch
----------------------

Hinweise zum Format der Eingabedateien

- Zeilen, die mit einem "#" beginnen sind Kommentarzeilen
- Die Dateien haben jeweils genau 6 Zeilen mit 6 Zahlen.
- Die Zahlen sind durch jeweils ein Leerzeichen voneinander getrennt.
- Jede Zeile entspricht einem Tag der Woche, Zeile 1 ist Montag,
  Zeile 2 ist Dienstag, usw.
- Die 6 Zahlen einer Reihe beschreiben, welche Transporte an dem
  jeweiligen Tag durchgeführt werden müssen. Wenn die Standorte 
  "A", "B" und "C" heißen, und "A->B" Transport von A nach B bedeutet,
  steht in einer Zeile A->B A->C B->A B->C C->A C->B.
- Die Zeile

     1 2 3 4 5 6

  bedeutet also:
  - 1 Container von A nach B
  - 2 Container von A nach C
  - 3 Container von B nach A
  - 4 Container von B nach C
  - 5 Container von C nach A
  - 6 Container von C nach B
