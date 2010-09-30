\documentclass{scrreprt}

\usepackage[utf8]{inputenc}
\usepackage[T1]{fontenc}
\usepackage {ngerman}

\author {Robert Clausecker -- Team "`Herder"'}
\title {L"osung zur Aufgabe 3}
\subtitle {des 29. Bundeswettbewerbs Informatik}
\begin{document}
\maketitle

\chapter {Teilaufgabe 1: Keine Optimierungen}

\section {Grundidee}
Die Idee zur L"osung dieser Teilaufgabe ist es, pro Lager nachzuschauen, wie
viele Fahrzeuge pro Tag an- bzw. abfahren.  Da ein Fahrzeug pro Tag genau eine
Strecke zur"ucklegen darf, kann man nun eine Art Fahrzeugbestand f"uhren.
Nachdem das Programm die Anzahl der an- und abfahrenden Fahrzeuge berechnet
hat, wird diese in den aktuellen Fahrzeugbestand einberechnet.

\section {Algorithmus}
Der Fahrzeugbestand wird als Paar nat"urlicher Zahlen dargestellt, die
erste stellt die Anzahl der sich an dem Lager befindlichen Fahrzeuge dar, die
zweite ist der Wert, wieviele Fahrzeuge insgesamt zu dieser Station geordert
werden m"ussen.  Aus dem Paar der anfahrenden- und abfahrenden Fahrzeuge sowie
dem vorherigen Bestand wird nach folgendem Schema der neue Bestand bestimmt:

Der Tupel der An- und abfahrenden Fahrzeuge sei $(an,ab)$, der
Tupel des alten Bestandes sei $(order,ist)$. Der neue Fahrzeugbestand sei
der Tupel $(order',ist')$.

$$
(order',ist') := \left \{\begin{array}{rl}
\mathbf{ab - ist > 0:} & (ist - ab + an, order)
\\
\mathbf{ansonsten:}    & (an, order + ist - ab)
\end{array}\right .$$
Am Anfang der Woche wird mit einem Startwert von $(0,0)$ begonnen, die Liste der
Fahrten nach Wochentagen wird dann von Montag bis Samstag mit der Funktion
\texttt{foldl} gefaltet.  Der Einfachheit halber wurde der Algorithmus zur
Bestimmung der An- und Abfahrten hart verdrahtet, mit geringerem Mehraufwand
lie"se sich der Algorithmus auch wesentlich generischer schreiben.

Die Ausgabe der Funktion sind drei Tupel, f"ur jede Station einen.  Es werden
allerdings nur die ersten Werte der Tupel gebraucht, der Rest wird am Ende
verworfen. Die Anzahl der nun wirklich beno"tigten Fahrzeuge ist die Summe jener
drei Werte.
\end{document}
