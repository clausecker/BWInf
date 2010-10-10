-- Dieses Modul enthält den statistischen Kram.
module Aufgabe4.Statistik (
  bewerteKartenspiel
) where

import Aufgabe4.Datentypen

-- Liste der Wahrscheinlichkeiten der Augen zweier Würfel
{-# SPECIALIZE wahrscheinlichkeitAugen :: [Rational] #-}
wahrscheinlichkeitAugen :: Fractional t => [t]
wahrscheinlichkeitAugen =
  [ 1/36 --  2
  , 1/18 --  3
  , 1/12 --  4
  , 1/ 9 --  5
  , 5/36 --  6
  , 1/ 6 --  7
  , 5/36 --  8
  , 1/ 9 --  9
  , 1/12 -- 10
  , 1/18 -- 11
  , 1/36 -- 12
  ]

-- Liste alle Möglichkeiten, eine Augenzahl durch ein bis zwei Karten abzudecken
kombinationen :: [[Auswahl]]
kombinationen =
  [  --  2
    [ Left   Karte2
  ], --  3
    [ Right (Karte1,Karte2)
    , Left   Karte3
  ], --  4
    [ Right (Karte1,Karte3)
    , Left   Karte4
  ], --  5
    [ Right (Karte1,Karte4)
    , Right (Karte2,Karte3)
    , Left   Karte5
  ], --  6
    [ Right (Karte1,Karte5)
    , Right (Karte2,Karte4)
    , Left   Karte6
  ], --  7
    [ Right (Karte1,Karte6)
    , Right (Karte2,Karte5)
    , Right (Karte3,Karte4)
    , Left   Karte7
  ], --  8
    [ Right (Karte1,Karte7)
    , Right (Karte2,Karte6)
    , Right (Karte3,Karte5)
    , Left   Karte8
  ], --  9
    [ Right (Karte1,Karte8)
    , Right (Karte2,Karte7)
    , Right (Karte3,Karte6)
    , Right (Karte4,Karte5)
    , Left   Karte9
  ], -- 10
    [ Right (Karte1,Karte9)
    , Right (Karte2,Karte8)
    , Right (Karte3,Karte7)
    , Right (Karte4,Karte6)
  ], -- 11
    [ Right (Karte2,Karte9)
    , Right (Karte3,Karte8)
    , Right (Karte4,Karte7)
    , Right (Karte5,Karte6)
  ], -- 12
    [ Right (Karte3,Karte9)
    , Right (Karte4,Karte8)
    , Right (Karte5,Karte7)
  ]]

-- Berechnet die Anzahl der Punkte, die ein Kartenspiel bringt.
{-# SPECIALIZE punkte :: Kartenspiel -> Rational #-}
punkte :: (Num a, Enum a) => Kartenspiel -> a
punkte (Kartenspiel k1 k2 k3 k4 k5 k6 k7 k8 k9) =
  sum . map snd . filter (not . fst) $ zip [k1,k2,k3,k4,k5,k6,k7,k8,k9] [1..9]

{-
Die Nachfolgende Funktion ist die Kernfunktion des Programms.  Sie berechnet den
zu erwartenden Durchschnittswert eines Kartenspiels.  Die Idee ist, dass für
alle möglichen Augensummen ermittelt wird, welche Möglichkeiten es gibt, geei-
gnete Karten auszuwählen.
  Sollte es keine Möglichkeit geben, so wird die Summe aller Karten zurück gege-
ben, ansonsten wird die Funktion rekursiv für alle entstehenden Kartenspiele
aufgerufen.  Die so erhaltenen Ergebnisse werden anschließend mit der Wahr-
scheinlichkeit, dass sie eintreten multipliziert, summiert und zurückgegeben.
-}

{-# SPECIALIZE bewerteKartenspiel :: Kartenspiel -> Rational #-}
bewerteKartenspiel :: (Fractional a, Ord a, Enum a) => Kartenspiel -> a
bewerteKartenspiel spiel = sum gewichteteWertungen where
  anwendbareAuswahlen = map (filter (auswahlAnwendbar spiel)) kombinationen
  angewandteAuswahlen = map (map $ wendeAuswahlAn spiel) anwendbareAuswahlen
  bewertungen         = map (map bewerteKartenspiel) angewandteAuswahlen
  besteBewertungen    = map getBest bewertungen where
    getBest [] = punkte spiel -- Wir wissen, wenn es keine Möglichkeit gibt,
    getBest x  = maximum x    -- dann  müssen wir nicht weiter schauen.
  gewichteteWertungen = zipWith (*) wahrscheinlichkeitAugen besteBewertungen
