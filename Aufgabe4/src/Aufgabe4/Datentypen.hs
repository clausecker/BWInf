module Aufgabe4.Datentypen (
  Kartenspiel,
  Karte (..),
  Auswahl,
  startaufstellung,
  spielende,
  auswahlAnwendbar,
  wendeAuswahlAn,
  addKarte,
  punkte
) where

import Data.Bits

-- Wir stellen das Kartenspiel als Bitset dar.
type Kartenspiel = Int

-- Berechnet die Anzahl der Punkte, die ein Kartenspiel bringt. Ist hier zwar
-- recht unpassend, muss aber sein um Kartenspiel zu abstrahieren.
punkte :: Kartenspiel -> Int
punkte karten = punkte' karten 1 0 where
  punkte'  ks 10 k             = ks `seq` k
  punkte'  ks  n k | rest == 0  = punkte' ks' (n+1) (k+n)
                   | otherwise = punkte' ks' (n+1)  k    where
    (ks',rest) = ks `divMod` 2

-- Häufig gebrauchte Werte
startaufstellung, spielende :: Kartenspiel
startaufstellung = 1023
spielende = 0

data Karte
  = Karte1 | Karte2 | Karte3
  | Karte4 | Karte5 | Karte6
  | Karte7 | Karte8 | Karte9
 deriving (Enum)

-- Möglichkeit, eine Karte auszuwählen.
type Auswahl = Either Karte (Karte,Karte)

-- existiert die Karte im Kartenspiel?
haveKarte :: Karte -> Kartenspiel -> Bool
haveKarte k ks = testBit ks (fromEnum k)

addKarte :: Kartenspiel ->  Karte -> Kartenspiel
addKarte ks k = setBit ks (fromEnum k)

removeKarte :: Karte -> Kartenspiel -> Kartenspiel -- Wird nur hier gebraucht,
removeKarte k ks = clearBit ks (fromEnum k) -- so spar ich mir ein paar flips

auswahlAnwendbar :: Kartenspiel -> Auswahl -> Bool
auswahlAnwendbar spiel (Left karte) = haveKarte karte spiel
auswahlAnwendbar spiel (Right (a,b)) = haveKarte a spiel && haveKarte b spiel

wendeAuswahlAn ::  Kartenspiel -> Auswahl -> Kartenspiel
wendeAuswahlAn spiel (Left  karte) = removeKarte karte spiel
wendeAuswahlAn spiel (Right (a,b)) = removeKarte b . removeKarte a $ spiel
