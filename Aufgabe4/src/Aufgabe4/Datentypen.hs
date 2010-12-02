-- Wir verstecken den häßlichen Kram in diesem Modul.
module Aufgabe4.Datentypen (
  Kartenspiel,
  Karte,
  Auswahl,
  karte1, karte2, karte3,
  karte4, karte5, karte6,
  karte7, karte8, karte9,
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

type Karte = Int

karte1, karte2, karte3, karte4, karte5, karte6, karte7, karte8, karte9 :: Int
karte1 = bit 0
karte2 = bit 1
karte3 = bit 2
karte4 = bit 3
karte5 = bit 4
karte6 = bit 5
karte7 = bit 6
karte8 = bit 7
karte9 = bit 8

-- Möglichkeit, eine Karte auszuwählen.
type Auswahl = Either Karte (Karte,Karte)

-- existiert die Karte im Kartenspiel?
haveKarte :: Karte -> Kartenspiel -> Bool
haveKarte k ks = ks .&. k /= 0

addKarte :: Kartenspiel ->  Karte -> Kartenspiel
addKarte ks k = ks .|. k

removeKarte :: Karte -> Kartenspiel -> Kartenspiel -- Wird nur hier gebraucht,
removeKarte k ks = (complement k) .&. ks -- so spar ich mir ein paar flips

auswahlAnwendbar :: Kartenspiel -> Auswahl -> Bool
auswahlAnwendbar spiel (Left karte) = haveKarte karte spiel
auswahlAnwendbar spiel (Right (a,b)) = haveKarte a spiel && haveKarte b spiel

wendeAuswahlAn ::  Kartenspiel -> Auswahl -> Kartenspiel
wendeAuswahlAn spiel (Left  karte) = removeKarte karte spiel
wendeAuswahlAn spiel (Right (a,b)) = removeKarte b . removeKarte a $ spiel
