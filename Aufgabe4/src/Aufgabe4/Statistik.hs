-- Dieses Modul enthält den statistischen Kram.
{-# LANGUAGE BangPatterns #-}
module Aufgabe4.Statistik (
  bewerteKartenspiel,
  bewerteSpiel,
  besteAuswahl,
  getKandidaten,
  punkte
) where

import Aufgabe4.Datentypen
import Data.List (maximumBy)

-- Liste der Wahrscheinlichkeiten der Augen zweier Würfel
wahrscheinlichkeitAugen :: [Double]
wahrscheinlichkeitAugen =
  [ 1/36, 1/18, 1/12, 1/ 9, 5/36, 1/ 6, 5/36, 1/ 9, 1/12, 1/18, 1/36]
--   2     3     4     5     6     7     8     9    10    11    12

-- Liste alle Möglichkeiten, eine Augenzahl durch ein bis zwei Karten abzudecken
kombinationen :: [[Auswahl]]
kombinationen =
  [ [Left   Karte2] -- 2
  , [Right (Karte1,Karte2), Left   Karte3] -- 3
  , [Right (Karte1,Karte3), Left   Karte4] -- 4
  , [Right (Karte1,Karte4), Right (Karte2,Karte3), Left   Karte5] -- 5
  , [Right (Karte1,Karte5), Right (Karte2,Karte4), Left   Karte6] -- 6
  , [Right (Karte1,Karte6), Right (Karte2,Karte5), Right (Karte3,Karte4)
    ,Left   Karte7] -- 7
  , [Right (Karte1,Karte7), Right (Karte2,Karte6), Right (Karte3,Karte5)
    ,Left   Karte8] -- 8
  , [Right (Karte1,Karte8), Right (Karte2,Karte7), Right (Karte3,Karte6)
    ,Right (Karte4,Karte5), Left   Karte9] -- 9
  , [Right (Karte1,Karte9), Right (Karte2,Karte8), Right (Karte3,Karte7)
    ,Right (Karte4,Karte6)] -- 10
  , [Right (Karte2,Karte9), Right (Karte3,Karte8), Right (Karte4,Karte7)
    ,Right (Karte5,Karte6)] -- 11
  , [Right (Karte3,Karte9), Right (Karte4,Karte8), Right (Karte5,Karte7)]]-- 12

-- Berechnet die Anzahl der Punkte, die ein Kartenspiel bringt.
{-# SPECIALISE punkte :: Kartenspiel -> Double #-}
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

bewerteKartenspiel :: Kartenspiel -> Double
bewerteKartenspiel !spiel = sum gewichteteWertungen where
  anwendbareAuswahlen = map (filter (auswahlAnwendbar spiel)) kombinationen
  angewandteAuswahlen = map (map $ wendeAuswahlAn spiel) anwendbareAuswahlen
  bewertungen         = map (map bewerteKartenspiel) angewandteAuswahlen
  besteBewertungen    = map getBest bewertungen where
    getBest [] = punkte spiel -- Wir wissen, wenn es keine Möglichkeit gibt,
    getBest x  = maximum x    -- dann  müssen wir nicht weiter schauen.
  gewichteteWertungen = zipWith (*) wahrscheinlichkeitAugen besteBewertungen

-- Berechnet das zu erwartende Ergebnis nach vielen Spielen.
bewerteSpiel :: Double
bewerteSpiel = bewerteKartenspiel $
  Kartenspiel True True True True True True True True True

-- Berechnet und wertet alle Kandidaten.
getKandidaten :: Int -> Kartenspiel -> [(Auswahl,Double)]
getKandidaten zahl ks | zahl < 2 || zahl > 12 = error msg
                      | otherwise = zip kandidaten wertungen where
  kandidaten = filter (auswahlAnwendbar ks) $ kombinationen !! (zahl - 2)
  wertungen = map (bewerteKartenspiel . wendeAuswahlAn ks) kandidaten
  msg = "Seit wann kann man " ++ show zahl ++ " würfeln?"

-- Berechnet, welche Auswahl man für ein möglichst gutes Ergebnis treffen sollte
-- Nothing wenn keine Auswahl möglich ist-
besteAuswahl :: Int {- Gewürfelte Zahl -} -> Kartenspiel -> Maybe Auswahl
besteAuswahl zahl ks = beste >>= Just . fst where
  auswahlen = getKandidaten zahl ks
  beste | null auswahlen = Nothing
        | otherwise = Just $ maximumBy (\(_,a) (_,b)-> compare a b) auswahlen
