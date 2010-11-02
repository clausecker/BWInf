-- Dieses Modul enthält den statistischen Kram.
{-# LANGUAGE BangPatterns #-}
module Aufgabe4.Statistik (
  bewerteKartenspiel,
  getKandidaten,
  punkte,
  macheZug
) where

import Aufgabe4.Datentypen
import Data.Array
import Data.List (maximumBy)
import Data.Function (on)

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
punkte :: Kartenspiel -> Int
punkte ks = case ks of
  (Kartenspiel k1 k2 k3 k4 k5 k6 k7 k8 k9) -> punkte' 0 1 lst where
    lst = [k1,k2,k3,k4,k5,k6,k7,k8,k9]
    punkte' n !_ [] = n
    punkte' n  k (False:xs) = punkte' (n + k) (k + 1) xs
    punkte' n  k (True:xs)  = punkte' n       (k + 1) xs

{-
Die Nachfolgende Funktion ist die Kernfunktion des Programms.  Sie berechnet den
zu erwartenden Durchschnittswert eines Kartenspiels.  Die Idee ist, dass für
alle möglichen Augensummen ermittelt wird, welche Möglichkeiten es gibt, geei-
gnete Karten auszuwählen.
  Sollte es keine Möglichkeit geben, so wird die Summe aller Karten zurück gege-
ben, ansonsten wird die Funktion rekursiv für alle entstehenden Kartenspiele
aufgerufen.  Die so erhaltenen Ergebnisse werden anschließend mit der Wahr-
scheinlichkeit, dass sie eintreten multipliziert, summiert und zurückgegeben.
  Dazu wird ein Cache verwendet, der Cache ist ein faules Array und enthält alle
möglichen Spiele. Anstatt eines direkten rekursiven Aufrufs wird einfach der
entsprechende Wert aus dem Cache angefragt, aufgrund der Laziness wird dieser
erst dann berechnet. (Und berechnet wahrscheinlich gleich noch ein paar mehr...)
-}

bewertungsCache :: Array Kartenspiel Double
bewertungsCache = listArray arrRange werte where
  werte         = map baueCache $ range arrRange
  arrRange      = (spielende,startaufstellung)

bewerteKartenspiel, baueCache :: Kartenspiel -> Double
bewerteKartenspiel = (bewertungsCache !)
{-
baueCache !spiel   = sum gewichteteWertungen where
  anwendbareAuswahlen = map (filter (auswahlAnwendbar spiel)) kombinationen
  angewandteAuswahlen = map (map $ wendeAuswahlAn spiel) anwendbareAuswahlen
  bewertungen         = map (map bewerteKartenspiel) angewandteAuswahlen
  besteBewertungen    = map getBest bewertungen where
    getBest [] = fromIntegral $ punkte spiel -- wenn es keine Möglichkeit gibt,
    getBest x  = maximum x    -- dann  müssen wir nicht weiter schauen.
  gewichteteWertungen = zipWith (*) wahrscheinlichkeitAugen besteBewertungen
-}
baueCache !spiel = sum gewichteteWertungen where
  bewertungen = map (snd . getKandidaten spiel) [2..12]
  besteBewertungen    = map getBest bewertungen where
    getBest [] = fromIntegral $ punkte spiel -- wenn es keine Möglichkeit gibt,
    getBest x  = maximum x    -- dann  müssen wir nicht weiter schauen.
  gewichteteWertungen = zipWith (*) wahrscheinlichkeitAugen besteBewertungen

-- Berechnet und wertet alle Kandidaten.
getKandidaten :: Kartenspiel -> Int -> ([Auswahl],[Double])
getKandidaten ks zahl = (kandidaten,wertungen) where
  kandidaten = filter (auswahlAnwendbar ks) $ kombinationen !! (zahl - 2)
  wertungen  = map (bewerteKartenspiel . wendeAuswahlAn ks) kandidaten

-- Wir cachen den besten Zug für die Performance.
zugCache :: Array (Int,Kartenspiel) (Maybe Kartenspiel)
zugCache   = listArray arrRange werte where
  werte    = map baueZugCache $ range arrRange
  arrRange = ((2,spielende),(12,startaufstellung))

-- Funktion lässt den Computer einen Zug machen.
macheZug :: Int -> Kartenspiel -> Maybe Kartenspiel
macheZug = curry $ (zugCache !)
-- Bei Spielende ändern wir nichts

baueZugCache :: (Int,Kartenspiel) -> Maybe Kartenspiel
baueZugCache (augenzahl,ks) | null kand = Nothing
                            | otherwise = Just $ wendeAuswahlAn ks zug where
  kand    = uncurry zip $ getKandidaten ks augenzahl --Wenn null, dann Spielende
  (zug,_) = maximumBy (compare `on` snd) kand
