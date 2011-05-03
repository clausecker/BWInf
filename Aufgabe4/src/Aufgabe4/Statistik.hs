-- Dieses Modul enthält den statistischen Kram.
module Aufgabe4.Statistik (
  bewerteKartenspiel,
  getKandidaten,
  macheZug
) where

import Aufgabe4.Datentypen
import Data.Array
import Data.List (maximumBy)
import Data.Ord (comparing)

-- Liste der Wahrscheinlichkeiten der Augen zweier Würfel
wahrscheinlichkeitAugen :: [Double]
wahrscheinlichkeitAugen =
  [ 1/36, 1/18, 1/12, 1/ 9, 5/36, 1/ 6, 5/36, 1/ 9, 1/12, 1/18, 1/36]
--   2     3     4     5     6     7     8     9    10    11    12

-- Liste alle Möglichkeiten, eine Augenzahl durch ein bis zwei Karten abzudecken
kombinationen :: [[Auswahl]]
kombinationen =
  [ [Left   karte2] -- 2
  , [Right (karte1,karte2), Left   karte3] -- 3
  , [Right (karte1,karte3), Left   karte4] -- 4
  , [Right (karte1,karte4), Right (karte2,karte3), Left   karte5] -- 5
  , [Right (karte1,karte5), Right (karte2,karte4), Left   karte6] -- 6
  , [Right (karte1,karte6), Right (karte2,karte5), Right (karte3,karte4)
    ,Left   karte7] -- 7
  , [Right (karte1,karte7), Right (karte2,karte6), Right (karte3,karte5)
    ,Left   karte8] -- 8
  , [Right (karte1,karte8), Right (karte2,karte7), Right (karte3,karte6)
    ,Right (karte4,karte5), Left   karte9] -- 9
  , [Right (karte1,karte9), Right (karte2,karte8), Right (karte3,karte7)
    ,Right (karte4,karte6)] -- 10
  , [Right (karte2,karte9), Right (karte3,karte8), Right (karte4,karte7)
    ,Right (karte5,karte6)] -- 11
  , [Right (karte3,karte9), Right (karte4,karte8), Right (karte5,karte7)]]-- 12

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

baueCache spiel = sum gewichteteWertungen where
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
macheZug = curry (zugCache !)

baueZugCache :: (Int,Kartenspiel) -> Maybe Kartenspiel
baueZugCache (augenzahl,ks) | null kand = Nothing
                            | otherwise = Just $ wendeAuswahlAn ks zug where
  kand    = uncurry zip $ getKandidaten ks augenzahl --Wenn null, dann Spielende
  (zug,_) = maximumBy (comparing snd) kand
