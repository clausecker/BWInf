-- Modul zur Ein- und Ausgabe. Keine der Funktionen führt wirkliches IO aus.
{-# LANGUAGE TupleSections #-}
module Aufgabe4.IO (
  parseKartenspiel,
  schoeneAnalyse,
  spielauswertung
)where

import Aufgabe4.Datentypen
import Aufgabe4.Statistik

import Data.Bits
import System.Random
import Text.Printf (printf)

import Control.Monad.State.Strict (State(..))
import Control.Monad (replicateM)
import Data.Array.Unboxed

-- Eingabeformat: Siehe Dokumentation.
parseKartenspiel :: String -> Kartenspiel
parseKartenspiel karten | all (`elem` ['1'..'9']) karten = resultat
                        | otherwise                      = error errorMsg where
  resultat = foldl addKarte spielende eingabeAlsKarte
  eingabeAlsKarte = map (bit . (subtract 1) . read . (:[])) karten
  errorMsg = "Aufgabe4.IO.parseKartenspiel: Ungültige Eingabe"

-- Lässt den Computer gegen sich selbst spielen.  Das Ergebnis des Spieles wird
-- mit dem neuen Generator zurückgegeben.
computerSpiel :: Kartenspiel -> State StdGen Int
computerSpiel ks = do
  let random16 = State $ randomR (1,6) -- Der Generator ist der Zustand
  auge1 <- ks `seq` random16
  auge2 <- random16
  let ks' = macheZug (auge1 + auge2) ks
  maybe (return $ punkte ks) computerSpiel ks' -- Nothing, wenn Spiel zu Ende.

-- Analysiert einen Spielstand und gibt ein schönes Resultat aus.
schoeneAnalyse :: Kartenspiel -> Int -> String
schoeneAnalyse ks x | x < 1 || x > 12 = error msg
                    | otherwise       = printf
  "Erwartungswert: %f\n\n%s" (bewerteKartenspiel ks) ratings where
  msg         = "Aufgabe4.IO.schoeneAnalyse: Ungültiger Parameter n: " ++ show x
  kandidaten  = map (\n -> (n,uncurry zip $ getKandidaten ks n)) [2..12]
  showAuswahl (Left k)      = show $ (fromEnum k + 1)
  showAuswahl (Right (a,b)) = show (fromEnum a+1) ++ ", " ++ show (fromEnum b+1)
  ratings     = kandidaten >>= kandidatenliste
  kandidatenliste (n,lst) | x `notElem` [1,n] = "" -- 1: Nicht spezifizieren
                          | otherwise = (printf ("Würfelergebnis %2d\n  " ++
    "Erwartungswert: %2f\n  Kandidaten:\n%s\n") n ew liste) where
    ew | null lst  = fromIntegral $ punkte ks
       | otherwise = maximum $ map snd lst
    liste = lst >>= \(k, r) -> (printf "    Auswahl %-5s Erwartung %2f\n"
      (showAuswahl k ++ ",") r) :: String

-- Die Auswertung gliedert sich in zwei Teile: Im ersten werden die Ergebnisse
-- eingesammelt, im zweiten wird die Statistik übersichtlich zusammengefasst
-- ausgegeben.
spielauswertung :: Kartenspiel -> Int -> State StdGen String
spielauswertung ks n = spieleNmal ks n >>= return . baueErgebnisse n

spieleNmal :: Kartenspiel -> Int -> State StdGen (UArray Int Int)
spieleNmal ks x = spiele >>= return . baueArray where
  spiele = replicateM x (computerSpiel ks >>= return . (,()))
  baueArray = accumArray (const . (+1)) 0 (2,45)

baueErgebnisse :: Int -> UArray Int Int -> String
baueErgebnisse n werteliste = zusammenfassung ++ details where
  n'           = fromIntegral n :: Double
  anzahlen     = filter ((/=0) . snd) $ assocs werteliste
  relAnzahlen  = map (\(_,a) -> fromIntegral a / n' * 100) anzahlen
  zusammen     = zipWith (\(x,a) r -> (x,a,r)) anzahlen relAnzahlen
  durchschnitt = sum $ map
    (\(x,a) -> fromIntegral x * fromIntegral a / n') anzahlen
  zusammenfassung = printf ("Statistik aus %d Spielen:\nDurchschnittliches \
    \Ergebnis: %f\nVerteilung der Ergebnisse:\n") n durchschnitt
  -- Wie auch oben: x: Ergebnisnummer, a: Anzahl, r: relative Anzahl
  details = zusammen >>= \(x,a,r) -> printf "\t%2d: %7d (%5.2f%%)\n" x a r
