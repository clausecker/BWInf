-- Modul zur Ein- und Ausgabe. Keine der Funktionen führt wirkliches IO aus.
module Aufgabe4.IO (
  parseKartenspiel,
  schoeneAnalyse,
  spielauswertung
)where

import Aufgabe4.Datentypen
import Aufgabe4.Statistik

import qualified Data.Map as Map
import Data.Bits
import Random.Xorshift
import Text.Printf (printf)

import Control.Monad.State.Strict (State,state,runState)

-- Eingabeformat: Siehe Dokumentation.
parseKartenspiel :: String -> Kartenspiel
parseKartenspiel karten | all (`elem` ['1'..'9']) karten = resultat
                        | otherwise                      = error errorMsg where
  resultat = foldl addKarte spielende eingabeAlsKarte
  eingabeAlsKarte = map (bit . subtract 1 . read . return) karten
  errorMsg = "Aufgabe4.IO.parseKartenspiel: Ungültige Eingabe"

-- Lässt den Computer gegen sich selbst spielen.  Das Ergebnis des Spieles wird
-- mit dem neuen Generator zurückgegeben.
computerSpiel :: Kartenspiel -> State Xorshift Int
computerSpiel ks = do
  let random16 = state $ randomR (1,6) -- Der Generator ist der Zustand
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
  showAuswahl (Left k)      = show $ fromEnum k + 1
  showAuswahl (Right (a,b)) = show (fromEnum a+1) ++ ", " ++ show (fromEnum b+1)
  ratings     = kandidaten >>= kandidatenliste
  kandidatenliste (n,lst) | x `notElem` [1,n] = "" -- 1: Nicht spezifizieren
                          | otherwise = printf ("Würfelergebnis %2d\n  " ++
    "Erwartungswert: %2f\n  Kandidaten:\n%s\n") n ew liste where
    ew | null lst  = fromIntegral $ punkte ks
       | otherwise = maximum $ map snd lst
    liste = lst >>= \(k, r) -> printf "    Auswahl %-5s Erwartung %2f\n"
      (showAuswahl k ++ ",") r :: String

-- Funktion führt n Spiele aus und gibt ein Resultat zurück
spielauswertung :: Kartenspiel -> Int -> Xorshift -> String
spielauswertung ks n g | n < 0 = error $ printf "Anzahl Spiele (%d) negativ." n
                       | otherwise = fst . ($g). runState $ do
  let n'           = fromIntegral n :: Double -- Hilfsfunktionen
      iterateM 0 _ x = x `seq` return x -- Wendet eine (monadische) Funktion
      iterateM i f x = x `seq` f x >>= iterateM (i - 1) f -- n mal an.
      schritt st = do -- Diese Funktion führt ein Spiel aus und fügt es in die
        wertung <- computerSpiel ks -- Liste ein.
        return (Map.insertWith' (+) wertung 1 st)
  -- Der nachfolgende Block berechnet die gesamte Statistik.
  werteliste <- iterateM n schritt Map.empty
  let anzahlen     = Map.toList werteliste
      relAnzahlen  = map (\(_,a) -> fromIntegral a / n' * 100) anzahlen
      zusammen     = zipWith (\(x,a) r -> (x, a::Int, r)) anzahlen relAnzahlen
      durchschnitt = sum $ map
        (\(x,a) -> fromIntegral x * fromIntegral a / n') anzahlen
      msg1 = printf ("Statistik aus %d Spielen:\n" ++
         "Durchschnittliches Ergebnis: %f\n" ++
         "Verteilung der Ergebnisse:\n") n durchschnitt
      -- Wie auch oben: x: Ergebnisnummer, a: Anzahl, r: relative Anzahl
      msg2 = zusammen >>= \(x,a,r) -> printf "\t%2d: %7d (%5.2f%%)\n" x a r
  return (msg1 ++ msg2)
