-- Modul zur Ein- und Ausgabe, Interaktion, etc.
-- Alle Funktionen geben Informationen nach ihrem verbosity level aus.
{-# LANGUAGE BangPatterns #-}
module Aufgabe4.IO (
  parseKartenspiel,
  schoeneAnalyse,
  spielauswertung
)where

import Aufgabe4.Datentypen
import Aufgabe4.Statistik

import qualified Data.Map as Map
import Control.Monad.State.Strict

import System.Random
import Text.Printf (printf)

-- Eingabeformat: Siehe Dokumentation.
parseKartenspiel :: String -> Kartenspiel
parseKartenspiel karten | all (`elem` ['1'..'9']) karten = resultat
                        | otherwise                      = error errorMsg where
  resultat = foldl (flip addKarte) spielende eingabeAlsKarte
  eingabeAlsKarte = map (toEnum . pred . read . (:[])) karten
  errorMsg = "Aufgabe4.IO.parseKartenspiel: Ungültige Eingabe"

-- Lässt den Computer gegen sich selbst spielen.  Das Ergebnis des Spieles wird
-- mit dem neuen Generator zurückgegeben.
computerSpiel :: Kartenspiel -> State StdGen Int
computerSpiel ks = do
  let random16 = State $ randomR (1,6) -- Der Generator ist der Zustand
  auge1 <- random16
  auge2 <- random16
  let augen = auge1 + auge2
      ks'   = macheZug augen ks
  if ks == ks'
    then return $ punkte ks
    else computerSpiel ks'

-- Analysiert einen Spielstand und gibt ein schönes Resultat aus.
schoeneAnalyse :: Kartenspiel -> Int -> String
schoeneAnalyse ks x | x < 1 || x > 12 = error msg
                    | otherwise       = printf
  "Erwartungswert: %f\n\n%s" (bewerteKartenspiel ks) ratings where
  msg         = "Aufgabe4.IO.schoeneAnalyse: Ungültiger Parameter n: " ++ show x
  kandidaten  = map (\n -> (n,getKandidaten n ks)) [2..12]
  showAuswahl (Left k)      = show $ (fromEnum k + 1)
  showAuswahl (Right (a,b)) = show (fromEnum a+1) ++ ", " ++ show (fromEnum b+1)
  ratings     = kandidaten >>= kandidatenliste -- Teil 1 der Ausgabe
  kandidatenliste (n,lst)
    | x /= 1 && n /= x = ""
    | otherwise = (printf "Würfelergebnis %2d\n  Erwartungswert: %2f\
    \\n  Kandidaten:\n%s\n" n ew liste) :: String where
    ew | null lst  = fromIntegral $ punkte ks
       | otherwise = maximum $ map snd lst
    liste = lst >>= \(k, r) -> (printf "    Auswahl %-5s Erwartung %2f\n"
      (showAuswahl k ++ ",") r) :: String

-- Funktion führt n Spiele aus und gibt ein Resultatat zurück
spielauswertung :: Kartenspiel -> Int -> StdGen -> String
spielauswertung ks n g | n < 0 = error $ printf "Anzahl Spiele (%d) negativ." n
                       | otherwise = fst . ($g). runState $ do
  let n'           = fromIntegral n :: Double -- Hilfsfunktionen
      iterateM 0 _ !x = return x -- Wendet eine (monadische) Funktion n mal an.
      iterateM i f !x = f x >>= iterateM (i - 1) f
      schritt st = do -- Diese Funktion führt ein Spiel aus und fügt es in die
        wertung <- computerSpiel ks -- Liste ein.
        return (Map.insertWith' (+) wertung 1 st)
  -- Der nachfolgende Block berechnet die gesamte Statistik.
  werteliste <- iterateM n schritt Map.empty
  let anzahlen     = Map.toList werteliste
      relAnzahlen  = map (\(_,a) -> fromIntegral a / n' * 100) anzahlen
      zusammen     = zipWith (\(x,a) r -> (x,(a::Int),r)) anzahlen relAnzahlen
      durchschnitt = sum $ map
        (\(x,a) -> fromIntegral x * fromIntegral a / n') anzahlen
      msg1 = printf "Statistik aus %d Spielen:\n\
         \Durchschnittliches Ergebnis: %f\n\
         \Verteilung der Ergebnisse:\n" n durchschnitt
      -- Wie auch oben: x: Ergebnisnummer, a: Anzahl, r: relative Anzahl
      msg2 = zusammen >>= \(x,a,r) -> printf "\t%2d: %7d (%5.2f%%)\n" x a r
  return (msg1 ++ msg2)
