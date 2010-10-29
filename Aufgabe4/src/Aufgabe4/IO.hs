-- Modul zur Ein- und Ausgabe, Interaktion, etc.
-- Alle Funktionen geben Informationen nach ihrem verbosity level aus.
{-#LANGUAGE BangPatterns #-}
module Aufgabe4.IO (
  parseKartenspiel,
  schoeneAnalyse,
  spieleNmal
)where

import Aufgabe4.Datentypen
import Aufgabe4.Statistik

import Data.List (maximumBy)
import Data.Maybe (fromJust)
import Control.Monad.State.Strict

import System.Random
import Text.Printf (printf)

{- Eingabeformat:
Das Kartenspiel wird als Ziffernfolge eingelesen und sollte recht selbsterklä-
rend sein.  Es werden alle Karten genannt, die noch nicht umgedreht sind, so ist
z. B. "12589" ein Kartenspiel, bei dem die Karten 3, 4, 6 und 7 umgedreht sind.
Verbosity:
  1: Kurzinformationen
  2: Detaillierte Informationen
  3: Debug
-}

parseKartenspielS :: String -> Maybe Kartenspiel
parseKartenspielS karten | all (`elem` ['1'..'9']) karten = Just resultat
                         | otherwise                      = Nothing where
  eingabeAlsKarte = map (toEnum . pred . read . (:[])) karten
  resultat = foldl (flip addKarte) spielende eingabeAlsKarte

-- Das selbe nur mit Exceptions
parseKartenspiel :: String -> Kartenspiel
parseKartenspiel karten | all (`elem` ['1'..'9']) karten = resultat
                        | otherwise                      = error errorMsg where
  resultat = fromJust (parseKartenspielS karten)
  errorMsg = "Aufgabe4.IO.parseKartenspiel: Ungültige Eingabe"

-- Funktion lässt den Computer einen Zug machen.
macheZug :: Int -> Kartenspiel -> Kartenspiel
macheZug !augenzahl !ks | null kandidaten = ks -- Bei Spielende ändern wir nichts
                       | otherwise       = wendeAuswahlAn ks zug where
  kandidaten = getKandidaten augenzahl ks -- Wenn null, dann Spiel Ende.
  -- Vergleicht nach Bewertungen
  (zug,_)    = maximumBy ((.snd) . (compare . snd)) kandidaten

-- Hilfsfunktion für Zufall, der Generator ist ein Zustand.
randomRm :: (RandomGen g, Random a) => (a,a) -> State g a
randomRm bereich = do
  g <- get
  let (a,g') = randomR bereich g
  put g'
  return a

-- Lässt den Computer gegen sich selbst spielen.  Das Ergebnis des Spieles wird
-- mit dem neuen Generator zurückgegeben.
computerSpiel :: RandomGen g => Kartenspiel -> State g Int
computerSpiel ks = do
  auge1 <- randomRm (1,6) -- Der Generator ist der Zustand
  auge2 <- randomRm (1,6)
  let augen = auge1 + auge2
      ks'   = macheZug augen ks
  if ks == ks'
    then return $ punkte ks
    else computerSpiel ks'

spieleNmal :: RandomGen g => Int -> Kartenspiel -> State g [Int]
spieleNmal = flip (.) computerSpiel . replicateM

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
    ew | null lst  = punkte ks
       | otherwise = maximum $ map snd lst
    liste = lst >>= \(k, r) -> (printf "    Auswahl %-5s Erwartung %2f\n"
      (showAuswahl k ++ ",") r) :: String
