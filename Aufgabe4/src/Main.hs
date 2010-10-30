-- Hauptprogram
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-#LANGUAGE BangPatterns #-}
module Main (
  main
) where

import Aufgabe4.IO
import Aufgabe4.Datentypen (Kartenspiel,startaufstellung)

import Control.Monad.State.Strict
import qualified Data.Map as Map

import System.Console.GetOpt
import System.Environment (getArgs)
import System.Random
import Text.Printf


data Modus
  = Hilfe
  | SpieleAutomatisch
  | Analysiere
  deriving (Show)

data Optionen = Optionen
  { modus :: Modus
  , spielstand :: Kartenspiel
  , anzahlSpiele :: Int
  } deriving (Show)

stdOptionen :: Optionen
stdOptionen = Optionen Analysiere startaufstellung 1

optionsbeschreibungen :: [OptDescr (Optionen -> Optionen)]
optionsbeschreibungen =
  [ Option "?h" ["help"]    (NoArg $ \o -> o {modus = Hilfe}) "Hilfe anzeigen"
  , Option "a"  ["analyze"] (NoArg $ \o -> o {modus = Analysiere})
    "Spielstand analysieren"
  , Option "c"  ["auto"] (NoArg $ \o -> o {modus = SpieleAutomatisch})
    "den Computer spielen lassen"
  , Option "s"  ["spielstand"] (ReqArg (\s o -> o {spielstand =
    parseKartenspiel s}) "SPIELSTAND") "Zu verwendenden Spielstand setzen"
  , Option "n"  ["anzahl","augenzahl"]
    (ReqArg (\n o -> o {anzahlSpiele = read n}) "N") "Anzahl der Spiele setzen"
  ]

hilfe :: String
hilfe = usageInfo header optionsbeschreibungen where
  header = "Lösung zur Aufgabe 4 des 29. Bundeswettbewerbs Informatik\n\
  \Team Herder, Robert Clausecker.\n\n\
  \Die Eingabe der Spielstände erfolgt als Liste von Ziffern.  Alle Ziffern,\n\
  \die angegeben wurden sind auch die Karten, die noch nicht umgedreht sind.\n\
  \So entspricht etwa \"12389\" einem Spielstand, bei dem die Karten 4, 5, 6\n\
  \und 7 bereits umgedreht sind.\n\
  \  Das Programm kennt verschiedene Modi, probieren sie es einfach mal aus!\n\
  \\nParameter:\n"

makeOptionen :: [String] -> Optionen
makeOptionen cmdArgs = if null errors then appliedOptions
  else error $ unlines $ "Folgende Fehler traten bei der Verarbeitung der\
    \Argumente auf:" : errors where
  (rawArgs,_,errors) = getOpt Permute optionsbeschreibungen cmdArgs
  -- Wir interpretieren unerkannte Sachen als Spielstand.
  appliedOptions = foldl (flip ($)) stdOptionen rawArgs

main :: IO ()
main = do
  rawArgs <- getArgs
  let optionen = makeOptionen rawArgs
      funktion = modus optionen
      zustand  = spielstand optionen
      n        = anzahlSpiele optionen
  case funktion of
    Hilfe -> putStrLn hilfe
    Analysiere -> putStrLn $ schoeneAnalyse zustand n
    SpieleAutomatisch -> do
      when (n < 0) (error $ printf "Anzahl Spiele (%d) negativ." n)
      gen <- getStdGen
      let n'           = fromIntegral n :: Double -- Hilfsfunktionen
          iterateM :: Monad m =>  (a -> m a) -> a -> Int -> m a
          iterateM _ !x 0 = return x
          iterateM f !x i = f x >>= \x' -> iterateM f x' (i - 1)
          -- Der nachfolgende Block berechnet die gesamte Statistik.
          schritt st = do -- Diese Funktion führt ein Spiel aus.
            wertung <- computerSpiel zustand
            return (Map.insertWith' (+) wertung 1 st)
          anzahlen     = Map.toList . fst . runState (iterateM schritt Map.empty n) $ gen
          relAnzahlen  = map (\(_,a) -> fromIntegral a / n' * 100) anzahlen
          zusammen     = zipWith (\(x,a) r -> (x,a,r)) anzahlen relAnzahlen
          durchschnitt = sum $ map
            (\(x,a) -> fromIntegral x * fromIntegral a / n') anzahlen
      printf "Statistik aus %d Spielen:\n\
             \Durchschnittliches Ergebnis: %f\n\
             \Verteilung der Ergebnisse:\n" n durchschnitt
      -- Wie auch oben: x: Ergebnisnummer, a: Anzahl, r: relative Anzahl
      mapM_ (\(x,a,r) -> printf "\t%2d: %5d (%5.2f%%)\n" x (a::Int) r) zusammen
