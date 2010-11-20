-- Hauptmodul, parst Argumente und ruft die nötigen Funktion auf.
{-#LANGUAGE BangPatterns #-}
module Main (
  main
) where

import Aufgabe4.Datentypen (Kartenspiel,startaufstellung)
import Aufgabe4.IO

import System.Console.GetOpt
import System.Environment (getArgs)
import System.Random

data Modus
  = Hilfe
  | SpieleAutomatisch
  | Analysiere

data Optionen = Optionen
  { modus :: Modus
  , spielstand :: Kartenspiel
  , anzahlSpiele :: Int
  }

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
    parseKartenspiel s}) "SPIELSTAND") "Spielstand setzen"
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
  else error . unlines $ "Folgende Fehler traten bei der Verarbeitung der \
    \Argumente auf:" : errors where
  (rawArgs,_,errors) = getOpt Permute optionsbeschreibungen cmdArgs
  -- Wir interpretieren unerkannte Sachen als Spielstand.
  appliedOptions = foldl (flip ($)) stdOptionen rawArgs

main :: IO ()
main = do
  rawArgs <- getArgs
  let (Optionen funktion zustand n) = makeOptionen rawArgs
  case funktion of
    Hilfe -> putStrLn hilfe
    Analysiere -> putStrLn $ schoeneAnalyse zustand n
    SpieleAutomatisch -> getStdGen >>= putStrLn . spielauswertung zustand n
