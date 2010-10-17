-- Hauptprogram
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Main (
  main
) where

import Aufgabe4.IO
import Aufgabe4.Datentypen (Kartenspiel,startaufstellung)
import System.Console.GetOpt
import System.Environment (getArgs)
import Control.Monad (when)
import System.Random
import Control.Monad.Writer (runWriter)
import Data.List (sort,group)
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
  , verbosity :: Int
  } deriving (Show)

stdOptionen :: Optionen
stdOptionen = Optionen Analysiere startaufstellung 1 0

optionsbeschreibungen :: [OptDescr (Optionen -> Optionen)]
optionsbeschreibungen =
  [ Option "?h" ["help"]    (NoArg $ \o -> o {modus = Hilfe}) "Hilfe anzeigen"
  , Option "a"  ["analyze"] (NoArg $ \o -> o {modus = Analysiere})
    "Spielstand analysieren"
  , Option "c"  ["auto"] (NoArg $ \o -> o {modus = SpieleAutomatisch})
    "den Computer spielen lassen"
  , Option "s"  ["spielstand"] (ReqArg (\s o -> o {spielstand =
    parseKartenspiel s}) "SPIELSTAND") "Zu verwendenden Spielstand setzen"
  , Option "n"  ["anzahl"] (ReqArg (\n o -> o {anzahlSpiele = read n}) "ZAHL")
    "Anzahl der Spiele setzen"
  , Option "v"  ["verbose"] (NoArg $ \o -> o {verbosity = 1 + verbosity o})
    "Gebe mehr Informationen aus"
  ]

hilfe :: String
hilfe = usageInfo header optionsbeschreibungen where
  header = "Lösung zur Aufgabe 4 des 29. Bundeswettbewerbs Informatik\n\
  \Team Herder, Robert Clausecker.\n\n\
  \Die Eingabe der Spielstände erfolgt als Liste von Ziffern.  Alle Ziffern,\n\
  \die angegeben wurden sind auch die Karten, die noch nicht umgedreht sind.\n\
  \So entspricht etwa \"12389\" einem Spielstand, bei dem die Karten 4, 5, 6\n\
  \und 7 bereits umgedrehts sind.\n\
  \  Das Programm kennt verschiedene Modi, probieren sie es einfach mal aus!"

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
      v        = verbosity optionen
  when (v >= 3) (putStrLn $ "Optionen: " ++ show optionen)
  when (v >= 2) (putStrLn $ "Führe Funktionsmodus aus: " ++ show funktion)
  case funktion of
    Hilfe -> putStrLn hilfe
    Analysiere -> putStrLn $ schoeneAnalyse zustand
    SpieleAutomatisch -> do
      when (n < 0) (error $ printf "Anzahl Spiele (%d) negativ." n)
      gen <- getStdGen
      let ((stats,_),msg) = runWriter $ spieleNmal n v gen zustand
          n'              = fromIntegral n :: Double
          gruppiert       = group $ sort stats
          anzahlen        = gruppiert >>= \x -> return (length x,head x)
          relAnzahlen     = map (\(x,_) -> fromIntegral x / n' * 100) anzahlen
          zusammen        = zipWith (\(a,x) r -> (a,r,x)) anzahlen relAnzahlen
          durchschnitt    = sum $ map (\x -> fromIntegral x / n') stats
      putStrLn msg -- Erstmal das ganze Geschwurbel ausgeben...
      printf "Statistik aus %d Spielen:\n\
             \Durchschnittliches Ergebnis: %f\n\
             \Verteilung der Ergebnisse:\n" n durchschnitt
      mapM_ (\(a,r,x) -> printf "\t%2d: %5d (%5.2f%%)\n" x a r) zusammen
