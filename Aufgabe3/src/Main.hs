{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Main (
  main
) where

import Aufgabe3.Algorithmen
import Aufgabe3.IO
import Text.Printf
import System.Environment (getArgs)
import Control.Monad (when)
import System.Console.GetOpt
import System.IO
import System.Exit (exitSuccess)

data Options = Options
  { inputFile    :: FilePath
  , outputFile   :: FilePath
  , machTeil1    :: Bool
  , machTeil2    :: Bool
  , getI         :: Int
  , getS         :: Int
  , printHelp    :: Bool
  }

defaultOptions :: Options
defaultOptions   = Options
  { inputFile    = "-" -- stdin
  , outputFile   = "-" -- stdout
  , machTeil1    = False
  , machTeil2    = False
  , getI         = 3
  , getS         = 1
  , printHelp    = False
  }

options :: [OptDescr (Options -> Options)]
options =
 [ Option ['o'] ["output","output-file"]
   (ReqArg (\s o -> o {outputFile = s}) "DATEI")
   "Setzt die Ausgabe-Datei"
 , Option ['1'] ["teil-1"]
   (NoArg (\o -> o {machTeil1 = True}))
   "Löse Teil 1 der Aufgabe"
 , Option ['2'] ["teil-2"]
   (NoArg (\o -> o {machTeil2 = True}))
   "Löse Teil 2 der Aufgabe"
 , Option ['i'] []
   (ReqArg (\i o -> o {getI = read i}) "ZAHL")
   "Setze den Parameter i"
 , Option ['s'] []
   (ReqArg (\s o -> o {getS = read s}) "ZAHL")
   "Setze den Parameter s"
 , Option ['h','?'] ["help"]
   (NoArg (\o -> o {printHelp = True}))
   "Gebe diese Hilfe aus und beende das Programm."
 ]

usage :: String
usage = usageInfo usageString options where
  usageString =
    "Lösung zur Aufgabe 3 des 29. BWInf.\n\
    \Geschrieben vom Team Herder, Robert Clausecker.\n\
    \Für detailierte Informationen schlagen Sie bitte in der Dokumentation\n\
    \nach, die sich im Verzeichnis doc/ befindet."


makeArguments :: [String] -> Options
makeArguments cmdArgs = if null errors
  then parsedOptions
  else error $ unlines $ "Folgende Fehler traten bei der Verarbeitung der\
    \Argumente auf:" : errors
  where
  (rawArgs,optional,errors) = getOpt Permute options cmdArgs
  appliedOptions = foldl (flip ($)) defaultOptions rawArgs
  parsedOptions = case optional of
    []      -> appliedOptions
    [input] -> appliedOptions { inputFile = input }
    _       -> error "Kann nicht mehr als eine Eingabedatei verarbeiten."

main :: IO ()
main = do
  rawArgs <- getArgs
  let args = makeArguments rawArgs
      s  = getS args
      i  = getI args
      ip = inputFile args
      op = outputFile args
  inFile  <- if ip == "-" then return stdin  else openFile ip ReadMode
  outFile <- if op == "-" then return stdout else openFile op WriteMode
  when (printHelp args)
    (hPutStrLn outFile usage >> exitSuccess)
  ab <- readAuftragsbuch inFile
  let (a,b,c)    = benoetigteFahrzeuge ab
      gesamt     = a + b + c
      ab'        = optimiereFahrten s i ab
      (a',b',c') = benoetigteFahrzeuge ab'
      gesamt'    = a' + b' + c'
  when (machTeil1 args)
    (hPrintf outFile "#Teilaufgabe 1:\n#Benötigte Fahrzeuge:\n\
      \#Lager 1: %3d\n#Lager 2: %3d\n#Lager 3: %3d\n#Gesamt:  %3d\n"
      a b c gesamt)
  when (machTeil2 args)
    (do hPutStrLn outFile "#Teilaufgabe 2:"
        hPrint outFile ab
        hPrintf outFile "#Benötigte Fahrzeuge:\n\
          \#Lager 1: %3d\n#Lager 2: %3d\n#Lager 3: %3d\n#Gesamt:  %3d\n"
          a' b' c' gesamt')
