-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main (
  main
) where

import Aufgabe3.Algorithmen
import Aufgabe3.IO
import Text.Printf

main :: IO ()
main = do
  ab <- auftragsbuchAusStdin
  putStrLn "Repräsentation der Eingabedaten:"
  print ab
  putStrLn "Anzahl der benötigten Fahrzeuge:"
  let (a,b,c) = benötigteFahrzeuge ab
  printf "Lager A: %3d\nLager B: %3d\nLager C: %3d" a b c
  printf "Insgesamt: %4d" (a + b + c)
