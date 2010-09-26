{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
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
import System.Environment (getArgs)
import Control.Monad (when)

main :: IO ()
main = do
  ab <- auftragsbuchAusStdin
  args <- getArgs
  when (length args < 2)
    (fail "Es werden zwei Argumente benötigt.")
  putStrLn "Repräsentation der Eingabedaten:"
  print ab
  putStrLn "Anzahl der benötigten Fahrzeuge:"
  let (a,b,c) = benötigteFahrzeuge ab
  printf "Lager A: %3d\nLager B: %3d\nLager C: %3d\n" a b c
  printf "Insgesamt: %4d\n\n" (a + b + c)
  putStrLn "Optimiere..."
  let [s,i] = map read (take 2 args)
      ab' = optimiereFahrten s i ab
      (a',b',c') = benötigteFahrzeuge ab'
  printf "Lager A: %3d\nLager B: %3d\nLager C: %3d\n" a' b' c'
  printf "Insgesamt: %4d\n\n" (a' + b' + c')
