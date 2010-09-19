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
) where

import Aufgabe3.Algorithmen
import Aufgabe3.IO

main :: IO ()
main = do
  ab <- auftragsbuchAusStdin
  print ab
