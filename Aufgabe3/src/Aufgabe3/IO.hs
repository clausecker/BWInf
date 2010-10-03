module Aufgabe3.IO
  ( auftragsbuchAusString
  , readAuftragsbuch
) where
-- Funktionen, die zur Ein- und Ausgabe benötigt werden

import Aufgabe3.Datatypes
import Control.Monad (when)
import System.IO (hGetContents, Handle)

-- Wir brauchen die fail Funktion.
auftragsbuchAusString :: Monad m => String -> m Auftragsbuch
auftragsbuchAusString string = do
  let
    linedString     = lines string
    withoutComments = --Filtert leere Zeilen und Zeilen die mit "#" beginnen.
      filter (\line -> words line /= [] && head line /= '#') linedString
  when (length withoutComments /= 6)
    (fail "Aufgabe3.IO: Ungültige Eingabe: Ungültige Anzahl an Tagen")
  tourSets <- tourSetAusString `mapM` withoutComments
  let auftragsbuch = auftragsbuchAusListe tourSets
  return $ auftragsbuch `seq` auftragsbuch -- Wir brauchen das strikt.

tourSetAusString :: Monad m => String -> m TourSet
tourSetAusString string = do
  let woerter = words string
  when (length woerter /= 6)
    (fail "Aufgabe3.IO.tourSetAusByteString: Ungültige Anzahl an Touren.")
  let zahlen = map read woerter
  return $ tourSetAusListe zahlen

readAuftragsbuch :: Handle -> IO Auftragsbuch
readAuftragsbuch h = do
  file <- hGetContents h
  auftragsbuchAusString file
