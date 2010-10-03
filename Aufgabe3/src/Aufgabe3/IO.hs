module Aufgabe3.IO
  ( auftragsbuchAusByteString
  , auftragsbuchAusDatei
  , auftragsbuchAusStdin
) where
-- Funktionen, die zur Ein- und Ausgabe benötigt werden

import Aufgabe3.Datatypes
import qualified Data.ByteString.Lazy.Char8 as B
import Control.Monad (when)

-- Wir brauchen die fail Funktion.
auftragsbuchAusByteString :: Monad m => B.ByteString -> m Auftragsbuch
auftragsbuchAusByteString string = do
  let
    linedString     = B.lines string
    withoutComments = --Filtert leere Zeilen und Zeilen die mit "#" beginnen.
      filter (\line -> B.words line /= [] && B.head line /= '#') linedString
  when (length withoutComments /= 6)
    (fail "Aufgabe3.IO: Ungültige Eingabe: Ungültige Anzahl an Tagen")
  tourSets <- tourSetAusByteString `mapM` withoutComments
  let auftragsbuch = auftragsbuchAusListe tourSets
  return $ auftragsbuch `seq` auftragsbuch -- Wir brauchen das strikt.

tourSetAusByteString :: Monad m => B.ByteString -> m TourSet
tourSetAusByteString string = do
  let woerter = B.words string
  when (length woerter /= 6)
    (fail "Aufgabe3.IO.tourSetAusByteString: Ungültige Anzahl an Touren.")
  zahlen <- maybe (fail $ "Aufgabe3.IO.tourSetAusByteString: Eingabe " ++
    "konnte nicht als Zahl verarbeitet werden.")
    (return . map fst) (mapM B.readInt woerter)
  return $ tourSetAusListe zahlen

auftragsbuchAusDatei :: FilePath -> IO Auftragsbuch
auftragsbuchAusDatei fp = do
  file <- B.readFile fp
  auftragsbuchAusByteString file

auftragsbuchAusStdin :: IO Auftragsbuch
auftragsbuchAusStdin = do
  file <- B.getContents
  auftragsbuchAusByteString file
