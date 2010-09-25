module Aufgabe3.IO
  ( auftragsbuchAusByteString
  , auftragsbuchAusDatei
  , auftragsbuchAusStdin
) where
-- Funktionen, die zur Ein- und Ausgabe benötigt werden

import Aufgabe3.Datatypes
import qualified Data.ByteString.Lazy.Char8 as B
--import System.IO
import Control.Monad (when)

-- Wir brauchen die fail Funktion.
auftragsbuchAusByteString :: Monad m => B.ByteString -> m Auftragsbuch
auftragsbuchAusByteString string = do
  let
    linedString     = B.lines string
    withoutComments = filter (\line -> B.words line /= [] && B.head line /= '#') linedString
  when (length withoutComments /= 6)
    (fail "Aufgabe3.IO: Ungültige Eingabe: Ungültige Anzahl an Tagen")
  tourSets <- tourSetAusByteString `mapM` withoutComments
  let auftragsbuch = auftragsbuchAusListe tourSets
  return $ auftragsbuch `seq` auftragsbuch -- Wir brauchen das strikt.

tourSetAusByteString :: Monad m => B.ByteString -> m TourSet
tourSetAusByteString string = do
  let wörter = B.words string
  when (length wörter /= 6)
    (fail "Aufgabe3.IO.tourSetAusByteString: Ungültige Anzahl an Touren.")
  zahlen <- maybe (fail $ "Aufgabe3.IO.tourSetAusByteString: Eingabe " ++
    "konnte nicht als Zahl verarbeitet werden.")
    (return . map fst) (mapM B.readInt wörter)
  return $ tourSetAusListe zahlen

auftragsbuchAusDatei :: FilePath -> IO Auftragsbuch
auftragsbuchAusDatei fp = do
  file <- B.readFile fp
  ab <- auftragsbuchAusByteString file
  ab `seq` return ab

auftragsbuchAusStdin :: IO Auftragsbuch
auftragsbuchAusStdin = do
  file <- B.getContents
  ab <- auftragsbuchAusByteString file
  ab `seq` return ab
