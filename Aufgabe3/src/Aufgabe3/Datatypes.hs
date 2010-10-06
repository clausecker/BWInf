{-# LANGUAGE BangPatterns #-}
module Aufgabe3.Datatypes
    ( TourSet (..)
    , Auftragsbuch (..)
    , auftragsbuchAlsListe
    , auftragsbuchAusListe
  )
  where

-- Typen, die vom Programm gebraucht werden

data TourSet = TourSet -- Dieser Datentyp vereinfacht die Berechnungen.
  { aZuB :: !Int
  , aZuC :: !Int
  , bZuA :: !Int
  , bZuC :: !Int
  , cZuA :: !Int
  , cZuB :: !Int
  } deriving (Eq,Ord)

instance Show TourSet where -- Wie eine Zeile des Eingabeformats
  show set = unwords $ map (show . ($ set)) [aZuB, aZuC, bZuA, bZuC, cZuA, cZuB]

data Auftragsbuch = Auftragsbuch -- Wie auch TourSet vereinfacht dieser Datentyp
  { montags     :: !TourSet      -- die Lösung
  , dienstags   :: !TourSet
  , mittwochs   :: !TourSet
  , donnerstags :: !TourSet
  , freitags    :: !TourSet
  , samstags    :: !TourSet
  } deriving (Eq,Ord)

instance Show Auftragsbuch where -- Wie Eingabe
  show = unlines . map show . auftragsbuchAlsListe

-- Zur Iteration über die Wochentage, etc.
auftragsbuchAlsListe :: Auftragsbuch -> [TourSet]
auftragsbuchAlsListe buch = map ($ buch) -- Endlich mal ein Zweck für ($)!
  [montags,dienstags,mittwochs,donnerstags,freitags,samstags]

auftragsbuchAusListe :: [TourSet] -> Auftragsbuch
auftragsbuchAusListe [mo,di,mi,dO,fr,sa] = Auftragsbuch mo di mi dO fr sa
auftragsbuchAusListe _ = error $ "Aufgabe3.Datatypes.auftragsbuchAusListe: \
  \Ungültige Eingabe"
