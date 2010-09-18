module Aufgabe3.Datatypes
    ( Wochentag (..)
    , Lager (..)

    , Tour (..)
    , TourSet (..)
    , tourAnwenden
  )
  where

-- Typen, die vom Programm gebraucht werden.

import Control.Exception (assert)

data Wochentag
  = Montag
  | Dienstag
  | Mittwoch
  | Donnerstag
  | Freitag
  | Samstag
  | Sonntag
  deriving (Show,Eq,Enum,Bounded,Ord)

data Lager = LagerA | LagerB | LagerC
  deriving (Show,Eq,Enum,Bounded,Ord)

data Tour = Tour
  { start   :: Lager
  , ziel    :: Lager
  , fahrten :: Int -- Anzahl der Fahrzeuge, die fahren.
  } deriving (Show,Eq)

-- Der folgende Datentyp wird gebraucht, um die Berechnung der Fahrten zu vereinfachen.
data TourSet = TourSet
  { aZuB :: Int
  , aZuC :: Int
  , bZuA :: Int
  , bZuC :: Int
  , cZuA :: Int
  , cZuB :: Int
  } deriving (Show,Eq)

-- Hilfsfunktion, wendet eine Tour auf das Set an.
tourAnwenden :: Tour -> TourSet -> TourSet
tourAnwenden (Tour LagerA LagerB f) set = set { aZuB = aZuB set + f}
tourAnwenden (Tour LagerA LagerC f) set = set { aZuC = aZuC set + f}
tourAnwenden (Tour LagerB LagerA f) set = set { bZuA = bZuA set + f}
tourAnwenden (Tour LagerB LagerC f) set = set { bZuC = bZuC set + f}
tourAnwenden (Tour LagerC LagerA f) set = set { cZuA = cZuA set + f}
tourAnwenden (Tour LagerC LagerB f) set = set { cZuB = cZuB set + f}
tourAnwenden _                      set = set -- start == ziel

-- Auf diese Weise ist die Struktur einfacher
data Auftragsbuch = Auftragsbuch
  { montags     :: TourSet
  , dienstags   :: TourSet
  , mittwochs   :: TourSet
  , donnerstags :: TourSet
  , freitags    :: TourSet
  , samstags    :: TourSet
  , sonntags    :: TourSet
  } deriving (Show,Eq)

-- Zur Iteration über die Wochentage, etc.
auftragsbuchAlsListe :: Auftragsbuch -> [TourSet]
auftragsbuchAlsListe buch = map ($ buch) -- Endlich mal ein Zweck für ($)!
  [montags,dienstags,mittwochs,donnerstags,freitags,samstags,sonntags]
