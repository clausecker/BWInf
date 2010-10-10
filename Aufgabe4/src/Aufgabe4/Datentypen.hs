{-# LANGUAGE BangPatterns #-}
module Aufgabe4.Datentypen (
    Kartenspiel (..),
    Karte (..),
    Auswahl,
    auswahlAnwendbar,
    wendeAuswahlAn,
    addKarte,
    removeKarte
  ) where

import Data.Bits ()

-- Wir stellen das Kartenspiel als algebraischen Datentyp dar.
data Kartenspiel = Kartenspiel
  { karte1 :: !Bool
  , karte2 :: !Bool
  , karte3 :: !Bool
  , karte4 :: !Bool
  , karte5 :: !Bool
  , karte6 :: !Bool
  , karte7 :: !Bool
  , karte8 :: !Bool
  , karte9 :: !Bool
  } deriving (Show)


data Karte
  = Karte1
  | Karte2
  | Karte3
  | Karte4
  | Karte5
  | Karte6
  | Karte7
  | Karte8
  | Karte9
  deriving (Show,Enum)

-- Möglichkeit, eine Karte auszuwählen.
type Auswahl = Either Karte (Karte,Karte)

-- Funktionen, um obrige Datentypen zu manipulieren.

-- existiert die Karte im Kartenspiel?
haveKarte :: Karte -> Kartenspiel -> Bool
haveKarte = ([karte1,karte2,karte3,karte4,karte5,karte6,karte7,karte8,karte9]
  !!) . fromEnum -- Funktion nimmt die Karte und liefert den Accessor zurück.

-- Setzt den Wert einer beliebigen Karte
changeKarte :: Karte -> Bool -> Kartenspiel -> Kartenspiel
changeKarte karte value spiel = sets !! (fromEnum karte) where
  sets =
    [ spiel { karte1  = value}
    , spiel { karte2  = value}
    , spiel { karte3  = value}
    , spiel { karte4  = value}
    , spiel { karte5  = value}
    , spiel { karte6  = value}
    , spiel { karte7  = value}
    , spiel { karte8  = value}
    , spiel { karte9  = value}
    ]

addKarte, removeKarte :: Karte -> Kartenspiel -> Kartenspiel
addKarte    = (flip changeKarte) True
removeKarte = (flip changeKarte) False

auswahlAnwendbar :: Kartenspiel -> Auswahl -> Bool
auswahlAnwendbar spiel (Left karte) = haveKarte karte spiel
auswahlAnwendbar spiel (Right (a,b)) = haveKarte a spiel && haveKarte b spiel

wendeAuswahlAn :: Kartenspiel -> Auswahl -> Kartenspiel
wendeAuswahlAn spiel (Left  karte) = removeKarte karte spiel
wendeAuswahlAn spiel (Right (a,b)) = removeKarte b . removeKarte a $ spiel
