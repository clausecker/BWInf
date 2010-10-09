{-# LANGUAGE BangPatterns #-}
module Aufgabe4.Datentypen {- (
    Kartenspiel (..),
    Karte (..),
    Auswahl
  ) -} where

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
  } deriving (Eq,Ord,Show)


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
  deriving (Show,Ord,Eq,Enum,Bounded)

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

auswahlAnwendbar :: Auswahl -> Kartenspiel -> Bool
auswahlAnwendbar (Left karte)  spiel = haveKarte karte spiel
auswahlAnwendbar (Right (a,b)) spiel = haveKarte a spiel && haveKarte b spiel

wendeAuswahlAn :: Auswahl -> Kartenspiel -> Kartenspiel
wendeAuswahlAn (Left  karte) = removeKarte karte
wendeAuswahlAn (Right (a,b)) = removeKarte b . removeKarte a
