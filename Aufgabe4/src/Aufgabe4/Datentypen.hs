{-# LANGUAGE BangPatterns #-}
module Aufgabe4.Datentypen (
    Kartenspiel (..),
    Wuerfel (..),
    ZweiWuerfel
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
  } deriving (Eq,Ord,Enum,Bounded,Show)

-- Was heißt Bits? ... Genau!
instance Bits Kartenspiel where
  Kartenspiel (a1 a2 a3 a4 a5 a6 a7 a8 a9) .&.
    Kartenspiel (b1 b2 b3 b4 b5 b6 b7 b8 b9) =
      Kartenspiel (
        (a1 && b1) (a2 && b2) (a3 && b3)
        (a4 && b4) (a5 && b5) (a6 && b6)
        (a7 && b7) (a8 && b8) (a9 && b9)
      )
  Kartenspiel (a1 a2 a3 a4 a5 a6 a7 a8 a9) .|.
    Kartenspiel (b1 b2 b3 b4 b5 b6 b7 b8 b9) =
      Kartenspiel (
        (a1 || b1) (a2 || b2) (a3 || b3)
        (a4 || b4) (a5 || b5) (a6 || b6)
        (a7 || b7) (a8 || b8) (a9 || b9)
      )
  -- TODO: Boilerplate beenden

-- Um die Typsicherheit zu erhöhen, wird ein Würfel als Aufzählung dargestellt.

data Wuerfel = Auge1
             | Auge2
             | Auge3
             | Auge4
             | Auge5
             | Auge6
             deriving(Show,Bounded,Eq,Ord)

-- Wir hätten gerne Auge1 = 1, also leiten wir Enum selbst ab.
instance Enum Wuerfel where
  toEnum 1 = Auge1
  toEnum 2 = Auge2
  toEnum 3 = Auge3
  toEnum 4 = Auge4
  toEnum 5 = Auge5
  toEnum 6 = Auge6
  toEnum _ = error "Enum.toEnum.Wuerfel: Ungültiges Argument"
  fromEnum Auge1 = 1
  fromEnum Auge2 = 2
  fromEnum Auge3 = 3
  fromEnum Auge4 = 4
  fromEnum Auge5 = 5
  fromEnum Auge6 = 6
  -- den Rest macht die Defaultimplentierung für uns.

-- Wir Repräsentieren ein paar Augen als Tupel

type ZweiWuerfel = (Wuerfel,Wuerfel)

--
