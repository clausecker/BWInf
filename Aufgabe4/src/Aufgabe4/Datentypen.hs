{-# OPTIONS_JHC -fcpp -p function #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, CPP #-}
module Aufgabe4.Datentypen (
  Kartenspiel,
  Karte (..),
  Auswahl,
  startaufstellung,
  spielende,
  auswahlAnwendbar,
  wendeAuswahlAn,
  addKarte,
  punkte
) where

import Data.Ix
import Data.Bits

#ifndef __GLASGOW_HASKELL__
import Data.Function
#endif

-- Wir stellen das Kartenspiel als algebraischen Datentyp dar.
newtype Kartenspiel = Kartenspiel {fromKS :: Int}
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Ix,Bits,Num,Show)
#else
  deriving (Eq,Ord)

instance Ix Kartenspiel where
    range (Kartenspiel a, Kartenspiel b) = map Kartenspiel $ range (a,b)
    index (Kartenspiel a, Kartenspiel b) (Kartenspiel c) = index (a,b) c
    inRange (Kartenspiel a, Kartenspiel b) (Kartenspiel c) = inRange (a,b) c
    rangeSize (Kartenspiel a, Kartenspiel b) = rangeSize (a,b)

-- Weil JHC Num nicht als Voraussetzung für Bits ansieht, lasse ich das weg.
instance Bits Kartenspiel where
    a .&. b = Kartenspiel $ ((.&.) `on` fromKS) a b
    a .|. b = Kartenspiel $ ((.|.) `on` fromKS) a b
    xor a b = Kartenspiel $ (xor   `on` fromKS) a b
    complement (Kartenspiel a) = Kartenspiel $ complement a
    shift  (Kartenspiel a)     = Kartenspiel . shift  a
    rotate (Kartenspiel a)     = Kartenspiel . rotate a
    setBit (Kartenspiel a)     = Kartenspiel . setBit a
    clearBit (Kartenspiel a)   = Kartenspiel . clearBit a
    complementBit (Kartenspiel a) = Kartenspiel . complementBit a
    bit = Kartenspiel . bit
    testBit  (Kartenspiel a) = testBit a
    bitSize  (Kartenspiel a) = bitSize a
    isSigned (Kartenspiel a) = isSigned a
    shiftL   (Kartenspiel a) = Kartenspiel . shiftL a
    shiftR   (Kartenspiel a) = Kartenspiel . shiftR a
    rotateL  (Kartenspiel a) = Kartenspiel . rotateL a
    rotateR  (Kartenspiel a) = Kartenspiel . rotateR a
#endif

-- Berechnet die Anzahl der Punkte, die ein Kartenspiel bringt. Ist hier zwar
-- recht unpassend, muss aber sein um Kartenpspiel zu abstrahieren.
punkte :: Kartenspiel -> Int
punkte karten = punkte' (fromKS karten) 1 0 where
  punkte'  ks 10 k             = ks `seq` k
  punkte'  ks  n k | rest == 0 = punkte' ks' (n+1) (k+n)
                   | otherwise = punkte' ks' (n+1)  k    where
    (ks',rest) = ks `divMod` 2

-- Häufig gebrauchte Werte
startaufstellung, spielende :: Kartenspiel
startaufstellung = Kartenspiel 1023
spielende = Kartenspiel 0

data Karte
  = Karte1 | Karte2 | Karte3
  | Karte4 | Karte5 | Karte6
  | Karte7 | Karte8 | Karte9
 deriving (Enum)

-- Möglichkeit, eine Karte auszuwählen.
type Auswahl = Either Karte (Karte,Karte)

-- existiert die Karte im Kartenspiel?
haveKarte :: Karte -> Kartenspiel -> Bool
haveKarte k ks = testBit ks (fromEnum k)

addKarte :: Kartenspiel ->  Karte -> Kartenspiel
addKarte ks k = setBit ks (fromEnum k)

removeKarte :: Karte -> Kartenspiel -> Kartenspiel -- Wird nur hier gebraucht,
removeKarte k ks = clearBit ks (fromEnum k) -- so spar ich mir ein paar flips

auswahlAnwendbar :: Kartenspiel -> Auswahl -> Bool
auswahlAnwendbar spiel (Left karte) = haveKarte karte spiel
auswahlAnwendbar spiel (Right (a,b)) = haveKarte a spiel && haveKarte b spiel

wendeAuswahlAn ::  Kartenspiel -> Auswahl -> Kartenspiel
wendeAuswahlAn spiel (Left  karte) = removeKarte karte spiel
wendeAuswahlAn spiel (Right (a,b)) = removeKarte b . removeKarte a $ spiel
