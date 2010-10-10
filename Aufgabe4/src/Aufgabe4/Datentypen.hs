{- # LANGUAGE BangPatterns # -}
module Aufgabe4.Datentypen (
  Kartenspiel (..),
  Karte (..),
  Auswahl,
  auswahlAnwendbar,
  wendeAuswahlAn,
  addKarte,
  removeKarte
) where

-- Wir stellen das Kartenspiel als algebraischen Datentyp dar.
data Kartenspiel = Kartenspiel
  { karte1 :: !Bool , karte2 :: !Bool , karte3 :: !Bool
  , karte4 :: !Bool , karte5 :: !Bool , karte6 :: !Bool
  , karte7 :: !Bool , karte8 :: !Bool , karte9 :: !Bool
  } deriving (Show)

data Karte
  = Karte1 | Karte2 | Karte3
  | Karte4 | Karte5 | Karte6
  | Karte7 | Karte8 | Karte9
 deriving (Show)

-- Möglichkeit, eine Karte auszuwählen.
type Auswahl = Either Karte (Karte,Karte)

-- existiert die Karte im Kartenspiel?
haveKarte :: Karte -> Kartenspiel -> Bool
haveKarte Karte1 = karte1
haveKarte Karte2 = karte2
haveKarte Karte3 = karte3
haveKarte Karte4 = karte4
haveKarte Karte5 = karte5
haveKarte Karte6 = karte6
haveKarte Karte7 = karte7
haveKarte Karte8 = karte8
haveKarte Karte9 = karte9

-- Setzt den Wert einer beliebigen Karte
changeKarte :: Karte -> Bool -> Kartenspiel -> Kartenspiel
changeKarte Karte1 x ks = ks { karte1 = x}
changeKarte Karte2 x ks = ks { karte2 = x}
changeKarte Karte3 x ks = ks { karte3 = x}
changeKarte Karte4 x ks = ks { karte4 = x}
changeKarte Karte5 x ks = ks { karte5 = x}
changeKarte Karte6 x ks = ks { karte6 = x}
changeKarte Karte7 x ks = ks { karte7 = x}
changeKarte Karte8 x ks = ks { karte8 = x}
changeKarte Karte9 x ks = ks { karte9 = x}


addKarte, removeKarte :: Karte -> Kartenspiel -> Kartenspiel
addKarte    = (flip changeKarte) True
removeKarte = (flip changeKarte) False

auswahlAnwendbar :: Kartenspiel -> Auswahl -> Bool
auswahlAnwendbar spiel (Left karte) = haveKarte karte spiel
auswahlAnwendbar spiel (Right (a,b)) = haveKarte a spiel && haveKarte b spiel

wendeAuswahlAn :: Kartenspiel -> Auswahl -> Kartenspiel
wendeAuswahlAn spiel (Left  karte) = removeKarte karte spiel
wendeAuswahlAn spiel (Right (a,b)) = removeKarte b . removeKarte a $ spiel

