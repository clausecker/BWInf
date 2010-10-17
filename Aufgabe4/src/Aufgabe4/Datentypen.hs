{- # LANGUAGE BangPatterns # -}
module Aufgabe4.Datentypen (
  Kartenspiel (..),
  Karte (..),
  Auswahl,
  startaufstellung,
  auswahlAnwendbar,
  wendeAuswahlAn,
  addKarte,
) where

-- Wir stellen das Kartenspiel als algebraischen Datentyp dar.
data Kartenspiel = Kartenspiel
  { karte1 :: !Bool , karte2 :: !Bool , karte3 :: !Bool
  , karte4 :: !Bool , karte5 :: !Bool , karte6 :: !Bool
  , karte7 :: !Bool , karte8 :: !Bool , karte9 :: !Bool
  } deriving (Show,Eq)

data Karte
  = Karte1 | Karte2 | Karte3
  | Karte4 | Karte5 | Karte6
  | Karte7 | Karte8 | Karte9
 deriving (Show,Enum)

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
changeKarte :: Bool -> Karte -> Kartenspiel -> Kartenspiel
changeKarte x Karte1 ks = ks { karte1 = x}
changeKarte x Karte2 ks = ks { karte2 = x}
changeKarte x Karte3 ks = ks { karte3 = x}
changeKarte x Karte4 ks = ks { karte4 = x}
changeKarte x Karte5 ks = ks { karte5 = x}
changeKarte x Karte6 ks = ks { karte6 = x}
changeKarte x Karte7 ks = ks { karte7 = x}
changeKarte x Karte8 ks = ks { karte8 = x}
changeKarte x Karte9 ks = ks { karte9 = x}

startaufstellung :: Kartenspiel
startaufstellung = Kartenspiel True True True True True True True True True

addKarte, removeKarte :: Karte -> Kartenspiel -> Kartenspiel
addKarte    = changeKarte True
removeKarte = changeKarte False

auswahlAnwendbar :: Kartenspiel -> Auswahl -> Bool
auswahlAnwendbar spiel (Left karte) = haveKarte karte spiel
auswahlAnwendbar spiel (Right (a,b)) = haveKarte a spiel && haveKarte b spiel

wendeAuswahlAn :: Kartenspiel -> Auswahl -> Kartenspiel
wendeAuswahlAn spiel (Left  karte) = removeKarte karte spiel
wendeAuswahlAn spiel (Right (a,b)) = removeKarte b . removeKarte a $ spiel
