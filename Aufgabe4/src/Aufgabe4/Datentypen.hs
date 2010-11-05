module Aufgabe4.Datentypen (
  Kartenspiel (..),
  Karte (..),
  Auswahl,
  startaufstellung,
  spielende,
  auswahlAnwendbar,
  wendeAuswahlAn,
  addKarte
) where

import Data.Ix

-- Wir stellen das Kartenspiel als algebraischen Datentyp dar.
data Kartenspiel = Kartenspiel
  { karte1 :: !Bool , karte2 :: !Bool , karte3 :: !Bool
  , karte4 :: !Bool , karte5 :: !Bool , karte6 :: !Bool
  , karte7 :: !Bool , karte8 :: !Bool , karte9 :: !Bool
  } deriving (Eq,Ord,Ix)

-- Häufig gebrauchte Werte
startaufstellung, spielende :: Kartenspiel
startaufstellung = Kartenspiel True True True True True True True True True
spielende = Kartenspiel False False False False False False False False False

data Karte
  = Karte1 | Karte2 | Karte3
  | Karte4 | Karte5 | Karte6
  | Karte7 | Karte8 | Karte9
 deriving (Enum)

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
changeKarte :: Bool -> Kartenspiel -> Karte -> Kartenspiel
changeKarte x ks Karte1 = ks { karte1 = x}
changeKarte x ks Karte2 = ks { karte2 = x}
changeKarte x ks Karte3 = ks { karte3 = x}
changeKarte x ks Karte4 = ks { karte4 = x}
changeKarte x ks Karte5 = ks { karte5 = x}
changeKarte x ks Karte6 = ks { karte6 = x}
changeKarte x ks Karte7 = ks { karte7 = x}
changeKarte x ks Karte8 = ks { karte8 = x}
changeKarte x ks Karte9 = ks { karte9 = x}

addKarte :: Kartenspiel ->  Karte -> Kartenspiel
addKarte = changeKarte True

removeKarte :: Karte -> Kartenspiel -> Kartenspiel -- Wird nur hier gebraucht,
removeKarte = flip $ changeKarte False -- so spar ich mir ein paar flips

auswahlAnwendbar :: Kartenspiel -> Auswahl -> Bool
auswahlAnwendbar spiel (Left karte) = haveKarte karte spiel
auswahlAnwendbar spiel (Right (a,b)) = haveKarte a spiel && haveKarte b spiel

wendeAuswahlAn ::  Kartenspiel -> Auswahl -> Kartenspiel
wendeAuswahlAn spiel (Left  karte) = removeKarte karte spiel
wendeAuswahlAn spiel (Right (a,b)) = removeKarte b . removeKarte a $ spiel
