{-# LANGUAGE TupleSections #-}
module Aufgabe3.Algorithmen
  ( benoetigteFahrzeuge
  , benoetigteFahrzeugeGesamt
  , optimiereFahrten
) where

-- Dieses Modul enthält die Algorithmen, mit denen das Problem gelöst wird.

import Aufgabe3.Datatypes
--import Data.Tree
import Control.Applicative ((<$>))
import Data.Set (Set)
import qualified Data.Set as Set

{-
Lösungsansatz:

Die Funktion berechnet für jede Station für jeden Tag, wieviele Fahrzeuge an-
bzw. abfahren.  Anschließend wird aus diesen Werten mithilfe eines folds
iterativ die Anzahl der benötigten Fahrzeuge berechnet, indem diese Funktion
wiederholt angewendet wird.
-}

-- Diese Funktion ist die Hälfte der Lösung der 1. Teilaufgabe
-- Obwohl es möglich währe, diese zu generalisieren, lasse ich das bleiben.
benoetigteFahrzeuge :: Auftragsbuch -> (Int,Int,Int)
benoetigteFahrzeuge = (\((a,_),(b,_),(c,_)) -> (a,b,c))
  . foldl wendeTourSetAn ((0,0),(0,0),(0,0))
  . auftragsbuchAlsListe where
  -- (x,y) -> x: Anzahl der zu bestellenden Fahrzeuge,
  --          y: Anzahl der am Lager befindlichen Fahrzeuge
  wendeTourSetAn (lagerA,lagerB,lagerC) ts = (lagerA',lagerB',lagerC') where
    lagerUpdate (zuBest,befindlich) fahrtenHin fahrtenWeg =
      (zuBest',befindlich') where
      tmp = befindlich - fahrtenWeg
      befindlich' = if tmp <= 0
        then fahrtenHin
        else tmp + fahrtenHin
      zuBest'     = if tmp < 0
        then zuBest - tmp
        else zuBest
    lagerA' = lagerUpdate lagerA (bZuA ts + cZuA ts) (aZuB ts + aZuC ts)
    lagerB' = lagerUpdate lagerB (aZuB ts + cZuB ts) (bZuA ts + bZuC ts)
    lagerC' = lagerUpdate lagerC (aZuC ts + bZuC ts) (cZuA ts + cZuB ts)

benoetigteFahrzeugeGesamt :: Auftragsbuch -> Int
benoetigteFahrzeugeGesamt = (\(a,b,c) -> a + b + c) . benoetigteFahrzeuge

bfgx :: Auftragsbuch -> (Auftragsbuch,Int)
bfgx ab = (ab,benoetigteFahrzeugeGesamt ab)

{-
Aufgabenteil 2:

Die Anzahl der Fahrten wird optimiert, indem nach Verbesserungen gesucht wird.
Dazu wird ein Suchbaum aufgebaut, der in jeder Ebene alle möglichen Hinzufügun-
gen von Strecken enthält. In diesem Baum wird dann mithilfe der Tiefensuche
eine Optimierung gesucht:

  * Berechne die Anzahl der benötigten Fahrzeuge. Ist sie ...
    * Größer: Inkrementiere s, wenn s > Schwellwert, terminiere
    * Gleich: Inkrementiere s, wenn s > Schwellwert, terminiere
    * kleiner: wende Algorithmus rekursiv auf alle weiteren Veränderungen an.

Der Wert s wird benötigt, da es unter Umständen möglich ist, dass erst die Hin-
zufügung mehrerer Fahrten zu einer Verbesserung führt.

Dadurch, dass nur Fahrten hinzugefügt werden, ist eine Terminierung des Ver-
fahren gewährleistet.
-}

optimiereFahrten ::
  Int -> -- Maximale Anzahl an Versuchen, eine nicht verbessernde Änderung zu
         -- Benutzen.
  Int -> -- Maximale Iterationstiefe, bis zu der nicht verbessernde ausprobiert
  Auftragsbuch -> --werden
  Auftragsbuch
optimiereFahrten s i ab = fst $ fst $ optimiereFahrten'
  s s i Set.empty $ bfgx ab

----------
-- Hilfsfunktionen für Teil 2
----------

--Funktion zu Rekursionszwecken
optimiereFahrten' ::
  Int -> -- Schwellwert
  Int -> -- aktueller Schwellwert
  Int -> -- Maximale Iterationstiefe
  Set Auftragsbuch -> -- Ausnahmen
  (Auftragsbuch,Int) -> -- (Zu bearbeitendes Buch,Anzahl der Fahrzeuge)
  ((Auftragsbuch,Int), -- (Beste Lösung, Anzahl der Fahrzeuge)
  Set Auftragsbuch) -- Neue Ausnahmeliste

-- Rekursive Tiefensuche
-- cs: current s
optimiereFahrten' s cs i ausnahmen start@(ab,guete) = bestesErgebnis where
  -- Es kann angenommen werden, dass die Verbeserungen bereits analysiert wurden
  analysierteAenderungen =
    filter ((`Set.notMember` ausnahmen) . fst) $ map bfgx $ moeglicheAenderungen ab
  bessereAenderungen = filter ((< guete) . snd) analysierteAenderungen
  -- Hängt von s ab
  sonstigeAenderungen = if cs == 0 || i <=  0 then []
    else filter ((== guete) . snd) analysierteAenderungen
  ausnahmen' = foldl  -- Wir fügen nur die besseren Werte hinzu, da man so
    (flip Set.insert) -- erheblich Speicherplatz sparen kann
    ausnahmen
    (map fst bessereAenderungen)
  evaluierenUndVergleichen s' (vorschlag,set) neuer = (besserer,set') where
    (evaluiert,set') = optimiereFahrten' s s' (i - 1) set neuer
    besserer = if snd vorschlag < snd evaluiert then vorschlag else evaluiert
  (ergebnisBeste,ausnahmen'') = foldl -- Bester der sowieso besseren
    (evaluierenUndVergleichen s) -- s wird zurückgesetzt
    (start,ausnahmen')
    bessereAenderungen
  (ergebnisSonstige,ausnahmen''') = foldl -- Bester der sonst besseren
    (evaluierenUndVergleichen (cs-1))
    (start,ausnahmen'')
    sonstigeAenderungen
  bestesErgebnis = (,ausnahmen''') $ if snd ergebnisBeste < snd ergebnisSonstige
    then ergebnisBeste
    else ergebnisSonstige

moeglicheAenderungen :: Auftragsbuch -> [Auftragsbuch]
moeglicheAenderungen ab = nachWochentagen where
  -- Wir brauchen uns um Montag und Samstag nicht kümmern, es ist egal
  -- wo die Autos am Anfang und Ende stehen.
  -- Boilerplate
  (Auftragsbuch _ die mit don fre _) = ab
  nachWochentagen = concat [ -- Ich liebe record syntax!
    (\x -> ab {dienstags   = x}) <$> mts die,
    (\x -> ab {mittwochs   = x}) <$> mts mit,
    (\x -> ab {donnerstags = x}) <$> mts don,
    (\x -> ab {freitags    = x}) <$> mts fre]
  -- Wendet Änderungen Tageweise an.
  -- mts = modifiziereTourSet
  -- Boilerplate :-(
  mts (TourSet ab ac ba bc ca cb) =
    [ TourSet (ab + 1) ac ba bc ca cb
    , TourSet ab (ac + 1) ba bc ca cb
    , TourSet ab ac (ba + 1) bc ca cb
    , TourSet ab ac ba (bc + 1) ca cb
    , TourSet ab ac ba bc (ca + 1) cb
    , TourSet ab ac ba bc ca (cb + 1)
    ]
