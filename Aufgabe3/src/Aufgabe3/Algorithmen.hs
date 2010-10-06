{-# LANGUAGE TupleSections #-}
module Aufgabe3.Algorithmen
  ( benoetigteFahrzeuge
  , benoetigteFahrzeugeGesamt
  , optimiereFahrten
) where

-- Dieses Modul enthält die Algorithmen, mit denen das Problem gelöst wird.

import Aufgabe3.Datatypes
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
    * Größer: terminiere
    * Gleich: Inkrementiere cs; wenn c > s oder Iterationstiefe > i, terminiere,
              sonst verfahre wie unten.
    * kleiner: wende Algorithmus rekursiv auf alle weiteren Veränderungen an,
               Setze cs := 0.

Der Wert s wird benötigt, da es unter Umständen möglich ist, dass erst die Hin-
zufügung mehrerer Fahrten zu einer Verbesserung führt.

Dadurch, dass nur Fahrten hinzugefügt werden, ist eine Terminierung des Ver-
fahren gewährleistet.
-}

optimiereFahrten ::
  Int -> -- Parameter s
  Int -> -- Parameter i
  Auftragsbuch ->
  Auftragsbuch
optimiereFahrten s i ab = fst $ fst $ optimiereFahrten'
  s s i Set.empty $ bfgx ab

-- Hilfsfunktionen für Teil 2
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
    filter ((`Set.notMember` ausnahmen) . fst)
    . map bfgx $ moeglicheAenderungen ab
  bessereAenderungen = filter ((< guete) . snd) analysierteAenderungen
  sonstigeAenderungen = if cs == 0 || i <=  0 then []
    else filter ((== guete) . snd) analysierteAenderungen
  ausnahmen' = foldl  -- Wir fügen nur die besseren Werte hinzu, da man so
    (flip Set.insert) -- erheblich Speicherplatz sparen kann
    ausnahmen
    (map fst bessereAenderungen)
  evaluierenUndVergleichen s' (vorschlag,set) neuer = (besserer,set') where
    (evaluiert,set') = optimiereFahrten' s s' (i - 1) set neuer
    besserer = if snd vorschlag < snd evaluiert then vorschlag else evaluiert
  (ergebnisBeste,ausnahmen'') = foldl -- Bester Wert der Permutationen, die
    (evaluierenUndVergleichen s) -- die Fahrzeugzahl verringern.
    (start,ausnahmen') -- ^ s wird zurückgesetzt.
    bessereAenderungen
  (ergebnisSonstige,ausnahmen''') = foldl -- Bester Wert jener Permutationen,
    (evaluierenUndVergleichen (cs-1)) -- die den Fahrzeugwert nicht verändern.
    (start,ausnahmen'')
    sonstigeAenderungen
  bestesErgebnis = (,ausnahmen''') $ if snd ergebnisBeste < snd ergebnisSonstige
    then ergebnisBeste
    else ergebnisSonstige

moeglicheAenderungen :: Auftragsbuch -> [Auftragsbuch]
moeglicheAenderungen ab = nachWochentagen where
  (Auftragsbuch _ die mit don fre _) = ab     -- Wir brauchen uns um Montag und
  nachWochentagen = concat [                  -- Samstag nicht kümmern, es ist
    (\x -> ab {dienstags   = x}) <$> mts die, -- egal wo die Autos am Anfang und
    (\x -> ab {mittwochs   = x}) <$> mts mit, -- Ende stehen.
    (\x -> ab {donnerstags = x}) <$> mts don, -- Boilerplate
    (\x -> ab {freitags    = x}) <$> mts fre] -- PS: Ich liebe record syntax!

  mts (TourSet ab ac ba bc ca cb) =   -- Wendet Änderungen Tageweise an.
    [ TourSet (ab + 1) ac ba bc ca cb -- mts = modifiziereTourSet
    , TourSet ab (ac + 1) ba bc ca cb -- Boilerplate :-(
    , TourSet ab ac (ba + 1) bc ca cb
    , TourSet ab ac ba (bc + 1) ca cb
    , TourSet ab ac ba bc (ca + 1) cb
    , TourSet ab ac ba bc ca (cb + 1)
    ]
