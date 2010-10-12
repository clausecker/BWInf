-- Modul zur Ein- und Ausgabe, Interaktion, etc.
-- Alle Funktionen geben Informationen nach ihrem verbosity level aus.
module Aufgabe4.IO where

import Aufgabe4.Datentypen
import Aufgabe4.Statistik
import Control.Monad.Writer
import Text.Printf
import Data.List (maximumBy)
import System.Random

{- Eingabeformat:
Das Kartenspiel wird als Ziffernfolge eingelesen und sollte recht selbsterklä-
rend sein.  Es werden alle Karten genannt, die noch nicht umgedreht sind, so ist
z. B. "12589" ein Kartenspiel, bei dem die Karten 3, 4, 6 und 7 umgedreht sind.
Verbosity:
  1: Kurzinformationen
  2: Detaillierte Informationen
  3: Debug
-}

parseKartenspiel :: String -> Maybe Kartenspiel
parseKartenspiel karten | all (`elem` ['1'..'9']) karten = Just resultat
                        | otherwise                      = Nothing where
  start    = Kartenspiel False False False False False False False False False
  resultat = foldl (flip addKarte) start $
    map (toEnum . pred . read . (:[])) karten

-- Funktion lässt den Computer einen Zug machen.
macheZug :: Int -> Int -> Kartenspiel -> Writer String Kartenspiel
macheZug verbosity augenzahl ks = do
  when (verbosity >= 3)
    (tell "\n\nBerechne mögliche Züge...")
  let kandidaten      = getKandidaten augenzahl ks
      tellZuege (a,w) = tell $ printf "\n  Auswahlmöglichkeit: %s\n\
        \  Bewertung: %s" (show a) (show w)
  -- Wir geben die Züge aus...
  when (verbosity >= 3) (mapM_ tellZuege kandidaten)
  -- Und ermitteln den besten
  if null kandidaten -- Besser Maybe?
    then do
      when (verbosity >= 2) (tell "\nKein Zug möglich. Spiel zu Ende")
      return ks
    else do
      let (zug,wertung) = maximumBy (\(_,x) (_,y) -> compare x y) kandidaten
      when (verbosity >= 2) (tell $ printf
        "\nZiehe %s\n  (Wertung: %s)" (show zug) (show wertung))
      return $ wendeAuswahlAn ks zug

-- Lässt den Computer gegen sich selbst spielen.  Das Ergebnis des Spieles wird
-- zurückgegeben.
computerSpiel :: RandomGen g => Int -> g -> Kartenspiel -> Writer String (Int,g)
computerSpiel v gen ks = do
  when (v >= 1) $ tell "\nSpiele gegen Computer... "
  when (v >= 2) $ tell $ "\nAnfangszustand: " ++ show ks
  -- Die Nachfolgende Schleife hat einen Parameter vom Typ
  --  RandomGen g => Writer (Kartenspiel,g,Bool)
  let spielschleife = until (\(Writer ((_,_,x),_)) -> x) $ \x -> do
        (karten,g,_) <- x
        let (auge1,g' ) = randomR (1,6) g  -- Erstes  Auge
            (auge2,g'') = randomR (1,6) g' -- Zweites Auge
            augen       = auge1 + auge2
        when (v >= 3) $ tell $ printf "\nWürfle...\n\
          \  Erstes  Auge: %d\n  Zweites Auge: %d" auge1 auge2
        when (v >= 2) $ tell $ printf "\nWürfle %d" augen
        karten' <- macheZug v augen karten
        if (karten' == karten) -- Spiel vorbei?
          then return (karten,g'',True)
          else do
            when (v >= 2) $ tell $ "Zustand nach Zug: " ++ show karten'
            return (karten',g'',False)
  (ks',g,_) <- spielschleife $ return (ks,gen,False)
  let wertung = punkte ks'
  when (v == 1) $ tell $ printf "(%d)" wertung
  when (v > 1) $ tell $ "Endergebnis: " ++ show wertung
  return (wertung,g)
