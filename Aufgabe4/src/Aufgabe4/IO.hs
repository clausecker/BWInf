-- Modul zur Ein- und Ausgabe, Interaktion, etc.
-- Alle Funktionen geben Informationen nach ihrem verbosity level aus.
module Aufgabe4.IO (
  parseKartenspiel,
  schoeneAnalyse,
  spieleNmal
)where

import Aufgabe4.Datentypen
import Aufgabe4.Statistik
import Control.Monad.Writer
import Text.Printf
import Data.List (maximumBy)
import System.Random
import Data.Maybe (fromJust)

{- Eingabeformat:
Das Kartenspiel wird als Ziffernfolge eingelesen und sollte recht selbsterklä-
rend sein.  Es werden alle Karten genannt, die noch nicht umgedreht sind, so ist
z. B. "12589" ein Kartenspiel, bei dem die Karten 3, 4, 6 und 7 umgedreht sind.
Verbosity:
  1: Kurzinformationen
  2: Detaillierte Informationen
  3: Debug
-}

parseKartenspielS :: String -> Maybe Kartenspiel
parseKartenspielS karten | all (`elem` ['1'..'9']) karten = Just resultat
                         | otherwise                      = Nothing where
  eingabeAlsKarte = map (toEnum . pred . read . (:[])) karten
  resultat = foldl (flip addKarte) spielende eingabeAlsKarte

-- Das selbe nur mit Exceptions
parseKartenspiel :: String -> Kartenspiel
parseKartenspiel karten | all (`elem` ['1'..'9']) karten = resultat
                        | otherwise                      = error errorMsg where
  resultat = fromJust (parseKartenspielS karten)
  errorMsg = "Aufgabe4.IO.parseKartenspiel: Ungültige Eingabe"

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
            when (v >= 2) $ tell $ "\nZustand nach Zug: " ++ show karten'
            return (karten',g'',False)
  (ks',g,_) <- spielschleife $ return (ks,gen,False)
  let wertung = punkte ks'
  when (v == 1) $ tell $ printf "(%d)" wertung
  when (v > 1) $ tell $ "\nEndergebnis: " ++ show wertung
  return (wertung,g)

spieleNmal :: RandomGen g => Int -> -- Anzahl der Spiele; wie oben
  Int -> g -> Kartenspiel -> Writer String ([Int],g)
spieleNmal 0 _ gen _  = return ([],gen)
spieleNmal n v gen ks = do
  when (v >= 1) $ tell $ printf "\nSpiele %d mal..." n
  when (v < -2) $ tell $ printf "\nNoch %d Spiele" n
  let v' | v > 0     = -v -- Hack: Wir wollen obige Nachricht nur einmal
         | otherwise =  v -- ausgeben.
  (resultat,gen') <- computerSpiel v gen ks
  (resultate,gen'') <- spieleNmal (n - 1) v' gen' ks
  return (resultat : resultate, gen'')

-- Analysiert einen Spielstand und gibt ein schönes Resultat aus.
schoeneAnalyse :: Kartenspiel -> String
schoeneAnalyse ks = printf "Erwartungswert: %f\nDetails:\n%s"
  (bewerteKartenspiel ks) ratings where
  kandidaten  = map (\n -> (n,getKandidaten n ks)) [2..12]
  showAuswahl (Left k)      = show $ (fromEnum k + 1)
  showAuswahl (Right (a,b)) = show (fromEnum a + 1) ++ ", " ++ show (fromEnum b + 1)
  ratings     = kandidaten >>= kandidatenliste -- Teil 1 der Ausgabe
  kandidatenliste (n,lst) = (printf "  Würfelergebnis %2d\nErwartungswert: %2f\
    \\nKandidaten:\n%s" n ew liste) :: String where
    ew | null lst  = punkte ks
       | otherwise = maximum $ map snd lst
    liste = lst >>= \(k, r) -> (printf "    Auswahl %-5s Erwartung %2f\n"
      (showAuswahl k ++ ",") r) :: String
