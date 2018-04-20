module Main where

import Text.Read (readMaybe)
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict

-- Helper functions

readRetry :: Read a => IO a
readRetry = do
  line <- getLine
  case readMaybe line of
    Nothing -> putStrLn "Złe dane, spróbuj ponownie" >> readRetry
    Just x -> return x

retryRetry :: Read a => String -> IO a
retryRetry s = putStrLn s >> readRetry

tellStory :: [String] -> IO ()
tellStory = mapM_ (\line -> putStrLn line >> putStrLn continueMsg >> getLine)
  where
     continueMsg = "przerwa budująca napięcie. wciśnij return aby kontynuować"



-- Game implementation
type STIO a = StateT (Double, Double) IO a

data AType = Pieta | Wlosy | Dowcip deriving (Read, Show)
data Atak = Atak AType Double Double

obrazeniaTrolla :: Atak -> Double
obrazeniaTrolla (Atak Pieta f _) = f
obrazeniaTrolla (Atak Wlosy f v) = f+v
obrazeniaTrolla (Atak Dowcip f l) = (1/f)*l

obrazeniaGracza :: Atak -> Double
obrazeniaGracza (Atak Pieta _ v) = v
obrazeniaGracza (Atak Wlosy _ v) = v
obrazeniaGracza (Atak Dowcip f _) = f

intro = ["Pascal obudzil sie dzis wczesniej niz zwykle.",
  "Przypomniala mu sie piekna monada z sasiedniego krolestwa jezykow funkcyjnych.",
  "Chyba to wiosna.", "Zew przygody.", "Guardy, rekurencje, strzalki, funkcje.",
  "Wreszcie bez tych przebrzydlych petli i klas.", "Dziedziczenia jakiegos.",
  "Pascal wyruszyl w swoja podroz zycia. Ku pieknej monadzie.",
  "Jednak gdy juz byl o krok od przejscia w lepszy swiat, na moscie do niego droge zastapila mu zla trollica.",
  "Byla to instrukcja GOTO.", "Aby programowac funkcyjnie, trzeba najpierw pokonac swe demony."]

outro_ginieTroll = ["I dla instrukcji GOTO juz,", "ostatni to dzien,", "bo smialy programista funkcyjny,", "uderza wen!"]

outro_ginieGracz = ["Czasami tak bywa,", "pies utonal, lancuch plywa,",
  "demony programowania imperatywnego znow cie pokonaly,", "czy programowac w Haskellu nigdy nie dasz rady?"]

outro_ginaOboje = ["Zgineli oboje.", "Podobno po smierci kazdy trafia tam,", "gdzie wierzyl, ze trafi, za zycia.",
  "Moze w zaswiatach Pascal pozna wreszcie jakas mila monade i spedzi z nia wiecznosc.",
  "Moze instrukcja GOTO tam wlasnie pozna jakiegos milosnika.",
  "W koncu kazda potwora znajdzie swego amatora."]

dobierzTekstDoWyniku :: (Double, Double) -> [String]
-- Założyłem, że pierwszy element krotki to życie trolla, a drugi to życie gracza
-- więc żeby nie przepisywać całości kodu, to zamieniłem kolejność w poniższym nagłówku
dobierzTekstDoWyniku (zycieTrolla, zycieGracza)
   | zycieGracza <= 0 && zycieTrolla <= 0 = outro_ginaOboje
   | zycieGracza <= 0 = outro_ginieGracz
   | zycieTrolla <= 0 = outro_ginieTroll
   | otherwise = error "Cos poszlo bardzo nie tak"

shouldStillPlay :: (Double, Double) -> Bool
shouldStillPlay (tHealth, pHealth) = tHealth >= 0 && pHealth >= 0

readAtak :: IO Atak
readAtak = Atak <$> retryRetry "Podaj typ ataku (Pieta, Wlosy, Dowcip)" <*> retryRetry "Podaj pierwszy parametr" <*> retryRetry "Podaj drugi parametr"

wykonajAtak :: Atak -> (Double, Double) -> (Double, Double)
wykonajAtak atak (tHealth, pHealth) = (tHealth - pHit, pHealth - tHit)
  where
    pHit = obrazeniaGracza atak
    tHit = obrazeniaTrolla atak

makePlayerMove :: STIO ()
makePlayerMove = do
  atak <- lift readAtak
  state <- get
  let newState = wykonajAtak atak state in
    put newState

displayState :: (Double, Double) -> IO ()
displayState (tHealth, pHealth) = displayPHealth >> displayTHealth
  where
    displayTHealth = putStrLn ("Punkty zycia trolla: " ++ (show tHealth))
    displayPHealth = putStrLn ("Punkty zycia gracza: " ++ (show pHealth))

gameLoop :: STIO ()
gameLoop = do
  state <- get
  lift $ displayState state
  if shouldStillPlay state then
    makePlayerMove >> gameLoop
  else
    lift $ tellStory $ dobierzTekstDoWyniku state


main :: IO ()
main = do
  tellStory intro
  void $ execStateT gameLoop (100, 100)
