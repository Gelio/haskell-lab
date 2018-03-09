import Data.List

sumujListe :: (Num a) => [a] -> a
sumujListe = foldl (+) 0

sumujListeStrict :: (Num a) => [a] -> a
sumujListeStrict = foldl' (+) 0

plusPlus :: [a] -> [a] -> [a]
plusPlus [] a = a
plusPlus a [] = a
plusPlus x y = foldr (:) y x

myconcat :: [[a]] -> [a]
myconcat = foldl (flip $ foldr (:)) []