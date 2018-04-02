module NDSort where

import Data.List

ndSort :: Ord a => [a] -> [a]
ndSort = ndSortStep []

ndSortStep sortedList [] = sortedList
ndSortStep sortedList elementsLeft = do
  (head, newElementsLeft) <- splitElements sortedList elementsLeft
  ndSortStep (head:sortedList) newElementsLeft

splitElements :: Ord a => [a] -> [a] -> [(a, [a])]
splitElements _ [] = []
splitElements [] (x:xs) = (x, xs):nextElements
  where
    rest = splitElements [] xs
    nextElements = map (\(head, elements) -> (head, x:elements)) rest
splitElements sortedList@(head:rest) (x:xs)
  | x <= head = (x, xs):nextElements
  | otherwise = nextElements
  where
    rest = splitElements sortedList xs
    nextElements = map (mapSnd (x:)) rest

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)
