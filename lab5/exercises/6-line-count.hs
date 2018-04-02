{-# OPTIONS -Wall #-}
module Main where

import Control.Monad
import Text.Read

main :: IO ()
main = parseLine >> main

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

splitByComma = wordsWhen (==',')

parseLine = do
  line <- getLine
  printOutput line $ convertNumbers $ splitByComma line

readDouble :: String -> Maybe Double
readDouble = readMaybe

printOutput :: String -> Maybe (Double, Double, Double) -> IO ()
printOutput line Nothing = print $ length line
printOutput _ (Just lineInfo) = print lineInfo

infinity = read "Infinity"

convertNumbers :: [String] -> Maybe (Double, Double, Double)
convertNumbers list = (sequence $ mapM readDouble list) >>= (\l -> Just $ foldr updateLineInfo (0, infinity, -infinity) l)

updateLineInfo :: Double -> (Double, Double, Double) -> (Double, Double, Double)
updateLineInfo x (sum, min', max') = (sum+x, min min' x, max max' x)