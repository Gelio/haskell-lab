{-# OPTIONS -Wall #-}
module Main where

import Control.Monad
import Text.Read
import System.IO.Error
import System.IO

main :: IO ()
main = catchIOError (parseLine >> main) ignoreEOFError

ignoreEOFError :: IOError -> IO ()
ignoreEOFError e
  | isEOFError e = return ()
  | otherwise = ioError e

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

splitByComma :: String -> [String]
splitByComma = wordsWhen (==',')

parseLine = do
  line <- getLine
  printOutput line $ convertNumbers $ splitByComma line

readDouble :: String -> Maybe Double
readDouble = readMaybe

printOutput :: String -> Maybe (Double, Double, Double) -> IO ()
printOutput line Nothing = print $ length line
printOutput _ (Just lineInfo) = print lineInfo

infinity :: Double
infinity = read "Infinity"

convertNumbers :: [String] -> Maybe (Double, Double, Double)
convertNumbers list = mapM readDouble list >>= Just . (updateFst (/ listLength)) . (foldr updateLineInfo (0, infinity, -infinity))
  where
    listLength = fromIntegral $ length list

updateLineInfo :: Double -> (Double, Double, Double) -> (Double, Double, Double)
updateLineInfo x (sum', min', max') = (sum'+x, min min' x, max max' x)

updateFst :: (a -> b) -> (a, c, d) -> (b, c, d)
updateFst f (a, b, c) = (f a, b, c)
