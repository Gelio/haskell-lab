{-# OPTIONS -Wall #-}
module Main where

import System.Environment
import Control.Monad
import IO

main :: IO ()
main = getArgs >>= head . (map wc) >> return ()

wc :: String -> IO ()
wc fileName = readFile fileName >>= (printOutput . (foldl processCharacter (0, 0)))
  where
    processCharacter ::  (Int, Int) -> Char -> (Int, Int)
    processCharacter (bytes, lines) c
      | c == '\n' = (bytes + 1, lines + 1)
      | otherwise = (bytes + 1, lines)
    
    printOutput :: (Int, Int) -> IO ()
    printOutput (bytes, lines) = putStrLn $ "Bytes: " ++ (show bytes) ++ ", lines: " ++ (show lines)
