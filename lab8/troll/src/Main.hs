module Main where

import Text.Read (readMaybe)
import Control.Monad

readRetry :: Read a => IO a
readRetry = do
  line <- getLine
  case readMaybe line of
    Nothing -> putStrLn "Wrong format, try again" >> readRetry
    Just x -> return x

retryRetry :: Read a => String -> IO a
retryRetry s = putStrLn s >> readRetry

tellStory :: [String] -> IO ()
tellStory = mapM_ (\line -> putStrLn line >> putStrLn continueMsg >> getLine)
  where
     continueMsg = "przerwa budująca napięcie. wciśnij return aby kontynuować"

main :: IO ()
main = do
  putStrLn "hello world"
