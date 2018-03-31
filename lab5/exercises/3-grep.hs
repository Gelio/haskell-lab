module Main where
  import System.Environment
  import Data.List
  import System.IO

  main :: IO ()
  main = getArgs >>= checkParam . head' 
  
  checkParam :: Maybe String -> IO ()
  checkParam param = case param of
    Nothing -> putStrLn "Usage: 3-grep pattern"
    Just pattern -> filterByPattern pattern
  
  filterByPattern :: String -> IO ()
  filterByPattern pattern = do
    done <- isEOF
    if done then
      return ()
    else do
      line <- getLine
      if pattern `isPrefixOf` line then
        putStrLn line >> filterByPattern pattern
      else
        filterByPattern pattern

  head' :: [a] -> Maybe a
  head' [] = Nothing
  head' (x:_) = Just x