module Main where

  import System.Environment
  import Data.List
  import System.IO
  import System.IO.Error

  main :: IO ()
  main = getArgs >>= parseArguments

  parseArguments :: [String] -> IO ()
  parseArguments [] = putStrLn "Usage: 5-better-grep pattern [files...]"
  parseArguments (pattern:files) = filterFiles pattern files >> safeFilterFile pattern stdin

  filterFiles :: String -> [String] -> IO ()
  filterFiles pattern files = mapM_ (openAndFilterFile pattern) files

  openAndFilterFile :: String -> String -> IO ()
  openAndFilterFile pattern filePath = withFile filePath ReadMode (safeFilterFile pattern)

  safeFilterFile :: String -> Handle -> IO ()
  safeFilterFile pattern handle = catchIOError (filterFile pattern handle) ignoreEOFError

  ignoreEOFError :: IOError -> IO ()
  ignoreEOFError e = if isEOFError e then return () else putStrLn $ ioeGetErrorString e

  filterFile :: String -> Handle -> IO ()
  filterFile pattern handle = do
    line <- hGetLine handle
    if pattern `isPrefixOf` line then
      putStrLn line >> filterFile pattern handle
    else
      filterFile pattern handle
