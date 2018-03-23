module Main where

  checkWord :: String -> IO ()
  checkWord "unicorn" = putStrLn "Good!!!"
  checkWord _ = putStrLn "Wrong."
  
  main :: IO ()
  main = putStrLn "Say unicorn" >> getLine >>= checkWord