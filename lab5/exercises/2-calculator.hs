module Main where

  main :: IO ()
  main = do
    opChar <- getLine
    op <- return $ readOperationMaybe opChar
    arg1 <- readDouble
    arg2 <- readDouble
    putStrLn $ show $ do
      op1 <- op
      return $ op1 arg1 arg2


  readDouble :: IO Double
  readDouble = readLn

  readOperationMaybe :: String -> Maybe (Double -> Double -> Double)
  readOperationMaybe op
    | op == "*" = Just (*)
    | op == "+" = Just (+)
    | op == "-" = Just (-)
    | op == "/" = Just (/)
    | otherwise = Nothing
