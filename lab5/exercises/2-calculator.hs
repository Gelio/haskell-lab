module Main where

  main :: IO ()
  main = do
    op <- readOperation
    arg1 <- readNumber
    arg2 <- readNumber
    putStrLn $ op arg1 arg2

  readNumber :: (Num a) => Maybe a
  readNumber = getLine >>= read

  readOperation :: (Num a) => String -> Maybe (a -> a -> a)
  readOperation op
    | op == "*" = Just (*)
    | op == "+" = Just (+)
    | op == "-" = Just (-)
    | op == "/" = Just (/)
    | op == "mod" = Just (mod)
    | otherwise = Nothing
