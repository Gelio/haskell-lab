import Data.Char

-- Patern matching only
capitalize :: String -> Maybe String
capitalize s = mapM capitalizeSingleLetter s
  where
    capitalizeSingleLetter :: Char -> Maybe Char
    capitalizeSingleLetter letter
      | isLowercase = Just $ toUpper letter
      | otherwise = Nothing
      where
        asciiNum = ord letter
        isLowercase = asciiNum >= 97 && asciiNum <= 122

unaryNumber :: Int -> Maybe String
unaryNumber x
  | x >= 0 = Just $ numToZeros x
  | otherwise = Nothing
  where
    numToZeros x
      | x > 0 = "0" ++ numToZeros (x - 1)
      | otherwise = ""

interleave :: String -> String -> String
interleave "" _ = ""
interleave _ "" = ""
interleave (a:as) (b:bs) = a:b:(interleave as bs)
