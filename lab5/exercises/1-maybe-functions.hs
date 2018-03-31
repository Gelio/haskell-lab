import Data.Char

-- Common functions
isLowercase x = asciiNum >= 97 && asciiNum <= 122
  where
    asciiNum = ord x

capitalizeSingleLetter :: Char -> Maybe Char
capitalizeSingleLetter letter
| isLowercase letter = Just $ toUpper letter
| otherwise = Nothing

-- Pattern matching only
capitalize :: String -> Maybe String
capitalize s = mapM capitalizeSingleLetter s

capitalize' :: Maybe String -> Maybe String
capitalize' Nothing = Nothing
capitalize' (Just x) = capitalize x

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

interleave' :: Maybe String -> Maybe String -> Maybe String
interleave' Nothing _ = Nothing
interleave' _ Nothing = Nothing
interleave' (Just x) (Just y) = interleave x y

