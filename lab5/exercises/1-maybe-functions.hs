module MaybeFunctions where

import Data.Char
import Control.Monad
import Control.Applicative

-- Common functions
-- isLowercase could be substitued for Data.Char.isLower
isLowercase x = asciiNum >= 97 && asciiNum <= 122
  where
    asciiNum = ord x

capitalizeSingleLetter :: Char -> Maybe Char
capitalizeSingleLetter letter
  | isLowercase letter = Just $ toUpper letter
  | otherwise = Nothing

-- Pattern matching only
capitalize :: String -> Maybe String
capitalize "" = Just ""
capitalize (x:xs) = case capitalizeSingleLetter x of
  Just x' -> case capitalize xs of
    Just xs' -> Just (x':xs')
    Nothing -> Nothing
  Nothing -> Nothing

capitalizeMaybe :: Maybe String -> Maybe String
capitalizeMaybe Nothing = Nothing
capitalizeMaybe (Just x) = capitalize x

unaryNumber :: Int -> Maybe String
unaryNumber x
  | x >= 0 = Just $ numToZeros x
  | otherwise = Nothing
  where
    numToZeros x
      | x > 0 = "0" ++ numToZeros (x - 1)
      | otherwise = ""

unaryNumberMaybe :: Maybe Int -> Maybe String
unaryNumberMaybe Nothing = Nothing
unaryNumberMaybe (Just x) = unaryNumber x

interleave :: String -> String -> String
interleave "" _ = ""
interleave _ "" = ""
interleave (a:as) (b:bs) = a:b:(interleave as bs)

interleaveMaybe :: Maybe String -> Maybe String -> Maybe String
interleaveMaybe Nothing _ = Nothing
interleaveMaybe _ Nothing = Nothing
interleaveMaybe (Just x) (Just y) = Just $ interleave x y

-- Using Monad operators
capitalize' :: String -> Maybe String
capitalize' s = mapM capitalizeSingleLetter s

capitalizeMaybe' :: Maybe String -> Maybe String
capitalizeMaybe' = join . liftM capitalize'

-- No idea how to make unaryNumber with monad operators

-- interleave using monads does not make sense since it operates on strings
interleaveMaybe' :: Maybe String -> Maybe String -> Maybe String
interleaveMaybe' = liftM2 interleave

-- Using Applicative operators
capitalize'' :: String -> Maybe String
capitalize'' s = sequenceA $ fmap capitalizeSingleLetter s

-- join is monadic, so I guess it's a bit of a cheat
capitalizeMaybe'' :: Maybe String -> Maybe String
capitalizeMaybe'' = join . liftA capitalize''

unaryNumber'' :: Int -> Maybe String
unaryNumber'' x
  | x == 0 = Just ""
  | x < 0 = Nothing
  | otherwise = ('0':) <$> unaryNumber (x-1)
