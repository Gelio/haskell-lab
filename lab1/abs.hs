module Abs
  where

abs'' :: Integer -> Integer
abs'' x = if x < 0 then (-x) else x

abs' :: Integer -> Integer
abs' x
  | x < 0 = (-x)
  | otherwise = x