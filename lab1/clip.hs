module Clip
  where

clip :: Integer -> Integer -> Integer -> Integer
clip min max x = 
  if x < min
    then min
    else
      if x > max
        then max
        else x

clip' :: Integer -> Integer -> Integer -> Integer
clip' min_val max_val x = ((max min_val) . (min max_val)) x