module Xyzzy
  where

xyzzy :: Double -> Double
xyzzy x = negate (ceiling (abs (cos x)))

xyzzy' :: Double -> Double
xyzzy' x = negate $ ceiling $ abs $ cos x