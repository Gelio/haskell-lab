module FunMax
  where

funMax :: (Double -> Double) -> (Double -> Double) -> Double -> Double
funMax f1 f2 x = if f1 x > f2 x then f1 x else f2 x

funMax' :: (Double -> Double) -> (Double -> Double) -> Double -> Double
funMax' f1 f2 x = max (f1 x) (f2 x)
