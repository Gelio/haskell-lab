module Task4 where

import Control.Monad

distSquared :: Int -> Int -> Int
distSquared x y = (x - y)^2

distance3 :: (Int, Int, Int) -> (Int, Int, Int) -> Double
distance3 (x1, y1, z1) (x2, y2, z2) = sqrt $ fromIntegral $ (distSquared x1 x2) + (distSquared y1 y2) + (distSquared z1 z2)

punktyWkuli :: (Int, Int, Int) -> Int -> [(Int, Int, Int)]
punktyWkuli c@(cx, cy, cz) r = [p |
  x <- xRange, y <- yRange, z <- zRange,
  let p = (x, y, z),
  distance3 c p <= fromIntegral r]
  where
    xRange = [(cx - r)..(cx + r)]
    yRange = [(cy - r)..(cy + r)]
    zRange = [(cz - r)..(cz + r)]
