module Task4 where

distSquared :: Int -> Int -> Int
distSquared x y = (x - y)^2

distance3 :: (Int, Int, Int) -> (Int, Int, Int) -> Double
distance3 (x1, y1, z1) (x2, y2, z2) = sqrt $ fromIntegral $ (distSquared x1 x2) + (distSquared y1 y2) + (distSquared z1 z2)

-- Rozwiązanie z list comprehension
punktyWkuli :: (Int, Int, Int) -> Int -> [(Int, Int, Int)]
punktyWkuli c@(cx, cy, cz) r = [p |
  x <- xRange, y <- yRange, z <- zRange,
  let p = (x, y, z),
  distance3 c p <= fromIntegral r]
  where
    xRange = [(cx - r)..(cx + r)]
    yRange = [(cy - r)..(cy + r)]
    zRange = [(cz - r)..(cz + r)]

-- Drugie rozwiązanie, przekształcenie pierwszego na notację bez list comprehension
getAllPoints :: (Int, Int, Int) -> Int -> [(Int, Int, Int)]
getAllPoints (cx, cy, cz) r = do
  x <- xRange
  y <- yRange
  z <- zRange

  return (x, y, z)
  where
    xRange = [(cx - r)..(cx + r)]
    yRange = [(cy - r)..(cy + r)]
    zRange = [(cz - r)..(cz + r)]

punktyWkuli' :: (Int, Int, Int) -> Int -> [(Int, Int, Int)]
punktyWkuli' c r = filter (\p -> distance3 c p <= fromIntegral r) points
  where points = getAllPoints c r
