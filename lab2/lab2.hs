{-# OPTIONS -Wall #-}

module Lab2
  where

fibloop :: Integer -> Integer -> Integer -> Integer
fibloop n n_1 n_2
  | n <= 1 = nsum
  | otherwise = fibloop (nsum `seq` (n-1)) nsum n_1
  where
    nsum = n_1 + n_2

fib' :: Integer -> Integer
fib' n = fibloop n 1 0


data Point = Point Double Double
data Shape = Rect Point Point | Circle Point Double | Segment Point Point

distance :: Point -> Point -> Double
distance (Point x1 y1) (Point x2 y2) = ((x1 - x2)^2) + ((y1 - y2)^2)

dimension :: Shape -> Int
dimension (Rect _ _) = 2
dimension (Circle _ _) = 2
dimension (Segment _ _) = 1


area :: Shape -> Double
area (Rect (Point x1 y1) (Point x2 y2)) = xDelta * yDelta
  where
    xDelta = abs (x1 - x2)
    yDelta = abs (y1 - y2)
area (Circle _ radius) = pi * (radius ^ 2)
area (Segment _ _) = 0

scale :: Shape -> Double -> Shape
-- For the rectangle, transform origin is p1
scale (Rect p1@(Point x1 y1) (Point x2 y2)) ratio = Rect p1 scaledP2
  where
    xDelta = x1 - x2
    yDelta = y1 - y2
    scaledX2 = x1 + (ratio * xDelta)
    scaledY2 = y1 + (ratio * yDelta)
    scaledP2 = Point scaledX2 scaledY2

scale (Circle p1 radius) ratio = Circle p1 scaledRadius
  where
    scaledRadius = radius * ratio

scale (Segment (Point x1 y1) (Point x2 y2)) ratio = Segment scaledP1 scaledP2
  where
    xCenter = (x1 + x2) / 2
    yCenter = (y1 + y2) / 2
    scaledX1 = xCenter + ((x1 - xCenter) * ratio)
    scaledY1 = yCenter + ((y1 - yCenter) * ratio)
    scaledP1 = Point scaledX1 scaledY1
    scaledX2 = xCenter + ((x2 - xCenter) * ratio)
    scaledY2 = yCenter + ((y2 - yCenter) * ratio)
    scaledP2 = Point scaledX2 scaledY2