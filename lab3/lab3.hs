-- 1
elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' x (head:tail)
  | x == head = True
  | otherwise = elem' x tail

elem'' :: (Eq a) => a -> [a] -> Bool
elem'' x list = foldl (\found y -> found || (x == y)) False list

-- 2
pi' :: Int -> Double
pi' n = (4*) $ sum $ take n $ map elementValue [0..]
  where
    elementValue :: Int -> Double
    elementValue x
      | x `mod` 2 == 0 = value
      | otherwise = negate value
      where
        value = 1 / (2 * (fromIntegral x) + 1)

pi'' :: Int -> Double
pi'' n = (4*)
  $ sum
  $ take n
  $ [elementSign * 1 / (2 * (fromIntegral x) + 1)
    | x <- [0..],
    let elementSign = if x `mod` 2 == 0 then 1 else (-1)]

-- 3
task3 = (take 1 $ [x | x <- [100000,99999..], x `mod` 3829 == 0])!!0

-- 4
quickSort' :: (Ord a) => (Eq a) => [a] -> [a]
quickSort' [] = []
quickSort' (pivot:xs) = smaller ++ pivot:larger
  where
    smaller = quickSort' $ filter (<= pivot) xs
    larger = quickSort' $ filter (> pivot) xs

-- 5
isDescending :: (Ord a) => [a] -> Bool
isDescending [] = True
isDescending (head:tail) = not $ fst $ foldl foldingFn (False, head) tail
  where
    foldingFn (hasFailed, prev) x = if hasFailed then (hasFailed, x) else (prev <= x, x)
