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

-- 6
powersOfTen = 1:[10 * xx | xx <- powersOfTen]
sumDecimal :: [Int] -> Int
sumDecimal digits = sum $ zipWith (\x power -> x * power) (reverse digits) powersOfTen

-- 7
sieve :: Int -> [Int] -> [Int]
sieve n ns = filter (notDivBy n) ns
  where
    notDivBy n k = (k `mod` n) /= 0

eratos :: [Int] -> [Int]
eratos [] = []
eratos (n:ns) = n : (eratos $ sieve n ns)

primes :: [Int]
primes = eratos [2..]

-- 8
data Numb = Zero | Succ Numb

instance Show Numb where
  show Zero = ""
  show (Succ numb) = "0" ++ show numb


data Numb' = Zero' | Succ' Numb'

instance Show Numb' where
  show numb' = show $ numbValue numb'

numbValue :: Numb' -> Int
numbValue = numbValueRec 0
  where 
    numbValueRec :: Int -> Numb' -> Int
    numbValueRec sum Zero' = sum
    numbValueRec sum (Succ' numb) = numbValueRec sum' (seq sum' numb)
      where sum' = sum + 1

-- 9
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ acc [] = acc
myFoldl f acc (x:xs) = myFoldl f acc' xs
  where acc' = f acc x

myFoldl' :: (b -> a -> b) -> b -> [a] -> b
myFoldl' _ acc [] = acc
myFoldl' f acc (x:xs) = acc' `seq` myFoldl' f acc' xs
  where acc' = f acc x

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ acc [] = acc
myFoldr f acc (x:xs) = f x $ myFoldr f acc xs
