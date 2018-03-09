-- Na labach punktowanych:
-- 1. rekurencja ogonkowa + seq 
-- 2. struktura danych + typeclass
-- 3. lista i funkcja operująca na listach

powers :: (Num a) => a -> [a]
powers x = powersLoop x 1
  where
    powersLoop x xx = xx:(powersLoop x (x*xx))

powers' :: (Num a) => a -> [a]
powers' x = [x^i | i <- [0,1..]]

powers'' :: (Num a) => a -> [a]
powers'' x = 1:[x * i | i <- powers'' x]

powers''' :: (Num a) => a -> [a]
powers''' x = 1:(map (*x) (powers''' x))

powersGreater16' :: (Num a) => (Ord a) => a -> [a]
powersGreater16' = filter (>16) . powers'''

polynomialValue :: (Num a ) => [a] -> a -> a
polynomialValue coef x = sum $ zipWith (*) coef $ powers''' x

polynomialValueReversed :: (Num a ) => [a] -> a -> a
polynomialValueReversed coef x = sum $ zipWith (*) (reverse coef) $ powers''' x