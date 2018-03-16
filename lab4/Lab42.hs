module Lab42 where

seqA :: Double -> Double -> Int -> Double
seqA _ _ n
  | n < 0 = error "Negative n"
seqA _ _ 0 = 2.5
seqA p _ 1 = p
seqA p q n = seqA' q (n - 2) 2 p 2.5
  where  
    seqA' :: Double -> Int -> Double -> Double -> Double -> Double
    seqA' q elementsLeft n a_n1 a_n2
      | elementsLeft == 0 = a_n
      | elementsLeft > 0 = a_n `seq` n' `seq`
        seqA' q (elementsLeft - 1) n' a_n a_n1
        where
          n' = n + 1
          a_n = q * n * a_n2 / a_n1

seqAList :: Double -> Double -> [Double]
seqAList p q = 2.5:p:(zipWith a_n indices prev)
  where
    a_n n (a_n2, a_n1) = q * n * a_n2 / a_n1
    prev = zip list (drop 1 list)
      where list = seqAList p q
    indices = [2..]
