module Lab43 where

data BinDigit = Zero | One deriving Show

longNumber :: Int -> [BinDigit]
longNumber n = One:(zeros n)
  where
    zeros 0 = []
    zeros n = Zero:(zeros (n - 1))

bin2dec :: [BinDigit] -> Integer
bin2dec list = fst $ foldr f (0, 1) list
  where
    f Zero (sum, power) = sum `seq` nextPower `seq` (sum, nextPower)
      where 
        sum' = sum + power
        nextPower = if power == 0 then 1 else power * 2
    f One (sum, power) = sum' `seq` nextPower `seq` (sum', nextPower)
      where 
        sum' = sum + power
        nextPower = if power == 0 then 1 else power * 2

-- bin2dec' :: [BinDigit] -> Integer
-- bin2dec' list = bin2dec'' list (2 ^ ((length list) - 1)) 0
--   where
--     bin2dec'' 
-- bin2dec' [] = 0
-- bin2dec' (x:xs) =