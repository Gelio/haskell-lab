module Fib (fib, fib')
  where

fib :: Integer -> Integer
fib n
  | n <= 1 = 1
  | otherwise = fib (n-1) + fib (n - 2)

fibloop :: Integer -> Integer -> Integer -> Integer
fibloop n n_1 n_2
  | n <= 1 = n_1 + n_2
  | otherwise = fibloop (n-1) (n_1 + n_2) n_1

fib' :: Integer -> Integer
fib' n = fibloop n 1 0