silnia :: Int -> Int
silnia 1 = 1
silnia n = n * (silnia (n-1))

silnia' :: Int -> Int
silnia' n = silniarec n 1
  where
    silniarec 1 prod = prod
    silniarec k prod = silniarec (k-1) (prod*k)

silnia'' n = silniarec n 1
  where
    silniarec 1 prod = prod
    silniarec k prod = kk `seq` (silniarec (k-1) kk)
      where
        kk = prod*k