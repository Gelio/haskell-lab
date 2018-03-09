module SinTaylor
  where

sinTaylor :: Double -> Integer -> Double
sinTaylor x n = sinTaylorLoop x n 0 1 x 1 1

sinTaylorLoop :: Double -> Integer -> Double -> Double -> Double -> Integer -> Integer -> Double
sinTaylorLoop x n summed sign x_power factorial_val factorial_count
  | n == 1 = (summed + sign * x_power / (fromInteger factorial_val))
  | otherwise = 
    sinTaylorLoop x (n - 1)
      (summed + sign * x_power / (fromInteger factorial_val))
      (-sign)
      (x_power * x * x)
      (factorial_val * (factorial_count + 1) * (factorial_count + 2))
      (factorial_count + 2)