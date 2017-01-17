stepCountAnalytically :: (Integral a, Ord a) => a -> a
stepCountAnalytically towerHeigh
  | towerHeigh < 0 = 0
  | otherwise      = 2 ^ towerHeigh - 1

stepCountRecursivly :: (Num a, Ord a) => a -> a
stepCountRecursivly towerHeigh
  | towerHeigh <= 0 = 0
  | otherwise       = 2 * stepCountRecursivly(towerHeigh - 1) + 1

