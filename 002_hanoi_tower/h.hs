stepCountAnaliticly :: (Num a, Integral b) => b -> a
stepCountAnaliticly towerHeigh
  | towerHeigh < 0 = 0
  | otherwise      = 2 ^ towerHeigh - 1

stepCountRecursivly :: (Ord t, Num t, Num t1) => t -> t1
stepCountRecursivly towerHeigh
  | towerHeigh <= 0 = 0
  | otherwise       = 2 * stepCountRecursivly(towerHeigh - 1) + 1
