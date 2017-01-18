stepCountAnalytically :: (Integral a, Ord a) => a -> a
stepCountAnalytically towerHeigh
  | towerHeigh < 0 = 0
  | otherwise      = 2 ^ towerHeigh - 1

stepCountRecursivly :: (Num a, Ord a) => a -> a
stepCountRecursivly towerHeigh
  | towerHeigh <= 0 = 0
  | otherwise       = 2 * stepCountRecursivly(towerHeigh - 1) + 1


type TowerHeigh = Integer
type Peg = String
type Move = (Peg, Peg)

hanoiMoveSet :: TowerHeigh -> Peg -> Peg -> Peg -> [Move]
hanoiMoveSet 0 _ _ _ = []
hanoiMoveSet n from to buffer = topsToBuffer ++ [bottomMove] ++ topsFromBuffer
  where
    topsToBuffer = hanoiMoveSet (n - 1) from buffer to
    bottomMove = (from, to)
    topsFromBuffer = hanoiMoveSet (n - 1) buffer to from
