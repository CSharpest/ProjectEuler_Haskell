

sumOfDivisors :: Int -> Int
sumOfDivisors n = sum [x | x <- [1..n `div` 2], n `mod` x == 0]


amicableNumbers :: Int -> [Int]
amicableNumbers n
  | n == 1 = []
  | sumOfDivisors d == n = if n == d
                           then amicableNumbers (n-1)
                           else n : amicableNumbers (n-1)
  | otherwise = amicableNumbers (n-1)
  where d = sumOfDivisors n


{-

calc :: Int -> State S Int
calc 1 = state $ \s -> (1, M.insert 1 1 s)
calc n = state $ \s ->
    case M.lookup n s of
       Just l  -> (l, s)
       Nothing -> let (l', s') = runState (calc (next n)) s
                      l = 1 + l'
                  in  l `seq` (l, M.insert n l s')
-}
