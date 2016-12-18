

collatzDic = [(10,3), (12,1), (5,20)]


addToDict :: (Num a) => []

max' :: (Num a, Ord a) => [(a,a)] -> (a,a)
max' = foldl1 (\(x,y) (a,b) -> if b > y then (a,b) else (x,y))

collatz :: Int -> Int
collatz 1 = 1
collatz n = if (odd n)
             then (3 * n + 1)
             else n `div` 2

collatzSequence :: Int -> [Int]
collatzSequence = terminate . iterate collatz
  where terminate (1:_) = [1]
        terminate (x:xs) = x:terminate xs
