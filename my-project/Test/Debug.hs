import Data.List  

main :: IO()

qsort [] = [] 
qsort (a:as) = qsort left ++ [a] ++ qsort right
  where (left,right) = (filter (<=a) as, filter (>a) as)

factor :: Integer -> [Integer]
factor = go (2:[3,5..]) where
    go (p:ps) n
        | p*p > n        = [n]
        | n `mod` p == 0 = p : go (p:ps) (n `div` p)
        | otherwise      = go ps n


sumSquares :: Integer -> Integer
sumSquares 1 = 1
sumSquares n = (n^2) + sumSquares (n-1)



digs :: Integral x => x -> [x]
digs 0 = []
digs x = x `mod` 10 : digs (x `div` 10)