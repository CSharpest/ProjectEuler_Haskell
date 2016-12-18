module Common where

primes = 2 : filter (null . tail . primeFactors) [3,5..]

primeFactors n = factor n primes
  where
    factor n (p:ps)
        | p*p > n        = [n]
        | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
        | otherwise      =     factor n ps

digits :: Integral x => x -> [x]
digits 0 = []
digits x = digits (x `div` 10) ++ [x `mod` 10]

--[1,2,3,4,5,6]
fromDigits :: [Integer] -> Integer
fromDigits = foldl addDigit 0
   where addDigit num d = 10*num + d

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

digitCount :: Integer -> Int
digitCount = go 1 . abs
    where
        go ds n = if n >= 10 then go (ds + 1) (n `div` 10) else ds

takeUntilInclusive :: (a -> Bool) -> [a] -> [a]
takeUntilInclusive f (x:xs)
  | f x = [x]
  | otherwise = x : takeUntilInclusive f xs

fromMaybe' :: Maybe v -> v
fromMaybe' (Just v) = v

findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing
