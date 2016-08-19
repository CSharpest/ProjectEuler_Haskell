palindromeProduct :: Int -> Int -> [Int]
palindromeProduct x y
    | x == 1 && y == 1 = []
    | y == 1 = palindromeProduct (x-1) 999
    | x == 1 = palindromeProduct x (y-1)
    | isPalindrome (show product') = product' : palindromeProduct (x-1) 999
    | otherwise = palindromeProduct x (y - 1)
    where product' = x * y

isPalindrome :: String -> Bool
isPalindrome x = x == reverse x

palindromeProduct' :: Int -> Int -> Int
palindromeProduct' n multiplier
    | isPalindrome (show product') = product'
    | multiplier == 1 = palindromeProduct' (n-1) 999
    | otherwise = palindromeProduct' n (multiplier - 1)
    where product' = n * multiplier
