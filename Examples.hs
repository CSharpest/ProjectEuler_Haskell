import           Prelude hiding (filter, map)
import Text.Printf
import Control.Exception
import System.CPUTime

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = x
    where maxTail = maximum' xs

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

--curied by default
addThree :: (Num a) => a -> a -> a -> a
addThree x y z = x + y + z

--addThree :: (Num a) => a -> a -> a -> a
--ddThree = \x -> \y -> \z -> x + y + z

first' :: [a] -> a
first' [x] = x
first' (x:_) = x
first' [] = error "Can't do last of an empty list!"

last' :: [a] -> a
last' [x] = x
last' [x, y] = x
last' (x:xs) = last xs --if there's anything in the head, continue until there's one element left
--  if length xs == 1 then x
--  		else last' xs
last' [] = error "Can't do last of an empty list!"


head' :: [a] -> a
head' (x:_) = x

tail' :: [a] -> [a]
tail' (x:xs) =  xs

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x:replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0   = []
take' _ []     = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' (x:xs) = reverse' xs ++ [x]

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"
    | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
    | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"
    | otherwise                 = "You're a whale, congratulations!"

bmiTell' :: (RealFloat a) => a -> a -> String
bmiTell' weight height
    | x ^ 2 <= 18.5 = "You're underweight, you emo, you!"
    | x ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
    | x ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"
    where x = weight / height
repeat' :: Int -> a -> [a]
repeat' n x = take' n (x:repeat x)

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x    = True
    | otherwise = a `elem'` xs

 --let multTwoWithNine = multThree 9
--multTwoWithNine 2 3

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

applyThree :: (a -> a) -> a -> a
applyThree f x = f(f (f x))

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

getInitials' :: String -> String -> String
getInitials' (x:xs) (y:ys) =  x:' ':[y]

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in  smallerSorted ++ [x] ++ biggerSorted

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) = quicksort (filter (<=x) xs)  ++ [x] ++ quicksort (filter (>x) xs)

qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (a:as) = qsort left ++ [a] ++ qsort right
  where (left,right) = (filter (<=a) as, filter (>a) as)

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

-- filter (\n -> n `mod` 4 == 0) (map (+3)[3,5,6,9])
filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs)
    | p x       = x : filter p xs
    | otherwise = filter p xs

largestDivisible :: (Integral a) => a
largestDivisible = head (filter f [100000,99999..])
    where f x = x `mod` 3829 == 0

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n = n:chain (n `div` 2)
    | otherwise = n:chain (n*3 + 1)

countdown :: Int -> [Int]
countdown 0 = []
countdown n = n : countdown (n - 1)

fib::Int->Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs :: [Integer]
fibs = 1:1:zipWith (+) fibs (tail fibs)

fib' n = take n fiblist
  where fiblist = 0:1:(zipWith (+) fiblist (tail fiblist))

fibList' :: Int -> [Int]
fibList' 1 = []
fibList' n = map fib [1..n]

factor = go (2:[3,5..11]) where
    go (p:ps) n
        | p*p > n        = [n]
        | n `mod` p == 0 = p : go (p:ps) (n `div` p)
        | otherwise      = go ps n


{-
fib n = take n fiblist
  where fiblist = 1:1:(zipWith (+) fiblist (tail fiblist))
-}


numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15

flip' :: (a -> b -> c) -> b -> a -> c
flip' f y x = f x y

--let listOfFuns = map (*) [0..]
--(listOfFuns !! 4) 5
--map (\(a,b) -> a + b) [(1,2),(3,5),(6,3),(2,6),(2,5)]
--zipWith (\a b -> (a * 30 + 3) / b) [5,4,3,2,1] [1,2,3,4,5]

--sum' :: (Num a) => [a] -> a
--sum' xs = foldl (\acc x -> acc + x) 0 xs

sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

plus' x = 10 + x

--map' :: (a -> b) -> [a] -> [b]
--map' f xs = foldr (\x acc -> f x : acc) [] xs

--sqrt (3 + 4 + 9)
--sqrt $ 3 + 4 + 9
