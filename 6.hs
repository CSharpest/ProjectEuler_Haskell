--6HigherOrderFunctions

--lib functions using foldss
maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (\x acc -> if x > acc then x else acc)

reverse' :: [a] -> [a]
--reverse' = foldl (flip (:)) []
reverse' = foldl (\acc x -> x : acc) []

product' :: (Num a) => [a] -> a
product' = foldr1 (*)

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

head' :: [a] -> a
head' = foldr1 (\x _ -> x)

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

--curried functions
compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred x = compare 100 x

--sum' :: (Num a) => a -> [a] -> a
--sum' xs = foldl (\acc x ->  acc + x) 0 xs
--sum' :: (Num a) => [a] -> a
--sum' xs = foldl (\acc x -> acc + x) 0 xs

--expecting one fewer arg error
--sum' :: (Int a) => [a] -> a
--sum' xs = foldl (\acc x -> 2 * if acc == 0 then 1 else acc + x) 0 xs

--sum' :: (Foldable t, Num a, Eq a) => t a -> a
sum' n xs = foldl (\acc x -> n * if acc == 0 then 1 else acc + x) 0 xs

--right folds when building new lists from lists
--right folds work on infinite lists
--Folds can be used to implement any function where you traverse a list once, element by element,
-- and then return something based on that. Whenever you want to traverse a list to return something,
--chances are you want a fold.
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs
