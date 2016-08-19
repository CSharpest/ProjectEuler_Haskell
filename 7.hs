import           Control.Monad
import           Control.Monad.Reader
import           Data.List

main :: IO ()
main = putStrLn "Hello World"

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

(.&&.) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
--(&&) f g a = f a && g a
-- or, in points-free style:
(.&&.) = liftM2 (&&)

both :: (a -> Bool) -> (a -> Bool) -> a -> Bool
both f g x = f x && g x

append xs ys = foldr (\x y -> x:y) ys xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _pred []    = []
filter' pred (x:xs)
  | pred x         = x : filter' pred xs
  | otherwise      = filter' pred xs

--let div3 = filter (\x -> x `mod` 3 == 0) $ [1..10]
--let div5 = filter (\x -> x `mod` 5 == 0) $ [1..10]

--let combined = append div3 div5


--transpose
--1 2 3
--4 5 6
--7 8 9
--
--1 4 7
--2 5 8
--3 6 9
--
--h e y
--t h e r e
--g u y s
--
--h t g
--e h u
--y e y
--r s
--e

--polynomials
--x2 + 5x + 9, 10x3 + 9 and 8x3 + 5x2 + x - 1
--represented in Haskell [[0,3,5,9],[10,0,0,9],[8,5,1,-1]]

--inter parse
{-
ghci> intersperse '.' "MONKEY"
"M.O.N.K.E.Y"
ghci> intersperse 0 [1,2,3,4,5,6]
[1,0,2,0,3,0,4,0,5,0,6]
-}
