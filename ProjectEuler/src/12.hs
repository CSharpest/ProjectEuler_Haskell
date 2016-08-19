import           Common
import           Data.List


triangles :: [Integer]
triangles = 1 : zipWith (+) [2..] triangles

--primeFactorsFor 28
--(28,[2,2,7])
primeFactorsFor  :: Integer -> (Integer, [Integer])
primeFactorsFor x = (x, primeFactors x)

--factorCount $ primeFactorsFor 28
--(28,6)
factorCount  :: Eq a => (t, [a]) -> (t, Int)
factorCount (x, xs) = (x, product . map ((+1) . length) . group $ xs)

solution = find (\(a,b) -> b > 500) . map (factorCount . primeFactorsFor) $ triangles
