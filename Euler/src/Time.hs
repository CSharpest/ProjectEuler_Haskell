import           Common
import           Control.Exception
import           System.CPUTime
import           Text.Printf

import           Data.List

triangles :: [Integer]
triangles = 1 : zipWith (+) [2..] triangles

--primeFactorPairs 28
--(28,[2,2,7])
primeFactorPair  :: Integer -> (Integer, [Integer])
primeFactorPair x = (x, primeFactors x)

--factorCount $ primeFactorPair 28
--(28,6)
factorCount  :: Eq a => (t, [a]) -> (t, Int)
factorCount (x, xs) = (x, product . map ((+1) . length) . group $ xs)

solution = find (\(a, b) -> b > 500) . map (factorCount . primeFactorPair) $ triangles

time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    return v

main = do
    putStrLn "Starting..."
    time $ solution `seq` return ()
    putStrLn "Done."
{-
main2 :: IO ()
main2 = do
    start <- getCurrentTime
    answer <- evaluate solution
    end <- getCurrentTime
    print (answer, diffUTCTime end start)
-}
