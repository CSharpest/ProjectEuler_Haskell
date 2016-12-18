import           Data.Char
import           Data.List

euler_8 = do
   str <- readFile "number.txt"
   print . maximum
         . map product
         . foldr (zipWith (:)) (repeat [])
         . take 6 . tails . map (fromIntegral . digitToInt)
         . concat
         . lines
         $ str
