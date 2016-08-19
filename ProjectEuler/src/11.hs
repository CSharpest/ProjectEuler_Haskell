import           Data.List

grid = [[1,3,2,9,4,6],
        [7,5,8,3,7,4],
        [2,0,9,1,6,5],
        [2,2,3,8,6,5],
        [6,4,9,8,2,1],
        [4,5,3,1,6,5]]

grid' = [[1,2,6],
         [3,4,7],
         [2,8,3]]

transpose'               :: [[a]] -> [[a]]
transpose' []             = []
transpose' ([]   : xss)   = transpose' xss
transpose' ((x:xs) : xss) = (x : [h | (_:h:_) <- xss]) : transpose' (xs : [ t | (_:t) <- xss])

subsequence :: (Num a) => Int -> [a] -> [[a]]
subsequence _ [] = []
subsequence n xs = foldr (zipWith (:)) (repeat []) . take n $ tails xs

maxRowProd = maximum . map product $ concatMap (subsequence 3) grid
maxColProd = maximum . map product $ concatMap (subsequence 3) . transpose $ grid

main = print . maximum $ [maxRowProd, maxColProd ]

--row in 3's
--concatMap (subsequence 3) grid
--map (product) $ concatMap (subsequence 3) grid

--column in 3's
--concatMap (subsequence 3) . transpose $ grid
