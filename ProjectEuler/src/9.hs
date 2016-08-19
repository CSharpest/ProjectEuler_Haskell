x = "This is some text which we escape \n\
      \   and unescape to keep writingdsfsdfsdf"
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

number = 763134830

filterLength2:: Int -> [Int]
filterLength2 xs
  | xs == [] = []
  | otherwise = xs
  where digits = digs xs


isAdjacent :: Int -> Int -> Bool
isAdjacent x y
  | x == y = True
  | x + 1 == y = True
  | x - 1 == y = True
  | otherwise = False
