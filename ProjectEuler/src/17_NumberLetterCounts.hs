import           Data.List

words' = [(1,"one"),
        (2,"two"),
        (3,"three"),
        (4,"four"),
        (5,"five"),
        (6,"six"),
        (7,"seven"),
        (8,"eight"),
        (9,"nine"),
        (10,"ten"),
        (11, "eleven"),
        (12,"twelve"),
        (13,"thirteen"),
        (14,"fourteen"),
        (15,"fifteen"),
        (16,"sixteen"),
        (17,"seventeen"),
        (18,"eighteen"),
        (19,"nineteen"),
        (20,"twenty"),
        (30,"thirty"),
        (40,"forty"),
        (50,"fifty"),
        (60,"sixty"),
        (70,"seventy"),
        (80,"eighty"),
        (90,"ninety")]

say :: Integer -> String
say n
      | n == 1000 = "onethousand"
      | (n < 20) || (n < 100 && divisibleByTen) = say' . findKey n $ words'
      | n < 100 && not divisibleByTen = (say' . findKey (d*10) $ words') ++ (say' . findKey r $ words')
      | divisibleByHundred = (say' . findKey d2 $ words') ++ "hundred"
      | otherwise = ((say' . findKey (d2) $ words') ++ "hundredand") ++ (say r2)
      where divisibleByTen = n `mod` 10 == 0
            divisibleByHundred = n `mod` 100 == 0
            (d,r) = divMod n 10
            (d2,r2) = divMod n 100

say' :: Maybe String -> String
say' x = case x of
  Just x -> x
  Nothing -> ""

  findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
  findKey key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing

solution = length . concatMap (say) $ [1..1000]
