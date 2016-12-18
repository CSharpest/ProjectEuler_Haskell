

digs :: Integral x => x -> [x]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

fromDigits = foldl addDigit 0
   where addDigit num d = 10*num + d

--main :: IO -> ()
main = do
   str <- readFile "number.txt"
   print . fromDigits . take 10 . digs . sum . map (\x -> read x ::Integer) . lines $ str
