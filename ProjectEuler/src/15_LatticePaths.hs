
latticePath :: Integral a => (a, a) -> a
latticePath (x,y) = div top bottom
    where top = product  [x+y,x+y-1..2]
          bottom = product [2..x] * (product [2..y])
