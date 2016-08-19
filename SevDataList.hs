import           Control.Monad.State
import           Control.Parallel.Strategies
import           Data.Bits                   (shiftR, (.&.))
import qualified Data.IntMap.Strict          as M
import           Data.List                   (maximumBy)
import           Data.Tuple                  (swap)
next :: Int -> Int
next 1 = 1
next n
    | n .&. 1 == 0 = n `quot` 2
    | otherwise    = 3 * n + 1

type S = M.IntMap Int

calc :: Int -> State S Int
calc 1 = state $ \s -> (1, M.insert 1 1 s)
calc n = state $ \s ->
    case M.lookup n s of
       Just l  -> (l, s)
       Nothing -> let (l', s') = runState (calc (next n)) s
                      l = 1 + l'
                  in  l `seq` (l, M.insert n l s')

calcRange :: Int -> Int -> State S ()
calcRange from end
    | from == end = return ()
    | otherwise   = calc from >> calcRange (from+1) end

solute :: Int -> Int -> Int
solute f e = snd . maximum . map swap . M.toList . snd $ runState (calcRange f e) M.empty
