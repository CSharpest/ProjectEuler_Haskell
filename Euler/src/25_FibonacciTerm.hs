import           Common

solution =  length . tail . takeUntilInclusive (\x -> digitCount x == 1000) $ fibs
