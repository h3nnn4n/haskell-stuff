module MillerRabin where

isPrime :: Integer n -> int k -> Bool
isPrime  n k
    | n == 2                = True
    | n `mod` 2 == 0        = False
    | otherwise             = do ws <- witnesses n k 
                                 return $ and [ test 
