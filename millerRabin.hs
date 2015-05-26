module MillerRabin where

factor2 :: Integer -> (Integer, Integer)
factor2 n = fN n 0
    where
    fN n s | even n    = fN (n `div` 2) (s + 1)
           | otherwise = (n,s)

congruente :: Integer -> Integer -> Integer -> Bool
congruente p q n
    | q - p `mod` n == 0    = True
    | otherwise             = False

iter :: Integer -> (Integer, Integer) -> [Integer]
iter n (m, k) = map (\x -> m^2^x) [1, 2 .. k]
