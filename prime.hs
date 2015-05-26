module Prime where

-- 3057601 isnt prime but outputs as one
-- same for 252601
-- 3057601 = 47 * 71107
-- 252601 = 41 * 6161

maybePrime :: Integer -> Integer -> Bool
maybePrime n a = nope2 (iter a (factor2 (n-1)) n) n

teste :: Integer ->Integer -> [Integer]
teste n a = iter a (factor2 (n-1)) n

factor2 :: Integer -> (Integer, Integer)
factor2 n = f2 n 0
    where
    f2 n s | even n    = f2 (n `div` 2) (s + 1)
           | otherwise = (n,s)

congruente :: Integer -> Integer -> Integer -> Bool
congruente p q n
    | (p - q) `mod` n == 0    = True
    | otherwise             = False

iter :: Integer -> (Integer, Integer) -> Integer -> [Integer]
iter a (k, d) n = map (\x -> power a (k*x, 1) n) [ 2^x | x <- [0, 1 .. d]]

is_one :: Integer -> Bool
is_one 1 = True
is_one _ = False

power :: Integer -> (Integer, Integer) -> Integer -> Integer
power a (d, k) n = (a^d) `mod` n

has_one :: [Integer] -> Bool
has_one [] = False
has_one ys = h2 ys where
    h2 (x:xs)
        | is_one x      = True
        | otherwise     = has_one xs

nope :: [Integer] -> Integer -> Bool
nope (x:xs) n
     | head xs == 1  = (congruente (x^2) (-1) n) || (congruente (x^2) (1) n)
     | otherwise     = nope xs n

nope2 :: [Integer] -> Integer -> Bool
nope2 xs n
    | has_one xs    = nope xs n 
    | otherwise     = False
