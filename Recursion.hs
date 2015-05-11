module Recursion where

factorial :: Integer -> Integer
factorial 0 = 1
{-factorial (x + 1) = (x + 1) * factorial x-}
factorial x = x * factorial (x - 1)

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 2) + fib (n - 1)

reverse' :: [a] -> [a]
reverse' [] = []
reverse' [x] = [x]
reverse' ( x:xs ) = reverse' xs ++ [x]

insertOrd :: Ord a => a -> [a] -> [a]
insertOrd x [] = [x]
insertOrd x (y:ys) | x <= y = x : y : ys
                   | otherwise = y : insertOrd x ys

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip xs ys

-- Exercise from Hutton's book
merge :: Ord a => [a] -> [a] -> [a]
merge [] ( x:xs ) = x : xs
merge ( x:xs ) [] = x : xs
merge (x:xs) (y:ys) | x < y = x : merge xs ([y] ++ ys)
                    | otherwise = y : merge ([x] ++ xs) (ys)
