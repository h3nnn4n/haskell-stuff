module Main where -- Main also must be the module file name

double :: (Num a) => a -> a -- `Num' is the class of all numbers: Int, Float etc.
double x = x + x

qsort :: (Ord a) => [a] -> [a] -- `Ord' = all types that can be ordered: numbers, letters, etc.
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort greater
  where
  smaller = [x' | x' <- xs, x' <= x]
  greater = [x' | x' <- xs, x' > x]

length' :: [a] -> Int -- the ' used to distinguish with the Prelude `length'
length' [] = 0
length' (_ : xs) = 1 + length' xs

-- `halved' instead of `halve'. Remember, not a command, but a statement.
halved :: [a] -> ([a], [a]) -- no bound type needed, polymorphic type sufficient
halved [] = error "Empty list\n"
halved [x] = error "Singleton list\n" --
halved xs = (take ((length' xs) `div` 2) xs , drop ((length' xs) `div` 2) xs)

factors :: (Integral a) => a -> [a] -- Integral class gives you greater range of values
factors n = if n <= 0 then error "n must be greater than 0"
  else [ x | x <- [1 .. n], n `mod` x == 0]

isPrime :: (Integral a) => a -> Bool
isPrime n = length' (factors n) == 2

isPrime' :: (Integral a) => a -> Bool
isPrime' n = all (\x -> x == 1 || x == n) (factors n) -- using lambda a function

hasTwin :: (Integral t) => t -> Bool
hasTwin n = if n <= 2 then error "input must be greater than 2"
  else (isPrime n) && ((isPrime (n + 2)) || (isPrime (n - 2)))

primesUpTo :: (Integral t) => t -> [t] -- `until' denotes time. Remember, not a command, but a statement.
primesUpTo n = [x | x <- [1 .. n], isPrime x]

twinsUpTo :: (Integral t) => t -> [t]
twinsUpTo n = [ x | x <- [3 .. n], hasTwin(x)]

-- You are re-arranging the list, you are not using the elements in a calculation
-- therefore, use a polymorphic type
-- `dropLast' is a command; withoutLast is more appropriate
withoutLast :: [a] -> [a]
withoutLast [] = []
withoutLast [_] = []
withoutLast [x, _] = [x]
withoutLast ( x : xs ) = x : withoutLast xs

firstHalf :: [a] -> [a]
-- firstHalf [] = error "Empty list\n"
firstHalf [] = [] -- This is a base case, cant be an error.
firstHalf ( x : xs ) = x : firstHalf ( withoutLast xs ) -- very good!

-- lastEl being last element
lastEl :: [a] -> a
lastEl [] = error "Empty list\n"
lastEl [x] = x
lastEl ( x : xs ) = lastEl xs

secondHalf :: [a] -> [a]
secondHalf [x] = []
secondHalf [_, x] = [x]
secondHalf ( x : xs ) = secondHalf (withoutLast xs) ++ [lastEl xs]

-- This is what I intended to when I wrote the firstHalf and secondHalf functions.
halved' :: [a] -> ([a], [a])
halved' [] = ([],[])		-- Same as below
halved' [x] = ([],[x]) 		-- Since halved [1 .. 3] = ([1],[2,3]) should not halved [1] = ([],[1]) ?
halved' xs = (firstHalf xs , secondHalf xs)

-- All in all, a very good first time effort.
-- You obviously have a natural aptitude for functional programming.

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

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort ( xs ) = merge ( mergeSort (firstHalf xs)) (mergeSort (secondHalf xs))

