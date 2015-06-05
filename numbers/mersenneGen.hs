import System.Environment
import System.IO
import System.IO.Error

import Prime

show_numbers [n]    = print n
show_numbers (x:xs) =
    do print x
       show_numbers xs

mersenneN = iterate (\x -> 2*x + 1) 0

mersennePrimes k = [ x | x <- mersenneN, millerRabin x k]

main = do args <- getArgs
          let numbers = mersennePrimes (read (args !! 0) :: Int)

          show_numbers numbers

          return ()
