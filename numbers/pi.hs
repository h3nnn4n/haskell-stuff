import Data.List

piVomit = g(1,0,1,1,3,3)
    where
        g(q,r,t,k,n,l)
            | 4 * q + r - t < n * t =  n : g( 10 * q, 10 * (r - n * t), t, k, div( 10 * ( 3 * q + r )) t - 10 * n, l)
            | otherwise             =      g( q * k, ( 2 * q + r ) * l, t * l, k + 1, div(q * (7 * k + 2) + r * l) (t * l), l + 2)

main = do 
          print $ concat $ intersperse "" $ map (\x -> show x) $ piVomit
          return ()
