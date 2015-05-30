module ExtendedEuclides where

extendedEuclid t d = ee t t t d 1
    where
        ee _ a b 1 d = d
        ee t a b c d
            | d < 0             = ee t a b c (d `mod` t)
            | otherwise         = ee t c d (a - (a `div` c) * c) (b - (a `div` c) * d)
