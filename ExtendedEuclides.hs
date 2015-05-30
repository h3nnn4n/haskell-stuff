module ExtendedEuclides where

--extendedEuclid :: Integer -> Integer -> [(Integer, Integer)]
--extendedEuclid e phi = ee [(phi, phi), (e, 1)]
    --where
        --ee [(Integer, Integer)] -> [(Integer, Integer)]
        --ee ((a,b):(c,d))
            -- | a == 1     = [(a,b):(c,d)]
            -- | d <= 0     = ee [(a,b):(c,d `mod` phi)]
            -- | otherwise  = ee [(c,d):(a - ((a `div`c) * c), b - ((a `div` c) * d))]

extendedEuclid t d = ee t t t d 1
    where
        ee _ a b 1 d = d
        ee t a b c d
            | d < 0             = ee t a b c (d `mod` t)
            | otherwise         = ee t c d (a - (a `div` c) * c) (b - (a `div` c) * d)

--ee :: Integer -> [(Integer, Integer)] -> [(Integer, Integer)]
--ee phi ((a,b):(c,d)) = ee phi [(c,d),(a - ((a `div` c) * c), b - ((a `div` c) * d))]
    -- | a == 1     = [(a,b),(c,d)]
    -- | d <= 0     = ee phi [(a,b),(c,d `mod` phi)]
    -- | otherwise  = ee phi [(c,d),(a - ((a `div` c) * c), b - ((a `div` c) * d))]
