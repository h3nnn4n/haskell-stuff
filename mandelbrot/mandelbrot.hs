import Data.Number.BigFloat
import Data.Complex
import Data.Function

max_iter = 100

screenx = 400

screeny = 300

start_x = 2.0
stop_x = -1.5

start_y = 1.5
stop_y = -1.5

dy = (stop_y - start_y) / screeny
dx = (stop_x - start_x) / screenx

pixels = [ x :+ y | x <- xPixels, y <- yPixels ]
    where
        xPixels = [ x | x <- [ start_x, start_x + dx .. stop_x ]]
        yPixels = [ y | y <- [ start_y, start_y + dy .. stop_y ]]


iter z = length $ takeWhile (\x -> magnitude x <= 2 ) $ take max_iter $ iterate (quadratic z) 0
    where
        quadratic z c = z*z + c

get_set = map iter pixels

max_point (x:xs) = foldr (max) x xs

min_point (x:xs) = foldr (min) x xs

--normalize xs a b = map applyRatio xs
    --where
        --applyRatio x = floor $ x * (div 255 (b-a))
        --divv = (/) `on` fromIntegral

main = do
          let points = get_set
              maxP = max_point points
              minP = min_point points
              --normalized = normalize points minP maxP
              point_file = foldl (\a b -> a ++ (show b) ++ " ") "" points

          --print maxP
          --print minP
          --print $ take 10 normalized

          --print $ take 10 point_file

          writeFile "img.ppm" $ "P2 \n80 60 \n255 \n" ++ point_file

          return ()
