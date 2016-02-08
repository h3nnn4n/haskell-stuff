import System.IO
import Data.List.Extra

next file = do
    end <-  hIsEOF file
    if end then return () else next' file

next' file = do
    tx <- hGetLine file

    let ss = split (==' ') tx
        ff = read ( last $ take 1 ss ) :: Int
        ll = read ( last ss ) :: Int

        ff' = show ( ff `div` 10 )
        ll' = show ( ll `div` 10 )

    putStrLn $ ff' ++ " " ++ ll'

    next file

main = do
    file <- openFile "76_pr" ReadMode

    next file

    hClose file
