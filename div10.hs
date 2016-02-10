import System.Environment
import System.IO
import Data.List.Extra

next file numb = do
    end <-  hIsEOF file
    if end then return () else next' file numb

next' file numb = do
    tx <- hGetLine file

    let ss = split (==' ') tx
        ff = read ( last $ take 1 ss ) :: Int
        ll = read ( last ss ) :: Int

        ff' = show ( ff `div` numb )
        ll' = show ( ll `div` numb )

    putStrLn $ ff' ++ " " ++ ll'

    next file numb

main = do
    args <- getArgs

    let name = args !! 0
        numb = read (args !! 1)

    file <- openFile name ReadMode

    next file numb

    hClose file
