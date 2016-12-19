--day 9 part 1

import Data.List.Split
import Data.List

main = do str <- readFile "day9input.txt"
          print $ (length $ decompress str) -1

decompress :: String -> String
decompress [] = []
decompress ('(':xs) = (concat (replicate (read (amounts !! 1) :: Int) (take (read (amounts !! 0) :: Int) (drop amountToDrop xs))))++
                              decompress (drop (amountToDrop + (read (amounts !! 0) :: Int)) xs) where
                                amountToDrop = (length (takeWhile (/=')') xs)) +1
                                amounts = splitOn "x" (takeWhile (/=')') xs)
decompress (x:xs)   = x:decompress xs
