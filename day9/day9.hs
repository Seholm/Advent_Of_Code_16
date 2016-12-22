--day 9 part 1

import Data.List.Split
import Data.List

main = do str <- readFile "day9input.txt"
          print $ (length $ decompress str) -1
          print $ (decompress1 str)-1

decompress :: String -> String
decompress [] = []
decompress ('(':xs) = (concat (replicate (read (amounts !! 1) :: Int) (take (read (amounts !! 0) :: Int) (drop amountToDrop xs))))++
                              decompress (drop (amountToDrop + (read (amounts !! 0) :: Int)) xs) where
                                amountToDrop = (length (takeWhile (/=')') xs)) +1
                                amounts = splitOn "x" (takeWhile (/=')') xs)
decompress (x:xs)   = x:decompress xs

--part2

decompress1 :: String -> Int
decompress1 [] = 0
decompress1 ('(':xs) = decompressxTimes (take ((read (amounts !! 0) :: Int)+ amountToDrop) ('(':xs) ) +
                              decompress1 (drop ((amountToDrop-1) + (read (amounts !! 0) :: Int)) xs) where
                                amountToDrop = (length (takeWhile (/=')') xs)) +2
                                amounts = splitOn "x" (takeWhile (/=')') xs)
decompress1 (x:xs)   = (decompress1 xs)+1

decompressxTimes :: String -> Int
decompressxTimes []       = 0
decompressxTimes ('(':xs) =  times * decompress1 (drop amountToDrop (xs))  where
                              amountToDrop = (length (takeWhile (/=')') xs)) +1
                              amounts = splitOn "x" (takeWhile (/=')') xs)
                              times = read (amounts !! 1) :: Int
