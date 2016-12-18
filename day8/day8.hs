--day8 part 1

import Data.List
import Data.List.Split

--Part 2 is done manually
main = do x<- readFile "day8input.txt"
          print $ length $ filter (==1) (concat (doInstructions (lines x) newEmpty))
          print $ map (drop 45) (map (take 50) (doInstructions (lines x) newEmpty))


--parses the instructions and call for the right method
doInstructions :: [String] -> [[Int]] -> [[Int]]
doInstructions [] l = l
doInstructions (x:xs) l = doInstructions xs (doInstructions' x l) where
  doInstructions' ('r':'e':'c':'t':' ':xs) l = turnOn l (read((splitOn "x" xs) !! 1) :: Int) (read((splitOn "x" xs) !! 0) :: Int)
  doInstructions' ('r':'o':'t':'a':'t':'e':' ':'r':xs) l = rotateXRows l (read (head (splitOn " " ((splitOn "=" xs)!!1))):: Int)
                                                           (read (last (splitOn " " ((splitOn "=" xs)!! 1)))::Int)
  doInstructions' ('r':'o':'t':'a':'t':'e':' ':'c':xs) l = rotateXCols  l (read (head (splitOn " " ((splitOn "=" xs)!!1))))
                                                            (read (last (splitOn " " ((splitOn "=" xs)!!1))))


rotateXRows :: [[Int]] -> Int -> Int -> [[Int]]
rotateXRows l i 0 = l
rotateXRows l i x = rotateXRows (rotateRowOnce l i) i (x-1)

rotateXCols :: [[Int]] -> Int -> Int -> [[Int]]
rotateXCols l i 0 = l
rotateXCols l i x = rotateXCols (rotateColOnce (transpose l) i) i (x-1)

newEmpty :: [[Int]]
newEmpty = replicate 6 ((replicate 50 0))

turnOn :: [[Int]] -> Int -> Int -> [[Int]]
turnOn l r c = ((map ((replicate c 1)++) (map (drop c) (take r l)))) ++ (drop r l)

--input is a transposed list and which column to rotate
rotateColOnce :: [[Int]] -> Int -> [[Int]]
rotateColOnce l i = transpose $ (take i l)++[(rotateColOnce' (l !! i))]++(drop (i+1) l) where
  rotateColOnce' l = (last l):[ l !! (x-1)  |x<-[1..5]]

--input is which row to rotate
rotateRowOnce :: [[Int]] -> Int -> [[Int]]
rotateRowOnce l i =  (take i l)++[(rotateRowOnce' (l !! i))]++(drop (i+1) l) where
  rotateRowOnce' l = (last l):[ l !! (x-1)  |x<-[1..49]]
