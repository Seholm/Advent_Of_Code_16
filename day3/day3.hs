--Day3 part 1
import Data.List.Split
import Text.Read
import Data.List

--parse input into a list of tuple of the sides and check if valid triangle
parseInput1 :: String -> [Bool]
parseInput1 s = nubBy (\ x y -> y==False) [ checkValid [ x |x <- (splitOn " " l) , x /= "" ] | l<-(splitOn "\n" s) ]

--checks if a triangle is valid or not by checking the sides
checkValid :: [String] -> Bool
checkValid (x1:x2:x3:xs) = (read x1) + (read x2) > (read x3) && (read x1) + (read x3) > (read x2) &&
                                  (read x2) + (read x3) > (read x1)
checkValid l = False

main1 :: IO()
main1 = do s<- readFile "inputday3.txt"
           print $ length (parseInput1 s)


--Part2

--parses the input into the wanted format
parseInput2 :: String -> [[String]]
parseInput2 s = [[ x |x <- (splitOn " " l) , x /= "" ] | l<-(splitOn "\n" s) ]

--transposes a "three block" to get the wanted columns as rows
makeColsToRows :: [[String]] -> [[String]]
makeColsToRows [] = []
makeColsToRows l = (transpose (take 3 l))++(makeColsToRows (drop 3 l))

--checks if each row is a valid triangle
checkValidTri :: [[String]] -> [[String]]
checkValidTri l = [ x | x<-l, checkValid x]

main2 :: IO()
main2 = do s<- readFile "inputday3.txt"
           print $ length $ checkValidTri $ makeColsToRows $ parseInput2 s
