--Day6 part 1
import Data.List

main = do s<- readFile "inputday6.txt"
          print $ getMessagePart1 s
          print $ getMessagePart2 s

--get the fist message by making a frequency list and getting the char with 23 occurences
getMessagePart1 str =  concat $ map (map (fst)) $ map (filter (\(c,i) -> i==23)) $ getFreqList str

--Gets a frequenzy list of the strings
getFreqList :: String -> [[(Char,Int)]]
getFreqList str = map (map (\x -> (head x, length x)) ) $ map group  $ map (\x ->sort x) (transpose $ lines str)

--part2

--get the fist message by making a frequency list and getting the char with 21 occurences
getMessagePart2 str =  concat $ map (map (fst)) $ map (filter (\(c,i) -> i==21)) $ getFreqList str
