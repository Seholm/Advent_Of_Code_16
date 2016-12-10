--Day 2 PART 1
import Data.List.Split
import Data.List
import Data.Char

--move one step given one instruction and a number, return a Number
oneStep :: Char -> Char -> Int
oneStep 'R' n | n `elem` ['3','6','9'] = digitToInt n
              | otherwise = (digitToInt n)+1
oneStep 'L' n | n `elem` ['1','4','7'] = digitToInt n
              | otherwise = (digitToInt n)-1
oneStep 'U' n | n `elem` ['1','2','3'] = digitToInt n
              | otherwise = (digitToInt n)-3
oneStep 'D' n | n `elem` ['7','8','9'] = digitToInt n
              | otherwise = (digitToInt n)+3

--move all the steps for one line, return final number
steps :: String -> (Char -> Char -> Int) -> Int
steps s f = steprek s 5 f where
  steprek [] n _ = n
  steprek (c:cs) n f = steprek cs  (f c (intToDigit n)) f


--calculate all 5 numbers
main1 :: IO ()
main1 = do str<- readFile "./inputday2"
           print $ take 5 [ steps ele oneStep |ele<- (splitOn "\n" str)]



--PART 2

oneStep2 :: Char -> Char -> Int
oneStep2 'R' n | n `elem` ['1','4','9','C','D','c','d'] = digitToInt n
               | otherwise = (digitToInt n)+1
oneStep2 'L' n | n `elem` ['1','2','5','A','D','a','d'] = digitToInt n
               | otherwise = (digitToInt n)-1
oneStep2 'U' n | n `elem` ['1','2','4','5','9'] = digitToInt n
               | n `elem` ['3','D','d'] = (digitToInt n)-2
               | otherwise = (digitToInt n)-4
oneStep2 'D' n | n `elem` ['5','9','A','C','D','a','c','d'] = digitToInt n
               | n `elem` ['1','B','b'] = (digitToInt n)+2
               | otherwise = (digitToInt n)+4

--calculate all 5 numbers
main2 :: IO ()
main2 = do str<- readFile "./inputday2"
           print $ take 5 [ intToDigit $ steps ele oneStep2 |ele<- (splitOn "\n" str)]
