import Data.List.Split
import Data.List
import Data.Ord
import Data.Char
import Data.Maybe


--Day 4 part 1

parseInput :: String -> [String]
parseInput s = splitOn "\n" s

--parses the string to the wanted fromat
fetchFreqString :: String -> String
fetchFreqString s =  addAll $ take ((length (splitOn "-" s))-1) (splitOn "-" s) where
  addAll []     = []
  addAll (x:xs) = x++ addAll xs

--calculates the frequency of the characters, also sorts them with the most common one at the end
calculatefrequency :: String -> [(Char,Int)]
calculatefrequency s = calculatefrequency' s [] where
  calculatefrequency' [] l    = sortBy (comparing snd) l
  calculatefrequency' (x:xs) l = calculatefrequency' (filter (/=x) (x:xs)) ((addEle x s):l) where
    addEle x s =  (x,((length s) - length (filter (/=x) s)))

--sorts alfabethicly, only returns the 5 most common chars
sortByAlfa :: [(Char,Int)] -> [(Char,Int)]
sortByAlfa l = sortByAlfa' l [] where
  sortByAlfa' [] l = drop ((length l)-5) l
  sortByAlfa' l1 l2 = sortByAlfa' (drop (length sortByfreq) l1) (l2++(reverse $ sortBy (comparing fst) sortByfreq))  where
                          sortByfreq = (filter (\x -> snd x == snd (head l1) ) l1)

--checks if a row is valid
checkIfValid :: String -> [(Char,Int)] -> Maybe Int
checkIfValid str l = if succes str l then Just (read (parseForVal str) :: Int ) else Nothing where
  parseForVal str =  [ c | c<-str, isDigit c]
  succes str l = succes' (take 5 (drop ((length str)-6) str)) (reverse l) where
    succes' []  l = True
    succes' (y:ys) (x:xs) = y == fst x && succes' ys xs

--checks if all rows are valid
checkAllRows :: String -> Int
checkAllRows s =  checkAllRows' (parseInput s) 0 where
  checkAllRows' [] v = v
  checkAllRows' (x:xs) v | isJust value = checkAllRows' xs  (v+(fromJust value))
                         | otherwise    = checkAllRows' xs v  where
    value | length x >0 = checkIfValid x (sortByAlfa $ calculatefrequency $ fetchFreqString x)
          | otherwise   = Just 0

main :: IO ()
main = do s<- readFile "inputday4.txt"
          print $ checkAllRows s
          print $ decryptAll s

--part2

--gets all valid rows with their encrypted name and ID number
getValidRows :: String -> [(String,Int)]
getValidRows s = getValidRows' (delete "" (parseInput s)) [] where
  getValidRows' [] l = l
  getValidRows' (x:xs) l = if isJust value then getValidRows' xs ((x,(fromJust value)):l) else getValidRows' xs l where
    value = checkIfValid x (sortByAlfa $ calculatefrequency $ fetchFreqString x)

--decrypts a character geiven a storage ID
convertChar :: Char -> Int -> Char
convertChar c i = chr (((((ord c)-96) + i) `mod` 26) + 96)

--decrypts an entire row given a encrypted name and ID
decryptRow :: (String,Int) -> String
decryptRow (s,i) = decryptRow' str i where
  decryptRow' [] i       = []
  decryptRow' (' ':xs) i = ' ':decryptRow' xs i
  decryptRow' (x:xs) i   = (convertChar x i):decryptRow' xs i
  str = addAll $ take ((length (splitOn "-" s))-1) (splitOn "-" s) where
    addAll []     = []
    addAll (x:xs) = x++" "++ addAll xs

--decrypts all rows and filters out the rows taht dont start with "north"
decryptAll :: String -> [String]
decryptAll s = [ (decryptRow r) ++ show (snd r) | r<-getValidRows s, take 5 ((decryptRow r) ++ show (snd r)) == "north" ]





--
