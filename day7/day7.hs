--day7 part 1

import Data.List.Split
import Data.List

main = do input <- readFile "day7input.txt"
          print $ checkAllStrings input
          print $ checkAllStringsSSL input

checkAllStrings :: String -> Int
checkAllStrings str = length $ filter (==True) (map checkValidString (lines str))

checkValidString :: String -> Bool
checkValidString str = not (or (map checkForAbba (getWithinBrackets str odd))) && or (map checkForAbba (getWithinBrackets str even))

--gets the valid / invalid parts of the string, pass even to get the valid, and odd to get the invalid (then part within [])
getWithinBrackets :: String -> (Int -> Bool) -> [String]
getWithinBrackets str f = [ (splitOneOf "[]" str) !! x |x<-[0..(length ((splitOneOf "[]" str))-1)], f x]

--checks if a string contains an ABBA
checkForAbba :: String -> Bool
checkForAbba []     = False
checkForAbba (x:xs) = (length (take 4 (x:xs)) == 4 && take 4 (x:xs) !! 0 == take 4 (x:xs) !! 3 &&
                        take 4 (x:xs) !! 1 == take 4 (x:xs) !! 2) && take 4 (x:xs) !! 0 /= take 4 (x:xs) !! 1  || checkForAbba xs

--part2
checkAllStringsSSL :: String -> Int
checkAllStringsSSL str = length $ filter (==True) (map checkValidStringSSL (lines str))

checkValidStringSSL :: String -> Bool
checkValidStringSSL str =  containsBabs (constructBAB $ ( concat $ map constructABA $ getWithinBrackets str even)) (getWithinBrackets str odd)

--checks if any string in a list of strings contains any of the substrings given in the second list
containsBabs :: [String] -> [String] -> Bool
containsBabs []      _   = False
containsBabs (x:xs) strs =  or (map (x `isInfixOf`) strs) || (containsBabs xs strs)

--takes  a list of ABA's and constructs BAB's
constructBAB :: [String] -> [String]
constructBAB []     = []
constructBAB (x:xs) = [(x !! 1),(x !! 0),(x !! 1)]:(constructBAB xs)

--constructs ABA's out of a string
constructABA :: String -> [String]
constructABA []     = []
constructABA (x:xs) | length (take 3 (x:xs)) == 3 && take 3 (x:xs) !! 0 == take 3 (x:xs) !! 2 && take 3 (x:xs) !! 0 /= take 4 (x:xs) !! 1
                               = (take 3 (x:xs)):(constructABA xs)
                   | otherwise = constructABA xs
