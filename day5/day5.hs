
import Data.Hash.MD5
import Data.List
import Data.Ord

input = "ugkcyxxp"

main :: IO ()
main = do --print $ getSixth $ fetchHashes
          print $ getSeventh $ sortBy(comparing snd) (part2 0 [])

--fetches the sixth element for part2
getSeventh  l = [ (fst x)!!6 | x<-l]

--fetches the fifth element for part1
getSixth  l = [ x!!5 | x<-l]

--fetches all the hashes for part1
fetchHashes =  take 8 $ filter (\x -> (take 5 x) == "00000") $ map md5s $ map Str $ map (input++) (map show [0..])


--solvers part2
part2 n l | length l == 8 = l
          | (take 5 (md5s $ Str (input++ (show n)))) == "00000" = part2 (n+1) (addHashToList l (md5s( Str (input++ (show n)))))
          | otherwise = part2 (n+1) l

--adds a hash to a list if that position isn't already occupied by another hash
addHashToList :: [(String,Char)] -> String -> [(String,Char)]
addHashToList l str = if length (filter ((\x -> (snd x) == (str!!5))) l) == 0 && (str!!5)
                      `elem` ['0','1','2','3','4','5','6','7'] then (str,(str!!5)):l else l
