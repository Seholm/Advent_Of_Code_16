--day1 PART1
import Data.List
import Data.List.Split
import Data.Maybe

data DirectionDist = NORTH Integer| SOUTH Integer| EAST Integer| WEST Integer | NONE
    deriving (Show, Eq)

--makes a list of "instructions" from the input string, also parses spaces
listfrominput :: String -> [String]
listfrominput str = [ delete ' ' s | s<-splitOn "," str]

--creates a list of DirectionDist given a list of string "instructions"
parseInstructions :: [String] -> DirectionDist-> [DirectionDist]
parseInstructions []     _  = []
parseInstructions (s:ss) dir = (getdirection s dir):(parseInstructions ss (getdirection s dir))

--returns the next direction given a string instruction and precious direction
getdirection :: String -> DirectionDist -> DirectionDist
getdirection str (WEST _) = if str !! 0 == 'R' then NORTH (read $ drop 1 str ) else SOUTH (read $ drop 1 str ) --WEST
getdirection str (EAST _) = if str !! 0 == 'R' then SOUTH (read $ drop 1 str ) else NORTH (read $ drop 1 str ) --EAST
getdirection str (SOUTH _) = if str !! 0 == 'R' then WEST (read $ drop 1 str ) else EAST (read $ drop 1 str ) --SOUTH
getdirection str _ = if str !! 0 == 'R' then EAST (read $ drop 1 str ) else WEST (read $ drop 1 str ) --NORTH or NONE

--given a list of DirectionDist, it calculates the distance travelled in y and x idrection
calcUlateXandY :: [DirectionDist]-> (Integer,Integer) -> (Integer,Integer)
calcUlateXandY [] (y,x)             = (y,x)
calcUlateXandY ((NORTH n):xs) (y,x) = calcUlateXandY xs ((y+n),x)
calcUlateXandY ((SOUTH n):xs) (y,x) = calcUlateXandY xs ((y-n),x)
calcUlateXandY ((EAST n):xs) (y,x) = calcUlateXandY xs (y,(x+n))
calcUlateXandY ((WEST n):xs) (y,x) = calcUlateXandY xs (y,(x-n))

--solves the problem using the above functions
solve1 = abs (fst position) + abs (snd position) where
          position = calcUlateXandY (parseInstructions (listfrominput input) NONE) (0,0)



--input given from the problem
input = "R3, R1, R4, L4, R3, R1, R1, L3, L5, L5, L3, R1, R4, L2, L1, R3, L3, R2, R1, R1, L5, L2, L1, R2, L4, R1, L2, L4, R2, R2, L2, L4, L3, R1, R4, R3, L1, R1, L5, R4, L2, R185, L2, R4, R49, L3, L4, R5, R1, R1, L1, L1, R2, L1, L4, R4, R5, R4, L3, L5, R1, R71, L1, R1, R186, L5, L2, R5, R4, R1, L5, L2, R3, R2, R5, R5, R4, R1, R4, R2, L1, R4, L1, L4, L5, L4, R4, R5, R1, L2, L4, L1, L5, L3, L5, R2, L5, R4, L4, R3, R3, R1, R4, L1, L2, R2, L1, R4, R2, R2, R5, R2, R5, L1, R1, L4, R5, R4, R2, R4, L5, R3, R2, R5, R3, L3, L5, L4, L3, L2, L2, R3, R2, L1, L1, L5, R1, L3, R3, R4, R5, L3, L5, R1, L3, L5, L5, L2, R1, L3, L1, L3, R4, L1, R3, L2, L2, R3, R3, R4, R4, R1, L4, R1, L5"

--PART 2

solve2 = abs (fst $ fromJust position) + abs (snd $ fromJust position) where
    position = stopWhenTwice (parseInstructions (listfrominput input) NONE) [(0,0)] []

--finds when we have visited the same position twice and returns that position, or nothing if not found
stopWhenTwice :: [DirectionDist] -> [(Integer,Integer)] -> [(Integer, Integer)] -> Maybe (Integer,Integer)
stopWhenTwice  [] _ _   =  Nothing
stopWhenTwice (d:ds) l1 l2 =  if length existsIn >0 then Just (head existsIn) else stopWhenTwice ds (newPos:l1) (newPoses++l2) where
                              newPos = calcUlateXandY [d] (head l1)
                              newPoses = calculatePositions newPos (head l1)
                              existsIn = [ ele | ele<-newPoses, ele `elem` l2]

--calculates the positions bewteen two positions
calculatePositions :: (Integer,Integer) -> (Integer,Integer) -> [(Integer,Integer)]
calculatePositions (y1,x1) (y2,x2) | abs (y1-y2) /=0 = if y1< y2 then [(y,x1) | y<- [y1..(y2-1)]] else [(y,x1) | y<- [(y2+1)..y1]] --moved in y direction
                                   | otherwise       = if x1< x2 then [(y1,x) | x<- [x1..(x2-1)]] else [(y1,x) | x<- [(x2+1)..x1]] --moved in x direction
