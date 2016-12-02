import Data.Char

directions = "R1, R3, L2, L5, L2, L1, R3, L4, R2, L2, L4, R2, L1, R1, L2, R3, L1, L4, R2, L5, R3, R4, L1, R2, L1, R3, L4, R5, L4, L5, R5, L3, R2, L3, L3, R1, R3, L4, R2, R5, L4, R1, L1, L1, R5, L2, R1, L2, R188, L5, L3, R5, R1, L2, L4, R3, R5, L3, R3, R45, L4, R4, R72, R2, R3, L1, R1, L1, L1, R192, L1, L1, L1, L4, R1, L2, L5, L3, R5, L3, R3, L4, L3, R1, R4, L2, R2, R3, L5, R3, L1, R1, R4, L2, L3, R1, R3, L4, L3, L4, L2, L2, R1, R3, L5, L1, R4, R2, L4, L1, R3, R3, R1, L5, L2, R4, R4, R2, R1, R5, R5, L4, L1, R5, R3, R4, R5, R3, L1, L2, L4, R1, R4, R5, L2, L3, R4, L4, R2, L2, L4, L2, R5, R1, R4, R3, R5, L4, L4, L5, L5, R3, R4, L1, L3, R2, L2, R1, L3, L5, R5, R5, R3, L4, L2, R4, R5, R1, R4, L3,"

stringToInt :: String -> Int
stringToInt s = sum [ (digitToInt x)*(10^i) | (x,i) <- (zip (reverse s) [0..])]

dStringToCouples :: String -> [(Char,Int)]
dStringToCouples s = [ (x,(stringToInt (init y))) | (x:y) <- (words s)]

data Direction = North | East | South | West

walk :: (Int,Int) -> Direction -> Int -> (Int,Int)
walk (x,y) North step   = ( ( x + step) , y )
walk (x,y) East step    = ( x , ( y + step ))
walk (x,y) South step   = ( ( x - step) , y )
walk (x,y) West step    = ( x , ( y - step ))


turn :: Direction -> Char -> Direction
turn North 'R' = East
turn North 'L' = West
turn East  'R' = South
turn East  'L' = North
turn South 'R' = West
turn South 'L' = East
turn West  'R' = North
turn West  'L' = South

execute :: (Int,Int) -> Direction -> [(Char,Int)] -> (Int,Int)
execute (x,y) _ []     = (x,y)
execute (x,y) d ((t,s):cs) = execute (walk (x,y) (turn d t) s) (turn d t) cs 

findFirstDouble :: (Int,Int) -> Direction -> [(Char,Int)] -> [(Int,Int)] -> (Int,Int)
findFirstDouble _ _ [] _ = (0,0)
findFirstDouble (x,y) d ((t,0):cs) ls = findFirstDouble (x,y) (turn d (fst (head cs))) (cs) ls
findFirstDouble (x,y) d ((t,s):cs) ls | (x,y) `elem` ls = (x,y)
                                      | otherwise       = findFirstDouble (walk (x,y) d 1) d ((t,(s-1)):cs) ((x,y):ls)
