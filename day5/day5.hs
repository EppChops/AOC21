import Data.List.Split

testInput = "0,9 -> 5,9\n8,0 -> 0,8\n9,4 -> 3,4\n2,2 -> 2,1\n7,0 -> 7,4\n6,4 -> 2,0\n0,9 -> 2,9\n3,4 -> 1,4\n0,0 -> 8,8\n5,5 -> 8,2"

type Point = (Int, Int)
type Line = (Point, Point)

checkIsect :: [Line] -> [(Point, Int)]
checkIsect (l:ls) = undefined

updatePoints :: [Point] -> [(Point, Int)]
updatePoints (p:pts) = undefined2

points :: Line -> [Point]
points ((x1, y1), (x2,y2)) 
    | (x1,y1) == (x2,y2) = [(x2,y2)]
    | y1 == y2 || x1 == x2 = [(x1, y1)] ++ points ((x1 + signum (x2 - x1), y1 + signum (y2 - y1)),(x2, y2))
                  
readLine :: String -> Line
readLine str = (p1, p2)
    where (p1, p2) = ([readPoint p | p <- (splitOn " -> " str)] !! 0, [readPoint p | p <- (splitOn " -> " str)] !! 1)

readPoint :: String -> (Int, Int)
readPoint str = (i1, i2)
        where (i1, i2) = (read ((splitOn "," str) !! 0) :: Int, read ((splitOn "," str) !! 1) :: Int)