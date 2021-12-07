import Data.List.Split
import qualified Data.HashMap as HM

testInput = "0,9 -> 5,9\n8,0 -> 0,8\n9,4 -> 3,4\n2,2 -> 2,1\n7,0 -> 7,4\n6,4 -> 2,0\n0,9 -> 2,9\n3,4 -> 1,4\n0,0 -> 8,8\n5,5 -> 8,2"

type Point = (Int, Int)
type Line = (Point, Point)

ptsMap = HM.empty

sol :: HM.Map Point Int -> Int
sol m = length [ i |i <- HM.elems m, i > 1 ]

readAndSolve fileP = do
    input <- readFile fileP
    print $ sol $ isect $ map readLine (lines input)

isect :: [Line] -> HM.Map Point Int 
isect ls = updatePoints $ concatMap points ls

updatePoints :: [Point] -> HM.Map Point Int
updatePoints []      = HM.empty
updatePoints (p:pts) = HM.insertWith (\a b -> a + b) p 1 $ updatePoints pts

points :: Line -> [Point]
points ((x1, y1), (x2,y2))
    | (x1,y1) == (x2,y2) = [(x2,y2)]
    | otherwise = [(x1, y1)] ++ points ((x1 + signum (x2 - x1), y1 + signum (y2 - y1)),(x2, y2))

points1 :: Line -> [Point]
points1 ((x1, y1), (x2,y2)) 
    | (x1,y1) == (x2,y2) = [(x2,y2)]
    | y1 == y2 || x1 == x2 = [(x1, y1)] ++ points ((x1 + signum (x2 - x1), y1 + signum (y2 - y1)),(x2, y2))
    | otherwise = []
                  
readLine :: String -> Line
readLine str = (p1, p2)
    where (p1, p2) = ([readPoint p | p <- (splitOn " -> " str)] !! 0, [readPoint p | p <- (splitOn " -> " str)] !! 1)

readPoint :: String -> (Int, Int)
readPoint str = (i1, i2)
        where (i1, i2) = (read ((splitOn "," str) !! 0) :: Int, read ((splitOn "," str) !! 1) :: Int)