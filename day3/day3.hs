import Data.List

readAndPrint :: FilePath -> IO()
readAndPrint fileP = do
    input <- readFile fileP
    print $ printGamma $ helper input
    print $ printEpsilon $ helper input
    print $ helper input
    print $ getLargest (help2 (lines input)) (makeTuple (lines input))
    print $ getSmallest (help2 (lines input)) (makeTuple (lines input))
    --print (transpose (lines input))

helper m = commonBitOccurance $ transpose $ lines m

countOnes :: [Char] -> Int
countOnes ('1':xs)  = countOnes xs + 1
countOnes (_:xs)    = countOnes xs
countOnes []        = 0

countZeros :: [Char] -> Int
countZeros ('0':xs) = 1 + countZeros xs
countZeros (_:xs)   = countZeros xs
countZeros []       = 0

commonBitOccurance :: [[Char]] -> [(Int, Int)]
commonBitOccurance m = (zip (map (countOnes) m) (map (countZeros) m))

printGamma :: [(Int, Int)] -> String
printGamma []                   = ""
printGamma ((a,b):abs) | a > b = "0" ++ printGamma abs
                        | otherwise = "1" ++ printGamma abs

printEpsilon :: [(Int, Int)] -> String
printEpsilon []         = ""
printEpsilon ((a,b):abs) | a > b = "1" ++ printEpsilon abs
                        | otherwise = "0" ++ printEpsilon abs

saveMostOcc :: [(Int, Int)] -> [[Char]] -> [Char]
saveMostOcc [] _            = []
--saveMostOcc ((a,b):abs) m   | a > b = 


-------------------------PART 2-----------------------------------
makeTuple :: [String] -> [(String, String)]
makeTuple m = [("", str) | str <- m]

help2 [] = (0,0)
help2 m  = head $ commonBitOccurance $ transpose m

getLargest :: (Int, Int) -> [(String, String)] -> [(String, String)]
getLargest abs   []              = []
getLargest (0,0)    strings        = strings
getLargest (a,b) ((s1,s2):ss)   | a > b = getLargest (help2 (keepRight (helperGetL ((s1,s2):ss) '1')))(helperGetL ((s1,s2):ss) '1')
                                | otherwise = getLargest (help2 (keepRight (helperGetL ((s1,s2):ss) '0')))(helperGetL ((s1,s2):ss) '0')

helperGetL strings c = moveHeadLeft (filter (\(s1,s2) -> head s2 == c) strings)

moveHeadLeft :: [(String, String)] -> [(String, String)]
moveHeadLeft []             = []
moveHeadLeft ((s1, []):ss)  = ss
moveHeadLeft ss = [(s1 ++ [head s2], tail s2) | (s1,s2) <- ss]

keepRight :: [(String, String)] -> [String]
keepRight []            = []
keepRight ((s1, []):ss) = []
keepRight ((s1,s2):ss)  = [s2] ++ keepRight ss



getSmallest :: (Int, Int) -> [(String, String)] -> [(String, String)]
getSmallest (0,0)    strings        = strings
getSmallest _       [(s1,s2)]       = [(s1,s2)]
getSmallest (a,b) ((s1,s2):ss)  | a < b = getSmallest (help2 (keepRight (helperGetL ((s1,s2):ss) '1')))(helperGetL ((s1,s2):ss) '1')
                                | otherwise = getSmallest (help2 (keepRight (helperGetL ((s1,s2):ss) '0')))(helperGetL ((s1,s2):ss) '0')