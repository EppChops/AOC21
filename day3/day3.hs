import Data.List

readAndPrint :: FilePath -> IO()
readAndPrint fileP = do
    input <- readFile fileP
    print $ printGamma $ helper input
    print $ printEpsilon $ helper input
    print $ helper input
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


