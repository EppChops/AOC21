import Data.List.Split

readSol :: FilePath -> IO ()
readSol fileP = do
    input <- readFile fileP
    print $ count $ concat $ map (words) $ readOut $ lines $ input



setVariables (s:ss) | length s == 2 = 

getOne :: String -> [Char]
getOne str  | length str == 2 = str
            | otherwise = []

getFour :: String -> [Char]
getFour str | length str == 4 = str
            | otherwise = []

getSeven :: String -> [Char]
getSeven str | length str == 3 = str
            | otherwise = []

getEight :: String -> [Char]
getEight str | length str == 8 = str
            | otherwise = []

getThree :: String -> String -> [Char]
getThree one all = filter (\s -> s `elem` one) all

topMidBot :: String -> String -> [Char]
topMidBot one three = filter (\c -> c `elem` one) three

top :: String -> String -> [Char]
top seven one = filter (\c -> c `elem` one) seven

mid :: String -> String -> String -> [Char]
mid topMidBot top four = filter (\c -> c `elem` midBot) four
    where midBot = filter (\c -> not (elem c top)) topMidBot

getZero :: String -> String -> [Char]
getZero mid eight = filter (\c -> not (elem c mid)) eight

topLeft :: String -> String -> [Char]
topLeft four three = filter (\c -> not (elem c four)) same
    where same = filter (\c -> c `elem` four) three

--Input has length 5 
getFive :: String -> String -> [Char]
getFive topLeft input   | head topLeft `elem` input = input
                        | otherwise = []

getNine :: String -> String -> [Char]
getNine three topLeft = three ++ topLeft

getTwo :: String -> String -> [Char]
getTwo five all = (filter (\c -> not (elem c five) all)) ++ (topMidBot one three)

topRight :: String -> String -> [Char]
topRight four five = filter (\c -> not (elem c five)) four

getSix :: String -> String -> [Char]
getSix topRight eight = [c | c <- eight, c not $ head topRight]



readOut :: [String] -> [String]
readOut []        = []
readOut (s:ss)    = [splitOn " | " s !! 1] ++ readOut ss

count :: [String] -> Int
count []        = 0
count (x:xs)    | len < 5 && len > 1 || len == 7 = 1 + count xs
                | otherwise = count xs
                where len = length x
