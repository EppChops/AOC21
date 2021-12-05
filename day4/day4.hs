import System.IO
import Data.List
--import Data.Maybe
import Parsing
import Data.Char


data Cell = Drawn Integer | Undrawn Integer deriving(Eq, Show, Ord)
type Row = [Cell]

data Bingo = Bingo [Row]
    deriving (Eq, Show)

draws = [85,84,30,15,46,71,64,45,13,90,63,
        89,62,25,87,68,73,47,65,78,2,27,
        67,95,88,99,96,17,42,31,91,98,57,
        28,38,93,43,0,55,49,22,24,82,54,59,
        52,3,26,9,32,4,48,39,50,80,21,5,1,
        23,10,58,34,12,35,74,8,6,79,40,76,
        86,69,81,61,14,92,97,19,7,51,33,11,
        77,75,20,70,29,36,60,18,56,37,72,41,
        94,44,83,66,16,53]

tstBingo = "78 13  8 62 67\n42 89 97 16 65\n 5 12 73 50 56\n45 10 63 41 64\n49  1 95 71 17"


updateCell :: Cell -> Integer -> Cell
updateCell (Undrawn i) draw | i == draw = Drawn i
                            | otherwise = Undrawn i 
updateCell (Drawn i) draw = Drawn i

updateBingo :: Bingo -> Integer -> Bingo
updateBingo b draw = Bingo [[updateCell c draw | c <- row]
                            | row <- rows b] 

                            
findBingo :: [Bingo] -> [Integer] -> (Bingo, Integer)
--findBingo b []         = (,0)
findBingo b (d:draws) | or [isSolved bingo | bingo <- b] = (head $ filter (isSolved) b, d)
                      | otherwise = findBingo ([updateBingo bingo d | bingo <- b]) draws


findLastBingo :: [Bingo] -> [Integer] -> (Bingo, Integer)
findLastBingo b (d:draws) | and [isSolved bingo | bingo <- b] = (head $ filter (isSolved) b, d)
                          | otherwise = findBingo ([updateBingo bingo d | bingo <- b]) draws


printAns :: (Bingo, Integer) -> Integer
printAns (b, i) = sum [sum [getUndrawnCell c | c <- r] | r <- rows b] * 96

getUndrawnCell (Undrawn c)  =  c
getUndrawnCell (Drawn c)    = 0

rows :: Bingo -> [Row]
rows (Bingo bs) = bs

isSolved :: Bingo -> Bool
isSolved b = or [rowSolved row | row <- rows b] || or [rowSolved row | row <- transpose $ rows b]

rowSolved :: Row -> Bool
rowSolved row = and [c >= Drawn 0 && c < Drawn 100 | c <- row]

createBingo :: [[Integer]] -> Bingo
createBingo bb = Bingo $ (map) createCell bb

createCell :: [Integer] -> [Cell]
createCell ints = [Undrawn i | i <- ints]
    
number :: Parser Integer
number = do
    c <- readsP
    return c

integers :: Parser [Integer]
integers = do 
    chain number (oneOrMore $ char ' ')

parseBingo :: Parser [[Integer]]
parseBingo = do
    --chain integers (char '\n')
    i1 <- integers
    char '\n'
    i2 <- integers
    char '\n'
    i3 <- integers
    char '\n'
    i4 <- integers
    char '\n'
    i5 <- integers
    char '\n'
    return [i1, i2, i3, i4, i5] 

parseBingos :: Parser [[[Integer]]]
parseBingos = do
    chain parseBingo (char '\n')

readBingo :: String -> Maybe [[[Integer]]]
readBingo str = do
    (b, s) <- parse parseBingos str
    return b

inputBingo :: FilePath -> IO [Bingo]
inputBingo fileP = do
    input <- readFile fileP
    return [createBingo b | b <- fromJust $ readBingo input]
   -- return $ createBingo $ fromJust $ readBingo input

readAndSolveBingo = do
    bingo <- inputBingo "bingos.txt"
    print (findBingo bingo draws)
    print $ printAns $ (findBingo bingo draws)
    print $ findLastBingo bingo draws

printBingo (bingo, i) = "(" ++ show bingo ++ "," ++ show i ++ ")"

fromJust :: Maybe [[[Integer]]] -> [[[Integer]]]
fromJust (Just i) = i

fromJ :: Maybe ([[Integer]],String) -> [[Integer]]
fromJ (Just (b, s)) = b

double :: Parser Double
double = do
    d <- readsP 
    return d

