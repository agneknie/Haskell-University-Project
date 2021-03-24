import Data.Char (ord, isLower, chr)
import Data.String()

-- Functions taken from lectures, needed for exercises
let2int :: Char -> Int 
let2int c = ord c - ord 'a'

int2let :: Int -> Char 
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26) | otherwise = c

-- Exercise 1
shiftMap :: String -> Int -> String
shiftMap s n = map (shift n) s

shiftList :: String -> Int -> String
shiftList s n = [shift n x | x <- s]

shiftRecursion :: String -> Int -> String
shiftRecursion [] _ = []
shiftRecursion (x:xs) n = shift n x : shiftRecursion xs n
              -- Same as [shift n x] ++ shiftRecursion xs n

-- Exercise 2
studentsClassified :: [(String, Int)] -> (Int, Int, Int, Int, Int, Int)
studentsClassified tuples = (count s30, count s40, count s50, 
                            count s60, count s70, count s100)
    where
        s30 n = n < 30
        s40 n = n >= 30 && n < 40
        s50 n = n >= 40 && n < 50
        s60 n = n >= 50 && n < 60
        s70 n = n >= 60 && n < 70
        s100 n = n >= 70
        count filter = length [student | student <- tuples, filter (snd student)]

-- Exercise 3
grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x, y) | x <- [0..m], y <- [0..n]]

-- Exercise 4
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `rem` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n-1], sumOfFactors x == x]
    where
    -- factors includes the number itself, so it's subtracted from the sum
    sumOfFactors x = sum (factors x) - x