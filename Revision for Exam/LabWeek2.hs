import Data.String ()
import Data.Char(toUpper)

type Point = (Int, Int)
data Shape = Circle Float | Rect Float Float deriving Show

-- Exercise 1
n :: Int
n = a `div` length xs
    where
    a = 10
    xs = [1, 2, 3, 4, 5]

-- Exercise 2
second :: [a] -> a
second xs = head (tail xs)

swap :: (b, a) -> (a, b)
swap (x,y) = (y,x)

pair :: a -> b -> (a, b)
pair x y = (x,y)

double :: Num a => a -> a   -- could be Int or Float
double x = x*2

palin :: Eq a => [a] -> Bool
palin xs = reverse xs == xs

twice :: (t -> t) -> t -> t
twice f x = f (f x)

-- Exercise 3
halve :: [a] -> ([a], [a])
halve list = (take half list, drop half list)
    where
    half = length list `div` 2

-- Exercise 4
euclideanDistance :: Point -> Point -> Double
euclideanDistance (x1, x2) (y1, y2) =
    sqrt (fromIntegral xPower + fromIntegral yPower)
    where
        xPower = (x2-x1) * (x2-x1)
        yPower = (y2-y1) * (y2-y1)

-- Exercise 5
firstWord :: String -> String
firstWord [] = []
firstWord (first:second:xs) 
    | first == ' '                      = firstWord (second:xs)
    | first /= ' ' && second == ' '     = [first]
    | first /= ' '                      = [first] ++ firstWord (second:xs)

-- Exercise 6
safeHead :: [a] -> a
safeHead (x:xs) = x

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (x:xs) = xs

-- Exercise 7
stack :: [a] -> [a]
stack (x:xs) = xs ++ [x]

range :: Ord a => Num a => a -> Bool
range a
    | a >= 0 && a <= 10      = True
    | otherwise              = False

addc :: Char -> String -> String 
addc char string = [char] ++ string

halves :: Integral a => Num a => [a] -> [a]
halves x = map (`div` 2) x

capitalizeStart :: String -> String 
capitalizeStart (x:xs) = [toUpper x] ++ xs

-- Exercise 8
modifyList :: Integral a => [a] -> [a]
modifyList a = map (^2) (filter even a)

-- Exercise 9
-- This function (\ (_:xs) -> xs) is equal to tail

-- Exercise 12
scale :: Float -> Shape -> Shape
-- For circle
scale scaler (Circle x) = Circle scaled
    where 
        scaled = x * scaler
-- For rectangle
scale scaler (Rect x y) = Rect xScaled yScaled
    where 
        xScaled = x * scaler
        yScaled = y * scaler

-- Exercise 13
luhnDouble :: Int -> Int
luhnDouble number 
    | number*2>9        = number*2-9
    | otherwise         = number*2
luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d 
    | (luhnDouble a + luhnDouble b + luhnDouble c + luhnDouble d) `rem` 10 == 0     = True
    | otherwise                                                                     = False