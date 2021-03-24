-- Exercise 1
fact :: Int -> Int
fact n 
    | n < 0         = error "Can't get factorial of a negative number"
    | otherwise     = product [1..n]

-- Exercise 3
euclid :: Int -> Int -> Int
euclid a b 
    | a == b        = a
    | a < b         = euclid a (b-a)
    | otherwise     = euclid b (a-b)

-- Exercise 4
sumDiff :: Num a => [a] -> a 
sumDiff [] = 0
sumDiff (x:xs) = x + sumDiff xs

takeDiff :: Int -> [a] -> [a]
takeDiff _ []           = error "Empty list"
takeDiff n (x:xs) 
    | n == 0            = []
    | n < 0             = error "Can't select negative number of elements"
    | otherwise         = [x] ++ takeDiff (n-1) xs

lastDiff :: [a] -> a
lastDiff (x:xs)
    | length (x:xs) == 1        = x
    | otherwise                 = lastDiff xs

-- Exercise 5
zipDiff :: [a] -> [b] -> [(a, b)]
zipDiff _ []            = []
zipDiff [] _            = []
zipDiff (x:xs) (y:ys)   = [(x, y)] ++ zipDiff xs ys