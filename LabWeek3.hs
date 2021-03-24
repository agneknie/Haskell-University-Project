module LabWeek3 where

euclid :: Int -> Int -> Int
euclid a b | b < a       = euclid b a
           | a == b      = a
           | otherwise   = euclid a (b-a)

-- Summing the elements of the list
sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs

-- Taking the first n elements of the list
takeList :: Int -> [a] -> [a]
takeList n [] = []
takeList 1 (x:xs) = [x]
takeList n (x:xs) = [x] ++ takeList (n-1) xs

-- Taking the last element of the list
lastList :: [a] -> a
lastList [] = error "empty list"
lastList [x] = x
lastList (x:xs) = lastList xs

-- Form a list of tuples from two provided lists
zipLists :: [a] -> [b] -> [(a, b)]
zipLists [] a = []
zipLists a [] = []
zipLists (x:xs) (y:ys) = [(x, y)] ++ zipLists xs ys
