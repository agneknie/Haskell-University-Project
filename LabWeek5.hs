module LabWeek5 where
    -- Implementation of Prelude maximum function
    myMaximum :: [Int] -> Int
    myMaximum [x] = x
    myMaximum (x:y:xys) | x > y        = myMaximum ([x] ++ xys)
                        | otherwise    = myMaximum ([y] ++ xys) 
    
    -- Exercise 4, part a
    ex4a :: [Int] -> Int -> [Int]
    ex4a [] n = []
    ex4a xs n = [x+n | x <- xs]

    -- Exercise 4, part b
    ex4b :: [Int] -> Int -> [Int]
    ex4b [] n = []
    ex4b xs n = [x | x <- xs, x>n]

